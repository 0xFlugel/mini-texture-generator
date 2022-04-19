//! This project is a tool to create textures by generating with runtime-parameters in a GUI.
//! It is inspired by ".werkzeug" by the group ".farbrausch", but should have a more beautiful GUI.
//!
//! # Implementation Details
//!
//! The generator pipeline is defined by its elements which are in turn defined in the ECS as
//! UI-nodes with the components: [Effect], [InputConnector]s and [OutputConnector]s.
//!
//! The connectors are start and end points for [Connection]s which connect exactly one input to one
//! output connector. A connection has a [Color] that is defined by its starting point, the output
//! connector id. Thus, all connections that transmit the same value have the same color. TODO
//!
//! A texture is generated based on a consuming [Effect], i.e. one that has no output.
//! [InputConnector]s without a [Connection] will assume a value of zero.

mod connection_management;
mod interaction;
mod math;
mod persistence;
mod text_entry;
mod util;

use crate::connection_management::{delete_connection, Connection, ConnectionAttachment};
use crate::interaction::{InteractionPlugin, Scroll};
use crate::math::SimplexSampler;
use crate::text_entry::{TextEntryPlugin, TextValue, ValueBinding};
use crate::util::{create_pipeline_element, delete_pipeline_element, gen_colors};
use bevy::core::FixedTimestep;
use bevy::ecs::event::Events;
use bevy::math::XY;
use bevy::prelude::*;
use bevy::render::render_resource::TextureFormat;
use bevy::sprite::Mesh2dHandle;
use bevy::transform::TransformSystem;
use bevy::window::WindowCloseRequested;
use bevy_mod_raycast::{DefaultPluginState, RayCastMesh, RayCastSource};
use clap::{Parser, ValueHint};
use connection_management::{InputConnector, InputConnectors, OutputConnector, OutputConnectors};
use interaction::{Draggable, Dragging, MousePosition, MyInteraction, MyRaycastSet};
use std::collections::HashMap;
use std::f32::consts::PI;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufWriter, Write};
use std::mem::size_of;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::sync::Mutex;

const SIDEBAR_BACKGROUND: [f32; 3] = [0.5, 0.5, 0.5];
/// The width of the sidebar in normalized coords (-1..1).
const SIDEBAR_WIDTH: f32 = 0.2;

/// The relative path after "/assets" in the project folder -- which containts the Cargo.toml.
// const FONT_FILENAME: &'static str = "FiraSans-Bold.ttf";
const FONT_FILENAME: &str = "Roboto-Regular.ttf";
/// Text size, high enough to have a acceptable render quality.
const FONT_SIZE: f32 = 15.0;
/// The line height of pipeline elements.
///
/// Specifies how much space is used per "line" in the rectangle. This must practically be greater
/// than 1.0 to accommodate the decorations of text entry boxes.
const LINE_HEIGHT: f32 = 1.25 * FONT_SIZE;
/// Size of the square shape for input and output connectors.
const IO_PAD_SIZE: f32 = 2. / 3. * LINE_HEIGHT;
/// The scaling factor for highlighting connector drop off points on hovering.
const HIGHLIGHT_SCALING: f32 = 1.5;

/// Number of pixels in each direction of the 2D texture.
const DEFAULT_TEXTURE_SIZE: Size<u32> = Size {
    width: 16,
    height: 16,
};
/// A multiplier to the OS scroll distance for vertical scrolling.
const SCROLL_MULTIPLIER: f32 = -3.0;
//// A multiplier for stepping through scale factors in the main view.
const SCALE_FACTOR: f32 = 1.2;

/// Global data defines. Used to make `create_image_entity` work with the same allocation size and
/// interpretaion as `update_teture`.
type PixelData = [f32; 4];
/// GPU texture format. Data is exported as RGB as `[u8; 3]`, without the alpha component.
const TEXTURE_FORMAT: TextureFormat = TextureFormat::Rgba32Float;

//TODO This is specific to my GPU architecture, I think. I have a Radeon, though. So little-endian
// may be correct for Intel (integrated), AMD and NVidia GPUs. Refrence:
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#hardware-implementation
const LOCAL_TO_GPU_BYTE_ORDER: &dyn Fn(f32) -> [u8; 4] = &f32::to_le_bytes;
const GPU_TO_LOCAL_BYTE_ORDER: &dyn Fn([u8; 4]) -> f32 = &f32::from_le_bytes;

//TODO Turn inserting multiple components on new entities into bundels for better readability.

#[derive(Debug, Clone, Parser)]
struct Args {
    /// The file to load from.
    #[clap(value_hint = ValueHint::AnyPath)]
    file: Option<PathBuf>,
    /// Number of minutes between auto-saves. Deactivated when not set.
    #[clap(long, short)]
    autosave: Option<f32>,
    /// The file to save to when auto-saving and when closing the program.
    #[clap(long, short, default_value = "autosave.ron", value_hint = ValueHint::FilePath)]
    save_to: PathBuf,
}

fn main() {
    let args: Args = Args::parse();
    let mut app = App::new();
    // setup for loading and saving.
    app.insert_resource(args.clone())
        .init_resource::<Events<MetaEvent>>()
        .add_system(persistence::load_from_file)
        .add_system(persistence::connect_loaded_effects)
        .add_system(shutdown)
        .add_system(persistence::save_to_file.exclusive_system().at_end());
    if let Some(minutes) = args.autosave {
        app.add_system_set(
            SystemSet::new()
                .with_system(autosave)
                .with_run_criteria(FixedTimestep::step(minutes as f64 * 60.0)),
        );
    }
    // setup for normal running
    app.add_plugins(DefaultPlugins)
        .add_plugin(InteractionPlugin)
        .add_plugin(TextEntryPlugin)
        .add_startup_system_set(SystemSet::new().with_system(setup))
        .add_system(create_element)
        .add_system(connection_management::start_connecting)
        .add_system_to_stage(
            CoreStage::PostUpdate,
            connection_management::render_connections.after(TransformSystem::TransformPropagate),
        )
        .add_system(connection_management::highlight_connection_acceptor)
        .add_system(connection_management::finish_connection)
        .add_system(bubble_up_dirty)
        .add_system(update_texture)
        .add_system(util::image_modified_detection)
        .add_system(save_image)
        .add_system(right_click_deletes_element);
    app.run();
}

fn shutdown(closing: EventReader<WindowCloseRequested>, mut meta_events: EventWriter<MetaEvent>) {
    if !closing.is_empty() {
        meta_events.send(MetaEvent::Save);
    }
}

fn autosave(mut meta_events: EventWriter<MetaEvent>) {
    meta_events.send(MetaEvent::Save);
}

enum MetaEvent {
    /// Save the current state to file.
    Save,
}

/// Save generated images with a click to a new file with a random name.
fn save_image(
    images: Query<(&MyInteraction, &Handle<ColorMaterial>, &Effect), Changed<MyInteraction>>,
    material_assets: Res<Assets<ColorMaterial>>,
    image_assets: Res<Assets<Image>>,
) {
    fn write_ppm_image(
        writer: &mut impl Write,
        data: &[u8],
        size: Size<u32>,
    ) -> Result<(), std::io::Error> {
        fn float2byte(f: &f32) -> u8 {
            // Convert to 0..=255. Exact 1.0 is clamped to 255 as well.
            // The alternative would be rounding, but that would make 0 and 255 rarer than
            // others, i.e. it would not be an equal distribution.
            (*f * (u8::MAX as f32 + 1.0)).round().min(u8::MAX as f32) as u8
        }

        assert_eq!(size_of::<f32>(), 4);

        writeln!(writer, "P6 {} {} 255", size.width, size.height)?;
        let floats = data
            .chunks(size_of::<f32>())
            .into_iter()
            .map(|a| [a[0], a[1], a[2], a[3]])
            .map(GPU_TO_LOCAL_BYTE_ORDER)
            .collect::<Vec<f32>>();
        writer.write(
            &floats
                // Drop alpha channel from RGBA pixels
                .chunks(4)
                .flat_map(|rgba| &rgba[..3])
                .map(float2byte)
                .collect::<Vec<_>>(),
        )?;
        Ok(())
    }

    let paths = (0..)
        .map(|i| PathBuf::from(format!("{}.ppm", i)))
        .filter(|p| !p.exists());
    let images = images
        .iter()
        .filter(|(act, _, _)| act == &&MyInteraction::PressedLeft)
        .filter_map(|(_, material, effect)| {
            material_assets
                .get(material)
                .and_then(|mat| mat.texture.as_ref())
                .and_then(|img_handle| image_assets.get(img_handle.clone()))
                .and_then(|img| effect.size().map(|size| (img, size)))
        });
    for ((img, size), path) in images.zip(paths) {
        match File::create(path.as_path()) {
            Ok(file) => {
                if let Err(e) = write_ppm_image(&mut BufWriter::new(file), &img.data, size) {
                    eprintln!(
                        "Failed to export image to file \"{}\": {}",
                        path.display(),
                        e
                    );
                }
            }
            Err(e) => eprintln!("Cannot create image file \"{}\": {}", path.display(), e),
        }
    }
}

/// Delete a pipeline element by right clicking.
fn right_click_deletes_element(
    mut cmds: Commands,
    to_delete: Query<(Entity, &MyInteraction), With<Effect>>,
    connections: Query<&mut Connection>,
    mut inputs: Query<&mut InputConnector>,
    mut outputs: Query<&mut OutputConnector>,
    io_pads: Query<(&OutputConnectors, &InputConnectors)>,
    parents: Query<&Parent>,
) {
    for (entity, interaction) in to_delete.iter() {
        if interaction == &MyInteraction::PressedRight {
            delete_pipeline_element(
                &mut cmds,
                entity,
                &connections,
                &mut inputs,
                &mut outputs,
                &io_pads,
                &parents,
            );
        }
    }
}

/// A dirty flag to be set on all texure displays (RGBA, HSVA, GRAY) when a dependency is changed in
/// any way.
#[derive(Component)]
struct Dirty;

/// Move the dirty flag from an effect to all end displays.
fn bubble_up_dirty(
    mut cmds: Commands,
    dirty: Query<(Entity, &Effect, &OutputConnectors), With<Dirty>>,
    output_connectors: Query<&OutputConnector>,
    connections: Query<&Connection>,
    parents: Query<&Parent>,
) {
    let mut remove = vec![];
    for (entity, effect, outputs) in dirty.iter() {
        if effect.size().is_none() {
            remove.push(entity);
            let dependents = outputs
                .0
                .iter()
                .flat_map(|e| &output_connectors.get(*e).unwrap().0)
                .filter_map(|c| match connections.get(*c).unwrap().input_connector {
                    ConnectionAttachment::Connector(e) => parents.get(e).ok(),
                    _ => None,
                });
            for Parent(entity) in dependents {
                cmds.entity(*entity).insert(Dirty);
            }
        }
    }
    for entity in remove {
        cmds.entity(entity).remove::<Dirty>();
    }
}

/// React to changes in connections and update the pipeline (i.e. texture generation function)
/// accordingly.
fn update_texture(
    mut cmds: Commands,
    dirty_effects: Query<
        (Entity, &Effect, &InputConnectors),
        (Without<SidebarElement>, With<Dirty>),
    >,
    effects: Query<(&Effect, &InputConnectors), Without<SidebarElement>>,
    connections: Query<&Connection>,
    input_connectors: Query<&InputConnector>,
    parents: Query<&Parent>,
    mut image_assets: ResMut<Assets<Image>>,
) {
    const ELEM_SIZE: usize = size_of::<PixelData>();

    fn write_pixel(img: &mut Image, at: XY<u32>, pixel: Color, width: u32) {
        let offset = (at.y * width + at.x) as usize * ELEM_SIZE;
        let pixel = pixel
            .as_rgba_f32()
            .into_iter()
            .flat_map(LOCAL_TO_GPU_BYTE_ORDER)
            .collect::<Vec<_>>();
        img.data[offset..offset + ELEM_SIZE].copy_from_slice(&pixel);
    }
    /// Calculates a single-channel color value recursively.
    ///
    /// # Parameter
    ///
    /// `at` is the coordinate to sample the input at.
    ///
    /// `connection` is the [Connection] entity for which to calculate the single-channel color
    /// value.
    ///
    /// # Return
    ///
    /// `Some` color value, or `None` if any entity resolution failed.
    fn calc(
        at: Vec2,
        input_connector: Entity,
        effects: &Query<(&Effect, &InputConnectors), Without<SidebarElement>>,
        connections: &Query<&Connection>,
        input_connectors: &Query<&InputConnector>,
        parents: &Query<&Parent>,
    ) -> Option<f32> {
        let connection = input_connectors.get(input_connector).ok()?.0?;
        let output_connector = connections.get(connection).ok()?.output_connector.entity();
        let previous = parents.get(output_connector).ok()?.0;
        let (effect, inputs): (&Effect, &InputConnectors) = effects.get(previous).ok()?;

        let transformed_at = transform_coordinate(effect, at);
        let calculated_inputs = inputs
            .0
            .iter()
            .map(|input| {
                calc(
                    transformed_at,
                    *input,
                    effects,
                    connections,
                    input_connectors,
                    parents,
                )
            })
            .collect::<Vec<_>>();
        derive_color(at, effect, calculated_inputs)
    }

    fn derive_color(at: Vec2, effect: &Effect, calculated_inputs: Vec<Option<f32>>) -> Option<f32> {
        let binary_op = |op: &dyn Fn(f32, f32) -> f32| {
            calculated_inputs[0].and_then(|a| calculated_inputs[1].map(|b| op(a, b)))
        };
        match effect {
            Effect::Rgba { .. } | Effect::Hsva { .. } | Effect::Gray { .. } => {
                unreachable!()
            }
            Effect::Constant { value } => Some(*value),
            Effect::LinearX => Some(at.x as f32 / DEFAULT_TEXTURE_SIZE.width as f32 + 0.5),
            Effect::Rotate { .. }
            | Effect::Offset { .. }
            | Effect::Scale { .. }
            | Effect::Cartesian2PolarCoords
            | Effect::Polar2CartesianCoords => calculated_inputs[0],
            Effect::Add => binary_op(&|a, b| a + b),
            Effect::Sub => binary_op(&|a, b| a - b),
            Effect::Mul => binary_op(&|a, b| a * b),
            Effect::Div => binary_op(&|a, b| a / b),
            Effect::SineX => {
                Some(0.5 * (at.x / DEFAULT_TEXTURE_SIZE.width as f32 * (2.0 * PI)).sin() + 0.5)
            }
            Effect::StepX => Some((at.x >= 0.0) as u8 as f32),
            Effect::SimplexNoise { cache, seed: _, .. } => Some(
                cache
                    .lock()
                    .expect("locking error")
                    .deref_mut()
                    .sample(at.x, at.y),
            ),
        }
    }

    fn transform_coordinate(effect: &Effect, at: Vec2) -> Vec2 {
        match effect {
            Effect::Rgba { .. } | Effect::Hsva { .. } | Effect::Gray { .. } => unreachable!(),
            Effect::Constant { .. }
            | Effect::LinearX
            | Effect::Add
            | Effect::Sub
            | Effect::Mul
            | Effect::Div
            | Effect::SineX
            | Effect::StepX
            | Effect::SimplexNoise { .. } => at,
            Effect::Rotate { degrees } => {
                // Degrees is more human friendly.
                let rad = degrees / 360.0 * (2.0 * PI);
                let rotation = Transform::from_rotation(Quat::from_rotation_z(rad));
                (rotation * at.extend(1.0)).truncate()
            }
            Effect::Offset { x, y } => at + Vec2::new(*x, *y),
            Effect::Scale { x, y } => at * Vec2::new(*x, *y),
            Effect::Cartesian2PolarCoords => {
                let [x, y] = at.to_array();
                let r = (x * x + y * y).sqrt();
                let phi = (y / x).atan();
                Vec2::new(r, phi)
            }
            Effect::Polar2CartesianCoords => {
                let [r, phi] = at.to_array();
                let (sin, cos) = phi.sin_cos();
                let x = r * cos;
                let y = r * sin;
                Vec2::new(x, y)
            }
        }
    }

    for (entity, effect, inputs) in dirty_effects.iter() {
        cmds.entity(entity).remove::<Dirty>();
        let image_size = match effect {
            Effect::Rgba {
                target: Some(h),
                resolution,
                ..
            }
            | Effect::Hsva {
                target: Some(h),
                resolution,
                ..
            }
            | Effect::Gray {
                target: Some(h),
                resolution,
                ..
            } => image_assets.get_mut(h).map(|img| (img, resolution)),
            Effect::Rgba { target: None, .. }
            | Effect::Hsva { target: None, .. }
            | Effect::Gray { target: None, .. } => {
                eprintln!("Consumer does not have a display.");
                None
            }
            _ => None,
        };
        if let Some((image, size)) = image_size {
            let center = Vec2::new(size.width as f32, size.height as f32) / 2.0;
            let subpixel_offset = Vec2::splat(0.5);
            for x in 0..size.width {
                for y in 0..size.height {
                    let at = (Vec2::new(x as f32, y as f32) - center + subpixel_offset)
                        // Normalize coords (-1..=1).
                        / Vec2::new(size.width as f32, size.height as f32);
                    let inputs = inputs
                        .0
                        .iter()
                        .map(|input_connector| {
                            calc(
                                at,
                                *input_connector,
                                &effects,
                                &connections,
                                &input_connectors,
                                &parents,
                            )
                        })
                        .collect::<Vec<_>>();
                    // Default alpha to opaque as that is the expected typical use.
                    let color = match effect {
                        Effect::Rgba { .. } => Color::rgba(
                            inputs[0].unwrap_or(0.0),
                            inputs[1].unwrap_or(0.0),
                            inputs[2].unwrap_or(0.0),
                            1.0,
                        ),
                        Effect::Hsva { .. } => Color::hsla(
                            inputs[0].unwrap_or(0.0),
                            inputs[1].unwrap_or(1.0),
                            inputs[2].unwrap_or(0.5),
                            1.0,
                        ),
                        Effect::Gray { .. } => Color::rgba(
                            inputs[0].unwrap_or(0.0),
                            inputs[0].unwrap_or(0.0),
                            inputs[0].unwrap_or(0.0),
                            1.0,
                        ),
                        _ => unreachable!(),
                    };
                    write_pixel(image, XY { x, y }, color, size.width);
                }
            }
        }
    }
}

/// A system to create new pipeline elements by copying the clicked sidebar element and initializing
/// a dragging state.
#[allow(clippy::type_complexity, clippy::too_many_arguments)]
fn create_element(
    mut cmds: Commands,
    changed_interactions: Query<(Entity, &MyInteraction), Changed<MyInteraction>>,
    template_data: Query<(
        &SidebarElement,
        &GlobalTransform,
        &Mesh2dHandle,
        &Handle<ColorMaterial>,
        &Children,
        &ElementSize,
    )>,
    mouse_position: Res<MousePosition>,
    windows: Res<Windows>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut image_assets: ResMut<Assets<Image>>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    meshes: Res<HashMap<MyMeshes, Mesh2dHandle>>,
    font: Res<Handle<Font>>,
    texts: Query<&Text>,
    root: Query<(Entity, &Transform), With<RootTransform>>,
) {
    /// Copy the relevent components directly from the existing template and create a pipeline
    /// element *that is currently being dragged*. The user does not have to click again.
    fn dragging_element_from_template(
        cmds: &mut Commands,
        data: (
            &SidebarElement,
            &GlobalTransform,
            &Mesh2dHandle,
            &Handle<ColorMaterial>,
            &Children,
            &ElementSize,
        ),
        mut position: Vec3,
        mouse_position: MousePosition,
        materials: &mut Assets<ColorMaterial>,
        image_assets: &mut Assets<Image>,
        mesh_assets: &mut Assets<Mesh>,
        meshes: &HashMap<MyMeshes, Mesh2dHandle>,
        font: &Handle<Font>,
        texts: &Query<&Text>,
        root: &Query<(Entity, &Transform), With<RootTransform>>,
    ) {
        // Root may be moved wherever. This element is starting at the given position, unaffected by
        // root transform.
        let inverted_root = root
            .iter()
            .next()
            .map(|(_, transform)| Transform::from_matrix(transform.compute_matrix().inverse()));
        position = inverted_root.unwrap_or_default() * position;

        let (SidebarElement(effect), _, mesh, material, children, element_size) = data;
        let label = children
            .iter()
            // Ignore non-text entities.
            .filter_map(|c| texts.get(*c).ok())
            // Defensively make a multi-line text. It should always be a single line though.
            .map(|t| {
                t.sections.iter().map(|section| &section.value).fold(
                    "".to_string(),
                    |mut lines, section| {
                        lines.push('\n');
                        lines.push_str(section);
                        lines
                    },
                )
            })
            .next()
            .unwrap_or_else(|| {
                eprintln!("Failed to find the text on the template element.");
                String::new()
            });
        let io_pad_mesh = meshes.get(&MyMeshes::IoConnector).unwrap().clone();
        let new = create_pipeline_element(
            effect,
            cmds,
            &label,
            (*material).clone(),
            materials,
            image_assets,
            mesh_assets,
            position.truncate(),
            (*font).clone(),
            io_pad_mesh,
            (mesh.clone(), *element_size),
            false,
        );
        // The cloned effect is immediately in a dragging state. No new clicking needed.
        // This is not done in the `create_pipeline_element` function based on the `template` flag
        // to not spread out logic and state definitions even further.
        cmds.entity(new)
            .insert(MyInteraction::PressedLeft)
            .insert(Draggable)
            .insert(Dragging {
                start: mouse_position,
                base: Transform::from_translation(position),
            });
        // Enable root transformations.
        cmds.entity(root.iter().next().unwrap().0).add_child(new);
    }

    let newly_clicked = changed_interactions
        .iter()
        // Look only at the event where the left mouse button was newly pressed down.
        .filter(|(_, i)| i.deref() == &MyInteraction::PressedLeft)
        // Exclude all non-sidebar elements to copy only the templates.
        .filter_map(|(e, _)| template_data.get(e).ok());
    let window = windows.get_primary().expect("Primary window must exist.");
    let offset = Vec2::new(window.width() / 2.0, window.height() / 2.0);
    for original_data in newly_clicked {
        let position = (mouse_position.position - offset).extend(original_data.1.translation.z);
        dragging_element_from_template(
            &mut cmds,
            original_data,
            position,
            *mouse_position,
            materials.as_mut(),
            image_assets.as_mut(),
            mesh_assets.as_mut(),
            meshes.as_ref(),
            font.as_ref(),
            &texts,
            &root,
        );
    }
}

/// Initialize ressources and the world.
fn setup(
    mut cmds: Commands,
    mut material_assets: ResMut<Assets<ColorMaterial>>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    mut image_assets: ResMut<Assets<Image>>,
    asset_server: Res<AssetServer>,
    windows: Res<Windows>,
) {
    let font: Handle<Font> = asset_server.load(FONT_FILENAME);
    cmds.insert_resource(font.clone());

    let window = windows.get_primary().unwrap();

    cmds.spawn_bundle(OrthographicCameraBundle::new_2d())
        .insert(RayCastSource::<MyRaycastSet>::new());
    cmds.insert_resource(DefaultPluginState::<MyRaycastSet>::default().with_debug_cursor());

    cmds.spawn()
        .insert_bundle(InteractionBundle::default())
        // Do not include a `Transform` as the entity is now drawn. Only the
        // `GlobalTransform` is needed for Parent-Child transform propagation.
        .insert(Transform::default())
        .insert(GlobalTransform::default())
        .insert(RootTransform);

    let template_effects = Effect::all();

    // Create sidebar
    let win_width = window.width();
    let win_height = window.height();
    let sidebar_width = win_width * SIDEBAR_WIDTH;
    let total_sidebar_lines =
        // +3 for input pads, outputs pads and title.
        template_effects.iter().map(|effect| effect.controls().len() + 3).sum::<usize>()
        // Spacer lines between elements.
        + template_effects.len().saturating_sub(1)
        // A spacer line at beginning and end of sidebar to center the elements.
        + 2;
    let sidebar_height = LINE_HEIGHT * total_sidebar_lines as f32;
    let sidebar_center = Vec2::new(
        -win_width / 2.0 + sidebar_width / 2.0,
        ((win_height / 2.0/*top*/) + (win_height / 2.0 - sidebar_height/*bottom*/)) / 2.0,
    );
    let sidebar = cmds
        .spawn_bundle(ColorMesh2dBundle {
            transform: Transform::from_translation(sidebar_center.extend(0.0)),
            mesh: Mesh2dHandle(mesh_assets.add(Mesh::from(shape::Quad::new(Vec2::new(
                sidebar_width,
                sidebar_height,
            ))))),
            material: material_assets.add(ColorMaterial::from(Color::from(SIDEBAR_BACKGROUND))),
            ..Default::default()
        })
        .insert(Sidebar)
        .insert_bundle(InteractionBundle::default())
        .insert(Scroll {
            position: 0.0,
            size: win_height,
            range: 0.0..sidebar_height,
        })
        .id();

    let colors = gen_colors(template_effects.len());

    let mut mesh_cache = HashMap::new();
    let io_pad_mesh =
        Mesh2dHandle::from(mesh_assets.add(Mesh::from(shape::Quad::new(Vec2::splat(IO_PAD_SIZE)))));
    mesh_cache.insert(MyMeshes::IoConnector, io_pad_mesh.clone());

    // One line space between effects + 3 lines per effect for IO connectors and Label + one line
    // per parameter field. Starting at to have them vertically centered instead of touching the
    // border above or below.
    let mut line_offset = 1;
    for (i, effect) in template_effects.iter().enumerate() {
        let this_lines = effect.controls().len() + 3;
        let height = LINE_HEIGHT * this_lines as f32;
        let width = 0.8 * sidebar_width;

        let element_mesh = Mesh2dHandle::from(
            mesh_assets.add(Mesh::from(shape::Quad::new(Vec2::new(width, height)))),
        );
        mesh_cache.insert(MyMeshes::from(effect), element_mesh.clone());

        let child = create_pipeline_element(
            effect,
            &mut cmds,
            effect.name(),
            material_assets.add(ColorMaterial::from(colors[i])),
            material_assets.as_mut(),
            image_assets.as_mut(),
            mesh_assets.as_mut(),
            Vec2::new(
                0.0,
                sidebar_height / 2.0 - height / 2.0 - LINE_HEIGHT * line_offset as f32,
            ),
            font.clone(),
            io_pad_mesh.clone(),
            (element_mesh, ElementSize(Size::new(width, height))),
            true,
        );
        cmds.entity(child).insert(SidebarElement(effect.clone()));
        cmds.entity(sidebar).add_child(child);

        line_offset += this_lines + 1; // 1 spacer line
    }

    cmds.insert_resource(mesh_cache);
}

#[derive(Debug, Component)]
enum Effect {
    /// Holds the entity that has a texture component which shows the generated texture.
    ///
    /// The sidebar (i.e. template) elements do not have a texture.
    Rgba {
        target: Option<Handle<Image>>,
        resolution: Size<u32>,
    },
    /// Holds the entity that has a texture component which shows the generated texture.
    ///
    /// The sidebar (i.e. template) elements do not have a texture.
    Hsva {
        target: Option<Handle<Image>>,
        resolution: Size<u32>,
    },
    /// Holds the entity that has a texture component which shows the generated texture.
    ///
    /// The sidebar (i.e. template) elements do not have a texture.
    Gray {
        target: Option<Handle<Image>>,
        resolution: Size<u32>,
    },
    /// Holds the constant value that is used for all sampled coordinates.
    Constant {
        value: f32,
    },
    /// The value for an (X,Y) position is X.
    LinearX,
    /// Holds an angle for rotating the coordinates for sampling.
    Rotate {
        degrees: f32,
    },
    /// Holds X and Y components offsetting the position for sampling.
    Offset {
        x: f32,
        y: f32,
    },
    /// Holds X and Y components for scaling the position for sampling.
    Scale {
        x: f32,
        y: f32,
    },
    /// Intensity addition of the inputs.
    Add,
    /// Intensity subtraction of the inputs.
    Sub,
    /// Intensity multiplication of the inputs.
    Mul,
    /// Intensity division of the inputs.
    Div,
    /// Sine function over intensive in X direction.
    SineX,
    /// Step function in X direction, stepping at X=0 from intensity zero to one.
    StepX,
    /// A typical noise patter that still has dependency between neighboring intensity values.
    SimplexNoise {
        seed: u32,
        cache: Mutex<SimplexSampler>,
    },
    /// Transform cartesian coordinates to polar.
    Cartesian2PolarCoords,
    Polar2CartesianCoords,
}

impl Clone for Effect {
    fn clone(&self) -> Self {
        match self {
            Effect::Rgba { target, resolution } => Effect::Rgba {
                target: (*target).clone(),
                resolution: *resolution,
            },
            Effect::Hsva { target, resolution } => Effect::Hsva {
                target: (*target).clone(),
                resolution: *resolution,
            },
            Effect::Gray { target, resolution } => Effect::Gray {
                target: (*target).clone(),
                resolution: *resolution,
            },
            Effect::Constant { value } => Effect::Constant { value: *value },
            Effect::LinearX => Effect::LinearX,
            Effect::Rotate { degrees } => Effect::Rotate { degrees: *degrees },
            Effect::Offset { x, y } => Effect::Offset { x: *x, y: *y },
            Effect::Scale { x, y } => Effect::Scale { x: *x, y: *y },
            Effect::Add => Effect::Add,
            Effect::Sub => Effect::Sub,
            Effect::Mul => Effect::Mul,
            Effect::Div => Effect::Div,
            Effect::SineX => Effect::SineX,
            Effect::StepX => Effect::StepX,
            Effect::SimplexNoise { seed, cache: _ } => Effect::SimplexNoise {
                seed: *seed,
                cache: Mutex::new(SimplexSampler::new(*seed)),
            },
            Effect::Cartesian2PolarCoords => Effect::Cartesian2PolarCoords,
            Effect::Polar2CartesianCoords => Effect::Polar2CartesianCoords,
        }
    }
}

/// Implement equality as being the same variant to be useful for [HashMap]s.
impl PartialEq for Effect {
    fn eq(&self, other: &Self) -> bool {
        self.ord() == other.ord()
    }
}
impl Eq for Effect {}
impl Hash for Effect {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.ord());
    }
}

impl Effect {
    /// A list of all variants.
    fn all() -> Vec<Self> {
        vec![
            Self::Rgba {
                target: None,
                resolution: DEFAULT_TEXTURE_SIZE,
            },
            Self::Hsva {
                target: None,
                resolution: DEFAULT_TEXTURE_SIZE,
            },
            Self::Gray {
                target: None,
                resolution: DEFAULT_TEXTURE_SIZE,
            },
            Self::Constant { value: 1.0 },
            Self::LinearX,
            Self::Rotate { degrees: 1.0 },
            Self::Offset { x: 1.0, y: 1.0 },
            Self::Scale { x: 1.0, y: 1.0 },
            Self::Add,
            Self::Sub,
            Self::Mul,
            Self::Div,
            Self::SineX,
            Self::StepX,
            Self::SimplexNoise {
                seed: 0,
                cache: Mutex::new(SimplexSampler::new(0)),
            },
            Self::Cartesian2PolarCoords,
            Self::Polar2CartesianCoords,
        ]
    }

    /// A display name for each variant.
    fn name(&self) -> &str {
        match self {
            Effect::Rgba { .. } => "RGBA",
            Effect::Hsva { .. } => "HSVA",
            Effect::Gray { .. } => "GRAY",
            Effect::Constant { .. } => "Constant",
            Effect::LinearX => "X Gradient",
            Effect::Rotate { .. } => "Rotate",
            Effect::Offset { .. } => "Offset",
            Effect::Scale { .. } => "Scale",
            Effect::Add => "Add",
            Effect::Sub => "Sub",
            Effect::Mul => "Mul",
            Effect::Div => "Div",
            Effect::SineX => "Sine",
            Effect::StepX => "Step",
            Effect::SimplexNoise { .. } => "Perlin Noise",
            Effect::Cartesian2PolarCoords => "Cartesian -> Polar Coords",
            Effect::Polar2CartesianCoords => "Polar -> Cartesian Coords",
        }
    }

    /// The number of input connections for the variant.
    fn inputs(&self) -> usize {
        match self {
            Effect::Rgba { .. } => 3,
            Effect::Hsva { .. } => 3,
            Effect::Gray { .. } => 1,
            Effect::Constant { .. } => 0,
            Effect::LinearX => 0,
            Effect::Rotate { .. } => 1,
            Effect::Offset { .. } => 1,
            Effect::Scale { .. } => 1,
            Effect::Add => 2,
            Effect::Sub => 2,
            Effect::Mul => 2,
            Effect::Div => 2,
            Effect::SineX => 0,
            Effect::StepX => 0,
            Effect::SimplexNoise { .. } => 0,
            Effect::Cartesian2PolarCoords => 1,
            Effect::Polar2CartesianCoords => 1,
        }
    }

    /// The number of output connections for the variant.
    fn outputs(&self) -> usize {
        match self {
            Effect::Rgba { .. } => 0,
            Effect::Hsva { .. } => 0,
            Effect::Gray { .. } => 0,
            Effect::Constant { .. } => 1,
            Effect::LinearX => 1,
            Effect::Rotate { .. } => 1,
            Effect::Offset { .. } => 1,
            Effect::Scale { .. } => 1,
            Effect::Add => 1,
            Effect::Sub => 1,
            Effect::Mul => 1,
            Effect::Div => 1,
            Effect::SineX => 1,
            Effect::StepX => 1,
            Effect::SimplexNoise { .. } => 1,
            Effect::Cartesian2PolarCoords => 1,
            Effect::Polar2CartesianCoords => 1,
        }
    }

    /// The names of internal parameters of each variant.
    fn controls(&self) -> &'static [&'static str] {
        match self {
            Effect::Rgba { .. } | Effect::Hsva { .. } | Effect::Gray { .. } => &["Width", "Height"],
            Effect::LinearX
            | Effect::Add
            | Effect::Sub
            | Effect::Mul
            | Effect::Div
            | Effect::SineX
            | Effect::StepX
            | Effect::Cartesian2PolarCoords
            | Effect::Polar2CartesianCoords => &[],
            Effect::Constant { .. } => &["Value"],
            Effect::Rotate { .. } => &["Angle"],
            Effect::Offset { .. } | Effect::Scale { .. } => &["X", "Y"],
            Effect::SimplexNoise { .. } => &["Seed"],
        }
    }

    /// Return a unique number for each variant.
    fn ord(&self) -> usize {
        match self {
            Effect::Rgba { .. } => 0,
            Effect::Hsva { .. } => 1,
            Effect::Gray { .. } => 2,
            Effect::Constant { .. } => 3,
            Effect::LinearX => 4,
            Effect::Rotate { .. } => 5,
            Effect::Offset { .. } => 6,
            Effect::Scale { .. } => 7,
            Effect::Add => 8,
            Effect::Sub => 9,
            Effect::Mul => 10,
            Effect::Div => 11,
            Effect::SineX => 12,
            Effect::StepX => 13,
            Effect::SimplexNoise { .. } => 14,
            Effect::Cartesian2PolarCoords { .. } => 15,
            Effect::Polar2CartesianCoords { .. } => 16,
        }
    }

    fn parameter(&self, idx: usize) -> f32 {
        match self {
            Effect::Rgba { resolution, .. }
            | Effect::Hsva { resolution, .. }
            | Effect::Gray { resolution, .. }
                if idx < 2 =>
            {
                if idx == 0 {
                    resolution.width as f32
                } else {
                    resolution.height as f32
                }
            }
            Effect::Constant { value: p } | Effect::Rotate { degrees: p } if idx == 0 => *p,
            Effect::Offset { x, y } | Effect::Scale { x, y } if idx < 2 => {
                if idx == 0 {
                    *x
                } else {
                    *y
                }
            }
            Effect::SimplexNoise { seed, .. } if idx == 0 => *seed as f32,
            Effect::Constant { .. }
            | Effect::Rotate { .. }
            | Effect::Offset { .. }
            | Effect::Scale { .. }
            | Effect::SimplexNoise { .. }
            | Effect::Rgba { .. }
            | Effect::Hsva { .. }
            | Effect::Gray { .. }
            | Effect::LinearX
            | Effect::Add
            | Effect::Sub
            | Effect::Mul
            | Effect::Div
            | Effect::SineX
            | Effect::StepX
            | Effect::Cartesian2PolarCoords
            | Effect::Polar2CartesianCoords => {
                panic!("Parameter does not exist.")
            }
        }
    }

    fn size(&self) -> Option<Size<u32>> {
        match self {
            Effect::Rgba { resolution, .. }
            | Effect::Hsva { resolution, .. }
            | Effect::Gray { resolution, .. } => Some(*resolution),
            Effect::Constant { .. }
            | Effect::LinearX
            | Effect::Rotate { .. }
            | Effect::Offset { .. }
            | Effect::Scale { .. }
            | Effect::Add
            | Effect::Sub
            | Effect::Mul
            | Effect::Div
            | Effect::SineX
            | Effect::StepX
            | Effect::SimplexNoise { .. }
            | Effect::Cartesian2PolarCoords
            | Effect::Polar2CartesianCoords => None,
        }
    }
}

/// IDs for the Mesh2dHandle cache resource.
#[derive(Debug, Eq, PartialEq, Hash)]
enum MyMeshes {
    EffectType(usize),
    IoConnector,
}

impl From<&Effect> for MyMeshes {
    fn from(e: &Effect) -> Self {
        Self::EffectType(e.ord())
    }
}

/// A marker for being a template in the sidebar, instead of an interactive pipeline element.
#[derive(Debug, Component)]
struct SidebarElement(Effect);

/// A marker for being the sidebar, the parent of the sidebar elements.
#[derive(Debug, Component)]
struct Sidebar;

impl Deref for SidebarElement {
    type Target = Effect;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A bundle for creating a full pipeline element entity.
#[derive(Bundle)]
struct PipelineElementBundle {
    effect: Effect,
    size: ElementSize,
    #[bundle]
    interaction: InteractionBundle,
    #[bundle]
    color_mesh_bundle: ColorMesh2dBundle,
}

/// A small helper bundle that has all necessary components for mouse interaction in this project.
#[derive(Default, Bundle)]
struct InteractionBundle {
    interaction: MyInteraction,
    raycast_set: RayCastMesh<MyRaycastSet>,
}

/// The size of a pipeline element.
///
/// # Impl
///
/// Used so that other internal elements can be put at the correct offset. Often a view-transform is
/// used to work on a local, normalized coordinate system, but that does not work here, because the
/// child elements shall not be affected by scaling which would be necessary.
#[derive(Debug, Copy, Clone, Component)]
struct ElementSize(Size);

/// A bundle for text entry fields, holding the currently shown text and the binding to the
/// manipulated effect parameter.
#[derive(Bundle)]
struct TextFieldBundle {
    value: TextValue,
    target_value: ValueBinding,
    #[bundle]
    interaction: InteractionBundle,
    #[bundle]
    color_mesh_bundle: ColorMesh2dBundle,
}

/// A marker for the root transform that allows dragging and scaling all parts of the pipeline --
/// except the sidebar.
#[derive(Debug, Default, Clone, Component)]
struct RootTransform;
