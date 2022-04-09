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
mod text_entry;

use crate::connection_management::Connection;
use crate::interaction::InteractionPlugin;
use crate::text_entry::{TextEntryPlugin, TextValue, ValueBinding};
use bevy::math::XY;
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::sprite::Mesh2dHandle;
use bevy_mod_raycast::{DefaultPluginState, RayCastMesh, RayCastSource};
use connection_management::{InputConnector, InputConnectors, OutputConnector, OutputConnectors};
use interaction::{Draggable, Dragging, MousePosition, MyInteraction, MyRaycastSet};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem::size_of;
use std::ops::Deref;

const SIDEBAR_BACKGROUND: [f32; 3] = [0.5, 0.5, 0.5];
/// The width of the sidebar in normalized coords (-1..1).
const SIDEBAR_WIDTH: f32 = 0.25;

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
const TEXTURE_SIZE: u32 = 16;

/// Global data defines. Used to make `create_image_entity` work with the same allocation size and
/// interpretaion as `update_teture`.
type PixelData = [f32; 4];
const TEXTURE_FORMAT: TextureFormat = TextureFormat::Rgba32Float;

//TODO Turn inserting multiple components on new entities into bundels for better readability.

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(InteractionPlugin)
        .add_plugin(TextEntryPlugin)
        .add_startup_system(setup)
        .add_system(create_element)
        .add_system(connection_management::start_connecting)
        .add_system(connection_management::render_connections)
        .add_system(connection_management::highlight_connection_acceptor)
        .add_system(connection_management::finish_connection)
        .add_system(update_texture)
        .run();
}

/// React to changes in connections and update the pipeline (i.e. texture generation function)
/// accordingly.
fn update_texture(
    effects: Query<(&Effect, &InputConnectors), Without<SidebarElement>>,
    connections: Query<&Connection>,
    input_connectors: Query<&InputConnector>,
    parents: Query<&Parent>,
    mut image_assets: ResMut<Assets<Image>>,
) {
    const ELEM_SIZE: usize = size_of::<PixelData>();

    fn write_pixel(img: &mut Image, at: XY<u32>, pixel: Color) {
        let offset = (at.y * TEXTURE_SIZE + at.x) as usize * ELEM_SIZE;
        let pixel = pixel
            .as_rgba_f32()
            .into_iter()
            //TODO generalize. this is specific to my architecture, i think.
            .flat_map(f32::to_le_bytes)
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
        connection: Entity,
        effects: &Query<(&Effect, &InputConnectors), Without<SidebarElement>>,
        connections: &Query<&Connection>,
        input_connectors: &Query<&InputConnector>,
        parents: &Query<&Parent>,
    ) -> Option<f32> {
        let output_connector = connections.get(connection).ok()?.output_connector.entity();
        let previous = parents.get(output_connector).ok()?.0;
        let (effect, inputs): (&Effect, &InputConnectors) = effects.get(previous).ok()?;
        match effect {
            Effect::Rgba(_) | Effect::Hsva(_) | Effect::Gray(_) => unreachable!(),
            Effect::Constant(c) => Some(*c),
            Effect::LinearX => Some(at.x as _),
            Effect::Rotate(angle) => {
                let rotation = Transform::from_rotation(Quat::from_rotation_z(*angle));
                let at = (rotation * at.extend(1.0)).truncate();
                calc(
                    at,
                    inputs.0[0],
                    &effects,
                    &connections,
                    &input_connectors,
                    &parents,
                )
            }
            Effect::Offset(x, y) => calc(
                at + Vec2::new(*x, *y),
                inputs.0[0],
                &effects,
                &connections,
                &input_connectors,
                &parents,
            ),
            Effect::Scale(x, y) => calc(
                at * Vec2::new(*x, *y),
                inputs.0[0],
                &effects,
                &connections,
                &input_connectors,
                &parents,
            ),
        }
    }

    for (effect, inputs) in effects.iter() {
        let image = match effect {
            Effect::Rgba(Some(h)) | Effect::Hsva(Some(h)) | Effect::Gray(Some(h)) => {
                image_assets.get_mut(h)
            }
            Effect::Rgba(None) | Effect::Hsva(None) | Effect::Gray(None) => {
                eprintln!("Consumer does not have a display.");
                None
            }
            _ => None,
        };
        if let Some(image) = image {
            for x in 0..TEXTURE_SIZE {
                for y in 0..TEXTURE_SIZE {
                    let at = Vec2::new(x as f32, y as f32);
                    let inputs = inputs
                        .0
                        .iter()
                        .map(|connection| {
                            calc(
                                at,
                                *connection,
                                &effects,
                                &connections,
                                &input_connectors,
                                &parents,
                            )
                            .unwrap_or(0.0)
                        })
                        .collect::<Vec<_>>();
                    let color = match effect {
                        Effect::Rgba(..) => Color::rgba(inputs[0], inputs[1], inputs[2], inputs[3]),
                        Effect::Hsva(..) => Color::hsla(inputs[0], inputs[1], inputs[2], inputs[3]),
                        Effect::Gray(..) => Color::rgba(inputs[0], inputs[0], inputs[0], inputs[1]),
                        _ => unreachable!(),
                    };
                    write_pixel(image, XY { x, y }, color);
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
        position: Vec3,
        mouse_position: MousePosition,
        materials: &mut Assets<ColorMaterial>,
        image_assets: &mut Assets<Image>,
        mesh_assets: &mut Assets<Mesh>,
        meshes: &HashMap<MyMeshes, Mesh2dHandle>,
        font: &Handle<Font>,
        texts: &Query<&Text>,
    ) {
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
            effect.clone(),
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
            .insert(effect.clone())
            .insert(MyInteraction::Pressed)
            .insert(Draggable)
            .insert(Dragging {
                start: mouse_position,
                base: Transform::from_translation(position),
            });
    }

    let newly_clicked = changed_interactions
        .iter()
        // Look only at the event where the left mouse button was newly pressed down.
        .filter(|(_, i)| i.deref() == &MyInteraction::Pressed)
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

    // Create sidebar
    let win_width = window.width();
    let win_height = window.height();
    let sidebar_width = win_width * SIDEBAR_WIDTH;
    let sidebar = cmds
        .spawn_bundle(ColorMesh2dBundle {
            transform: Transform::from_translation(Vec3::new(
                -win_width / 2.0 + sidebar_width / 2.0,
                0.0,
                0.0,
            )),
            mesh: Mesh2dHandle(mesh_assets.add(Mesh::from(shape::Quad::new(Vec2::new(
                sidebar_width,
                win_height,
            ))))),
            material: material_assets.add(ColorMaterial::from(Color::from(SIDEBAR_BACKGROUND))),
            ..Default::default()
        })
        .insert(Sidebar)
        .id();

    let colors = [
        Color::AQUAMARINE,
        Color::BLUE,
        Color::DARK_GRAY,
        Color::GREEN,
        Color::PURPLE,
        Color::TOMATO,
        Color::VIOLET,
        Color::YELLOW_GREEN,
    ];

    let mut mesh_cache = HashMap::new();
    let io_pad_mesh =
        Mesh2dHandle::from(mesh_assets.add(Mesh::from(shape::Quad::new(Vec2::splat(IO_PAD_SIZE)))));
    mesh_cache.insert(MyMeshes::IoConnector, io_pad_mesh.clone());

    // One line space between effects + 3 lines per effect for IO connectors and Label + one line
    // per parameter field. Starting at to have them vertically centered instead of touching the
    // border above or below.
    let mut line_offset = 1;
    for (i, effect) in Effect::all().iter().enumerate() {
        let this_lines = effect.controls().len() + 3;
        let height = LINE_HEIGHT * this_lines as f32;
        let width = 0.6 * sidebar_width;

        let element_mesh = Mesh2dHandle::from(
            mesh_assets.add(Mesh::from(shape::Quad::new(Vec2::new(width, height)))),
        );
        mesh_cache.insert(MyMeshes::from(effect), element_mesh.clone());

        let child = create_pipeline_element(
            effect.clone(),
            &mut cmds,
            effect.name(),
            material_assets.add(ColorMaterial::from(colors[i])),
            material_assets.as_mut(),
            image_assets.as_mut(),
            mesh_assets.as_mut(),
            Vec2::new(
                0.0,
                win_height / 2.0 - LINE_HEIGHT * line_offset as f32 - height / 2.0,
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

/// Create new new entity that is a pipeline element.
///
/// # Parameter
///
/// `template` is a flag whether the inner text field for setting effect parameter values are
/// disabled.
/// Opposed to the adding of components onto the created element, this flag exists so that the
/// callers do not need to dig up the text field entities again.
///
/// # Note
///
/// The element is interactive but not tagged as a sidebar element or a active pipeline part.
#[allow(clippy::too_many_arguments)]
fn create_pipeline_element(
    effect: Effect,
    cmds: &mut Commands,
    label: &str,
    material: Handle<ColorMaterial>,
    materials: &mut Assets<ColorMaterial>,
    image_assets: &mut Assets<Image>,
    mesh_assets: &mut Assets<Mesh>,
    translation: Vec2,
    font: Handle<Font>,
    io_pad_mesh: Mesh2dHandle,
    (element_mesh, element_size): (Mesh2dHandle, ElementSize),
    template: bool,
) -> Entity {
    /// Calculates the human visual brightness values of a color.
    fn gray(c: Color) -> f32 {
        // The HLS variant can unfortunately not just be queried for the L(uminosity) part.
        let [r, g, b, a] = c.as_rgba_f32();
        (0.299 * r + 0.587 * g + 0.114 * b) * a
    }
    /// Create an io pad positioned and scaled on a unit square.
    fn create_io_pad<C: Component + Default>(
        cmds: &mut Commands,
        tx_fraction: f32,
        ty: f32,
        material: Handle<ColorMaterial>,
        mesh: Mesh2dHandle,
        parent_size: ElementSize,
    ) -> Entity {
        let tx = (-1.0 + 2.0 * tx_fraction) * parent_size.0.width / 2.0;
        // Text is 0.5 layers in front. This is functional and must be in front text as well.
        let tz = 0.75;
        let id = cmds
            .spawn_bundle(ColorMesh2dBundle {
                transform: Transform::from_translation(Vec3::new(tx, ty, tz)),
                material,
                mesh,
                ..Default::default()
            })
            .insert(C::default())
            .insert_bundle((
                RayCastMesh::<MyRaycastSet>::default(),
                MyInteraction::default(),
            ))
            .id();
        id
    }

    // Create the texture display above the consuming pipeline elements.
    let add_texture_display = !template
        && matches!(
            effect,
            Effect::Rgba(..) | Effect::Hsva(..) | Effect::Gray(..)
        );
    let (effect, texture) = if add_texture_display {
        let size = element_size.0.width;
        let transform = Transform::from_translation(Vec3::new(
            0.0,
            // Put texture directly above the element.
            element_size.0.height / 2.0 + size / 2.0,
            // Raise the texture above all other elements as that is the central part of the entire
            // program.
            0.9,
        ));
        let (texture, image_handle) =
            create_image_entity(cmds, mesh_assets, materials, image_assets, size, transform);
        let linked = match effect {
            Effect::Rgba(_) => Effect::Rgba(Some(image_handle)),
            Effect::Hsva(_) => Effect::Hsva(Some(image_handle)),
            Effect::Gray(_) => Effect::Gray(Some(image_handle)),
            Effect::Constant(..)
            | Effect::LinearX
            | Effect::Rotate(..)
            | Effect::Offset(..)
            | Effect::Scale(..) => unreachable!(),
        };
        (linked, Some(texture))
    } else {
        (effect, None)
    };

    // Create main (clickable) box.
    let element = cmds
        .spawn_bundle(PipelineElementBundle {
            size: element_size,
            effect: effect.clone(),
            color_mesh_bundle: ColorMesh2dBundle {
                transform: Transform::from_translation(translation.extend(1.0)),
                mesh: element_mesh,
                material: material.clone(),
                ..Default::default()
            },
            interaction: InteractionBundle::default(),
        })
        .id();
    if let Some(texture) = texture {
        cmds.entity(element).add_child(texture);
    }
    let text_color = {
        let background = materials
            .get(material)
            .expect("Original material must exist.")
            .color;
        // Set the color to be white or black, whatever is more different than the background.
        let gray = 1.0 - gray(background).round();
        Color::rgb(gray, gray, gray)
    };

    let y = |line: usize| element_size.0.height / 2.0 - (line as f32 + 0.5) * LINE_HEIGHT;
    // Add labels.
    let labels = std::iter::once((label, HorizontalAlign::Center))
        .chain(
            effect
                .controls()
                .iter()
                .copied()
                .zip(std::iter::repeat(HorizontalAlign::Right)),
        )
        .zip(1..)
        .map(|((label, align), line)| {
            let x = match align {
                HorizontalAlign::Right => -5.0,
                _ => 0.0,
            };
            create_text(
                cmds,
                label,
                text_color,
                Transform::from_translation(Vec3::new(x, y(line), 0.0)),
                &font,
                align,
            )
        })
        .collect::<Vec<_>>();
    cmds.entity(element).push_children(&labels);

    let text_field_material = materials.add(ColorMaterial::from(Color::WHITE));
    let text_fields = (2..effect.controls().len() + 2)
        .map(|line| Rect {
            left: 5.0,
            right: element_size.0.width / 2.0 * 0.9,
            top: y(line) + 0.5 * LINE_HEIGHT,
            bottom: y(line) - 0.5 * LINE_HEIGHT,
        })
        .enumerate()
        .map(|(param_idx, rect)| {
            let text_field = create_text_field(
                cmds,
                rect,
                0.0,
                (element, param_idx),
                text_field_material.clone(),
                mesh_assets,
                &font,
            );
            if template {
                cmds.entity(text_field)
                    .insert_bundle(InteractionBundle::default());
            }
            text_field
        })
        .collect::<Vec<_>>();
    cmds.entity(element).push_children(&text_fields);

    // Add inputs and outputs.
    let mut inputs = vec![];
    let mut outputs = vec![];
    {
        // Move the center (0,0) to the top/bottom edge (y=+-1) of the element.
        let ty_top = -element_size.0.height / 2.0 + IO_PAD_SIZE / 2.0;
        let ty_bottom = element_size.0.height / 2.0 - IO_PAD_SIZE / 2.0;
        let material = materials.add(ColorMaterial::from(text_color));
        for i in 1..=effect.inputs() {
            let fraction = i as f32 / (effect.inputs() + 1) as f32;
            let id = create_io_pad::<InputConnector>(
                cmds,
                fraction,
                ty_top,
                material.clone(),
                io_pad_mesh.clone(),
                element_size,
            );
            inputs.push(id);
        }
        for i in 1..=effect.outputs() {
            let fraction = i as f32 / (effect.outputs() + 1) as f32;
            let id = create_io_pad::<OutputConnector>(
                cmds,
                fraction,
                ty_bottom,
                material.clone(),
                io_pad_mesh.clone(),
                element_size,
            );
            outputs.push(id);
        }
    }

    cmds.entity(element)
        .push_children(&inputs)
        .push_children(&outputs)
        // Overwrite empty defaults with actual data.
        .insert(InputConnectors(inputs))
        .insert(OutputConnectors(outputs));

    element
}

/// Create a text field entity.
fn create_text_field(
    cmds: &mut Commands,
    rect: Rect<f32>,
    value: f32,
    param_target: (Entity, usize),
    material: Handle<ColorMaterial>,
    mesh_assets: &mut Assets<Mesh>,
    font: &Handle<Font>,
) -> Entity {
    let center = Vec2::new(
        (rect.left + rect.right) / 2.0,
        (rect.top + rect.bottom) / 2.0,
    );
    let size = Vec2::new(rect.right - rect.left, rect.top - rect.bottom);
    let inner_text = create_text(
        cmds,
        &value.to_string(),
        Color::BLACK,
        Transform::default(),
        font,
        HorizontalAlign::Center,
    );
    cmds.spawn()
        .add_child(inner_text)
        .insert_bundle(TextFieldBundle {
            color_mesh_bundle: ColorMesh2dBundle {
                // z=+0.25 to be in front of the element background (+0) and behind text (+0.5).
                transform: Transform::from_translation(center.extend(0.25)),
                material,
                mesh: mesh_assets.add(Mesh::from(shape::Quad::new(size))).into(),
                ..Default::default()
            },
            value: TextValue::new(value.to_string(), inner_text),
            target_value: ValueBinding {
                entity: param_target.0,
                parameter_idx: param_target.1,
            },
            interaction: InteractionBundle::default(),
        })
        .id()
}

fn create_text(
    cmds: &mut Commands,
    label: &str,
    color: Color,
    transform: Transform,
    font: &Handle<Font>,
    horizontal: HorizontalAlign,
) -> Entity {
    let sections = label
        .lines()
        .map(|line| TextSection {
            value: line.to_string(),
            style: TextStyle {
                color,
                font_size: FONT_SIZE,
                font: (*font).clone(),
            },
        })
        .collect();
    // Move inner text clearly in front. 0.5 layers to not collide with a full layer in
    // front (if something is placed on that layer) while being bit-exact.
    let text_transform = Transform::from_translation(Vec3::new(0.0, 0.0, 0.5));
    cmds.spawn_bundle(Text2dBundle {
        transform: transform * text_transform,
        text: Text {
            sections,
            alignment: TextAlignment {
                vertical: VerticalAlign::Center,
                horizontal,
            },
        },
        ..Default::default()
    })
    .id()
}

/// Create an entity showing an image of a given square size.
///
/// # Note
///
/// All images are created as RGBA, even for GrayA [Effect]s.
fn create_image_entity(
    cmds: &mut Commands,
    mesh_assets: &mut Assets<Mesh>,
    material_assets: &mut Assets<ColorMaterial>,
    image_assets: &mut Assets<Image>,
    size: f32,
    transform: Transform,
) -> (Entity, Handle<Image>) {
    let handle = image_assets.add(Image::new(
        Extent3d {
            width: TEXTURE_SIZE,
            height: TEXTURE_SIZE,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        std::iter::repeat(Color::GRAY.as_rgba_f32())
            .take(TEXTURE_SIZE as usize * TEXTURE_SIZE as usize)
            .flatten()
            .flat_map(f32::to_le_bytes)
            .collect(),
        TEXTURE_FORMAT,
    ));
    let image = cmds
        .spawn_bundle(ColorMesh2dBundle {
            mesh: mesh_assets
                .add(Mesh::from(shape::Quad::new(Vec2::splat(size))))
                .into(),
            material: material_assets.add(ColorMaterial {
                color: Color::WHITE,
                texture: Some(handle.clone()),
            }),
            transform,
            ..Default::default()
        })
        .id();
    (image, handle)
}

#[derive(Debug, Clone, Component)]
enum Effect {
    /// Holds the entity that has a texture component which shows the generated texture.
    ///
    /// The sidebar (i.e. template) elements do not have a texture.
    Rgba(Option<Handle<Image>>),
    /// Holds the entity that has a texture component which shows the generated texture.
    ///
    /// The sidebar (i.e. template) elements do not have a texture.
    Hsva(Option<Handle<Image>>),
    /// Holds the entity that has a texture component which shows the generated texture.
    ///
    /// The sidebar (i.e. template) elements do not have a texture.
    Gray(Option<Handle<Image>>),
    /// Holds the constant value that is used for all sampled coordinates.
    Constant(f32),
    /// The value for an (X,Y) position is X.
    LinearX,
    /// Holds an angle for rotating the coordinates for sampling.
    Rotate(f32),
    /// Holds X and Y components offsetting the position for sampling.
    Offset(f32, f32),
    /// Holds X and Y components for scaling the position for sampling.
    Scale(f32, f32),
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
            Self::Rgba(None),
            Self::Hsva(None),
            Self::Gray(None),
            Self::Constant(1.0),
            Self::LinearX,
            Self::Rotate(1.0),
            Self::Offset(1.0, 1.0),
            Self::Scale(1.0, 1.0),
        ]
    }

    /// A display name for each variant.
    fn name(&self) -> &str {
        match self {
            Effect::Rgba(..) => "RGBA",
            Effect::Hsva(..) => "HSVA",
            Effect::Gray(..) => "GRAY",
            Effect::Constant(..) => "Constant",
            Effect::LinearX => "X Gradient",
            Effect::Rotate(..) => "Rotate",
            Effect::Offset(..) => "Offset",
            Effect::Scale(..) => "Scale",
        }
    }

    /// The number of input connections for the variant.
    fn inputs(&self) -> usize {
        match self {
            Effect::Rgba(..) => 4,
            Effect::Hsva(..) => 4,
            Effect::Gray(..) => 2,
            Effect::Constant(..) => 0,
            Effect::LinearX => 0,
            Effect::Rotate(..) => 1,
            Effect::Offset(..) => 1,
            Effect::Scale(..) => 1,
        }
    }

    /// The number of output connections for the variant.
    fn outputs(&self) -> usize {
        match self {
            Effect::Rgba(..) => 0,
            Effect::Hsva(..) => 0,
            Effect::Gray(..) => 0,
            Effect::Constant(..) => 1,
            Effect::LinearX => 1,
            Effect::Rotate(..) => 1,
            Effect::Offset(..) => 1,
            Effect::Scale(..) => 1,
        }
    }

    /// The names of internal parameters of each variant.
    fn controls(&self) -> &'static [&'static str] {
        match self {
            Effect::Rgba(..) => &[],
            Effect::Hsva(..) => &[],
            Effect::Gray(..) => &[],
            Effect::Constant(..) => &["Value"],
            Effect::LinearX => &[],
            Effect::Rotate(..) => &["Angle"],
            Effect::Offset(..) => &["X", "Y"],
            Effect::Scale(..) => &["X", "Y"],
        }
    }

    /// Return a unique number for each variant.
    fn ord(&self) -> usize {
        match self {
            Effect::Rgba(..) => 0,
            Effect::Hsva(..) => 1,
            Effect::Gray(..) => 2,
            Effect::Constant(..) => 3,
            Effect::LinearX => 4,
            Effect::Rotate(..) => 5,
            Effect::Offset(..) => 6,
            Effect::Scale(..) => 7,
        }
    }
}

/// IDs for the Mesh2dHandle cache resource.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum MyMeshes {
    EffectType(Effect),
    IoConnector,
}

impl From<&Effect> for MyMeshes {
    fn from(e: &Effect) -> Self {
        Self::EffectType(e.clone())
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
