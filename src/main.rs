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
//! connector id. Thus, all connections that transmit the same value have the same color.
//!
//! A texture is generated based on a consuming [Effect], i.e. one that has no output. It works by
//! recursively resolving its the values for all pixel positions per input connector.
//! [InputConnector]s without a [Connection] will assume a value of zero.

mod connection_management;
mod interaction;

use crate::interaction::InteractionPlugin;
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::sprite::Mesh2dHandle;
use bevy_mod_raycast::{DefaultPluginState, RayCastMesh, RayCastSource};
use connection_management::{InputConnector, InputConnectors, OutputConnector, OutputConnectors};
use interaction::{Draggable, Dragging, MousePosition, MyInteraction, MyRaycastSet};
use std::collections::HashMap;
use std::mem::size_of;
use std::ops::Deref;

const SIDEBAR_BACKGROUND: [f32; 3] = [0.5, 0.5, 0.5];
/// The width of the sidebar in normalized coords (-1..1).
const SIDEBAR_WIDTH: f32 = 0.25;

/// The relative path after "/assets" in the project folder -- which containts the Cargo.toml.
// const FONT_FILENAME: &'static str = "FiraSans-Bold.ttf";
const FONT_FILENAME: &str = "Roboto-Regular.ttf";
/// Text size, high enough to have a acceptable render quality.
const FONT_SIZE: f32 = 50.0;
/// This is set to fit all text into the pipeline elements & scales automatically with [FONT_SIZE].
const TEXT_SCALING: [f32; 2] = [
    1.0 / (2.0 * FONT_SIZE),
    1.0 / (2.0 * (2.0 / 3.0) * FONT_SIZE),
];
/// Scale factor of input and output connectors.
///
/// The values are normalized to a unit square parent to be scaled relative to the height of a
/// line in  the sidebar.
const IO_PAD_SIZE: f32 = 0.5;

/// The scaling factor for highlighting connector drop off points on hovering.
const HIGHLIGHT_SCALING: f32 = 1.5;

/// Number of pixels in each direction of the 2D texture.
const TEXTURE_SIZE: u32 = 16;

//TODO Add a system that removes pipeline elements that are dropped over the sidebar.
//TODO Add a system that moves overlapping pipeline elements away from each other.
//TODO Turn inserting multiple components on new entities into bundels for better readability.

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(InteractionPlugin)
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
    mut cmds: Commands,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    mut material_assets: ResMut<Assets<ColorMaterial>>,
    mut image_assets: ResMut<Assets<Image>>,
    mut img: Local<Option<(Entity, Handle<Image>)>>,
) {
    let img = img.get_or_insert_with(|| {
        let image = image_assets.add(Image::new(
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
            TextureFormat::Rgba32Float,
        ));
        let entity = cmds
            .spawn_bundle(ColorMesh2dBundle {
                mesh: mesh_assets
                    .add(Mesh::from(shape::Quad::new(Vec2::splat(2.0))))
                    .into(),
                material: material_assets.add(ColorMaterial {
                    color: Color::WHITE,
                    texture: Some(image.clone()),
                }),
                transform: Transform::from_translation(Vec3::new(0.0, 0.0, 5.0))
                    .with_scale(Vec3::new(200.0, 200.0, 1.0)),
                ..Default::default()
            })
            .id();
        (entity, image)
    });
    let image = image_assets.get_mut(img.1.clone()).unwrap();
    let elem_size = size_of::<[f32; 4]>();
    for x in 0..TEXTURE_SIZE {
        for y in 0..TEXTURE_SIZE {
            let offset = (y * TEXTURE_SIZE + x) as usize * elem_size;
            let color = if ((x + y) % 2) == 0 {
                Color::PURPLE
            } else {
                Color::BLACK
            };
            let data = color
                .as_rgba_f32()
                .into_iter()
                .flat_map(f32::to_le_bytes)
                .collect::<Vec<_>>();
            image.data[offset..offset + elem_size].copy_from_slice(&data);
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
    meshes: Res<HashMap<MyMeshes, Mesh2dHandle>>,
    font: Res<Handle<Font>>,
    texts: Query<&Text>,
    io_pad_size: Res<IoPadSize>,
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
        meshes: &HashMap<MyMeshes, Mesh2dHandle>,
        font: &Handle<Font>,
        texts: &Query<&Text>,
        io_pad_size: &Res<IoPadSize>,
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
            *effect,
            cmds,
            &label,
            (*material).clone(),
            materials,
            position.truncate(),
            (*font).clone(),
            (io_pad_mesh, ***io_pad_size),
            (mesh.clone(), *element_size),
        );
        cmds.entity(new)
            .insert(*effect)
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
            meshes.as_ref(),
            font.as_ref(),
            &texts,
            &io_pad_size,
        );
    }
}

/// Initialize ressources and the world.
///
/// # Parameters
///
/// `main_font`: The one font the program will use everywhere.
fn setup(
    mut cmds: Commands,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
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
            mesh: Mesh2dHandle(meshes.add(Mesh::from(shape::Quad::new(Vec2::new(
                sidebar_width,
                win_height,
            ))))),
            material: materials.add(ColorMaterial::from(Color::from(SIDEBAR_BACKGROUND))),
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

    // The amount of "lines" in the sidebar. One line space between effects + 2 lines per effect for
    // IO connectors and Label + one line per parameter field.
    // +1 offset to have them vertically centered instead of touching the border above or below.
    let num_lines = EffectType::all()
        .iter()
        .map(|e| e.controls().len() + 3)
        .sum::<usize>() as f32
        + 1.0;
    let line_height = win_height / num_lines;

    let mut mesh_cache = HashMap::new();
    let io_pad_size = IO_PAD_SIZE * line_height;

    let io_pad_mesh =
        Mesh2dHandle::from(meshes.add(Mesh::from(shape::Quad::new(Vec2::splat(io_pad_size)))));
    mesh_cache.insert(MyMeshes::IoConnector, io_pad_mesh.clone());

    let mut line_offset = 1;
    for (i, effect) in EffectType::all().iter().enumerate() {
        let this_lines = effect.controls().len() + 2;
        let offset = line_offset as f32;
        let y_from = win_height * (0.5 - offset / num_lines);
        let y_to = win_height * (0.5 - (offset + this_lines as f32) / num_lines);
        let size = Vec2::new(0.6 * sidebar_width, (y_to - y_from).abs());

        let element_mesh = Mesh2dHandle::from(meshes.add(Mesh::from(shape::Quad::new(size))));
        mesh_cache.insert(MyMeshes::from(effect), element_mesh.clone());

        let child = create_pipeline_element(
            *effect,
            &mut cmds,
            effect.name(),
            materials.add(ColorMaterial::from(colors[i])),
            materials.as_mut(),
            Vec2::new(0.0, (y_from + y_to) / 2.0),
            font.clone(),
            (io_pad_mesh.clone(), io_pad_size),
            (element_mesh, ElementSize(Size::new(size.x, size.y))),
        );
        cmds.entity(child).insert(SidebarElement(*effect));
        cmds.entity(sidebar).add_child(child);

        line_offset += this_lines + 1;
    }

    cmds.insert_resource(mesh_cache);
    cmds.insert_resource(IoPadSize(io_pad_size));
}

/// Create new new entity that is a pipeline element.
///
/// # Note
///
/// The element is interactive but not tagged as a sidebar element or a active pipeline part.
#[allow(clippy::too_many_arguments)]
fn create_pipeline_element(
    effect: EffectType,
    cmds: &mut Commands,
    label: &str,
    material: Handle<ColorMaterial>,
    materials: &mut Assets<ColorMaterial>,
    translation: Vec2,
    font: Handle<Font>,
    (io_pad_mesh, io_pad_size): (Mesh2dHandle, f32),
    (element_mesh, element_size): (Mesh2dHandle, ElementSize),
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
        let tx = (-1.0 + 2.0 * tx_fraction) * parent_size.0.width;
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

    let element_rect = Rect {
        left: translation.x,
        top: translation.y,
        right: translation.x + element_size.0.width,
        bottom: translation.y + element_size.0.height,
    };

    // Create main (clickable) box.
    let element = cmds
        .spawn_bundle(PipelineElementBundle {
            size: element_size,
            effect,
            color_mesh_bundle: ColorMesh2dBundle {
                transform: Transform::from_translation(translation.extend(1.0)),
                mesh: element_mesh,
                material: material.clone(),
                ..Default::default()
            },
            defaultable: PipelineElementBundleDefaultable::default(),
        })
        .id();
    let text_color = {
        let background = materials
            .get(material)
            .expect("Original material must exist.")
            .color;
        // Set the color to be white or black, whatever is more different than the background.
        let gray = 1.0 - gray(background).round();
        Color::rgb(gray, gray, gray)
    };

    // Add label.
    let label = create_text(cmds, label, text_color, element_rect, &font);
    cmds.entity(element).add_child(label);

    // Add inputs and outputs.
    let mut inputs = vec![];
    let mut outputs = vec![];
    {
        // Move the center (0,0) to the top/bottom edge (y=+-1) of the element.
        let ty_top = -element_size.0.height / 2.0 + io_pad_size / 2.0;
        let ty_bottom = element_size.0.height / 2.0 - io_pad_size / 2.0;
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

fn create_text(
    cmds: &mut Commands,
    label: &str,
    color: Color,
    parent_rect: Rect<f32>,
    font: &Handle<Font>,
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
    cmds.spawn_bundle(Text2dBundle {
        transform: Transform::from_translation(Vec3::new(
            (parent_rect.left + parent_rect.right) / 2.0,
            (parent_rect.top + parent_rect.bottom) / 2.0,
            // Move inner text clearly in front. 0.5 layers to not collide with a full layer in
            // front (if something is placed on that layer) while being bit-exact.
            0.5,
        ))
        .with_scale(Vec2::from(TEXT_SCALING).extend(1.0)),
        text: Text {
            sections,
            alignment: TextAlignment {
                vertical: VerticalAlign::Center,
                horizontal: HorizontalAlign::Center,
            },
        },
        ..Default::default()
    })
    .id()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Component)]
enum EffectType {
    Rgba,
    Hsva,
    Gray,
    Constant,
    Identity,
    Rotate,
    Offset,
    Scale,
    //TODO add, mul, div, functions (all on values instead of positions).
}

impl EffectType {
    /// A list of all variants.
    fn all() -> &'static [Self] {
        &[
            Self::Rgba,
            Self::Hsva,
            Self::Gray,
            Self::Constant,
            Self::Identity,
            Self::Rotate,
            Self::Offset,
            Self::Scale,
        ]
    }

    /// A display name for each variant.
    fn name(&self) -> &str {
        match self {
            EffectType::Rgba => "RGBA",
            EffectType::Hsva => "HSVA",
            EffectType::Gray => "GRAY",
            EffectType::Constant => "Constant",
            EffectType::Identity => "Identity",
            EffectType::Rotate => "Rotate",
            EffectType::Offset => "Offset",
            EffectType::Scale => "Scale",
        }
    }

    /// The number of input connections for the variant.
    fn inputs(&self) -> usize {
        match self {
            EffectType::Rgba => 4,
            EffectType::Hsva => 4,
            EffectType::Gray => 2,
            EffectType::Constant => 0,
            EffectType::Identity => 0,
            EffectType::Rotate => 1,
            EffectType::Offset => 1,
            EffectType::Scale => 1,
        }
    }

    /// The number of output connections for the variant.
    fn outputs(&self) -> usize {
        match self {
            EffectType::Rgba => 0,
            EffectType::Hsva => 0,
            EffectType::Gray => 0,
            EffectType::Constant => 1,
            EffectType::Identity => 1,
            EffectType::Rotate => 1,
            EffectType::Offset => 1,
            EffectType::Scale => 1,
        }
    }

    /// The names of internal parameters of each variant.
    fn controls(&self) -> &[&'static str] {
        match self {
            EffectType::Rgba => &[],
            EffectType::Hsva => &[],
            EffectType::Gray => &[],
            EffectType::Constant => &["Value"],
            EffectType::Identity => &[],
            EffectType::Rotate => &["Angle"],
            EffectType::Offset => &["X", "Y"],
            EffectType::Scale => &["X", "Y"],
        }
    }
}

/// IDs for the Mesh2dHandle cache resource.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum MyMeshes {
    EffectType(EffectType),
    IoConnector,
}

impl From<&EffectType> for MyMeshes {
    fn from(e: &EffectType) -> Self {
        Self::EffectType(*e)
    }
}

/// A resource, specifying the pixel size of the squares that are used as connectors.
#[derive(Debug, Copy, Clone)]
struct IoPadSize(f32);

impl Deref for IoPadSize {
    type Target = f32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A marker for being a template in the sidebar, instead of an interactive pipeline element.
#[derive(Debug, Component)]
struct SidebarElement(EffectType);

/// A marker for being the sidebar, the parent of the sidebar elements.
#[derive(Debug, Component)]
struct Sidebar;

impl Deref for SidebarElement {
    type Target = EffectType;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A bundle for creating a full pipeline element entity.
#[derive(Bundle)]
struct PipelineElementBundle {
    effect: EffectType,
    size: ElementSize,
    #[bundle]
    defaultable: PipelineElementBundleDefaultable,
    #[bundle]
    color_mesh_bundle: ColorMesh2dBundle,
}
#[derive(Default, Bundle)]
struct PipelineElementBundleDefaultable {
    inputs: InputConnectors,
    outputs: OutputConnectors,
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
