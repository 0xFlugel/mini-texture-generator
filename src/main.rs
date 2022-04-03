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

use bevy::input::mouse::MouseButtonInput;
use bevy::input::ElementState;
use bevy::prelude::*;
use bevy::sprite::Mesh2dHandle;
use bevy_mod_raycast::{
    DefaultPluginState, DefaultRaycastingPlugin, RayCastMesh, RayCastMethod, RayCastSource,
    RaycastSystem,
};
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
/// Scale factors of input and output connectors.
///
/// The values are normalized to a unit square parent.
const IO_PAD_SCALING: [f32; 2] = [0.1, 0.2];

//TODO Add a system that removes pipeline elements that are dropped over the sidebar.
//TODO Add a system that moves overlapping pipeline elements away from each other.

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(DefaultRaycastingPlugin::<MyRaycastSet>::default())
        .add_system_set_to_stage(
            CoreStage::PreUpdate,
            SystemSet::new()
                .with_system(track_mouse)
                .before(RaycastSystem::BuildRays),
        )
        .add_system_set_to_stage(
            CoreStage::PreUpdate,
            SystemSet::new()
                .with_system(apply_interactions)
                .after(RaycastSystem::UpdateRaycast),
        )
        .insert_resource(MousePosition::default())
        .add_startup_system(setup)
        .add_system(create_element)
        .add_system(dragging)
        .add_system(start_connecting)
        .add_system(render_connections)
        .add_system(highlight_connection_acceptor)
        .add_system(finish_connection)
        .add_system(pipeline_update)
        .run();
}

/// A system to start a connection from an output connector to another pipeline elements input
/// connector.
///
/// The movement for the floating connector is given to the `dragging` system.
fn start_connecting() {
    //TODO
}
/// Move connection end points according to the global transformations of the attached connectors
/// and calculate the path of the drawn connecting line.
///
/// # Impl
///
/// This is not included in the [finish_connection] system to automatically update the line when
/// a connector moves with a dragged pipeline element.
fn render_connections() {
    //TODO
}
/// A system to scale up an input connector when dragging a connection over it.
///
/// This gives feedback to the user that this interaction is good and also reduces the chances of
/// slightly missing an accepting drop-off point.
fn highlight_connection_acceptor(inputs: Query<(&mut Transform, &Parent), With<InputConnector>>) {
    //TODO
}
/// Stop dragging the floating connector, causing the pipeline data structure to change via the
/// [pipeline_update] system.
fn finish_connection() {
    //TODO
}
/// Apply changes to the generation functions based on changes to the [Connections] between pipeline
/// elements' connectors.
fn pipeline_update() {
    //TODO
}

/// A system to create new pipeline elements by copying the clicked sidebar element and initializing
/// a dragging state.
#[allow(clippy::type_complexity)]
fn create_element(
    mut cmds: Commands,
    changed_interactions: Query<(Entity, &MyInteraction), Changed<MyInteraction>>,
    template_data: Query<(
        &SidebarElement,
        &GlobalTransform,
        &Mesh2dHandle,
        &Handle<ColorMaterial>,
        &Children,
    )>,
    mouse_position: Res<MousePosition>,
    windows: Res<Windows>,
    mut materials: ResMut<Assets<ColorMaterial>>,
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
        ),
        position: Vec3,
        mouse_position: MousePosition,
        materials: &mut Assets<ColorMaterial>,
        font: &Handle<Font>,
        texts: &Query<&Text>,
    ) {
        let (SidebarElement(effect), global_transform, mesh, material, children) = data;
        let transform = Transform::from(*global_transform).with_translation(position);
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
        let new = create_pipeline_element(
            *effect,
            cmds,
            &label,
            (*material).clone(),
            mesh.clone(),
            materials,
            transform,
            (*font).clone(),
        );
        cmds.entity(new)
            .insert(*effect)
            .insert(MyInteraction::Pressed)
            .insert(Draggable)
            .insert(Dragging {
                start: mouse_position,
                base: transform,
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
            font.as_ref(),
            &texts,
        );
    }
}

/// Drag entities around their XY plane depending on cursor movement.
#[allow(clippy::type_complexity)]
fn dragging(
    start: Query<(Entity, &MyInteraction, &Transform), (With<Draggable>, Without<Dragging>)>,
    mut continue_: Query<(&Dragging, &mut Transform)>,
    stop: Query<(Entity, &MyInteraction), With<Dragging>>,
    mut cmds: Commands,
    current_mouse_position: Res<MousePosition>,
) {
    start
        .iter()
        .filter(|(_, i, _)| **i == MyInteraction::Pressed)
        .for_each(|(e, _, base)| {
            cmds.entity(e).insert(Dragging {
                start: *current_mouse_position,
                base: *base,
            });
        });
    stop.iter()
        .filter(|(_, i)| **i != MyInteraction::Pressed)
        .for_each(|(e, _)| {
            cmds.entity(e).remove::<Dragging>();
        });
    for (Dragging { start, base }, mut transform) in continue_.iter_mut() {
        let translate = Transform::from_translation(
            (current_mouse_position.position - start.position).extend(0.0),
        );
        *transform = translate * *base;
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
    let x_scale = window.width() / 2.0;
    let y_scale = window.height() / 2.0;

    cmds.spawn_bundle(OrthographicCameraBundle::new_2d())
        .insert(RayCastSource::<MyRaycastSet>::new());
    cmds.insert_resource(DefaultPluginState::<MyRaycastSet>::default().with_debug_cursor());

    // Create sidebar
    let normalized_square =
        Mesh2dHandle(meshes.add(Mesh::from(shape::Quad::new(Vec2::splat(2.0)))));
    let sidebar = cmds
        .spawn_bundle(ColorMesh2dBundle {
            transform: transform_from_rect(
                Rect {
                    left: -x_scale,
                    right: (-1.0 + SIDEBAR_WIDTH) * x_scale,
                    top: y_scale,
                    bottom: -y_scale,
                },
                0,
            ),
            mesh: normalized_square.clone(),
            material: materials.add(ColorMaterial::from(Color::from(SIDEBAR_BACKGROUND))),
            ..Default::default()
        })
        .insert(Sidebar)
        .id();
    let n = EffectType::all().len();

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

    for (i, effect) in EffectType::all().iter().enumerate() {
        let num = (3 * n + 1) as f32;
        let offset = (3 * i + 1) as f32;
        let transform = transform_from_rect(
            Rect {
                top: 1.0 - 2.0 * (offset / num),
                bottom: 1.0 - 2.0 * ((offset + 2.0) / num),
                left: -0.8,
                right: 0.8,
            },
            1,
        );
        let child = create_pipeline_element(
            *effect,
            &mut cmds,
            effect.name(),
            materials.add(ColorMaterial::from(colors[i])),
            normalized_square.clone(),
            materials.as_mut(),
            transform,
            font.clone(),
        );
        cmds.entity(child).insert(SidebarElement(*effect));
        cmds.entity(sidebar).add_child(child);
    }
}

/// Create new new entity that is a pipeline element.
///
/// # Note
///
/// The element is interactive but not tagged as a sidebar element or a active pipeline part.
fn create_pipeline_element(
    effect: EffectType,
    cmds: &mut Commands,
    label: &str,
    material: Handle<ColorMaterial>,
    mesh: Mesh2dHandle,
    materials: &mut Assets<ColorMaterial>,
    transform: Transform,
    font: Handle<Font>,
) -> Entity {
    /// Calculates the human visual brightness values of a color.
    fn gray(c: Color) -> f32 {
        // The HLS variant can unfortunately not just be queried for the L(uminosity) part.
        let [r, g, b, a] = c.as_rgba_f32();
        (0.299 * r + 0.587 * g + 0.114 * b) * a
    }

    // Create main (clickable) box.
    let element = cmds
        .spawn_bundle(ColorMesh2dBundle {
            transform,
            mesh: mesh.clone(),
            material: material.clone(),
            ..Default::default()
        })
        .insert(MyInteraction::default())
        .insert(RayCastMesh::<MyRaycastSet>::default())
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
    let label = create_text(cmds, label, text_color, transform, &font);
    cmds.entity(element).add_child(label);

    // Add inputs and outputs.
    let mut inputs = vec![];
    let mut outputs = vec![];
    cmds.entity(element)
        .with_children(|cmds| {
            fn create_io_pad<C: Component + Default>(
                cmds: &mut ChildBuilder,
                fraction: f32,
                ty: f32,
                material: Handle<ColorMaterial>,
                mesh: Mesh2dHandle,
            ) -> Entity {
                let tx = -1.0 + 2.0 * fraction;
                // Text is 0.5 layers in front. This is functional and must be in front text as well.
                let tz = 0.75;
                let id = cmds
                    .spawn_bundle(ColorMesh2dBundle {
                        transform: Transform::from_scale(Vec2::from(IO_PAD_SCALING).extend(1.0))
                            .with_translation(Vec3::new(tx, ty, tz)),
                        material,
                        mesh,
                        ..Default::default()
                    })
                    .insert(C::default())
                    .insert(RayCastMesh::<MyRaycastSet>::default())
                    .insert(MyInteraction::default())
                    .id();
                id
            }

            // Move the center (0,0) to the top/bottom edge (y=+-1) of the element.
            let ty_top = -1.0 + IO_PAD_SCALING[1];
            let ty_bottom = 1.0 - IO_PAD_SCALING[1];
            let material = materials.add(ColorMaterial::from(text_color));
            for i in 1..=effect.inputs() {
                let fraction = i as f32 / (effect.inputs() + 1) as f32;
                let id = create_io_pad::<InputConnector>(
                    cmds,
                    fraction,
                    ty_top,
                    material.clone(),
                    mesh.clone(),
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
                    mesh.clone(),
                );
                outputs.push(id);
            }
        })
        .insert(InputConnectors(inputs))
        .insert(OutputConnectors(outputs));

    element
}

fn create_text(
    cmds: &mut Commands,
    label: &str,
    color: Color,
    transform: Transform,
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
        transform: transform
            .with_scale(Vec2::from(TEXT_SCALING).extend(1.0))
            // Move inner text clearly in front. 0.5 layers to not collide with a full layer in
            // front (if something is placed on that layer) while being bit-exact.
            .with_translation(Vec3::new(0.0, 0.0, 0.5)),
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

/// A system to track the mouse location and make it available as a resource.
fn track_mouse(
    mut r: ResMut<MousePosition>,
    mut events: EventReader<CursorMoved>,
    mut rays: Query<&mut RayCastSource<MyRaycastSet>>,
) {
    match events.iter().last() {
        None => r.just_moved = false,
        Some(e) => {
            r.position = e.position;
            r.just_moved = true;
            for mut ray in rays.iter_mut() {
                ray.cast_method = RayCastMethod::Screenspace(r.position);
            }
        }
    }
}

/// A system to change states of [MyInteraction] components based on mouse input.
fn apply_interactions(
    mut interactive: Query<(Entity, &mut MyInteraction)>,
    mut events: EventReader<MouseButtonInput>,
    rays: Query<&RayCastSource<MyRaycastSet>>,
) {
    let hovering = rays
        .iter()
        .filter_map(RayCastSource::intersect_list)
        .flatten()
        .map(|(target, _)| *target)
        .next();
    for input in events.iter() {
        if let MouseButtonInput {
            button: MouseButton::Left,
            state,
        } = input {
            match state {
                ElementState::Pressed => {
                    if let Some((_, mut interaction)) =
                    hovering.and_then(|pressed| interactive.get_mut(pressed).ok())
                    {
                        *interaction = MyInteraction::Pressed;
                    }
                }
                ElementState::Released => {
                    // Defensively release *all* clicked elements, not just the single one from here.
                    for (_, mut interaction) in interactive.iter_mut() {
                        *interaction = MyInteraction::None;
                    }
                }
            }
        }
    }
}

/// Convert a rect in the normalized 2D space (-1..1 on X and Y axes) to a transform.
///
/// # Parameter
///
/// `layer` is the layer the object is residing on in the 2D scene. Make sure to put the children in
/// front of the parents...
fn transform_from_rect(rect: Rect<f32>, layer: usize) -> Transform {
    let x = (rect.left + rect.right) / 2.0;
    let y = (rect.top + rect.bottom) / 2.0;
    let scale_x = (rect.right - rect.left) / 2.0;
    let scale_y = (rect.top - rect.bottom) / 2.0;
    Transform::from_translation(Vec3::new(x, y, layer as _))
        .with_scale(Vec3::new(scale_x, scale_y, 1.0))
}

/// A marker for entities that can be dragged.
#[derive(Component)]
struct Draggable;
/// State while dragging an entity.
#[derive(Debug, Component)]
struct Dragging {
    start: MousePosition,
    /// The entity transform at the start of the drag action.
    ///
    /// # Note
    ///
    /// Changes from other systems to a currently dragged entity will not persist as its transform
    /// will be overwritten with the cached base value.
    base: Transform,
}

#[derive(Debug, Copy, Clone, Component)]
enum EffectType {
    Rgba,
    Hsva,
    Gray,
    Constant,
    Identity,
    Rotate,
    Offset,
    Scale,
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

/// Input connectors.
///
/// Holding the connector entities (which are also children) of pipeline elements in the placement
/// order from left to right.
///
/// # Design
///
/// The [OutputConnectors] are a different component to allow system queries to filter for the
/// specific type directly (on the engine level), instead of going through them on the system level.
/// This is faster and more convenient to use.
#[derive(Debug, Clone, Component)]
struct InputConnectors(Vec<Entity>);
/// Output connectors, like [InputConnectors].
#[derive(Debug, Clone, Component)]
struct OutputConnectors(Vec<Entity>);

/// Marker for an input connector on a pipeline element.
#[derive(Default, Component)]
struct InputConnector;
/// Marker for an output connector on a pipeline element.
#[derive(Default, Component)]
struct OutputConnector;

#[derive(Debug, Component, Copy, Clone, Eq, PartialEq)]
enum MyInteraction {
    None,
    Pressed,
}

impl Default for MyInteraction {
    fn default() -> Self {
        Self::None
    }
}

/// A resource to track the mouse location.
#[derive(Debug, Default, Copy, Clone)]
struct MousePosition {
    position: Vec2,
    just_moved: bool,
}

/// A marker for ray-castable entities.
struct MyRaycastSet;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transform_from_rect_nop() {
        let rect = Rect {
            top: 1.0,
            bottom: -1.0,
            left: -1.0,
            right: 1.0,
        };
        let result = dbg!(transform_from_rect(rect, 0));
        let expect = dbg!(Transform::from_translation(Vec3::new(0.0, 0.0, 0.0))
            .with_scale(Vec3::new(1.0, 1.0, 1.0)));
        let eps = 1e-6;
        assert!((result.translation.x - expect.translation.x).abs() < eps);
        assert!((result.translation.y - expect.translation.y).abs() < eps);
        assert!((result.translation.z - expect.translation.z).abs() < eps);
        assert!((result.scale.x - expect.scale.x).abs() < eps);
        assert!((result.scale.y - expect.scale.y).abs() < eps);
        assert!((result.scale.z - expect.scale.z).abs() < eps);
    }

    #[test]
    fn transform_from_rect_bottom_right_quadrant() {
        let rect = Rect {
            top: 0.0,
            bottom: -1.0,
            left: 0.0,
            right: 1.0,
        };
        let result = dbg!(transform_from_rect(rect, 0));
        let expect = dbg!(Transform::from_translation(Vec3::new(0.5, -0.5, 0.0))
            .with_scale(Vec3::new(0.5, 0.5, 1.0)));
        let eps = 1e-6;
        assert!((result.translation.x - expect.translation.x).abs() < eps);
        assert!((result.translation.y - expect.translation.y).abs() < eps);
        assert!((result.translation.z - expect.translation.z).abs() < eps);
        assert!((result.scale.x - expect.scale.x).abs() < eps);
        assert!((result.scale.y - expect.scale.y).abs() < eps);
        assert!((result.scale.z - expect.scale.z).abs() < eps);
    }

    #[test]
    fn transform_from_rect_bottom_half() {
        let rect = Rect {
            top: 0.0,
            bottom: -1.0,
            left: -1.0,
            right: 1.0,
        };
        let result = dbg!(transform_from_rect(rect, 0));
        let expect = dbg!(Transform::from_translation(Vec3::new(0.0, -0.5, 0.0))
            .with_scale(Vec3::new(1.0, 0.5, 1.0)));
        let eps = 1e-6;
        assert!((result.translation.x - expect.translation.x).abs() < eps);
        assert!((result.translation.y - expect.translation.y).abs() < eps);
        assert!((result.translation.z - expect.translation.z).abs() < eps);
        assert!((result.scale.x - expect.scale.x).abs() < eps);
        assert!((result.scale.y - expect.scale.y).abs() < eps);
        assert!((result.scale.z - expect.scale.z).abs() < eps);
    }

    #[test]
    fn transform_from_rect_bigger() {
        let rect = Rect {
            top: 3.0,
            bottom: -2.0,
            left: -3.0,
            right: 2.0,
        };
        let result = dbg!(transform_from_rect(rect, 0));
        let expect = dbg!(Transform::from_translation(Vec3::new(-0.5, 0.5, 0.0))
            .with_scale(Vec3::new(2.5, 2.5, 1.0)));
        let eps = 1e-6;
        assert!((result.translation.x - expect.translation.x).abs() < eps);
        assert!((result.translation.y - expect.translation.y).abs() < eps);
        assert!((result.translation.z - expect.translation.z).abs() < eps);
        assert!((result.scale.x - expect.scale.x).abs() < eps);
        assert!((result.scale.y - expect.scale.y).abs() < eps);
        assert!((result.scale.z - expect.scale.z).abs() < eps);
    }

    #[test]
    fn transform_from_rect_layers() {
        let rect = Rect {
            top: 1.0,
            bottom: -1.0,
            left: -1.0,
            right: 1.0,
        };
        let result = transform_from_rect(rect, 1);
        let expect = Transform::from_translation(Vec3::new(0.0, 0.0, 1.0));
        let eps = 1e-6;
        assert!((dbg!(result).translation.x - dbg!(expect).translation.x).abs() < eps);
        assert!((result.translation.y - expect.translation.y).abs() < eps);
        assert!((result.translation.z - expect.translation.z).abs() < eps);
        assert!((result.scale.x - expect.scale.x).abs() < eps);
        assert!((result.scale.y - expect.scale.y).abs() < eps);
        assert!((result.scale.z - expect.scale.z).abs() < eps);
    }
}
