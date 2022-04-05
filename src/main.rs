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

use bevy::ecs::query::QueryEntityError;
use bevy::input::mouse::MouseButtonInput;
use bevy::input::ElementState;
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, PrimitiveTopology, TextureDimension, TextureFormat};
use bevy::sprite::Mesh2dHandle;
use bevy::utils::{HashMap, HashSet};
use bevy_mod_raycast::{
    DefaultPluginState, DefaultRaycastingPlugin, RayCastMesh, RayCastMethod, RayCastSource,
    RaycastSystem,
};
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
/// Scale factors of input and output connectors.
///
/// The values are normalized to a unit square parent and are chosen to cause a square shape in the
/// non-square parent transform.
const IO_PAD_SCALING: [f32; 2] = [0.1, 0.2];

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

/// A system to start a [Connection] from an output connector to another pipeline element's input
/// connector.
///
/// The movement for the floating connector is given to the [dragging] system.
#[allow(clippy::type_complexity)]
fn start_connecting(
    mut cmds: Commands,
    mut clicked_output: Query<
        (
            Entity,
            &MyInteraction,
            &GlobalTransform,
            &mut OutputConnector,
        ),
        (Changed<MyInteraction>, Without<SidebarElement>),
    >,
    meshes: Query<&Mesh2dHandle>,
    materials: Query<&Handle<ColorMaterial>>,
    mouse_pos: Res<MousePosition>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
) {
    for (connector, _, transform, connections) in clicked_output
        .iter_mut()
        .filter(|(_, i, _, _)| i == &&MyInteraction::Pressed)
    {
        let connector: Entity = connector;
        let translation = Vec3::from(transform.translation);
        let mut connections: Mut<OutputConnector> = connections;

        let transform = Transform::from_translation(translation);
        let material = materials.get(connector).expect("connector has a material.");
        let floating = cmds
            .spawn_bundle((
                meshes
                    .get(connector)
                    .expect("connector has a mesh.")
                    .clone(),
                (*material).clone(),
                transform,
                Draggable,
                Dragging {
                    base: transform,
                    start: *mouse_pos,
                },
                RayCastMesh::<MyRaycastSet>::default(),
                MyInteraction::Pressed,
                GlobalTransform::default(),
                Visibility::default(),
                ComputedVisibility::default(),
            ))
            .id();
        let outgoing = cmds
            .spawn_bundle(ConnectionBundle {
                connection: Connection {
                    output_connector: ConnectionAttachment::Connector(connector),
                    input_connector: ConnectionAttachment::Floating(floating),
                },
                transform: Transform::default(),
                mesh: mesh_assets.add(gen_line(&[translation])).into(),
                material: (*material).clone(),
                global_transform: Default::default(),
                visibility: Default::default(),
                comp_vis: Default::default(),
            })
            .id();
        cmds.entity(floating).insert(FloatingConnector {
            connection: outgoing,
            drop_on: None,
        });
        connections.0.push(outgoing);
    }
}

/// Move connection end points according to the global transformations of the attached connectors
/// and calculate the path of the drawn connecting line.
///
/// # Impl
///
/// This is not included in the [finish_connection] system to automatically update the line when
/// a connector moves with a dragged pipeline element.
fn render_connections(
    changed_out: Query<&OutputConnector, Changed<GlobalTransform>>,
    changed_in: Query<&InputConnector, Changed<GlobalTransform>>,
    changed_float: Query<&FloatingConnector, Changed<GlobalTransform>>,
    connections: Query<(&Connection, &Mesh2dHandle)>,
    connectors: Query<&GlobalTransform>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    let changed_connections = changed_out
        .iter()
        .flat_map(|o| o.0.iter())
        .chain(changed_in.iter().flat_map(|i| i.0.iter()))
        .chain(changed_float.iter().map(|f| &f.connection))
        // Silently ignore lookup failures.
        .filter_map(|con| connections.get(*con).ok());
    for (connection, mesh) in changed_connections {
        let connection: &Connection = connection;
        let mesh: Option<&mut Mesh> = meshes.get_mut((*mesh).clone().0);

        let out_transform = connectors
            .get(connection.output_connector.entity())
            .unwrap();
        // Y=+-1 because the mesh is a unit square and the connection attaches above or below.
        // Multiplying with the global transform puts it into the reference frame, i.e. window.
        let from = out_transform.mul_vec3(Vec3::new(0.0, 1.0, 0.0));
        let in_transform = connectors.get(connection.input_connector.entity()).unwrap();
        let to = in_transform.mul_vec3(Vec3::new(0.0, -1.0, 0.0));

        let line_mesh = gen_line(&[from, to]);
        if let Some(mesh) = mesh {
            *mesh = line_mesh;
        } else {
            eprintln!("failed to update connection mesh.");
        }
    }
}
/// A system to scale up an input connector when dragging a connection over it.
///
/// This gives feedback to the user that this interaction is good and also reduces the chances of
/// slightly missing an accepting drop-off point.
fn highlight_connection_acceptor(
    inputs: Query<&Parent, With<InputConnector>>,
    outputs: Query<&Parent, With<OutputConnector>>,
    mut floating_connectors: Query<&mut FloatingConnector>,
    connections: Query<&Connection>,
    mut transforms: Query<(&mut Transform, &GlobalTransform)>,
    interaction_changed: Query<
        (Entity, &MyInteraction),
        (
            Changed<MyInteraction>,
            Or<(With<InputConnector>, With<OutputConnector>)>,
        ),
    >,
    mut highlighted: Local<HashMap<Entity, Transform>>,
) {
    //TODO Make the function more readable.
    //TODO Exlude same-type-connectors (input-input, out-out).

    // Update drop_on field.
    // Depends on the `highlighted` data from the last update.
    if let Some(mut f) = floating_connectors.iter_mut().next() {
        for (connector, interaction) in interaction_changed.iter() {
            if let Ok(hover_parent) = inputs.get(connector).or_else(|_| outputs.get(connector)) {
                // Only consider IO connectors for the open connection end.
                let is_other_element = match connections.get(f.connection) {
                    Ok(Connection {
                        input_connector: ConnectionAttachment::Floating(_),
                        output_connector: ConnectionAttachment::Connector(other_end),
                    }) => outputs
                        .get(*other_end)
                        .map_or(true, |parent| parent != hover_parent),
                    Ok(Connection {
                        input_connector: ConnectionAttachment::Connector(other_end),
                        output_connector: ConnectionAttachment::Floating(_),
                    }) => inputs
                        .get(*other_end)
                        .map_or(true, |parent| parent != hover_parent),
                    _ => false,
                };
                if is_other_element {
                    match *interaction {
                        MyInteraction::Hover => {
                            f.drop_on = Some(connector);
                        }
                        MyInteraction::None if highlighted.contains_key(&connector) => {
                            f.drop_on = None;
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    // Highlight
    for (e, t) in highlighted.iter() {
        if let Ok((mut transform, _global)) = transforms.get_mut(*e) {
            *transform = *t;
        }
    }
    highlighted.clear();
    for connector in floating_connectors.iter().filter_map(|f| f.drop_on) {
        if let Ok((mut transform, _global)) = transforms.get_mut(connector) {
            highlighted.insert(connector, *transform);
            transform.scale *= Vec3::new(HIGHLIGHT_SCALING, HIGHLIGHT_SCALING, 1.0);
        }
    }
}
/// Stop dragging the floating connector, causing the pipeline data structure to change via the
/// [pipeline_update] system.
fn finish_connection(
    mut cmds: Commands,
    dropped_floating: Query<(Entity, &FloatingConnector), Without<Dragging>>,
    mut connections: Query<&mut Connection>,
    mut inputs: Query<&mut InputConnector>,
    mut outputs: Query<&mut OutputConnector>,
) {
    for (
        floating_connector,
        FloatingConnector {
            drop_on,
            connection,
        },
    ) in dropped_floating.iter()
    {
        match drop_on {
            #[rustfmt::skip]
            None => delete_connection(*connection, &mut cmds, &connections, &mut inputs, &mut outputs),
            Some(drop_connector) => {
                if let Ok(Connection {
                    input_connector,
                    output_connector,
                }) = connections.get(*connection)
                {
                    match (input_connector, output_connector) {
                        // Was dropped on output connector.
                        (ConnectionAttachment::Connector(_), ConnectionAttachment::Floating(_)) => {
                            #[rustfmt::skip]
                            outputs.get_mut(*drop_connector).unwrap().0.push(*connection);
                            connections.get_mut(*connection).unwrap().output_connector =
                                ConnectionAttachment::Connector(*drop_connector);
                        }
                        // Was dropped on input connector.
                        (ConnectionAttachment::Floating(_), ConnectionAttachment::Connector(_)) => {
                            if let Some(previous) = inputs.get(*drop_connector).unwrap().0 {
                                #[rustfmt::skip]
                                delete_connection(previous, &mut cmds, &connections, &mut inputs, &mut outputs);
                            }
                            *&mut inputs.get_mut(*drop_connector).unwrap().0 = Some(*connection);
                            connections.get_mut(*connection).unwrap().input_connector =
                                ConnectionAttachment::Connector(*drop_connector);
                        }
                        // Illegal states.
                        _ => {
                            eprintln!("Deleted a connection with illegal state.");
                            #[rustfmt::skip]
                            delete_connection(*connection, &mut cmds, &connections, &mut inputs, &mut outputs);
                        }
                    }
                } else {
                    eprintln!("Connection does not exist?!");
                }
                cmds.entity(floating_connector).despawn();
            }
        }
    }
}

/// Properly despawn a connection, incl. the floating connector, and remove the references from IO
/// pads to it.
fn delete_connection<'a>(
    connection: Entity,
    cmds: &'a mut Commands,
    connections: &Query<&mut Connection>,
    inputs: &mut Query<&mut InputConnector>,
    outputs: &mut Query<&mut OutputConnector>,
) {
    fn inner<'a>(
        connection: Entity,
        cmds: &'a mut Commands,
        connections: &Query<&mut Connection>,
        inputs: &mut Query<&mut InputConnector>,
        outputs: &mut Query<&mut OutputConnector>,
    ) -> Result<(), QueryEntityError> {
        let Connection {
            input_connector,
            output_connector,
        } = connections.get(connection)?;
        match input_connector {
            ConnectionAttachment::Floating(connector) => cmds.entity(*connector).despawn(),
            ConnectionAttachment::Connector(connector) => inputs.get_mut(*connector)?.0 = None,
        }
        match output_connector {
            ConnectionAttachment::Floating(connector) => cmds.entity(*connector).despawn(),
            ConnectionAttachment::Connector(connector) => {
                let outgoing_connections = &mut outputs.get_mut(*connector)?.0;
                if let Some(idx) = outgoing_connections
                    .iter()
                    .position(|con| *con == connection)
                {
                    outgoing_connections.remove(idx);
                }
            }
        };
        cmds.entity(connection).despawn();
        Ok(())
    }
    if inner(connection, cmds, connections, inputs, outputs).is_err() {
        eprintln!("Failed to look up some entity while deleting an orphaned connection.");
    }
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
    fn create_io_pad<C: Component + Default>(
        cmds: &mut Commands,
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
    {
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
    }
    cmds.entity(element)
        .push_children(&inputs)
        .push_children(&outputs)
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
///
/// # Impl
///
/// Use `if let ...` instead of `.unwrap()` to silently ignore the unlikely condition that the
/// freshly generated entity list contains bad references.
fn apply_interactions(
    mut interactive: Query<(Entity, &mut MyInteraction)>,
    mut events: EventReader<MouseButtonInput>,
    rays: Query<&RayCastSource<MyRaycastSet>>,
) {
    // Apply `Hovering` status.
    let hovering = rays
        .iter()
        .filter_map(RayCastSource::intersect_top)
        .map(|(target, _intersection)| target)
        .collect::<HashSet<_>>();
    for (e, mut interaction) in interactive.iter_mut() {
        if *interaction == MyInteraction::Hover && !hovering.contains(&e) {
            *interaction = MyInteraction::None;
        }
    }
    for e in &hovering {
        if let Ok((_entity, mut interaction)) = interactive.get_mut(*e) {
            if *interaction == MyInteraction::None {
                *interaction = MyInteraction::Hover;
            }
        }
    }

    // Apply `Pressed` status.
    for input in events.iter() {
        if let MouseButtonInput {
            button: MouseButton::Left,
            state,
        } = input
        {
            match state {
                ElementState::Pressed => {
                    for pressed in &hovering {
                        if let Ok((_entity, mut interaction)) = interactive.get_mut(*pressed) {
                            *interaction = MyInteraction::Pressed;
                        }
                    }
                }
                ElementState::Released => {
                    // Defensively release *all* clicked elements, not just the single one from here.
                    for (e, mut interaction) in interactive.iter_mut() {
                        if *interaction == MyInteraction::Pressed {
                            *interaction = if hovering.contains(&e) {
                                MyInteraction::Hover
                            } else {
                                MyInteraction::None
                            };
                        }
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

/// Return a mesh that forms a line draw on screen based on point forming a curve.
//TODO Extend to generate a spline.
fn gen_line(points: &[Vec3]) -> Mesh {
    let mut mesh = Mesh::new(PrimitiveTopology::LineStrip);
    if points.len() < 2 {
        mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vec![[0.0; 3]; 0]);
        mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, vec![[0.0; 3]; 0]);
        mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, vec![[0.0; 3]; 0]);
    } else {
        mesh.insert_attribute(
            Mesh::ATTRIBUTE_POSITION,
            points.iter().map(Vec3::to_array).collect::<Vec<_>>(),
        );
        mesh.insert_attribute(
            Mesh::ATTRIBUTE_NORMAL,
            std::iter::repeat([0.0, 0.0, 1.0])
                .take(points.len())
                .collect::<Vec<_>>(),
        );
        mesh.insert_attribute(
            Mesh::ATTRIBUTE_UV_0,
            std::iter::repeat([0.0, 0.0])
                .take(points.len())
                .collect::<Vec<_>>(),
        );
    }
    mesh
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

/// An input connector on a pipeline element.
///
/// It holds an incoming [Connection].
#[derive(Debug, Default, Copy, Clone, Component)]
struct InputConnector(Option<Entity>);
/// An output connector on a pipeline element. The opposite of an [InputConnector].
///
/// It holds a list of all outgoing [Connection]s.
#[derive(Debug, Default, Clone, Component)]
struct OutputConnector(Vec<Entity>);
/// A temporary floating connector entity that exists while the user creates a connection.
#[derive(Debug, Component)]
struct FloatingConnector {
    drop_on: Option<Entity>,
    connection: Entity,
}
/// A connection between an [OutputConnector] and an [InputConnector] or a free end point while
/// dragging a connection to a new connector.
#[derive(Debug, Component)]
struct Connection {
    output_connector: ConnectionAttachment,
    input_connector: ConnectionAttachment,
}
/// A helper type for naming the variants of entities a connection can be attached to.
#[derive(Debug, Copy, Clone)]
enum ConnectionAttachment {
    Connector(Entity),
    Floating(Entity),
}

impl ConnectionAttachment {
    fn entity(&self) -> Entity {
        match self {
            ConnectionAttachment::Connector(e) | ConnectionAttachment::Floating(e) => *e,
        }
    }
}

#[derive(Debug, Component, Copy, Clone, Eq, PartialEq)]
enum MyInteraction {
    /// When no other variant applies.
    None,
    /// When the cursor is over the entity (and no other entity is in front) but the left mouse
    /// button is not pressed.
    Hover,
    /// Set when the left mouse button is pressed while the entity was hovered over. Is reset, when
    /// the left mouse button is released -- i.e. not reset when the cursor moves is moved away.
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

/// A bundle for creating a full pipeline element entity.
#[derive(Bundle)]
struct PipelineElementBundle {
    effect: EffectType,
    inputs: InputConnectors,
    outputs: OutputConnectors,
    interaction: MyInteraction,
    raycast_set: RayCastMesh<MyRaycastSet>,
    mesh: Mesh2dHandle,
    material: Handle<ColorMaterial>,
    transform: Transform,
    global_transform: GlobalTransform,
    visibility: Visibility,
    comp_vis: ComputedVisibility,
}

/// A bundle for creating a full [OutputConnector].
#[derive(Bundle)]
struct OutputConnectorBundle {
    connector: OutputConnector,
    interaction: MyInteraction,
    ray_cast_set: RayCastMesh<MyRaycastSet>,
    mesh: Mesh2dHandle,
    material: Handle<ColorMaterial>,
    transform: Transform,
    global_transform: GlobalTransform,
    visibility: Visibility,
    comp_vis: ComputedVisibility,
}

/// A bundle for creating a full [InputConnector].
#[derive(Bundle)]
struct InputConnectorBundle {
    connector: InputConnector,
    interaction: MyInteraction,
    ray_cast_set: RayCastMesh<MyRaycastSet>,
    mesh: Mesh2dHandle,
    material: Handle<ColorMaterial>,
    transform: Transform,
    global_transform: GlobalTransform,
    visibility: Visibility,
    comp_vis: ComputedVisibility,
}

/// A bundle for creating a full [InputConnector].
#[derive(Bundle)]
struct FloatingConnectorBundle {
    dragging: Dragging,
    draggable: Draggable,
    interaction: MyInteraction,
    ray_cast_set: RayCastMesh<MyRaycastSet>,
    mesh: Mesh2dHandle,
    material: Handle<ColorMaterial>,
    transform: Transform,
    global_transform: GlobalTransform,
    visibility: Visibility,
    comp_vis: ComputedVisibility,
}

/// A bundle for creating the a full [Connection] entity.
#[derive(Bundle)]
struct ConnectionBundle {
    /// The essential data that is manipulated by user interactions.
    connection: Connection,
    /// Should be generated via [VerticalSpline].
    mesh: Mesh2dHandle,
    material: Handle<ColorMaterial>,
    transform: Transform,
    global_transform: GlobalTransform,
    visibility: Visibility,
    comp_vis: ComputedVisibility,
}

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
