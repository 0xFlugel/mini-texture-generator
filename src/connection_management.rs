use crate::interaction::{Draggable, Dragging, MousePosition, MyInteraction, MyRaycastSet};
use crate::{RootTransform, SidebarElement, HIGHLIGHT_SCALING};
use bevy::ecs::query::QueryEntityError;
use bevy::prelude::*;
use bevy::render::mesh::PrimitiveTopology;
use bevy::sprite::Mesh2dHandle;
use bevy::utils::HashMap;
use bevy_mod_raycast::RayCastMesh;

/// A system to start a [Connection] from an output connector to another pipeline element's input
/// connector.
///
/// The movement for the floating connector is given to the
/// [crate::interaction::InteractionPlugin::dragging] system.
#[allow(clippy::type_complexity)]
pub(crate) fn start_connecting(
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
    root: Query<(Entity, &Transform), With<RootTransform>>,
    mouse_pos: Res<MousePosition>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
) {
    for (connector, _, transform, connections) in clicked_output
        .iter_mut()
        .filter(|(_, i, _, _)| i == &&MyInteraction::PressedLeft)
    {
        let connector: Entity = connector;
        // IntelliJ-Rust wrongly remarks an error when just referencing.
        #[allow(clippy::useless_conversion)]
        let translation = Vec3::from(transform.translation);
        let mut connections: Mut<OutputConnector> = connections;

        let (root_entity, root_transform) = root.iter().next().unwrap();
        let inverted_root = Transform::from_matrix(root_transform.compute_matrix().inverse());
        let transform = inverted_root * Transform::from_translation(translation);
        let material = materials
            .get(connector)
            .expect("connector has no material.");
        let floating = cmds
            .spawn_bundle((
                meshes
                    .get(connector)
                    .expect("connector has no mesh.")
                    .clone(),
                (*material).clone(),
                transform,
                Draggable,
                Dragging {
                    base: transform,
                    start: *mouse_pos,
                },
                RayCastMesh::<MyRaycastSet>::default(),
                MyInteraction::PressedLeft,
                GlobalTransform::default(),
                Visibility { is_visible: false },
                ComputedVisibility::default(),
            ))
            .id();
        cmds.entity(root_entity).add_child(floating); //Enable root transformations
        let outgoing_connection = cmds
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
            connection: outgoing_connection,
            drop_on: None,
        });
        connections.0.push(outgoing_connection);
    }
}

/// Move connection end points according to the global transformations of the attached connectors
/// and calculate the path of the drawn connecting line.
///
/// # Impl
///
/// This is not included in the [finish_connection] system to automatically update the line when
/// a connector moves with a dragged pipeline element.
///
/// `inner` function exists to allow `?` short-circuiting on deleted connections or otherwise bad
/// data.
pub(crate) fn render_connections(
    changed_out: Query<&OutputConnector, Changed<GlobalTransform>>,
    changed_in: Query<&InputConnector, Changed<GlobalTransform>>,
    changed_float: Query<&FloatingConnector, Changed<GlobalTransform>>,
    connections: Query<(&Connection, &Mesh2dHandle)>,
    connectors: Query<&GlobalTransform>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    fn inner(
        changed_out: &Query<&OutputConnector, Changed<GlobalTransform>>,
        changed_in: &Query<&InputConnector, Changed<GlobalTransform>>,
        changed_float: &Query<&FloatingConnector, Changed<GlobalTransform>>,
        connections: &Query<(&Connection, &Mesh2dHandle)>,
        connectors: &Query<&GlobalTransform>,
        meshes: &mut ResMut<Assets<Mesh>>,
    ) -> Option<()> {
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

            let (from, to) = calc_line_end_points(connection, connectors)?;
            if let Some(mesh) = mesh {
                *mesh = gen_line(&[from, to]);
            } else {
                eprintln!("failed to update connection mesh.");
            }
        }
        Some(())
    }

    let _ = inner(
        &changed_out,
        &changed_in,
        &changed_float,
        &connections,
        &connectors,
        &mut meshes,
    );
}

pub(crate) fn calc_line_end_points(
    connection: &Connection,
    connectors: &Query<&GlobalTransform>,
) -> Option<(Vec3, Vec3)> {
    let out_transform = connectors.get(connection.output_connector.entity()).ok()?;
    let in_transform = connectors.get(connection.input_connector.entity()).ok()?;
    // Y=+-1 because the mesh is a unit square and the connection attaches above or below.
    // Multiplying with the global transform puts it into the reference frame, i.e. window.
    let from = out_transform.mul_vec3(Vec3::new(0.0, 1.0, 0.0));
    let to = in_transform.mul_vec3(Vec3::new(0.0, -1.0, 0.0));
    Some((from, to))
}

/// A system to scale up an input connector when dragging a connection over it.
///
/// This gives feedback to the user that this interaction is good and also reduces the chances of
/// slightly missing an accepting drop-off point.
#[allow(clippy::type_complexity)]
pub(crate) fn highlight_connection_acceptor(
    input_parents: Query<&Parent, With<InputConnector>>,
    output_parents: Query<&Parent, With<OutputConnector>>,
    mut floating_connectors: Query<&mut FloatingConnector>,
    connections: Query<&Connection>,
    effect_input_connectors: Query<&InputConnectors>,
    input_connector_connections: Query<&InputConnector>,
    connector_parents: Query<&Parent, Or<(With<InputConnector>, With<OutputConnector>)>>,
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
    /// Check whether the `candidate` effect is contained in the dependency tree of the `origin`
    /// effect.
    fn is_dependency_element(
        origin: Entity,
        candidate: Entity,
        connections: &Query<&Connection>,
        effect_input_connectors: &Query<&InputConnectors>,
        input_connector_connections: &Query<&InputConnector>,
        connector_parents: &Query<&Parent, Or<(With<InputConnector>, With<OutputConnector>)>>,
    ) -> bool {
        let direct_dependencies = effect_input_connectors
            .get(origin)
            .unwrap()
            .0
            .iter()
            // Resolve input connector to opposite output connector.
            .filter_map(|input| {
                input_connector_connections
                    .get(*input)
                    .unwrap()
                    .0
                    .map(|connection| {
                        connections
                            .get(connection)
                            .unwrap()
                            .output_connector
                            .entity()
                    })
            })
            // Silently skip potential floating connectors.
            .filter_map(|output_connector| connector_parents.get(output_connector).ok())
            // Resolve to effect.
            .map(|parent| parent.0)
            .collect::<Vec<_>>();
        direct_dependencies.contains(&candidate)
            || direct_dependencies.into_iter().any(|origin| {
                is_dependency_element(
                    origin,
                    candidate,
                    connections,
                    effect_input_connectors,
                    input_connector_connections,
                    connector_parents,
                )
            })
    }

    // Update drop_on field.
    // Depends on the `highlighted` data from the last update.
    if let Some(mut f) = floating_connectors.iter_mut().next() {
        for (connector, interaction) in interaction_changed.iter() {
            // Inputs must lead to outputs and outputs must lead to inputs. There cannot be
            // output-output or input-input connections. Only consider IO connectors (of the other
            // type) for the open connection end.
            let parents = if let Ok(hover_parent) = input_parents.get(connector) {
                let fixed_parent = match connections.get(f.connection).unwrap() {
                    Connection {
                        input_connector: ConnectionAttachment::Floating(_),
                        output_connector: ConnectionAttachment::Connector(fixed_end),
                    } => output_parents.get(*fixed_end).ok(),
                    // A dragged connection has exactly one floating connector.
                    _ => None,
                };
                fixed_parent.map(|f| (hover_parent, f))
            } else if let Ok(hover_parent) = output_parents.get(connector) {
                // Only consider IO connectors (of the other type) for the open connection end.
                let fixed_parent = match connections.get(f.connection).unwrap() {
                    Connection {
                        input_connector: ConnectionAttachment::Connector(fixed_end),
                        output_connector: ConnectionAttachment::Floating(_),
                    } => input_parents.get(*fixed_end).ok(),
                    // A dragged connection has exactly one floating connector.
                    _ => None,
                };
                fixed_parent.map(|f| (hover_parent, f))
            } else {
                None
            };
            if let Some((hover_parent, fixed_parent)) = parents {
                let is_other_element = fixed_parent != hover_parent;
                // Forbid cycles, as they would create infinite loops in [crate::update_texture].
                let would_create_cycle = is_dependency_element(
                    fixed_parent.0,
                    hover_parent.0,
                    &connections,
                    &effect_input_connectors,
                    &input_connector_connections,
                    &connector_parents,
                );
                if is_other_element && !would_create_cycle {
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

/// Stop dragging the floating connector and drop the connection on the connector below.
pub(crate) fn finish_connection(
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
                            inputs.get_mut(*drop_connector).unwrap().0 = Some(*connection);
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
pub(crate) fn delete_connection<'a>(
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
#[derive(Debug, Default, Clone, Component)]
pub(crate) struct InputConnectors(pub(crate) Vec<Entity>);

/// Output connectors, like [InputConnectors].
#[derive(Debug, Default, Clone, Component)]
pub(crate) struct OutputConnectors(pub(crate) Vec<Entity>);

/// An input connector on a pipeline element.
///
/// It holds an incoming [Connection].
#[derive(Debug, Default, Copy, Clone, Component)]
pub(crate) struct InputConnector(pub(crate) Option<Entity>);

/// An output connector on a pipeline element. The opposite of an [InputConnector].
///
/// It holds a list of all outgoing [Connection]s.
#[derive(Debug, Default, Clone, Component)]
pub(crate) struct OutputConnector(pub(crate) Vec<Entity>);

/// A temporary floating connector entity that exists while the user creates a connection.
#[derive(Debug, Component)]
pub(crate) struct FloatingConnector {
    drop_on: Option<Entity>,
    connection: Entity,
}

/// A connection between an [OutputConnector] and an [InputConnector] or a free end point while
/// dragging a connection to a new connector.
#[derive(Debug, Component)]
pub(crate) struct Connection {
    pub(crate) output_connector: ConnectionAttachment,
    pub(crate) input_connector: ConnectionAttachment,
}

/// A helper type for naming the variants of entities a connection can be attached to.
#[derive(Debug, Copy, Clone)]
pub(crate) enum ConnectionAttachment {
    Connector(Entity),
    Floating(Entity),
}

impl ConnectionAttachment {
    pub(crate) fn entity(&self) -> Entity {
        match self {
            ConnectionAttachment::Connector(e) | ConnectionAttachment::Floating(e) => *e,
        }
    }
}

/// A bundle for creating a full [OutputConnector].
#[derive(Bundle)]
pub(crate) struct OutputConnectorBundle {
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
pub(crate) struct InputConnectorBundle {
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
pub(crate) struct FloatingConnectorBundle {
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
pub(crate) struct ConnectionBundle {
    /// The essential data that is manipulated by user interactions.
    pub(crate) connection: Connection,
    /// Should be generated via [gen_line].
    pub(crate) mesh: Mesh2dHandle,
    pub(crate) material: Handle<ColorMaterial>,
    pub(crate) transform: Transform,
    pub(crate) global_transform: GlobalTransform,
    pub(crate) visibility: Visibility,
    pub(crate) comp_vis: ComputedVisibility,
}

/// Return a mesh that forms a line draw on screen based on point forming a curve.
//TODO Extend to generate a spline.
pub(crate) fn gen_line(points: &[Vec3]) -> Mesh {
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
