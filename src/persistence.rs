use crate::connection_management::{
    calc_line_end_points, gen_line, ConnectionAttachment, ConnectionBundle,
};
use crate::{
    create_pipeline_element, gen_colors, Args, Connection, Draggable, Effect, ElementSize,
    InputConnector, InputConnectors, MetaEvent, MyInteraction, MyMeshes, OutputConnector,
    OutputConnectors, RootTransform, SidebarElement,
};
use bevy::prelude::*;
use bevy::sprite::Mesh2dHandle;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

/// Load the save file created by [save_to_file], if given, and overwrite the state from `setup`.
///
/// # Impl
///
/// Unfortunately, we cannot run this as a startup-system because the mesh size ressource
/// that is inserted in the [crate::setup] function is not accessible to other startup-systems.
/// Thus we use a local run-once flag.
pub(crate) fn load_from_file(
    mut cmds: Commands,
    mut root: Query<(Entity, &mut Transform), With<RootTransform>>,
    effect_sizes: Query<(&Effect, &ElementSize)>,
    mut material_assets: ResMut<Assets<ColorMaterial>>,
    mut image_assets: ResMut<Assets<Image>>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    meshes: Res<HashMap<MyMeshes, Mesh2dHandle>>,
    font: Res<Handle<Font>>,
    args: Res<Args>,
    mut run_once: Local<bool>,
) {
    if !*run_once {
        *run_once = true;

        if let Some(path) = args.file.as_ref() {
            let bytes = std::fs::read(path.as_path());
            match bytes {
                Ok(bytes) => match ron::de::from_bytes::<SaveState>(&bytes) {
                    Ok(state) => {
                        let (root_entity, mut root_transform) = root.iter_mut().next().unwrap();
                        *root_transform.translation = *state.view_translation;
                        *root_transform.scale = *state.view_scale;

                        let mut elements = vec![];
                        for element in &state.elements {
                            let ElementState {
                                position,
                                type_id,
                                parameters,
                                ..
                            } = element;
                            let effect = {
                                //TODO Fix pot. panic b/c remove input-data-defined type_id.
                                let mut effect = Effect::all().remove(*type_id);
                                set_parameters(&mut effect, parameters);
                                effect
                            };
                            let material = material_assets.add(ColorMaterial::from(
                                gen_colors(Effect::all().len())[effect.ord()],
                            ));
                            let io_pad_mesh_handle = meshes.get(&MyMeshes::IoConnector).unwrap();
                            let element_mesh = meshes.get(&MyMeshes::from(&effect)).unwrap();
                            let element_size = effect_sizes
                                .iter()
                                .find(|(eff, _)| eff.ord() == effect.ord())
                                .map(|(_, size)| size.clone())
                                .unwrap();
                            let element = create_pipeline_element(
                                &effect,
                                &mut cmds,
                                effect.name(),
                                material,
                                &mut material_assets,
                                &mut image_assets,
                                &mut mesh_assets,
                                position.truncate(),
                                font.clone(),
                                io_pad_mesh_handle.clone(),
                                (element_mesh.clone(), element_size),
                                false,
                            );
                            cmds.entity(element)
                                .insert(Draggable)
                                .insert(MyInteraction::None);

                            // Enable root transformations.
                            cmds.entity(root_entity).add_child(element);

                            elements.push((element, effect));
                        }
                        cmds.insert_resource((elements, state.elements));
                    }
                    Err(e) => {
                        eprintln!("Failed to parse contents of \"{}\": {}", path.display(), e)
                    }
                },
                Err(e) => eprintln!("Failed to load file \"{}\": {}", path.display(), e),
            }
        }
    }
}

fn set_parameters(effect: &mut Effect, parameters: &Vec<f32>) {
    match effect {
        Effect::SimplexNoise { seed, .. } => {
            if let Some(p) = parameters.first() {
                *seed = *p as u32;
            }
        }
        Effect::Constant { value: p1 } | Effect::Rotate { degrees: p1 } => {
            if let Some(p) = parameters.first() {
                *p1 = *p;
            }
        }
        Effect::Offset { x, y } | Effect::Scale { x, y } => {
            if let Some(p) = parameters.get(0) {
                *x = *p;
            }
            if let Some(p) = parameters.get(1) {
                *y = *p;
            }
        }
        Effect::Rgba { .. }
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
        | Effect::Polar2CartesianCoords => {}
    }
}

/// A helper system to connect loaded effects.
///
/// This is needed because the [crate::create_pipeline_element] function does not return mutable
/// references to the generated connector data.
///
/// # Note
///
/// This system must run *after* the transform propagation happened.
pub(crate) fn connect_loaded_effects(
    mut cmds: Commands,
    elements: Query<(&InputConnectors, &OutputConnectors)>,
    materials: Query<&Handle<ColorMaterial>>,
    mut in_con: Query<&mut InputConnector>,
    mut out_con: Query<&mut OutputConnector>,
    connectors: Query<&GlobalTransform>,
    mut to_connect: Option<ResMut<(Vec<(Entity, Effect)>, Vec<ElementState>)>>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
) {
    if let Some(to_connect) = to_connect.as_mut() {
        let (entities_effects, states) = std::mem::take(to_connect.as_mut());
        for ((to_entity, to_effect), to_state) in entities_effects.iter().cloned().zip(states) {
            for i in 0..to_effect.inputs() {
                let to_connector: Entity = elements.get(to_entity).unwrap().0 .0[i];
                if let Some((dep_entity_idx, output_connector_idx)) = to_state.input_dependencies[i]
                {
                    let (from_entity, from_effect) = &entities_effects[dep_entity_idx];
                    assert!(from_effect.outputs() > output_connector_idx); //TODO Missing good check. Fix in post.
                    let from_connector: Entity =
                        elements.get(*from_entity).unwrap().1 .0[output_connector_idx];
                    let material = materials
                        .get(from_connector)
                        .expect("connector has no material.");
                    let connection = Connection {
                        output_connector: ConnectionAttachment::Connector(from_connector),
                        input_connector: ConnectionAttachment::Connector(to_connector),
                    };
                    let mesh = {
                        let (from, to) = calc_line_end_points(&connection, &connectors)
                            .expect("IO pads should exist, but don't.");
                        mesh_assets.add(gen_line(&[from, to])).into()
                    };

                    let connection = cmds
                        .spawn()
                        .insert_bundle(ConnectionBundle {
                            connection,
                            transform: Transform::default(),
                            mesh,
                            material: (*material).clone(),
                            global_transform: Default::default(),
                            visibility: Default::default(),
                            comp_vis: Default::default(),
                        })
                        .id();

                    in_con.get_mut(to_connector).unwrap().0 = Some(connection);
                    out_con.get_mut(from_connector).unwrap().0.push(connection);
                }
            }
        }
        cmds.remove_resource::<(Vec<(Entity, Effect)>, Vec<ElementState>)>();
    }
}

/// Save the current world state to file to be loaded at the next startup with [load_from_file].
///
/// References:
/// * <https://github.com/bevyengine/bevy/discussions/1265>
/// * <https://github.com/bevyengine/bevy/issues/166>
pub(crate) fn save_to_file(
    mut meta_events: EventReader<MetaEvent>,
    root: Query<&Transform, With<RootTransform>>,
    elements: Query<
        (
            Entity,
            &Effect,
            &Transform,
            &InputConnectors,
            &OutputConnectors,
        ),
        Without<SidebarElement>,
    >,
    connections: Query<&Connection>,
    output_connectors: Query<(&OutputConnector, &Parent)>,
    input_connections: Query<&InputConnector>,
    args: Res<Args>,
) {
    fn convert_elements(
        elements: &Query<
            (
                Entity,
                &Effect,
                &Transform,
                &InputConnectors,
                &OutputConnectors,
            ),
            Without<SidebarElement>,
        >,
        connections: &Query<&Connection>,
        output_connectors: &Query<(&OutputConnector, &Parent)>,
        input_connectors: &Query<&InputConnector>,
    ) -> Vec<ElementState> {
        type OutputConnectorIndex = ConnectorIndex;
        type InputConnectorIndex = ConnectorIndex;

        // Pre-build index maps so that we do not need to resolve many times or work on partially
        // available data when building the ElementState.
        let entity_indices = HashMap::<Entity, ArrayIndex>::from_iter(
            elements.iter().enumerate().map(|(i, (e, ..))| (e, i)),
        );
        let mut dependency_indices =
            HashMap::<(Entity, InputConnectorIndex), (ArrayIndex, OutputConnectorIndex)>::new();
        for (entity, _, _, inputs, _) in elements.iter() {
            for (idx, input) in inputs.0.iter().enumerate() {
                let key: (Entity, InputConnectorIndex) = (entity, idx);
                if let &InputConnector(Some(connection)) = input_connectors.get(*input).unwrap() {
                    let output_connector = connections
                        .get(connection)
                        .unwrap()
                        .output_connector
                        .entity();
                    let source_effect: Entity =
                        output_connectors.get(output_connector).unwrap().1 .0;
                    let source_output_connectors: &OutputConnectors =
                        elements.get(source_effect).unwrap().4;
                    let out_con_index: OutputConnectorIndex = source_output_connectors
                        .0
                        .iter()
                        .position(|out_con| *out_con == output_connector)
                        .unwrap();
                    let value: (ArrayIndex, OutputConnectorIndex) =
                        (entity_indices[&source_effect].clone(), out_con_index);
                    dependency_indices.insert(key, value);
                }
            }
        }

        let mut result = vec![];
        for (entity, effect, transform, inputs, _) in elements.iter() {
            let entity: Entity = entity;
            let effect: &Effect = effect;
            let transform: &Transform = transform;
            let inputs: &InputConnectors = inputs;

            let parameters = match effect {
                Effect::SimplexNoise { seed, .. } => vec![*seed as f32],
                Effect::Rotate { degrees: p1 } | Effect::Constant { value: p1 } => vec![*p1],
                Effect::Offset { x, y } | Effect::Scale { x, y } => vec![*x, *y],
                Effect::Rgba { .. }
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
                | Effect::Polar2CartesianCoords => vec![],
            };
            result.push(ElementState {
                type_id: effect.ord(),
                parameters,
                input_dependencies: (0..inputs.0.len())
                    .map(|idx| dependency_indices.get(&(entity, idx)).copied())
                    .collect(),
                position: transform.translation,
            });
        }
        result
    }

    if meta_events.iter().any(|e| matches!(e, MetaEvent::Save)) {
        let root = root.iter().next().unwrap();
        let state = SaveState {
            view_translation: root.translation,
            view_scale: root.scale,
            elements: convert_elements(
                &elements,
                &connections,
                &output_connectors,
                &input_connections,
            ),
        };
        match File::create(args.save_to.as_path()) {
            Ok(mut file) => {
                let result = file.write_all(
                    ron::ser::to_string_pretty(&state, ron::ser::PrettyConfig::default())
                        .unwrap()
                        .as_bytes(),
                );
                if let Err(e) = result {
                    eprintln!(
                        "Failed to serialize state \"{}\": {}",
                        args.save_to.display(),
                        e
                    );
                }
            }
            Err(file_creation_error) => eprintln!(
                "Failed to write to file \"{}\": {}",
                args.save_to.display(),
                file_creation_error
            ),
        }
    }
}

/// Ex-/Importable world state.
#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
struct SaveState {
    /// Root transform translation component.
    view_translation: Vec3,
    /// Root transform scale component.
    view_scale: Vec3,
    /// All pipeline elements.
    ///
    /// Input dependencies are resovled as
    /// `elements[input_dependency.0]`->output_connectors[input_dependency.1]` (pseudo-code).
    elements: Vec<ElementState>,
}

pub(crate) type ArrayIndex = usize;
pub(crate) type ConnectorIndex = usize;

#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
pub(crate) struct ElementState {
    /// Value of [Effect::ord].
    type_id: usize,
    /// The values of all parameters (in order) for the [Effect] variant.
    parameters: Vec<f32>,
    /// For each connector (for the [Effect] variant), whether it is connected and which connector.
    input_dependencies: Vec<Option<(ArrayIndex, ConnectorIndex)>>,
    position: Vec3,
}
