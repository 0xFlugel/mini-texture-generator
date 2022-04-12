use crate::{
    Args, Connection, Effect, InputConnector, InputConnectors, OutputConnector, OutputConnectors,
    RootTransform, SidebarElement,
};
use bevy::prelude::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

/// Load the save file created by [save_to_file], if given, and overwrite the state from `setup`.
pub(crate) fn load_from_file(args: Res<Args>) {
    if let Some(path) = args.file.as_ref() {
        // Back out of default "assets/" subdirectory with "../".
        let bytes = std::fs::read(path.as_path());
        match bytes {
            Ok(bytes) => match ron::de::from_bytes::<SaveState>(&bytes) {
                Ok(_state) => {
                    todo!()
                }
                Err(e) => {
                    eprintln!("Failed to parse contents of \"{}\": {}", path.display(), e)
                }
            },
            Err(e) => eprintln!("Failed to load file \"{}\": {}", path.display(), e),
        }
    }
}

/// Save the current world state to file to be loaded at the next startup with [load_from_file].
///
/// References:
/// * <https://github.com/bevyengine/bevy/discussions/1265>
/// * <https://github.com/bevyengine/bevy/issues/166>
pub(crate) fn save_to_file(
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
        input_connections: &Query<&InputConnector>,
    ) -> Vec<ElementState> {
        type OutputConnectorIndex = ConnectorIndex;
        type InputConnectorIndex = ConnectorIndex;

        // Pre-built an index maps so that we do not need to resolve many times or work on partially
        // available data when building the ElementState.
        let entity_indices = HashMap::<Entity, ArrayIndex>::from_iter(
            elements.iter().enumerate().map(|(i, (e, ..))| (e, i)),
        );
        let mut dependency_indices =
            HashMap::<(Entity, InputConnectorIndex), (ArrayIndex, OutputConnectorIndex)>::new();
        for (entity, _, _, inputs, _) in elements.iter() {
            for (idx, input) in inputs.0.iter().enumerate() {
                let key: (Entity, InputConnectorIndex) = (entity, idx);
                if let &InputConnector(Some(connection)) = input_connections.get(*input).unwrap() {
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
        dbg!(&entity_indices, &dependency_indices);

        let mut result = vec![];
        for (entity, effect, transform, inputs, _) in elements.iter() {
            let entity: Entity = entity;
            let effect: &Effect = effect;
            let transform: &Transform = transform;
            let inputs: &InputConnectors = inputs;

            let parameters = match effect {
                Effect::PerlinNoise { seed }
                | Effect::SimplexNoise { seed }
                | Effect::WhiteNoise { seed } => vec![*seed as f32],
                Effect::Rotate { degrees: p1 } | Effect::Constant { value: p1 } => vec![*p1],
                Effect::Offset { x, y } | Effect::Scale { x, y } => {
                    vec![*x, *y]
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

type ArrayIndex = usize;
type ConnectorIndex = usize;

#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
struct ElementState {
    /// Value of [Effect::ord].
    type_id: usize,
    /// The values of all parameters (in order) for the [Effect] variant.
    parameters: Vec<f32>,
    /// For each connector (for the [Effect] variant), whether it is connected and which connector.
    input_dependencies: Vec<Option<(ArrayIndex, ConnectorIndex)>>,
    position: Vec3,
}
