use crate::{
    Args, Connection, Effect, InputConnector, InputConnectors, OutputConnector, OutputConnectors,
    RootTransform,
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
    elements: Query<(
        Entity,
        &Effect,
        &Transform,
        &InputConnectors,
        &OutputConnectors,
    )>,
    connections: Query<&Connection>,
    input_connectors: Query<(&InputConnector, &Parent)>,
    output_connectors: Query<(&OutputConnector, &Parent)>,
    args: Res<Args>,
) {
    fn convert_elements(
        elements: &Query<(
            Entity,
            &Effect,
            &Transform,
            &InputConnectors,
            &OutputConnectors,
        )>,
        connections: &Query<&Connection>,
        input_connectors: &Query<(&InputConnector, &Parent)>,
        output_connectors: &Query<(&OutputConnector, &Parent)>,
    ) -> Vec<ElementState> {
        let index_map = HashMap::<usize, Entity>::from_iter(
            elements.iter().enumerate().map(|(i, (e, ..))| (i, e)),
        );
        todo!()
    }

    let root = root.iter().next().unwrap();
    let state = SaveState {
        view_translation: root.translation,
        view_scale: root.scale,
        elements: convert_elements(
            &elements,
            &connections,
            &input_connectors,
            &output_connectors,
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
}
