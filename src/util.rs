use crate::{
    AssetEvent, Assets, ColorMaterial, EventReader, Events, Handle, HashSet, Image, Local,
    ManualEventReader, Res, ResMut,
};
use std::collections::HashMap;

/// Update all [ColorMaterial]s when their [Image] was modified.
///
/// This is currently not done automatically by Bevy
/// ([which is a bug](https://github.com/bevyengine/bevy/pull/3737)). Code copied from the link.
///
/// Whenever an [Image] is modified, if it is referenced by a [ColorMaterial]
/// we need to trigger an [AssetEvent::Modified] for the [ColorMaterial]
/// so that extract_render_assets detects it and calls for a new extraction
pub fn image_modified_detection(
    mut image_to_material: Local<HashMap<Handle<Image>, HashSet<Handle<ColorMaterial>>>>,
    mut image_events: EventReader<AssetEvent<Image>>,
    materials: Res<Assets<ColorMaterial>>,
    mut material_events_reader: Local<ManualEventReader<AssetEvent<ColorMaterial>>>,
    mut material_events: ResMut<Events<AssetEvent<ColorMaterial>>>,
) {
    // Keep the image_to_materials HashMap almost up to date.
    // When a `ColorMaterial` is modified we don't remove it from its previous image entry in the HashMap
    // thus we handle an `outdated` HashSet during modified_images iteration.
    for event in material_events_reader.iter(&material_events) {
        match event {
            AssetEvent::Created { handle } | AssetEvent::Modified { handle } => {
                if let Some(image) = materials.get(handle).and_then(|mat| mat.texture.as_ref()) {
                    image_to_material
                        .entry(image.clone_weak())
                        .or_default()
                        .insert(handle.clone_weak());
                }
            }
            AssetEvent::Removed { handle } => {
                if let Some(image) = materials.get(handle).and_then(|mat| mat.texture.as_ref()) {
                    image_to_material
                        .entry(image.clone_weak())
                        .or_default()
                        .remove(handle);
                }
            }
        }
    }
    let modified_images = image_events
        .iter()
        .filter_map(|event| {
            if let AssetEvent::Modified { handle } = event {
                Some(handle)
            } else {
                None
            }
        })
        .collect::<HashSet<_>>();
    if !modified_images.is_empty() {
        for image in modified_images.iter() {
            if let Some(material_handles) = image_to_material.get_mut(image) {
                let mut outdated = HashSet::default();
                for material_handle in material_handles.iter() {
                    if Some(*image)
                        == materials
                            .get(material_handle)
                            .and_then(|mat| mat.texture.as_ref())
                    {
                        material_events.send(AssetEvent::Modified {
                            handle: material_handle.clone_weak(),
                        })
                    } else {
                        outdated.insert(material_handle.clone_weak());
                    }
                }
                *material_handles = &*material_handles - &outdated;
            }
        }
    }
}
