use crate::{
    delete_connection, Connection, Effect, ElementSize, InputConnector, InputConnectors,
    InteractionBundle, MyInteraction, MyRaycastSet, OutputConnector, OutputConnectors,
    PipelineElementBundle, TextFieldBundle, TextValue, ValueBinding, DEFAULT_FONT_SIZE,
    IO_PAD_SIZE, LINE_HEIGHT, LOCAL_TO_GPU_BYTE_ORDER, TEXTURE_FORMAT,
};
use bevy::ecs::event::{Events, ManualEventReader};
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension};
use bevy::sprite::Mesh2dHandle;
use bevy::transform::TransformSystem;
use bevy_mod_raycast::RayCastMesh;
use std::collections::{HashMap, HashSet};

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
                        .remove(&handle);
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
                    if materials
                        .get(material_handle)
                        .and_then(|mat| mat.texture.as_ref())
                        .map_or(false, |texture| texture.id == image.id)
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

/// Delete a pipeline element instance.
///
/// This removes the `entity`, all children recursively and all direct connections to other
/// elements.
pub(crate) fn delete_pipeline_element(
    cmds: &mut Commands,
    entity: Entity,
    connections: &Query<&mut Connection>,
    inputs: &mut Query<&mut InputConnector>,
    outputs: &mut Query<&mut OutputConnector>,
    io_pads: &Query<(&OutputConnectors, &InputConnectors)>,
    parents: &Query<&Parent>,
) {
    if let Ok((o, i)) = io_pads.get(entity) {
        // Clean up connections to other entities.
        let resolve_inputs = |e: &Entity| inputs.get(*e).unwrap().0.as_ref();
        let resolve_outputs = |e: &Entity| outputs.get(*e).unwrap().0.iter();
        let to_delete =
            i.0.iter()
                .filter_map(resolve_inputs)
                .chain(o.0.iter().flat_map(resolve_outputs))
                .copied()
                .collect::<Vec<_>>();
        for connection in to_delete {
            delete_connection(connection, cmds, connections, inputs, outputs, parents);
        }

        // Finally delete this element.
        cmds.entity(entity).despawn_recursive();
    }
}

/// Generates `n` colors with the highest average difference.
pub(crate) fn gen_colors(n: usize) -> Vec<Color> {
    (0..n)
        // Map to evenly spread out hue and 0.25, 0.5 nad 0.75 lightness.
        .map(|i| {
            Color::hsla(
                i as f32 / n as f32 * 360.0,
                1.0,
                1.0 - ((i % 3) as f32 + 1.0) / 4.0,
                1.0,
            )
        })
        .collect()
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
pub(crate) fn create_pipeline_element(
    effect: &Effect,
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
    let resolution = effect.size().filter(|_| !template);
    let (effect, texture) = if let Some(resolution) = resolution {
        let size = element_size.0.width;
        let transform = Transform::from_translation(Vec3::new(
            0.0,
            // Put texture directly above the element.
            element_size.0.height / 2.0 + size / 2.0,
            // Raise the texture above all other elements as that is the central part of the entire
            // program.
            0.9,
        ));
        let (texture, image_handle) = create_image_entity(
            cmds,
            mesh_assets,
            materials,
            image_assets,
            size,
            transform,
            resolution,
        );
        cmds.entity(texture)
            .insert_bundle(InteractionBundle::default());
        let linked = match effect {
            Effect::Rgba { resolution, .. } => Effect::Rgba {
                target: Some(image_handle),
                resolution: *resolution,
            },
            Effect::Hsva { resolution, .. } => Effect::Hsva {
                target: Some(image_handle),
                resolution: *resolution,
            },
            Effect::Gray { resolution, .. } => Effect::Gray {
                target: Some(image_handle),
                resolution: *resolution,
            },
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
            | Effect::Polar2CartesianCoords => unreachable!(),
        };
        (linked, Some(texture))
    } else {
        (effect.clone(), None)
    };

    // Create main (clickable) box.
    let element = cmds
        .spawn_bundle(PipelineElementBundle {
            size: element_size,
            effect: effect.clone(),
            color_mesh_bundle: ColorMesh2dBundle {
                transform: Transform::from_translation(translation.extend(0.01)),
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
            .fake
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
                effect.parameter(param_idx),
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
pub(crate) fn create_text_field(
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
        .add_child(inner_text.fake)
        .insert_bundle(TextFieldBundle {
            color_mesh_bundle: ColorMesh2dBundle {
                // z=+0.25 to be in front of the element background (+0) and behind text (+0.5).
                transform: Transform::from_translation(center.extend(0.25)),
                material,
                mesh: mesh_assets.add(Mesh::from(shape::Quad::new(size))).into(),
                ..Default::default()
            },
            value: TextValue::new(value.to_string(), inner_text.real),
            target_value: ValueBinding {
                entity: param_target.0,
                parameter_idx: param_target.1,
            },
            interaction: InteractionBundle::default(),
        })
        .id()
}

/// Create a text
pub(crate) fn create_text(
    cmds: &mut Commands,
    label: &str,
    color: Color,
    transform: Transform,
    font: &Handle<Font>,
    horizontal: HorizontalAlign,
) -> FakeText {
    let sections = label
        .lines()
        .map(|line| TextSection {
            value: line.to_string(),
            style: TextStyle {
                color,
                font_size: DEFAULT_FONT_SIZE,
                font: (*font).clone(),
            },
        })
        .collect();
    // Move inner text clearly in front. 0.5 layers to not collide with a full layer in
    // front (if something is placed on that layer) while being bit-exact.
    let text_transform = Transform::from_translation(Vec3::new(0.0, 0.0, 0.5));
    let real = cmds
        .spawn_bundle(Text2dBundle {
            text: Text {
                sections,
                alignment: TextAlignment {
                    vertical: VerticalAlign::Center,
                    horizontal,
                },
            },
            ..Default::default()
        })
        .id();
    let fake = cmds
        .spawn_bundle(TransformBundle {
            local: transform * text_transform,
            global: GlobalTransform::default(),
        })
        .insert(TextDummy { real })
        .id();
    FakeText { real, fake }
}

pub(crate) struct FakeText {
    real: Entity,
    fake: Entity,
}

/// A component for a dummy entity that is used as an anchor for correctly place *and scale* text
/// without aliasing issues.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Component)]
pub(crate) struct TextDummy {
    real: Entity,
}

pub(crate) struct TextFixPlugin;

impl Plugin for TextFixPlugin {
    fn build(&self, app: &mut App) {
        app.add_system_set_to_stage(
            CoreStage::PostUpdate,
            SystemSet::new()
                .with_system(Self::update_text_transform)
                .after(TransformSystem::TransformPropagate),
        )
        .add_system(Self::synchronized_delete_text);
    }
}

impl TextFixPlugin {
    /// A system to apply transformations to text entities based on the corresponding [TextDummy]
    /// entities.
    pub(crate) fn update_text_transform(
        dummies: Query<(&GlobalTransform, &TextDummy), Changed<GlobalTransform>>,
        mut texts: Query<(&mut Transform, &mut GlobalTransform, &mut Text), Without<TextDummy>>,
    ) {
        for (transform, dummy) in dummies.iter() {
            if let Ok((mut local, mut global, mut text)) = texts.get_mut(dummy.real) {
                local.translation = transform.translation;
                global.translation = transform.translation;
                for section in &mut text.sections {
                    section.style.font_size =
                        DEFAULT_FONT_SIZE * transform.scale.truncate().min_element();
                }
            }
        }
    }

    /// Delete the real [Text] when the [DummyText] does not exist any more.
    ///
    /// # Impl Note
    ///
    /// `last` holds the (real) text entities that where referenced by
    pub(crate) fn synchronized_delete_text(
        mut cmds: Commands,
        dummies: Query<&TextDummy>,
        mut last: Local<HashSet<TextDummy>>,
    ) {
        let current = HashSet::from_iter(dummies.iter().copied());
        last.difference(&current)
            .for_each(|deleted| cmds.entity(deleted.real).despawn());
        *last = current;
    }
}

/// Create an entity showing an image of a given square size.
///
/// # Note
///
/// All images are created as RGBA, even for GrayA [Effect]s.
pub(crate) fn create_image_entity(
    cmds: &mut Commands,
    mesh_assets: &mut Assets<Mesh>,
    material_assets: &mut Assets<ColorMaterial>,
    image_assets: &mut Assets<Image>,
    size: f32,
    transform: Transform,
    resolution: Size<u32>,
) -> (Entity, Handle<Image>) {
    let handle = image_assets.add(Image::new(
        Extent3d {
            width: resolution.width,
            height: resolution.height,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        std::iter::repeat(Color::GRAY.as_rgba_f32())
            .take(resolution.width as usize * resolution.height as usize)
            .flatten()
            .flat_map(LOCAL_TO_GPU_BYTE_ORDER)
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
