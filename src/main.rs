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
        .run();
}

fn setup(
    mut cmds: Commands,
    _asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
    _images: ResMut<Assets<Image>>,
    windows: Res<Windows>,
) {
    let window = windows.get_primary().unwrap();
    let x_scale = window.width() / 2.0;
    let y_scale = window.height() / 2.0;

    cmds.spawn_bundle(OrthographicCameraBundle::new_2d())
        .insert(RayCastSource::<MyRaycastSet>::new());
    cmds.insert_resource(DefaultPluginState::<MyRaycastSet>::default().with_debug_cursor());

    // Create sidebar
    let normalized_square = Mesh2dHandle(meshes.add(Mesh::from(shape::Icosphere::default())));
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
        .insert(RayCastMesh::<MyRaycastSet>::default())
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
        let child = cmds
            .spawn_bundle(ColorMesh2dBundle {
                transform: transform_from_rect(
                    Rect {
                        top: 1.0 - 2.0 * (offset / num),
                        bottom: 1.0 - 2.0 * ((offset + 2.0) / num),
                        left: -0.8,
                        right: 0.8,
                    },
                    1,
                ),
                mesh: normalized_square.clone(),
                material: materials.add(ColorMaterial::from(colors[i])),
                ..Default::default()
            })
            .insert(SidebarElement(*effect))
            .insert(MyInteraction::default())
            .insert(RayCastMesh::<MyRaycastSet>::default())
            .id();
        cmds.entity(sidebar).add_child(child);
    }
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
    mut q: Query<(Entity, &mut MyInteraction)>,
    mut events: EventReader<MouseButtonInput>,
    mut pressed: Local<Option<Entity>>,
    rays: Query<&RayCastSource<MyRaycastSet>>,
) {
    let hovering = rays
        .iter()
        .inspect(|a| println!("{:?}", a.ray()))
        .filter_map(RayCastSource::intersect_list)
        .inspect(|a| println!("{:?}", a))
        .flatten()
        .map(|(target, _)| target.clone())
        .next();
    for input in events.iter() {
        #[allow(unreachable_patterns)] // catch-all arm is a false-positive
        match input {
            MouseButtonInput {
                button: MouseButton::Left,
                state,
            } => match state {
                ElementState::Pressed => {
                    *pressed = dbg!(hovering);
                    if let Some(target) = *pressed {
                        *q.get_mut(dbg!(target)).unwrap().1 = MyInteraction::Pressed;
                    }
                }
                ElementState::Released => {
                    if let Some(e) = pressed.take() {
                        *q.get_mut(dbg!(e)).unwrap().1 = MyInteraction::None;
                    }
                }
            },
            _ => {}
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

#[derive(Debug, Copy, Clone)]
enum EffectType {
    Rgba,
    Hsva,
    Grey,
    Constant,
    Identity,
    Rotate,
    Offset,
    Scale,
}

impl EffectType {
    fn all() -> &'static [Self] {
        &[
            Self::Rgba,
            Self::Hsva,
            Self::Grey,
            Self::Constant,
            Self::Identity,
            Self::Rotate,
            Self::Offset,
            Self::Scale,
        ]
    }
}

/// A marker for being a template in the sidebar, instead of an interactive pipeline element.
#[derive(Debug, Component)]
struct SidebarElement(EffectType);

impl Deref for SidebarElement {
    type Target = EffectType;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Component, Copy, Clone, Eq, PartialEq)]
enum MyInteraction {
    None,
    Hovered,
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
