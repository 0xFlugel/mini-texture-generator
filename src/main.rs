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

use bevy::prelude::*;

const SIDEBAR_BACKGROUND: [f32; 3] = [0.5, 0.5, 0.5];

fn main() {
    let mut app = App::new();
    app.add_plugins(DefaultPlugins).add_startup_system(setup);
    app.run();
}

fn setup(
    mut cmds: Commands,
    _asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
    _images: ResMut<Assets<Image>>,
) {
    cmds.spawn_bundle(OrthographicCameraBundle::new_2d());

    // Create sidebar
    {
        cmds.spawn_bundle(ColorMesh2dBundle {
            transform: transform_from_rect(Rect {
                left: -1.0,
                right: -1.0 + 1.0 / 8.0,
                top: 1.0,
                bottom: -1.0,
            }),
            mesh: meshes
                .add(Mesh::from(shape::Quad::new(Vec2::splat(2.0))))
                .into(),
            material: materials.add(ColorMaterial::from(Color::from(SIDEBAR_BACKGROUND))),
            ..Default::default()
        });
    }
}

/// Convert a rect in the normalized 2D space (-1..1 on X and Y axes) to a transform.
fn transform_from_rect(rect: Rect<f32>) -> Transform {
    let x = (rect.left + rect.right) / 2.0;
    let y = (rect.top + rect.bottom) / 2.0;
    let scale_x = (rect.right - rect.left) / 2.0;
    let scale_y = (rect.top - rect.bottom) / 2.0;
    Transform::from_translation(Vec3::new(x, y, 0.0)).with_scale(Vec3::new(scale_x, scale_y, 1.0))
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
        let result = dbg!(transform_from_rect(rect));
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
        let result = dbg!(transform_from_rect(rect));
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
        let result = dbg!(transform_from_rect(rect));
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
        let result = dbg!(transform_from_rect(rect));
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
}
