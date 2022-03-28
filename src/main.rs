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

use bevy::render::camera::ScalingMode;
use bevy::{
    core::FixedTimestep,
    input::mouse::MouseButtonInput,
    input::ElementState,
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
    sprite::{Mesh2dHandle, SpecializedMaterial2d},
    text::Text2dSize,
};
use bevy_mod_picking::*;
use std::ops::Range;

/// The resolution per axis of the texture.
const NUM_PIX: usize = 16;
/// GUI size per texture pixel.
const PIXEL_SHOW_RESOLUTION: usize = 8;

const NORMAL_BUTTON: Color = Color::rgb(0.15, 0.15, 0.15);
const FOCUSED_BUTTON: Color = Color::rgb(0.35, 0.35, 0.35);
const INPUT_CONNECTOR: Color = Color::rgb(0.75, 0.75, 0.9);
const OUTPUT_CONNECTOR: Color = Color::rgb(0.9, 0.75, 0.75);
const SIDEBAR: Color = Color::rgb(0.85, 0.85, 0.85);
const PIPELINE_BG: Color = Color::NONE;

const PIPELINE_ELEMENT_WIDTH: f32 = 0.125;
const PIPELINE_ELEMENT_HEIGHT: f32 = 0.35;
const CONNECTOR_SIZE: f32 = 0.0125;

fn main() {
    let mut app = App::new();
    app.add_plugins(DefaultPlugins)
        .add_startup_system(setup)
        ;
    app.run();
}

fn setup() {}