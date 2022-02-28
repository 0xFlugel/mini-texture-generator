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

use bevy::core::FixedTimestep;
use bevy::input::mouse::MouseButtonInput;
use bevy::input::ElementState;
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::sprite::MaterialMesh2dBundle;
use std::default::Default;
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

const PIPELINE_ELEMENT_WIDTH: f32 = 150.0;
const PIPELINE_ELEMENT_HEIGHT: f32 = 50.0;
const CONNECTOR_SIZE: f32 = 15.0;

/// Entities with this component spawn new elements with the contained effect instead of begin
/// dragged around themselfes.
#[derive(Component)]
struct SidebarButton(EffectType);
/// Entities with this component get focused and draged and can be deleted around instead of
/// duplicating.
#[derive(Component)]
struct PipelineElement(EffectType);
/// A marker for the texure to write to.
#[derive(Component)]
struct GeneratedTexture;
/// The entity with this component is the root panel for the UI.
#[derive(Component)]
struct RootPanel;
/// A marker for the background panel to react to clicks.
#[derive(Component)]
struct PipelineBackground;
/// Entities with this component will have their position changed based on cursor motion.
#[derive(Component)]
struct Focused;
/// A hack to react to removed [Focused] components.
#[derive(Component)]
struct Defocused;
/// A component for entities that are currently being dragged. By necessity, they are also
/// [Focused].
#[derive(Component)]
struct Dragging {
    starting_point: Vec2,
}

/// A marker for inputs on [PipelineElement]s allowing a [Connection] to be attached.
///
/// The parent component is the [PipelineElement].
#[derive(Component)]
struct InputConnector;
/// A marker for outputs on [PipelineElement]s allowing any number of [Connection]s to be attached.
/// See [InputConnector].
#[derive(Component)]
struct OutputConnector;
/// A marker for a connection between two Connectors. While dragging the connection, the referenced
/// [Entity] of either field can be a floating node.
#[derive(Component)]
struct Connection {
    input_connector: Option<Entity>,
    output_connector: Entity,
}
/// A list of [InputConnector]s.
#[derive(Component)]
struct Inputs(Vec<Entity>);
/// A list of [OutputConnector]s.
#[derive(Component)]
struct Outputs(Vec<Entity>);

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(TrackMouseLocation)
        .add_startup_system(setup)
        .add_system_set(
            SystemSet::new()
                .with_run_criteria(FixedTimestep::step(1.0 / 60.0))
                .with_system(gen_texture),
        )
        .add_system(make_new_element)
        .add_system(delete_pipeline_elements)
        .add_system(visual_button_focus)
        .add_system(focus_elements)
        .add_system_to_stage(CoreStage::PostUpdate, start_stop_dragging)
        .add_system(drag)
        .insert_resource(MouseStartingPosition::default())
        .add_system(start_connecting)
        .add_system(highlight_snapping_connector)
        .add_system(visualize_highlighted_connector)
        .add_system(stop_connecting)
        .add_system(visualize_connections)
        .run();
}

/// Start building a connection (attached to the clicked connector).
fn start_connecting(
    clicked_output_connector: Query<
        (Entity, &Interaction),
        (With<OutputConnector>, Changed<Interaction>),
    >,
    clicked_input_connector: Query<
        (Entity, &Interaction),
        (With<InputConnector>, Changed<Interaction>),
    >,
    mut connections: Query<(Entity, &mut Connection)>,
    focused_pipeline_elements: Query<Entity, (With<Focused>, Without<Defocused>)>,
    mut cmds: Commands,
) {
    // New Connection
    let connection = if let Some(connector) =
        clicked_output_connector.iter().find_map(|(e, i)| match i {
            Interaction::Clicked => Some(e as Entity),
            _ => None,
        }) {
        Some(
            cmds.spawn()
                .insert(Connection {
                    output_connector: connector,
                    input_connector: None,
                })
                .id(),
        )
    // Reconnect existing connection to new input.
    } else if let Some(connector_to_disconnect) =
        clicked_input_connector.iter().find_map(|(e, i)| match i {
            Interaction::Clicked => Some(e as Entity),
            _ => None,
        })
    {
        if let Some((entity_connection, mut connection)) = connections
            .iter_mut()
            .find(|(_e, c)| c.input_connector == Some(connector_to_disconnect))
        {
            connection.input_connector = None;
            Some(entity_connection)
        } else {
            None
        }
    } else {
        None
    };

    if let Some(connection) = connection {
        for elem in focused_pipeline_elements.iter() {
            cmds.entity(elem).insert(Defocused);
        }
        cmds.entity(connection).insert(Focused);
    }
}

/// A marker for highlighting an entity.
#[derive(Debug, Component)]
struct Highlight;

/// Hightlight a fitting connector that the currently creating [Connection] snaps to, when finished
/// right now.
fn highlight_snapping_connector(
    current_connection: Query<&Connection, With<Focused>>,
    interactions: Query<(Entity, &Interaction), (With<InputConnector>, Changed<Interaction>)>,
    pipeline_elements: Query<(&Inputs, &Outputs), With<PipelineElement>>,
    mut cmds: Commands,
) {
    if let Ok(current_connection) = current_connection.get_single() {
        let current_connection = current_connection as &Connection;

        if let Ok((entity_connector, interaction)) = interactions.get_single() {
            let entity_connector = entity_connector as Entity;
            let interaction = interaction as &Interaction;

            match interaction {
                Interaction::None => {
                    cmds.entity(entity_connector).remove::<Highlight>();
                }
                Interaction::Hovered => {
                    // Exclude connecting to self.
                    //TODO: expand to exclude all children in addition to self as well.
                    let self_inputs: &Inputs = pipeline_elements
                        .iter()
                        .find(|(_, o)| {
                            o.0.contains(&(current_connection as &Connection).output_connector)
                        })
                        .expect("A connection must always connect to an output connector.")
                        .0;
                    if !self_inputs.0.contains(&entity_connector) {
                        cmds.entity(entity_connector).insert(Highlight);
                    }
                }
                _ => {}
            }
        }
    }
}

fn visualize_highlighted_connector(
    mut highlighted: Query<&mut Transform, (With<InputConnector>, Added<Highlight>)>,
    mut normal: Query<&mut Transform, (With<InputConnector>, Without<Highlight>)>,
) {
    const HIGHLIGHT_SCALE: f32 = 1.5;

    for mut t in highlighted.iter_mut() {
        t.scale = Vec3::new(HIGHLIGHT_SCALE, HIGHLIGHT_SCALE, 1.0);
    }
    for mut t in normal.iter_mut() {
        t.scale = Vec3::new(1.0, 1.0, 1.0);
    }
}

/// Finish or abort creating a connection. Complement to `start_connecting`.
fn stop_connecting(
    mouse_buttons: Res<Input<MouseButton>>,
    mut connections: Query<(Entity, &mut Connection), With<Focused>>,
    snap_to: Query<Entity, (With<InputConnector>, With<Highlight>)>,
    mut cmds: Commands,
) {
    if !mouse_buttons.pressed(MouseButton::Left) {
        if let Ok((entity_connection, mut connection)) = connections.get_single_mut() {
            if let Ok(snap_connector) = snap_to.get_single() {
                debug_assert!(connection.input_connector.is_none());
                connection.input_connector = Some(snap_connector);
                cmds.entity(entity_connection).remove::<Focused>();
                cmds.entity(snap_connector).remove::<Highlight>();
            } else {
                cmds.entity(entity_connection).despawn_recursive();
            }
        }
    }
}

fn visualize_connections(
    connections: Query<&Connection>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    mut material_assets: ResMut<Assets<ColorMaterial>>,
    mut cmds: Commands,
    mut quad_handle: Local<Option<Handle<Mesh>>>,
    mouse_pos: Res<MousePosition>,
) {
    const CONNECTION_WIDTH: f32 = 10.0;

    let connector_center = |connector_entity: &Entity| -> Vec2 { todo!() };

    let handle =
        quad_handle.get_or_insert_with(|| mesh_assets.add(Mesh::from(shape::Quad::default())));
    for Connection {
        input_connector,
        output_connector,
    } in connections.iter()
    {
        let start: Vec2 = connector_center(output_connector);
        let end: Vec2 = input_connector
            .as_ref()
            .map(connector_center)
            .unwrap_or_else(|| mouse_pos.position);
        let distance = end.distance(start);

        cmds.spawn_bundle(MaterialMesh2dBundle {
            mesh: handle.clone().into(),
            material: material_assets.add(ColorMaterial {
                color: Color::BLUE,
                ..Default::default()
            }),
            transform: Transform {
                translation: Vec2::new().extend(0.0),
                scale: Vec3::new(CONNECTION_WIDTH, distance, 1.0),
                rotation: Quat::from_rotation_z(),
            },
            ..Default::default()
        });
    }
}

fn setup(mut cmds: Commands, assets: Res<AssetServer>) {
    cmds.spawn_bundle(UiCameraBundle::default());
    cmds.spawn_bundle(OrthographicCameraBundle::new_2d());
    let row_style = Style {
        flex_direction: FlexDirection::Row,
        align_items: AlignItems::FlexStart,
        size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
        ..Default::default()
    };
    let column_style = Style {
        flex_direction: FlexDirection::Column,
        size: Size::new(Val::Auto, Val::Percent(100.0)),
        justify_content: JustifyContent::FlexEnd,
        padding: Rect {
            right: Val::Px(5.0),
            left: Val::Px(5.0),
            ..Default::default()
        },
        ..row_style
    };
    let column_center_style = Style {
        flex_direction: FlexDirection::Column,
        align_items: AlignItems::Center,
        justify_content: JustifyContent::SpaceBetween,
        size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
        ..Default::default()
    };
    cmds.spawn_bundle(NodeBundle {
        style: row_style,
        color: UiColor(PIPELINE_BG),
        ..Default::default()
    })
    .insert(RootPanel)
    .with_children(|root| {
        let mut sidebar = root.spawn_bundle(NodeBundle {
            style: column_style,
            color: UiColor(SIDEBAR),
            ..Default::default()
        });
        sidebar.with_children(|sidebar| {
            for (text, effect) in EffectType::all().iter().map(|e| (e.name(), e)) {
                sidebar
                    .spawn_bundle(ButtonBundle {
                        style: Style {
                            flex_direction: FlexDirection::ColumnReverse,
                            size: Size::new(Val::Percent(100.0), Val::Auto),
                            margin: Rect {
                                top: Val::Px(5.0),
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                        ..Default::default()
                    })
                    .insert(SidebarButton(*effect))
                    .with_children(|btn| {
                        btn.spawn_bundle(TextBundle {
                            style: Style {
                                size: Size::new(Val::Undefined, Val::Px(25.)),
                                margin: Rect {
                                    left: Val::Px(5.0),
                                    right: Val::Px(5.0),
                                    ..Default::default()
                                },
                                ..Default::default()
                            },
                            text: Text::with_section(
                                text,
                                TextStyle {
                                    font: assets.load("FiraSans-Bold.ttf"),
                                    font_size: 25.,
                                    color: Color::WHITE,
                                },
                                Default::default(),
                            ),
                            ..Default::default()
                        });
                    });
            }
        });
        root.spawn_bundle(ButtonBundle {
            style: column_center_style.clone(),
            color: UiColor(PIPELINE_BG),
            ..Default::default()
        })
        .insert(PipelineBackground);
    });
}

/// A system to create new elements by taking them from the sidebar.
fn make_new_element(
    clicked_sidebar_button: Query<(&SidebarButton, &Interaction), Changed<Interaction>>,
    focused: Query<Entity, With<Focused>>,
    root: Query<Entity, With<RootPanel>>,
    mouse_pos: Res<MousePosition>,
    mut cmds: Commands,
    assets: Res<AssetServer>,
    mut asset_base: ResMut<Assets<Image>>,
) {
    if let Ok((SidebarButton(effect), Interaction::Clicked)) = clicked_sidebar_button.get_single() {
        let insert_texture = matches!(effect, EffectType::_Hsva | EffectType::_Rgba);
        let (width, height) = if insert_texture {
            (
                f32::max(
                    PIPELINE_ELEMENT_WIDTH,
                    (NUM_PIX * PIXEL_SHOW_RESOLUTION) as f32,
                ),
                PIPELINE_ELEMENT_HEIGHT + (NUM_PIX * PIXEL_SHOW_RESOLUTION) as f32,
            )
        } else {
            (PIPELINE_ELEMENT_WIDTH, PIPELINE_ELEMENT_HEIGHT)
        };

        for entity in focused.iter() {
            cmds.entity(entity).insert(Defocused);
        }
        let mut inputs = vec![];
        let mut outputs = vec![];
        cmds.entity(root.get_single().expect("Root panel should exist."))
            .with_children(|element| {
                element
                    .spawn_bundle(ButtonBundle {
                        style: Style {
                            position_type: PositionType::Absolute,
                            position: Rect {
                                left: Val::Px(mouse_pos.position.x - width / 2.0),
                                bottom: Val::Px(mouse_pos.position.y - height / 2.0),
                                ..Default::default()
                            },
                            size: Size::new(Val::Px(width), Val::Px(height)),
                            ..Default::default()
                        },
                        ..Default::default()
                    })
                    .insert(Focused)
                    .insert(PipelineElement(*effect))
                    .with_children(|btn| {
                        btn.spawn_bundle(TextBundle {
                            style: Style {
                                size: Size::new(Val::Auto, Val::Auto),
                                ..Default::default()
                            },
                            text: Text::with_section(
                                effect.name(),
                                TextStyle {
                                    font: assets.load("FiraSans-Bold.ttf"),
                                    font_size: 25.,
                                    color: Color::WHITE,
                                },
                                TextAlignment::default(),
                            ),
                            ..Default::default()
                        });
                        //connectors
                        //TODO draw text marker onto connectors.
                        for i in 0..effect.inputs() {
                            let per_i = (width - CONNECTOR_SIZE) / effect.inputs() as f32;
                            let left = Val::Px(per_i * i as f32 + per_i / 2.0);
                            inputs.push(
                                btn.spawn_bundle(ButtonBundle {
                                    style: Style {
                                        position_type: PositionType::Absolute,
                                        position: Rect {
                                            left,
                                            bottom: Val::Px(0.0),
                                            ..Default::default()
                                        },
                                        size: Size {
                                            width: Val::Px(CONNECTOR_SIZE),
                                            height: Val::Px(CONNECTOR_SIZE),
                                        },
                                        ..Default::default()
                                    },
                                    color: UiColor(INPUT_CONNECTOR),
                                    ..Default::default()
                                })
                                .insert(InputConnector)
                                .id(),
                            );
                        }
                        for i in 0..effect.outputs() {
                            let per_i = (width - CONNECTOR_SIZE) / effect.outputs() as f32;
                            let left = Val::Px(per_i * i as f32 + per_i / 2.0);
                            outputs.push(
                                btn.spawn_bundle(ButtonBundle {
                                    style: Style {
                                        position_type: PositionType::Absolute,
                                        position: Rect {
                                            left,
                                            bottom: Val::Px(height - CONNECTOR_SIZE),
                                            ..Default::default()
                                        },
                                        size: Size {
                                            width: Val::Px(CONNECTOR_SIZE),
                                            height: Val::Px(CONNECTOR_SIZE),
                                        },
                                        ..Default::default()
                                    },
                                    color: UiColor(OUTPUT_CONNECTOR),
                                    ..Default::default()
                                })
                                .insert(OutputConnector)
                                .id(),
                            );
                        }
                        if insert_texture {
                            // texture
                            btn.spawn()
                                .insert_bundle(NodeBundle {
                                    style: Style {
                                        size: Size::new(
                                            Val::Px((NUM_PIX * PIXEL_SHOW_RESOLUTION) as _),
                                            Val::Px((NUM_PIX * PIXEL_SHOW_RESOLUTION) as _),
                                        ),
                                        position: Rect {
                                            top: Val::Px(0.0),
                                            left: Val::Px(0.0),
                                            right: Val::Px(0.0),
                                            ..Default::default()
                                        },
                                        position_type: PositionType::Absolute,
                                        margin: Rect::all(Val::Px(2.0)),
                                        ..Default::default()
                                    },
                                    image: UiImage(asset_base.add(Image::new(
                                        Extent3d {
                                            width: NUM_PIX as _,
                                            height: NUM_PIX as _,
                                            depth_or_array_layers: 1,
                                        },
                                        TextureDimension::D2,
                                        vec![
                                            0;
                                            (NUM_PIX * NUM_PIX * 4/*channels*/ * 4/*bytes/float*/)
                                                as _
                                        ],
                                        TextureFormat::Rgba32Float,
                                    ))),
                                    ..Default::default()
                                })
                                .insert(GeneratedTexture);
                        }
                    })
                    .insert(Inputs(inputs))
                    .insert(Outputs(outputs));
            });
    }
}

fn delete_pipeline_elements(
    keys: Res<Input<KeyCode>>,
    focused: Query<Entity, With<Focused>>,
    mut cmds: Commands,
) {
    if keys.pressed(KeyCode::Delete) {
        for entity in focused.iter() {
            cmds.entity(entity).despawn_recursive();
        }
    }
}

#[derive(Default)]
struct MouseStartingPosition(Vec2);

fn drag(
    mut dragging: Query<(&mut Style, &Dragging)>,
    curr: Res<MousePosition>,
    start: Res<MouseStartingPosition>,
) {
    if curr.just_moved {
        for (mut style, drag_start_pos) in dragging.iter_mut() {
            let diff = curr.position - start.0;
            style.position.bottom = Val::Px(drag_start_pos.starting_point.y + diff.y);
            style.position.left = Val::Px(drag_start_pos.starting_point.x + diff.x);
        }
    }
}

fn start_stop_dragging(
    mut mouse_btn_events: EventReader<MouseButtonInput>,
    draggable: Query<
        (Entity, &Style),
        (
            With<Focused>,
            Without<Defocused>,
            // Without<InputConnector>,
            // Without<OutputConnector>,
        ),
    >,
    dragging: Query<Entity, With<Dragging>>,
    curr: Res<MousePosition>,
    mut start: ResMut<MouseStartingPosition>,
    mut cmds: Commands,
) {
    let mut pressed = false;
    let mut released = false;
    #[allow(unreachable_patterns)] // given for match _ (any) in error.
    for input in mouse_btn_events.iter() {
        match input {
            &MouseButtonInput {
                button: MouseButton::Left,
                state,
            } => match state {
                ElementState::Pressed => pressed = true,
                ElementState::Released => released = true,
            },
            _ => {}
        }
    }
    if pressed && !released {
        start.0 = curr.position;
        for (entity, style) in draggable.iter() {
            match (style.position.left, style.position.bottom) {
                (Val::Px(x), Val::Px(y)) => {
                    cmds.entity(entity).insert(Dragging {
                        starting_point: Vec2::new(x, y),
                    });
                }
                _ => eprintln!("Entity position value is not given by pixel."),
            }
        }
    } else if released {
        for entity in dragging.iter() {
            cmds.entity(entity).remove::<Dragging>();
        }
    }
}

fn focus_elements(
    interactions: Query<(Entity, &Interaction), (With<PipelineElement>, Changed<Interaction>)>,
    bg: Query<&Interaction, (With<PipelineBackground>, Changed<Interaction>)>,
    keyboard: Res<Input<KeyCode>>,
    focused: Query<Entity, With<Focused>>,
    mut cmds: Commands,
) {
    if let Ok((this, &Interaction::Clicked)) = interactions.get_single() {
        if keyboard.pressed(KeyCode::LControl) {
            // Focus *also* this.
            match focused.get(this) {
                Ok(_) => cmds.entity(this).insert(Defocused),
                Err(_) => cmds.entity(this).insert(Focused),
            };
        } else if focused.get(this).is_err() {
            // Focus *only* this.
            for entity in focused.iter() {
                cmds.entity(entity).insert(Defocused);
            }
            cmds.entity(this).insert(Focused);
        }
    } else if let Ok(&Interaction::Clicked) = bg.get_single() {
        for entity in focused.iter() {
            cmds.entity(entity).insert(Defocused);
        }
    }
}

fn visual_button_focus(
    mut focused: Query<&mut UiColor, (With<Button>, Added<Focused>, Without<Defocused>)>,
    mut unfocused: Query<
        (Entity, &mut UiColor),
        (With<Button>, Or<(Added<Defocused>, Added<SidebarButton>)>),
    >,
    mut cmds: Commands,
) {
    focused.for_each_mut(|mut c| *c = UiColor(FOCUSED_BUTTON));
    for (e, mut c) in unfocused.iter_mut() {
        *c = UiColor(NORMAL_BUTTON);
        cmds.entity(e).remove::<Focused>().remove::<Defocused>();
    }
}

//TODO render texture from pipeline.
fn gen_texture(
    q: Query<&UiImage, With<GeneratedTexture>>,
    t: Res<Time>,
    mut assets: ResMut<Assets<Image>>,
) {
    fn calc_pixel_value((x, y): (i32, i32), t: &Time) -> [f32; 4] {
        let black = Color::BLACK.as_rgba_f32();
        let purple = Color::PURPLE.as_rgba_f32();
        let alpha = ((((x + y) % 2) as f32 * std::f32::consts::PI
            + t.seconds_since_startup() as f32)
            .cos()
            + 1.0)
            / 2.0;
        [
            black[0] * alpha + purple[0] * (1.0 - alpha),
            black[1] * alpha + purple[1] * (1.0 - alpha),
            black[2] * alpha + purple[2] * (1.0 - alpha),
            black[3] * alpha + purple[3] * (1.0 - alpha),
        ]
    }

    for q in q.iter() {
        let img: &UiImage = q;
        match assets.get_mut(&img.0) {
            Some(img) => unsafe {
                let range: Range<*mut f32> = std::mem::transmute(img.data.as_mut_ptr_range());
                let flat_values = img.data.len() / 4 /*f32 bytes pro u8 byte*/;
                debug_assert_eq!(
                    flat_values,
                    (NUM_PIX * NUM_PIX) as usize /*resolution*/ * 4, /*channels*/
                );

                for x in 0..NUM_PIX {
                    for y in 0..NUM_PIX {
                        let idx = (y * NUM_PIX * 4 + x * 4) as usize;
                        for (offset, value) in calc_pixel_value((x as _, y as _), t.as_ref())
                            .iter()
                            .enumerate()
                        {
                            let offset = (idx + offset) as isize;
                            assert!(range.start.offset(offset) < range.end);
                            range.start.offset(offset).write(*value);
                        }
                    }
                }
            },
            None => eprintln!("Proc Gen Texture not accessible."),
        }
    }
}

#[derive(Copy, Clone)]
enum EffectType {
    _Constant,
    _Time,
    _Sine,
    _Addition,
    _Subtraction,
    _Multiply,
    _Division,
    _Modulus,
    _Rotate,
    _Hash1,
    _Threshold,
    _PerlinNoise,
    _Rgba,
    _Hsva,
}

impl EffectType {
    fn all() -> &'static [Self] {
        &[
            Self::_Constant,
            Self::_Time,
            Self::_Sine,
            Self::_Addition,
            Self::_Subtraction,
            Self::_Multiply,
            Self::_Division,
            Self::_Modulus,
            Self::_Rotate,
            Self::_Hash1,
            Self::_Threshold,
            Self::_PerlinNoise,
            Self::_Rgba,
            Self::_Hsva,
        ]
    }

    fn name(&self) -> &'static str {
        match self {
            EffectType::_Constant => "Constant",
            EffectType::_Time => "Time",
            EffectType::_Sine => "Sine",
            EffectType::_Addition => "Addition",
            EffectType::_Subtraction => "Subtraction",
            EffectType::_Multiply => "Multiply",
            EffectType::_Division => "Division",
            EffectType::_Modulus => "Modulus",
            EffectType::_Rotate => "Rotate",
            EffectType::_Hash1 => "Hash1",
            EffectType::_Threshold => "Threshold",
            EffectType::_PerlinNoise => "PerlinNoise",
            EffectType::_Rgba => "RGBA",
            EffectType::_Hsva => "HSV",
        }
    }

    fn inputs(&self) -> usize {
        match self {
            EffectType::_Constant => 0,
            EffectType::_Time => 0,
            EffectType::_Sine => 1,
            EffectType::_Addition => 2,
            EffectType::_Subtraction => 2,
            EffectType::_Multiply => 2,
            EffectType::_Division => 2,
            EffectType::_Modulus => 2,
            EffectType::_Rotate => 3,
            EffectType::_Hash1 => 3,
            EffectType::_Threshold => 2,
            EffectType::_PerlinNoise => 4,
            EffectType::_Rgba => 4,
            EffectType::_Hsva => 4,
        }
    }

    fn outputs(&self) -> usize {
        match self {
            EffectType::_Constant => 1,
            EffectType::_Time => 1,
            EffectType::_Sine => 1,
            EffectType::_Addition => 1,
            EffectType::_Subtraction => 1,
            EffectType::_Multiply => 1,
            EffectType::_Division => 1,
            EffectType::_Modulus => 1,
            EffectType::_Rotate => 2,
            EffectType::_Hash1 => 1,
            EffectType::_Threshold => 1,
            EffectType::_PerlinNoise => 1,
            EffectType::_Rgba => 0,
            EffectType::_Hsva => 0,
        }
    }
}

/// A resource to track the mouse location.
#[derive(Default)]
struct MousePosition {
    position: Vec2,
    just_moved: bool,
}
struct TrackMouseLocation;
impl Plugin for TrackMouseLocation {
    fn build(&self, app: &mut App) {
        app.add_system_to_stage(CoreStage::PreUpdate, TrackMouseLocation::track_mouse)
            .insert_resource(MousePosition::default());
    }
}
impl TrackMouseLocation {
    /// A system to track the mouse location and make it available as a resource.
    fn track_mouse(mut r: ResMut<MousePosition>, mut events: EventReader<CursorMoved>) {
        let mut events = events.iter();
        match events.next() {
            None => r.just_moved = false,
            Some(e) => {
                r.position = e.position;
                r.just_moved = true;
            }
        }
        for e in events {
            r.position = e.position;
        }
    }
}