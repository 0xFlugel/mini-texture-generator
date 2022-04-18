use crate::{RootTransform, LINE_HEIGHT, SCALE_FACTOR, SCROLL_MULTIPLIER};
use bevy::input::mouse::{MouseButtonInput, MouseScrollUnit, MouseWheel};
use bevy::input::ElementState;
use bevy::prelude::*;
use bevy::utils::hashbrown::HashSet;
use bevy_mod_raycast::{DefaultRaycastingPlugin, RayCastMethod, RayCastSource, RaycastSystem};
use std::ops::Range;

pub(crate) struct InteractionPlugin;

impl Plugin for InteractionPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugin(DefaultRaycastingPlugin::<MyRaycastSet>::default())
            .add_system_set_to_stage(
                CoreStage::PreUpdate,
                SystemSet::new()
                    .with_system(InteractionPlugin::track_mouse)
                    .before(RaycastSystem::BuildRays),
            )
            .add_system_set_to_stage(
                CoreStage::PreUpdate,
                SystemSet::new()
                    .with_system(InteractionPlugin::apply_interactions)
                    .after(RaycastSystem::UpdateRaycast),
            )
            .add_system(InteractionPlugin::dragging)
            .add_system(InteractionPlugin::sidebar_scrolling)
            .add_system(InteractionPlugin::root_transforms)
            .insert_resource(MousePosition::default());
    }
}

impl InteractionPlugin {
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
    ///
    /// # Impl
    ///
    /// Use `if let ...` instead of `.unwrap()` to silently ignore the unlikely condition that the
    /// freshly generated entity list contains bad references.
    fn apply_interactions(
        mut interactive: Query<(Entity, &mut MyInteraction)>,
        mut events: EventReader<MouseButtonInput>,
        rays: Query<&RayCastSource<MyRaycastSet>>,
    ) {
        // Apply `Hovering` status.
        let hovering = rays
            .iter()
            .filter_map(RayCastSource::intersect_top)
            .map(|(target, _intersection)| target)
            .collect::<HashSet<_>>();
        for (e, mut interaction) in interactive.iter_mut() {
            if *interaction == MyInteraction::Hover && !hovering.contains(&e) {
                *interaction = MyInteraction::None;
            }
        }
        for e in &hovering {
            if let Ok((_entity, mut interaction)) = interactive.get_mut(*e) {
                if *interaction == MyInteraction::None {
                    *interaction = MyInteraction::Hover;
                }
            }
        }

        // Apply `Pressed` status.
        for input in events.iter() {
            let MouseButtonInput { button, state } = input;
            let button = match button {
                MouseButton::Left => MyInteraction::PressedLeft,
                MouseButton::Right => MyInteraction::PressedRight,
                _ => continue,
            };
            match state {
                ElementState::Pressed => {
                    for pressed in &hovering {
                        if let Ok((_entity, mut interaction)) = interactive.get_mut(*pressed) {
                            *interaction = button;
                        }
                    }
                }
                ElementState::Released => {
                    // Defensively release *all* clicked elements, not just the single one from here.
                    for (e, mut interaction) in interactive.iter_mut() {
                        if *interaction == MyInteraction::PressedLeft {
                            *interaction = if hovering.contains(&e) {
                                MyInteraction::Hover
                            } else {
                                MyInteraction::None
                            };
                        }
                    }
                }
            }
        }
    }

    /// Drag entities around their XY plane depending on cursor movement.
    #[allow(clippy::type_complexity)]
    fn dragging(
        start: Query<(Entity, &MyInteraction, &Transform), (With<Draggable>, Without<Dragging>)>,
        mut continue_: Query<(&Dragging, &mut Transform)>,
        stop: Query<(Entity, &MyInteraction), With<Dragging>>,
        mut cmds: Commands,
        current_mouse_position: Res<MousePosition>,
        root: Query<&Transform, (With<RootTransform>, Without<Dragging>)>,
    ) {
        start
            .iter()
            .filter(|(_, i, _)| **i == MyInteraction::PressedLeft)
            .for_each(|(e, _, base)| {
                cmds.entity(e).insert(Dragging {
                    start: *current_mouse_position,
                    base: *base,
                });
            });
        stop.iter()
            .filter(|(_, i)| **i != MyInteraction::PressedLeft)
            .for_each(|(e, _)| {
                cmds.entity(e).remove::<Dragging>();
            });
        for (Dragging { start, base }, mut transform) in continue_.iter_mut() {
            let root_scale = root.iter().next().copied().unwrap_or_default().scale;
            let translate = (current_mouse_position.position - start.position).extend(0.0);
            transform.translation = base.translation + translate / root_scale;
        }
    }

    /// Change the root transformation and with that all pipeline elements. This concerns scaling and
    /// moving the view.
    fn root_transforms(
        mut root: Query<&mut Transform, With<RootTransform>>,
        mouse_position: Res<MousePosition>,
        mouse_buttons: Res<Input<MouseButton>>,
        mut mouse_wheels: EventReader<MouseWheel>,
        mut prev_position: Local<Option<Vec2>>,
        interactions: Query<&MyInteraction>,
        windows: Res<Windows>,
        cam: Query<&Camera>,
    ) {
        // Always read the events as they are stored up if not.
        let scale_units = mouse_wheels.iter().fold(0.0, |acc, elem| {
            acc + if elem.y.abs() < f32::EPSILON {
                0.0f32
            } else {
                elem.y.signum()
            }
        });

        //TODO This is bad design. This system must check for other system to be not run. Better
        // would be a pre-running system to organize which general type of interaction is executed:
        // * element manipulation,
        // * sidebar manipulation or
        // * root manipulation.
        let shall_affect_root = interactions.iter().all(|i| i == &MyInteraction::None);

        if shall_affect_root {
            if let Some(mut transform) = root.iter_mut().next() {
                // Scaling (at cursor position).
                if scale_units.abs() > 0.5 {
                    let window = windows.get_primary().unwrap();
                    let window_size = Vec2::new(window.width(), window.height());
                    let cam = cam.iter().next().unwrap();

                    // Project cursor position into world coordinate system.
                    let fix_point = Transform::from_matrix(cam.projection_matrix.inverse())
                        * (Vec2::splat(-1.0) + mouse_position.position / window_size * 2.0)
                            .extend(0.0);

                    let scale = Vec2::splat(SCALE_FACTOR.powf(scale_units)).extend(1.0);
                    *transform = Transform::from_translation(fix_point)
                        * Transform::from_scale(scale)
                        * Transform::from_translation(-fix_point)
                        * *transform;
                }

                // Dragging
                if mouse_buttons.pressed(MouseButton::Left) {
                    let current_position = mouse_position.position;
                    let translation = (*prev_position)
                        .filter(|_| shall_affect_root)
                        .map(|prev| (current_position - prev).extend(0.0))
                        .unwrap_or_default();
                    transform.translation += translation;

                    *prev_position = Some(current_position);
                } else {
                    *prev_position = None;
                }
            }
        }
    }

    /// Apply scrolling via the [MouseWheel] onto the [Transform]s for entities that have a [Scroll]
    /// component.
    fn sidebar_scrolling(
        mut inputs: EventReader<MouseWheel>,
        mut hovered: Query<(&mut Transform, &MyInteraction, &mut Scroll)>,
    ) {
        for MouseWheel { y, unit, .. } in inputs.iter() {
            let delta = SCROLL_MULTIPLIER
                * y
                * match unit {
                    MouseScrollUnit::Line => LINE_HEIGHT,
                    // 1.0 is the pixel size because the camera is using Window coordinates.
                    MouseScrollUnit::Pixel => 1.0,
                };
            for (transform, interaction, scroll) in hovered.iter_mut() {
                let mut transform: Mut<Transform> = transform;
                let interaction: &MyInteraction = interaction;
                let mut scroll: Mut<Scroll> = scroll;
                if interaction == &MyInteraction::Hover {
                    let Scroll {
                        size,
                        range,
                        position,
                    } = &mut *scroll;
                    let point = if delta > 0.0 {
                        *position + *size
                    } else {
                        *position
                    };
                    let bounded_delta = (point + delta).clamp(range.start, range.end) - point;
                    *position += bounded_delta;
                    *transform = *transform
                        * Transform::from_translation(Vec3::new(0.0, bounded_delta, 0.0));
                }
            }
        }
    }
}

/// A component to enable scrolling functionality on an entity.
///
/// Other needed components are [MyInteraction] and [Transform].
#[derive(Debug, Default, Clone, Component)]
pub(crate) struct Scroll {
    /// The extend of the view being scrolled. The `position` cannot by less than `range.start` and
    /// `position+size` cannot be greater than `range.end.`.
    pub(crate) range: Range<f32>,
    /// A point in the range.
    pub(crate) position: f32,
    /// Size of the view that is being scrolled -- typically the size of the object on the scrolling
    /// axis.
    pub(crate) size: f32,
}

/// A marker for entities that can be dragged.
#[derive(Component)]
pub(crate) struct Draggable;

/// State while dragging an entity.
#[derive(Debug, Component)]
pub(crate) struct Dragging {
    pub(crate) start: MousePosition,
    /// The entity transform at the start of the drag action.
    ///
    /// # Note
    ///
    /// Changes from other systems to a currently dragged entity will not persist as its transform
    /// will be overwritten with the cached base value.
    pub(crate) base: Transform,
}

#[derive(Debug, Component, Copy, Clone, Eq, PartialEq)]
pub(crate) enum MyInteraction {
    /// When no other variant applies.
    None,
    /// When the cursor is over the entity (and no other entity is in front) but the left mouse
    /// button is not pressed.
    Hover,
    /// Set when the left mouse button is pressed while the entity was hovered over. Is reset, when
    /// the left mouse button is released -- i.e. not reset when the cursor moves is moved away.
    PressedLeft,
    PressedRight,
}

impl Default for MyInteraction {
    fn default() -> Self {
        Self::None
    }
}

/// A resource to track the mouse location.
#[derive(Debug, Default, Copy, Clone)]
pub struct MousePosition {
    pub(crate) position: Vec2,
    pub(crate) just_moved: bool,
}

/// A marker for ray-castable entities.
pub(crate) struct MyRaycastSet;
