use crate::{Effect, MyInteraction};
use bevy::input::keyboard::KeyboardInput;
use bevy::input::ElementState;
use bevy::prelude::*;

pub(crate) struct TextEntryPlugin;

impl Plugin for TextEntryPlugin {
    fn build(&self, app: &mut App) {
        app.add_system(TextEntryPlugin::set_focus)
            .add_system(TextEntryPlugin::tab_focus)
            .add_system(TextEntryPlugin::change_text)
            .add_system(TextEntryPlugin::text_display)
            .add_system(TextEntryPlugin::update_bound_parameter)
            .insert_resource(Focus::default());
    }
}

impl TextEntryPlugin {
    /// Sets the text entry focus based on [MyInteraction]s.
    #[allow(clippy::type_complexity)]
    fn set_focus(
        interactions: Query<(Entity, &MyInteraction), (Changed<MyInteraction>, With<TextValue>)>,
        defocus_interactions: Query<&MyInteraction, (Changed<MyInteraction>, Without<TextValue>)>,
        mut focus: ResMut<Focus>,
    ) {
        for (entity, interaction) in interactions.iter() {
            match interaction {
                MyInteraction::Pressed => focus.0 = Some(entity),
                MyInteraction::None | MyInteraction::Hover => {}
            }
        }
        if defocus_interactions
            .iter()
            .any(|i| matches!(i, MyInteraction::Pressed))
        {
            focus.0 = None;
        }
    }

    /// Cycle focus on text entries based on position within a pipeline element.
    fn tab_focus(
        keyboard_input: ResMut<Input<KeyCode>>,
        focus: ResMut<Focus>,
        children: Query<&Children, With<Effect>>,
        text_entries: Query<(), With<TextValue>>,
        parents: Query<&Parent>,
    ) {
        fn inner(
            keyboard_input: ResMut<Input<KeyCode>>,
            mut focus: ResMut<Focus>,
            children: Query<&Children, With<Effect>>,
            text_entries: Query<(), With<TextValue>>,
            parents: Query<&Parent>,
        ) -> Result<(), bevy::ecs::query::QueryEntityError> {
            let is_text_entry = |e: &&Entity| text_entries.get(**e).is_ok();

            if keyboard_input.just_pressed(KeyCode::Tab) {
                let tab = if !keyboard_input.pressed(KeyCode::LShift) {
                    1isize
                } else {
                    -1
                };
                if let Some(focused) = focus.0.as_mut() {
                    let element = parents.get(*focused)?.0;
                    let candidates = children
                        .get(element)?
                        .iter()
                        .filter(is_text_entry)
                        .collect::<Vec<_>>();
                    let focus_idx =
                        candidates.iter().position(|e| **e == *focused).unwrap() as isize;
                    let new_focus =
                        (focus_idx + tab + candidates.len() as isize) as usize % candidates.len();
                    *focused = *candidates[new_focus];
                }
            }
            Ok(())
        }

        if let Err(e) = inner(keyboard_input, focus, children, text_entries, parents) {
            eprintln!("{:?}", e);
        }
    }

    /// Manipulates the text in the text field, according to keyboard input.
    fn change_text(
        mut text: Query<&mut TextValue>,
        focus: ResMut<Focus>,
        mut input: EventReader<KeyboardInput>,
    ) {
        if let Some(focused) = &focus.0 {
            if let Ok(mut text) = text.get_mut(*focused) {
                for event in input.iter() {
                    if let KeyboardInput {
                        key_code: Some(key_code),
                        state: ElementState::Pressed,
                        ..
                    } = event
                    {
                        let mut dirty = false;
                        let ch = match key_code {
                            KeyCode::Key1 | KeyCode::Numpad1 => Some('1'),
                            KeyCode::Key2 | KeyCode::Numpad2 => Some('2'),
                            KeyCode::Key3 | KeyCode::Numpad3 => Some('3'),
                            KeyCode::Key4 | KeyCode::Numpad4 => Some('4'),
                            KeyCode::Key5 | KeyCode::Numpad5 => Some('5'),
                            KeyCode::Key6 | KeyCode::Numpad6 => Some('6'),
                            KeyCode::Key7 | KeyCode::Numpad7 => Some('7'),
                            KeyCode::Key8 | KeyCode::Numpad8 => Some('8'),
                            KeyCode::Key9 | KeyCode::Numpad9 => Some('9'),
                            KeyCode::Key0 | KeyCode::Numpad0 => Some('0'),
                            KeyCode::Minus | KeyCode::NumpadSubtract => Some('-'),
                            KeyCode::Period
                            | KeyCode::Comma
                            | KeyCode::NumpadDecimal
                            | KeyCode::NumpadComma => Some('.'),
                            KeyCode::Back | KeyCode::Delete => {
                                text.text.pop();
                                dirty = true;
                                None
                            }
                            _ => None,
                        };
                        if let Some(ch) = ch {
                            text.text.push(ch);
                            dirty = true;
                        }
                        if dirty {
                            text.parsed = text.text.parse::<f32>().ok();
                        }
                    }
                }
            }
        }
    }

    /// Update the rendered text based on the [TextValue].
    fn text_display(values: Query<&TextValue, Changed<TextValue>>, mut displays: Query<&mut Text>) {
        for value in values.iter() {
            if let Ok(mut display) = displays.get_mut(value.display) {
                if let Some(section) = display.sections.first_mut() {
                    section.value.clone_from(&value.text);
                } else {
                    eprintln!("Text input does not have a text section.");
                }
            } else {
                eprintln!("Inner text not found for text field.");
            }
        }
    }

    /// Manipulates effect parameters based on changes to the text in text fields.
    fn update_bound_parameter(
        text: Query<(&TextValue, &ValueBinding), Changed<TextValue>>,
        mut params: Query<&mut Effect>,
    ) {
        for (
            TextValue { parsed, .. },
            ValueBinding {
                entity,
                parameter_idx,
            },
        ) in text.iter()
        {
            if let Some(parsed) = *parsed {
                match params.get_mut(*entity) {
                    Ok(mut effect) => match effect.as_mut() {
                        Effect::Constant(ref mut value) => *value = parsed,
                        Effect::Rotate(ref mut angle) => *angle = parsed,
                        Effect::Offset(ref mut x, ref mut y) => {
                            if *parameter_idx == 0 {
                                *x = parsed
                            } else {
                                *y = parsed
                            }
                        }
                        Effect::Scale(ref mut x, ref mut y) => {
                            if *parameter_idx == 0 {
                                *x = parsed
                            } else {
                                *y = parsed
                            }
                        }
                        _ => {}
                    },
                    _ => eprintln!("Text field bound to non-effect."),
                }
            }
        }
    }
}

/// A resource for holding the currently focused element.
#[derive(Debug, Default)]
struct Focus(Option<Entity>);

/// The currently held text in the text field.
///
/// The `parsed` field shall be updated with the parsed `text` whenever the `text` was changed.
/// The `display` entity is one with a [Text] component that will be updated with the `text`.
#[derive(Debug, Clone, Component)]
pub(crate) struct TextValue {
    text: String,
    parsed: Option<f32>,
    display: Entity,
}

impl TextValue {
    pub(crate) fn new(s: String, display: Entity) -> Self {
        Self {
            parsed: s.parse().ok(),
            text: s,
            display,
        }
    }
}

/// The binding of a value manipulation GUI element, like a [TextFieldBundle], from/to an entities'
/// effect parameter.
#[derive(Debug, Component)]
pub(crate) struct ValueBinding {
    /// The entity for which the [EffectType] is to be changed.
    pub(crate) entity: Entity,
    /// The parameter of the [EffectType], if there are multiple.
    pub(crate) parameter_idx: usize,
}
