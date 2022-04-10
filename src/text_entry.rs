use crate::{Effect, MyInteraction};
use bevy::input::keyboard::KeyboardInput;
use bevy::input::mouse::MouseWheel;
use bevy::input::ElementState;
use bevy::prelude::*;
use std::ops::BitXor;

pub(crate) struct TextEntryPlugin;

impl Plugin for TextEntryPlugin {
    fn build(&self, app: &mut App) {
        app.add_system(TextEntryPlugin::set_focus)
            .add_system(TextEntryPlugin::tab_focus)
            .add_system(TextEntryPlugin::change_text)
            .add_system(TextEntryPlugin::value_scrolling)
            .add_system(TextEntryPlugin::reparse_changed_text)
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
        focus: Res<Focus>,
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
                                None
                            }
                            KeyCode::E => Some('e'),
                            _ => None,
                        };
                        if let Some(ch) = ch {
                            text.text.push(ch);
                        }
                    }
                }
            }
        }
    }

    fn reparse_changed_text(mut text_entries: Query<&mut TextValue, Changed<TextValue>>) {
        for mut text in text_entries.iter_mut() {
            text.parsed = text.text.parse::<f32>().ok();
        }
    }

    /// A system to increment or decrement the lowest written digit of the parameter by mouse wheel.
    fn value_scrolling(
        mut entry_fields: Query<(&MyInteraction, &mut TextValue), With<TextValue>>,
        mut input: EventReader<MouseWheel>,
    ) {
        if let Some(wheel) = input.iter().next() {
            if wheel.y.abs() > f32::EPSILON {
                let hovered = entry_fields
                    .iter_mut()
                    .filter(|(i, _)| matches!(i, MyInteraction::Hover))
                    .map(|(_, text)| text);
                for text in hovered {
                    let mut text: Mut<TextValue> = text;
                    if text.parsed.is_some() {
                        let increment = wheel.y > 0.0;
                        let bcd = text.text.chars().collect::<Vec<char>>();
                        text.text = increment_bcd(bcd, increment).into_iter().collect();
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
                        Effect::Constant { value } => *value = parsed,
                        Effect::Rotate { degrees } => *degrees = parsed,
                        Effect::Offset { x, y } => {
                            if *parameter_idx == 0 {
                                *x = parsed
                            } else {
                                *y = parsed
                            }
                        }
                        Effect::Scale { x, y } => {
                            if *parameter_idx == 0 {
                                *x = parsed
                            } else {
                                *y = parsed
                            }
                        }
                        Effect::PerlinNoise { seed } => *seed = parsed.round() as u32,
                        Effect::SimplexNoise { seed } => *seed = parsed.round() as u32,
                        Effect::WhiteNoise { seed } => *seed = parsed.round() as u32,
                        Effect::Rgba { .. }
                        | Effect::Hsva { .. }
                        | Effect::Gray { .. }
                        | Effect::LinearX
                        | Effect::Add
                        | Effect::Sub
                        | Effect::Mul
                        | Effect::Div
                        | Effect::SineX
                        | Effect::StepX => {}
                    },
                    _ => eprintln!("Text field bound to non-effect."),
                }
            }
        }
    }
}

/// Increment or decrement (based on `increment` flag) a decimal number in form of a list of
/// chars of the digits.
///
/// This can process decimals (with or without fractions) in the positive and negative space.
/// ```
/// assert_eq!(
///     increment_bcd("0.0".chars().collect(), true).into_iter().collect::<String>(),
///     "0.1".to_string()
/// );
/// assert_eq!(
///     increment_bcd("0.00".chars().collect(), false).into_iter().collect::<String>(),
///     "-0.01".to_string()
/// );
/// ```
///
/// It ignores suffixes that are not continuous digits, meaning exponential forms are
/// compatible.
/// ```
/// assert_eq!(
///     increment_bcd("0.0e1".chars().collect(), true).into_iter().collect::<String>(),
///     "0.1e1".to_string()
/// );
/// ```
///
/// The number of trailing zeros are kept as is.
/// ```
/// assert_eq!(
///     increment_bcd("0.01".chars().collect(), false).into_iter().collect::<String>(),
///     "0.00".to_string()
/// );
/// ```
fn increment_bcd(mut chars: Vec<char>, increment: bool) -> Vec<char> {
    let least_significant_digit = chars
        .iter()
        .enumerate()
        .map_while(|(i, ch)| match ch {
            '0'..='9' => Some(Some(i)),
            '.' | '-' | '+' => Some(None),
            _ => None,
        })
        .last()
        .flatten();
    // Apply increment/decrement (and the rest of the addition/subtraction).
    if let Some(lsd) = least_significant_digit {
        let is_negative = chars.first() == Some(&'-');

        let add_to_magnitude = increment.bitxor(is_negative);
        let inc_dec = if add_to_magnitude { 1 } else { 0 };

        let mut idx = lsd as isize;
        let mut carry = true;
        while carry && idx >= 0 {
            let tmp = match chars[idx as usize] {
                '0' => Some([('9', true), ('1', false)][inc_dec]),
                '1' => Some([('0', false), ('2', false)][inc_dec]),
                '2' => Some([('1', false), ('3', false)][inc_dec]),
                '3' => Some([('2', false), ('4', false)][inc_dec]),
                '4' => Some([('3', false), ('5', false)][inc_dec]),
                '5' => Some([('4', false), ('6', false)][inc_dec]),
                '6' => Some([('5', false), ('7', false)][inc_dec]),
                '7' => Some([('6', false), ('8', false)][inc_dec]),
                '8' => Some([('7', false), ('9', false)][inc_dec]),
                '9' => Some([('8', false), ('0', true)][inc_dec]),
                '.' | '-' | '+' => None,
                _ => unreachable!(),
            };
            if let Some((digit, c)) = tmp {
                chars[idx as usize] = digit;
                carry = c;
            }
            idx -= 1;
        }
        if carry {
            if add_to_magnitude {
                // Insert after a leading sign.
                if chars.first() == Some(&'+') || chars.first() == Some(&'-') {
                    chars.insert(1, '1');
                } else {
                    chars.insert(0, '1');
                };
            } else {
                // Invert sign and set *LSD* to 1. Do not insert a leading '1'
                // since that is a different step size. Everthing in front of
                // the LSD was set to '9' (or left as `~[+-.]`).
                chars[lsd] = '1';
                if let Some(idx) = lsd.checked_sub(1) {
                    let mut idx = idx as isize;
                    while idx >= 0 {
                        if chars[idx as usize] == '9' {
                            chars[idx as usize] = '0';
                        }
                        idx -= 1;
                    }
                }
                if chars.first() == Some(&'+') {
                    chars[0] = '-';
                } else if chars.first() == Some(&'-') {
                    // Do not change to '+', just remove the sign.
                    chars.remove(0);
                } else {
                    chars.insert(0, '-');
                };
            }
        }
    }
    chars
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

/// The binding of a value manipulation GUI element, like a [crate::TextFieldBundle], from/to an entities'
/// effect parameter.
#[derive(Debug, Component)]
pub(crate) struct ValueBinding {
    /// The entity for which the [Effect] is to be changed.
    pub(crate) entity: Entity,
    /// The parameter of the `Effect`, if there are multiple.
    pub(crate) parameter_idx: usize,
}
