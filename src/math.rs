use crate::TEXTURE_SIZE;
use bevy::prelude::*;

pub(crate) type TexData = [[f32; TEXTURE_SIZE as usize]; TEXTURE_SIZE as usize];

/// Sample a grid of values with bi-linear interpolation and clamping.
pub(crate) fn sample(at: Vec2, pixels: &TexData) -> f32 {
    let zero = Vec2::splat(0.0);
    let one = Vec2::splat(1.0);
    let max = Vec2::splat(TEXTURE_SIZE as f32);

    let tl = at.floor().clamp(zero, max).to_array().map(|f| f as usize);
    let br = (at.floor() + one)
        .clamp(zero, max)
        .to_array()
        .map(|f| f as usize);
    let lerp_factor = at - at.floor();

    let a = lerp(pixels[tl[0]][tl[1]], pixels[tl[0]][br[1]], lerp_factor.y);
    let b = lerp(pixels[br[0]][tl[1]], pixels[br[0]][br[1]], lerp_factor.y);
    lerp(a, b, lerp_factor.x)
}

fn lerp(a: f32, b: f32, alpha: f32) -> f32 {
    a * alpha + b * (1.0 - alpha)
}

/// Generate a texture with simplex noise based on a `seed`.
pub(crate) fn fill_simplex_noise(seed: u32) -> TexData {
    todo!()
}
