use crate::TEXTURE_SIZE;
use bevy::prelude::*;

pub(crate) type TexData = [[f32; TEXTURE_SIZE as usize]; TEXTURE_SIZE as usize];

/// Sample a grid of values with bi-linear interpolation and clamping.
pub(crate) fn sample(at: Vec2, pixels: &TexData) -> f32 {
    let zero = Vec2::splat(0.0);
    let one = Vec2::splat(1.0);
    let max = Vec2::splat(TEXTURE_SIZE as f32 - 1.0);
    let offset = TEXTURE_SIZE as f32 / 2.0 - 0.5;

    let tl = (at + offset)
        .floor()
        .clamp(zero, max)
        .to_array()
        .map(|f| f as usize);
    let br = ((at + offset).floor() + one)
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
    // To remove the need for index wrapping, double the permutation table length
    // A lookup table to traverse the simplex around a given point in 4D.
    // Details can be found where this table is used, in the 4D noise method.
    // 2D simplex noise
    fn noise(xin: f64, yin: f64, perm: &[i32]) -> f64 {
        let grad3 = [
            Vec3::from([1.0, 1.0, 0.0]),
            Vec3::from([-1.0, 1.0, 0.0]),
            Vec3::from([1.0, -1.0, 0.0]),
            Vec3::from([-1.0, -1.0, 0.0]),
            Vec3::from([1.0, 0.0, 1.0]),
            Vec3::from([-1.0, 0.0, 1.0]),
            Vec3::from([1.0, 0.0, -1.0]),
            Vec3::from([-1.0, 0.0, -1.0]),
            Vec3::from([0.0, 1.0, 1.0]),
            Vec3::from([0.0, -1.0, 1.0]),
            Vec3::from([0.0, 1.0, -1.0]),
            Vec3::from([0.0, -1.0, -1.0]),
        ];

        // Skew the input space to determine which simplex cell we're in
        let f2 = 0.5 * (3f64.sqrt() - 1.0);
        let s = (xin + yin) * f2; // Hairy factor for 2D
        let i = (xin + s).floor() as i32;
        let j = (yin + s).floor() as i32;
        let g2 = (3.0 - 3f64.sqrt()) / 6.0;
        let t = (i as f64 + j as f64) * g2;
        let x0 = i as f64 - t; // Unskew the cell origin back to (x,y) space
        let y0 = j as f64 - t;
        let x0 = xin - x0; // The x,y distances from the cell origin
        let y0 = yin - y0;

        // For the 2D case, the simplex shape is an equilateral triangle.
        // Determine which simplex we are in.

        // Offsets for second (middle) corner of simplex in (i,j) coords
        let (i1, j1) = if x0 > y0 {
            (1, 0)
        } else {
            // lower triangle, XY order: (0,0)->(1,0)->(1,1)
            (0, 1)
        };

        // upper triangle, YX order: (0,0)->(0,1)->(1,1)
        // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
        // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
        // c = (3-sqrt(3))/6
        let x1: f64 = x0 - i1 as f64 + g2; // Offsets for middle corner in (x,y) unskewed coords
        let y1: f64 = y0 - j1 as f64 + g2;
        let x2: f64 = x0 - 1.0 + 2.0 * g2; // Offsets for last corner in (x,y) unskewed coords
        let y2: f64 = y0 - 1.0 + 2.0 * g2;
        // Work out the hashed gradient indices of the three simplex corners
        let ii: i32 = i & 255;
        let jj: i32 = j & 255;
        let gi0: i32 = perm[(ii + perm[jj as usize]) as usize] % 12;
        let gi1: i32 = perm[(ii + i1 + perm[jj as usize + j1 as usize]) as usize] % 12;
        let gi2: i32 = perm[(ii + 1 + perm[jj as usize + 1]) as usize] % 12;
        // Calculate the contribution from the three corners
        let mut t0 = 0.5 - x0 * x0 - y0 * y0;

        // Noise contributions from the three corners
        let n0 = if t0 < 0.0 {
            0.0
        } else {
            t0 *= t0;
            // (x,y) of grad3 used for 2D gradient
            t0 * t0
                * grad3[gi0 as usize]
                    .truncate()
                    .dot([x0 as f32, y0 as f32].into()) as f64
        };

        let mut t1 = 0.5 - x1 * x1 - y1 * y1;
        let n1 = if t1 < 0.0 {
            0.0
        } else {
            t1 *= t1;
            t1 * t1
                * grad3[gi1 as usize]
                    .truncate()
                    .dot([x1 as f32, y1 as f32].into()) as f64
        };

        let mut t2 = 0.5 - x2 * x2 - y2 * y2;
        let n2 = if t2 < 0.0 {
            0.0
        } else {
            t2 *= t2;
            t2 * t2
                * grad3[gi2 as usize]
                    .truncate()
                    .dot([x2 as f32, y2 as f32].into()) as f64
        };

        // Add contributions from each corner to get the final noise value.
        // The result is scaled to return values in the interval [-1,1].
        return 70.0 * (n0 + n1 + n2);
    }

    let random: Vec<i32> = std::iter::successors(Some(0xACE1_u16), |lfsr| {
        let mut lfsr = *lfsr;
        lfsr ^= lfsr >> 7;
        lfsr ^= lfsr << 9;
        lfsr ^= lfsr >> 13;
        Some(lfsr)
    })
    .map(|v| (v & 0x00FF) as i32)
    .skip(seed as usize)
    .take(256)
    .collect();

    let perm = (0..512).map(|i| random[i & 0x0FF]).collect::<Vec<_>>();

    let mut data = TexData::default();
    for x in 0..TEXTURE_SIZE as usize {
        for y in 0..TEXTURE_SIZE as usize {
            data[x][y] = noise(x as f64, y as f64, &perm) as _;
        }
    }
    data
}
