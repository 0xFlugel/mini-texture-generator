use bevy::prelude::*;
use bevy::render::render_resource::std140::Std140;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// Newtype of a float to implement hash-ability for memoization.
#[derive(Debug, Default, Copy, Clone)]
struct NoiseFloatType(f32);

impl PartialEq for NoiseFloatType {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for NoiseFloatType {}
impl Hash for NoiseFloatType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.0.as_bytes());
    }
}

/// An caching optimizer around [simplex_noise_sample].
#[derive(Debug)]
pub(crate) struct SimplexSampler {
    perm: Vec<i32>,
    history: HashMap<(NoiseFloatType, NoiseFloatType), NoiseFloatType>,
}

impl SimplexSampler {
    pub(crate) fn new(seed: u32) -> Self {
        Self {
            perm: gen_perm(seed),
            history: HashMap::default(),
        }
    }

    pub(crate) fn sample(&mut self, x: f32, y: f32) -> f32 {
        let value = simplex_noise_sample(x, y, &self.perm);
        self.history.insert(
            (NoiseFloatType(x), NoiseFloatType(y)),
            NoiseFloatType(value),
        );
        value
    }

    pub(crate) fn change_seed(&mut self, seed: u32) {
        self.history.clear();
        self.perm = gen_perm(seed);
    }
}

pub(crate) fn gen_perm(seed: u32) -> Vec<i32> {
    let lfsr_successor = |lfsr: &u16| {
        let mut lfsr = *lfsr;
        lfsr ^= lfsr >> 7;
        lfsr ^= lfsr << 9;
        lfsr ^= lfsr >> 13;
        Some(lfsr)
    };
    std::iter::successors(Some(0xACE1u16), lfsr_successor)
        .map(|v| (v & 0x00FF) as i32)
        .skip(seed as usize)
        .take(256)
        .cycle()
        .take(512)
        .collect()
}

// To remove the need for index wrapping, double the permutation table length
// A lookup table to traverse the simplex around a given point in 4D.
// Details can be found where this table is used, in the 4D noise method.
// 2D simplex noise
fn simplex_noise_sample(x: f32, y: f32, perm: &[i32]) -> f32 {
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
    let f2 = 0.5 * ((3.0 as f32).sqrt() - 1.0);
    let s = (x + y) * f2; // Hairy factor for 2D
    let i = (x + s).floor() as i32;
    let j = (y + s).floor() as i32;
    let g2 = (3.0 - (3.0 as f32).sqrt()) / 6.0;
    let t = (i as f32 + j as f32) * g2;
    let x0 = i as f32 - t; // Unskew the cell origin back to (x,y) space
    let y0 = j as f32 - t;
    let x0 = x - x0; // The x,y distances from the cell origin
    let y0 = y - y0;

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
    let x1 = x0 - i1 as f32 + g2; // Offsets for middle corner in (x,y) unskewed coords
    let y1 = y0 - j1 as f32 + g2;
    let x2 = x0 - 1.0 + 2.0 * g2; // Offsets for last corner in (x,y) unskewed coords
    let y2 = y0 - 1.0 + 2.0 * g2;
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
                .dot([x0 as f32, y0 as f32].into())
    };

    let mut t1 = 0.5 - x1 * x1 - y1 * y1;
    let n1 = if t1 < 0.0 {
        0.0
    } else {
        t1 *= t1;
        t1 * t1
            * grad3[gi1 as usize]
                .truncate()
                .dot([x1 as f32, y1 as f32].into())
    };

    let mut t2 = 0.5 - x2 * x2 - y2 * y2;
    let n2 = if t2 < 0.0 {
        0.0
    } else {
        t2 *= t2;
        t2 * t2
            * grad3[gi2 as usize]
                .truncate()
                .dot([x2 as f32, y2 as f32].into())
    };

    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    return 70.0 * (n0 + n1 + n2);
}
