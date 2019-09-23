fn gen_pi_viete() -> f64 {
    let n = 1000;
    let mut pi = 1_f64;
    for i in (2..=n).rev() {
        let mut f: f64 = 2_f64;
        for _j in 1..i {
            f = 2_f64 + f.sqrt();
        }
        f = f.sqrt();
        pi = pi * f / 2_f64;
    }
    pi = pi * 2_f64.sqrt() / 2_f64;
    pi = 2_f64 / pi;
    pi
}

fn gen_pi_nilakantha() -> f64 {
    let n = 1000000;
    let mut s = 1_f64;
    let mut pi = 3_f64;
    for i in (2..=n*2).step_by(2) {
        pi = pi + s * (4_f64 / (i as f64 * (i+1) as f64 * (i+2) as f64));
        s = -s;
    }
    pi
}

extern crate rug;
use rug::Integer;
use rug::Float;
fn gen_pi_nilakantha_arbitrary() -> Float {
    let n = 1000000;
    let mut s = 1_f64;
    let mut pi = Float::with_val(118, 3);
    for i in (2..=n*2).step_by(2) {
        pi = pi + Float::with_val(118, s) * 4 / (Integer::from(i) * (i+1) * (i+2));
        s = -s;
    }
    pi
}

fn taylor() -> Integer {
    let mut i = 1_u32;
    let mut x = Integer::from(3) * Integer::from(Integer::i_pow_u(10, 1020));
    let mut pi = Integer::from(&x);
    while x > 0 {
        x = x * i / ((i + 1) * 4);
        pi += Integer::from(&x) / (i + 2);
        i += 2;
    }
    pi / Integer::from(Integer::i_pow_u(10, 20))
}


fn main() {
    let pi_int = Integer::from(314159265358979323846264338327950288_u128);
    let pi_flo = Float::with_val(118, Float::parse("3.14159265358979323846264338327950288").unwrap());

    println!("PI int sig. bits     {:}",    pi_int.significant_bits());
    println!("PI hard-coded int    {:}",    pi_int);
    println!("PI hard-coded flo    {:.36}", pi_flo);
    println!("PI by Nilakantha arb {:.36}; diff = {:.36}",
             gen_pi_nilakantha_arbitrary(),
             pi_flo - gen_pi_nilakantha_arbitrary());
    println!("PI by taylor         {:.36}", taylor());
    println!("---- other: ----");
    println!("PI by Nilakantha     {:.36}", gen_pi_nilakantha());
    println!("PI by Viete          {:.36}", gen_pi_viete());
}
