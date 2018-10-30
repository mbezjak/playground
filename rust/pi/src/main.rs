fn gen_pi_nilakantha_procedural() -> f64 {
    let n = 1000000;
    let mut s = 1.0;
    let mut pi = 3.0;
    for i in (2..=n*2).step_by(2) {
        pi = pi + s * (4.0 / (i as f64 * (i+1) as f64 * (i+2) as f64));
        s = -s;
    }
    pi
}

fn main() {
    println!("PI in rust   {:.20}", std::f64::consts::PI);
    println!("Generated PI {:.20}", gen_pi_nilakantha_procedural());
}
