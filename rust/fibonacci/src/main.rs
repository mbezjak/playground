fn fib(n: u32) -> u64 {
    if n <= 2 {
        n as u64
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    (1..20).for_each(|n| println!("fib({:2 }) == {:4 }", n, fib(n)));
}
