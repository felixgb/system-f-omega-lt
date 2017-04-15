fn foo<'a>(x: u32) -> &'a u32 {
    &x
}

fn main() {
    foo(6);
}

// makeref = 
