use std::collections::HashSet;
use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

fn structural_eq(seen: &mut HashSet<(i64, i64)>, val1: i64, val2: i64) -> bool {
    if seen.contains(&(val1, val2)) {
        true
    } else {
        seen.insert((val1, val2));
        let type_bits = val1 & 3;
        if type_bits != val2 & 3 {
            false
        } else {
            match type_bits {
                1 | 0 => val1 == val2,
                2 => {
                    let addr1 = (val1 - 2) as *const i64;
                    let addr2 = (val2 - 2) as *const i64;
                    let len = unsafe { *addr1 >> 2 };
                    if len != unsafe { *addr2 >> 2 } {
                        false
                    } else {
                        for i in 1..=len {
                            if !structural_eq(seen, unsafe { *addr1.offset(i as isize) }, unsafe {
                                *addr2.offset(i as isize)
                            }) {
                                return false;
                            }
                        }
                        true
                    }
                }
                _ => false,
            }
        }
    }
}

fn snek_str(seen: &mut HashSet<i64>, val: i64) -> String {
    let type_bits = val & 3;
    match type_bits {
        1 => {
            if val == 1 {
                "false".to_string()
            } else {
                "true".to_string()
            }
        }
        0 => {
            format!("{}", val >> 2)
        }
        2 => {
            let addr = (val - 2) as *const i64;
            if seen.contains(&(addr as i64)) {
                return format!("...");
            }
            seen.insert(addr as i64);
            let len = unsafe { *addr >> 2 };
            let mut items: Vec<String> = vec![];
            for i in 1..=len {
                items.push(snek_str(seen, unsafe { *addr.offset(i as isize) }));
            }
            format!("(tuple {})", items.join(" "))
        }
        _ => format!("Unknown value: {}", val),
    }
}

#[export_name = "\x01snek_error"]
fn snek_error(errcode: i64) {
    match errcode {
        1 => eprintln!("invalid argument"),
        2 => eprintln!("overflow"),
        4 => eprintln!("out of bound"),
        _ => eprintln!("Unknown Error {errcode}"),
    };
    std::process::exit(1);
}

#[export_name = "\x01snek_eq"]
fn snek_eq(val1: i64, val2: i64) -> i64 {
    if structural_eq(&mut HashSet::new(), val1, val2) {
        5
    } else {
        1
    }
}

#[export_name = "\x01snek_print"]
fn snek_print(val: i64) -> i64 {
    println!("{}", snek_str(&mut HashSet::new(), val));
    return val;
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    match input {
        "true" => 5,
        "false" => 1,
        _ => input.parse::<i64>().unwrap() << 2,
    }
}

fn print_value(i: i64) {
    println!("{}", snek_str(&mut HashSet::new(), i))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
