use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
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

fn snek_str(val: i64) -> String {
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
            let addr = (val - 1) as *const i64;
            let len = unsafe { *addr as isize };
            let mut items: Vec<String> = vec![];
            for i in 1..=len {
                items.push(unsafe { snek_str(*addr.offset(i) as i64) })
            }
            format!("(tuple {})", items.join(" "))
        }
        _ => format!("Unknown value: {}", val),
    }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val: i64) -> i64 {
    println!("{}", snek_str(val));
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
    println!("{}", snek_str(i))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
