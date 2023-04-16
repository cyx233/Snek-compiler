mod infra;

// Your tests go here!
success_tests! {
    add1: "73",
    add: "15",
    nested_arith: "25",
    binding: "5",
    quick_brown_fox:"27",
    negative:"-5",
    let_arith:"9",
    nested_let:"11"
}

failure_tests! {
    unbound_id: "Unbound variable identifier x",
    duplicate_binding: "Duplicate binding",
}
