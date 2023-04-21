mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: block_set,
        file: "block_set.snek",
        expected: "6",
    },
    {
        name: loop_break,
        file: "loop_break.snek",
        expected: "-6",
    },
    {
        name: factorial_4,
        file: "factorial.snek",
        input: "4",
        expected: "24",
    },
    {
        name: factorial_5,
        file: "factorial.snek",
        input: "5",
        expected: "120",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
