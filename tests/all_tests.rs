mod infra;

// Your tests go here!
success_tests! {
    {
        name: simple_if,
        file: "simple_if.snek",
        expected: "true",
    },
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
    {
        name: shadow_binding,
        file: "shadow_binding.snek",
        expected: "21",
    },
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
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
    {
        name: overflow,
        file: "factorial.snek",
        input: "32",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
}
