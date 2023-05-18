mod infra;

// Your tests go here!
success_tests! {
    {
        name: simple,
        file: "egg_eater/simple_examples.boa",
        expected: "(tuple 1 2 (tuple 3 4))\n(tuple 3 4)",
    },
    {
        name: deeq_eq,
        file: "egg_eater/complex_examples.boa",
        expected: "(tuple 1 2 3)\n(tuple 1 2 4)\nfalse\nfalse\n(tuple 1 2 ...)\n(tuple 1 2 (tuple 1 2 ...))\nfalse\ntrue",
    },
    {
        name: points,
        file: "egg_eater/points.boa",
        expected: "(tuple 1 2)\n(tuple 3 4)\n(tuple 4 6)",
    },
    {
        name: bst,
        file: "egg_eater/bst.boa",
        expected: "true\nfalse\nfalse",
    },
}

runtime_error_tests! {
    {
        name: tag_error,
        file: "egg_eater/error-tag.boa",
        expected: "invalid argument",
    },
    {
        name: out_of_bounds,
        file: "egg_eater/error-bounds.boa",
        expected: "out of bound",
    },
}

static_error_tests! {
    {
        name: empty_tuple,
        file: "egg_eater/error3.boa",
        expected: "empty tuple",
    },
}
