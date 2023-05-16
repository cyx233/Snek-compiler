mod infra;

// Your tests go here!
success_tests! {
    {
        name: simple,
        file: "egg_eater/simple_examples.boa",
        expected: "true",
    },
    {
        name: points,
        file: "egg_eater/points.boa",
        expected: "true",
    },
    {
        name: bst,
        file: "egg_eater/bst.boa",
        expected: "true",
    },
}

runtime_error_tests! {
    {
        name: tag_error,
        file: "egg_eater/error-tag.boa",
        expected: "true",
    },
    {
        name: out_of_bounds,
        file: "egg_eater/error-bounds.boa",
        expected: "true",
    },
}

static_error_tests! {
    {
        name: empty_tuple,
        file: "egg_eater/error3.boa",
        expected: "true",
    },
}
