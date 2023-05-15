mod infra;

// Your tests go here!
success_tests! {
    {
        name: simple,
        file: "simple_examples.boa",
        expected: "true",
    },
    {
        name: points,
        file: "points.boa",
        expected: "true",
    },
    {
        name: bst,
        file: "bst.boa",
        expected: "true",
    },
}

runtime_error_tests! {
    {
        name: tag_error,
        file: "error-tag.boa",
        expected: "true",
    },
    {
        name: out_of_bounds,
        file: "error-bounds.boa",
        expected: "true",
    },
}

static_error_tests! {}
