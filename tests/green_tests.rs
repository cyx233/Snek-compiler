mod infra;

// Your tests go here!
success_tests! {
    {
        name: sum,
        file: "green/test1.snek",
        expected: "12502500",
    },
    {
        name: even_odd,
        file: "green/test2.snek",
        expected: "true",
    },
    {
        name: fib,
        file: "green/test3.snek",
        expected: "6765",
    }
}
