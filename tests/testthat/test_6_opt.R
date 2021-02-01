context("optimisation")

modello.reset()

tol = 1e-3

test_that("'optimisers' works", {
    expect_lt(test.opt(sgd.opt, 0.0001, 1000000), tol)
    expect_lt(test.opt(sgdwm.opt, 0.0001, 0.99, 1000000), tol)
    expect_lt(test.opt(adam.opt, 0.0001, 0.99, 0.8, 1000000), tol)
})

