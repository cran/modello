context("graphs")

modello.reset()

tol = 1e-6

test_that("'simple graphs' are consistent", {
    expect_lt(test.linreg(), tol)
    expect_lt(test.logisticreg(), tol)
    expect_lt(test.multinomreg(), tol)
})



