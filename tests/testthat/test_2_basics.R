context("basic testing")

a0 = runif(1)
x0 = number(a0, scalar=T)
a1 = array(runif(3))
x1 = number(a1)
a2 = matrix(runif(9), 3, 3)
x2 = number(a2)
a3 = array(runif(27), dim=c(3, 3, 3))
x3 = number(a3)

test_that("'numbers' are linked", {
    expect_true(x0$is.linked())
    expect_true(x1$is.linked())
    expect_true(x2$is.linked())
    expect_true(x3$is.linked())
})

test_that("'length' is consistent", {
    expect_equal(length(x0), length(a0))
    expect_equal(length(x1), length(a1))
    expect_equal(length(x2), length(a2))
    expect_equal(length(x3), length(a3))
})

test_that("'dim' is consistent", {
    expect_equal(dim(x0), integer(0))
    expect_equal(dim(x1), length(a1))
    expect_equal(dim(x2), dim(a2))
    expect_equal(dim(x3), dim(x3))
})

test_that("'rank' is consistent", {
    expect_equal(x0$rank(), 0)
    expect_equal(x1$rank(), 1)
    expect_equal(x2$rank(), 2)
    expect_equal(x3$rank(), 3)
})

test_that("'get.v' is consistent", {
    expect_equal(x0$v, a0)
    expect_equal(x0$get.v(), a0)
    expect_equal(x1$v, a1)
    expect_equal(x1$get.v(), a1)
    expect_equal(x2$v, a2)
    expect_equal(x2$get.v(), a2)
    expect_equal(x3$v, a3)
    expect_equal(x3$get.v(), a3)
})

test_that("'set.v' is consistent", {
    expect_is(expect_invisible(x0$set.v(a0 + 1)), 'modello_number')
    expect_equal(x0$v, a0 + 1)
    expect_equal({x0$v = a0; x0$get.v()}, a0)
    expect_is(expect_invisible(x1$set.v(a1 + 1)), 'modello_number')
    expect_equal(x1$v, a1 + 1)
    expect_equal({x1$v = a1; x1$get.v()}, a1)
    expect_is(expect_invisible(x2$set.v(a2 + 1)), 'modello_number')
    expect_equal(x2$v, a2 + 1)
    expect_equal({x2$v = a2; x2$get.v()}, a2)
    expect_is(expect_invisible(x3$set.v(a3 + 1)), 'modello_number')
    expect_equal(x3$v, a3 + 1)
    expect_equal({x3$v = a3; x3$get.v()}, a3)
})

test_that("'pop' is consistent", {
    expect_false(expect_is(expect_invisible(pop(x0)), 'modello_number')$is.linked())
    expect_false(expect_is(expect_invisible(pop(x1)), 'modello_number')$is.linked())
    expect_false(expect_is(expect_invisible(pop(x2)), 'modello_number')$is.linked())
    expect_false(expect_is(expect_invisible(pop(x3)), 'modello_number')$is.linked())
})    

