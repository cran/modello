context("auto-differentiation")

modello.reset()

tol = 1e-4


test_that("unary dx operators are consistent", {
    expect_lt(dunary.test(runif(1), exp), tol)
    expect_lt(dunary.test(runif(1), log), tol)
    expect_lt(dunary.test(runif(1), sin), tol)
    expect_lt(dunary.test(runif(1), cos), tol)
    expect_lt(dunary.test(runif(1), tan), tol)
    expect_lt(dunary.test(runif(1), sinh), tol)
    expect_lt(dunary.test(runif(1), cosh), tol)
    expect_lt(dunary.test(runif(1), tanh), tol)
    expect_lt(dunary.test(runif(1), sigmoid), tol)

    expect_lt(dunary.test(runif(sample(3:10, 1)), exp, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), log, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), sin, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), cos, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), tan, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), sinh, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), cosh, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), tanh, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), sigmoid, sclr=F), tol)
    ##expect_lt(dunary.test(runif(sample(3:10, 1)), softmax, sclr=F),tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), relu, sclr=F), tol)
    expect_lt(dunary.test(runif(sample(3:10, 1)), swish, sclr=F), tol)
})

test_that("binary dx operators are consistent", {
    expect_lt(dbinary.test(runif(1), runif(1), '+'), tol)
    expect_lt(dbinary.test(runif(1), runif(1), '-'), tol)
    expect_lt(dbinary.test(runif(1), runif(1), '*'), tol)
    expect_lt(dbinary.test(runif(1), runif(1), '^'), tol)
    expect_lt(dbinary.test(runif(1), runif(1), '/'), tol)

    expect_lt(dbinary.test(runif(sample(3:10, 1)), runif(1), '+', sclr1=F), tol)
    expect_lt(dbinary.test(runif(sample(3:10, 1)), runif(1), '-', sclr1=F), tol)
    expect_lt(dbinary.test(runif(sample(3:10, 1)), runif(1), '*', sclr1=F), tol)
    expect_lt(dbinary.test(runif(sample(3:10, 1)), runif(1), '^', sclr1=F), tol)
    expect_lt(dbinary.test(runif(sample(3:10, 1)), runif(1), '/', sclr1=F), tol)

    expect_lt(dbinary.test(runif(1), runif(sample(3:10, 1)), '+', sclr2=F), tol)
    expect_lt(dbinary.test(runif(1), runif(sample(3:10, 1)), '-', sclr2=F), tol)
    expect_lt(dbinary.test(runif(1), runif(sample(3:10, 1)), '*', sclr2=F), tol)
    expect_lt(dbinary.test(runif(1), runif(sample(3:10, 1)), '^', sclr2=F), tol)
    expect_lt(dbinary.test(runif(1), runif(sample(3:10, 1)), '/', sclr2=F), tol)

    expect_lt(dbinary.test(runif(10), runif(10), '+', sclr1=F, sclr2=F), tol)
    expect_lt(dbinary.test(runif(10), runif(10), '-', sclr1=F, sclr2=F), tol)
    expect_lt(dbinary.test(runif(10), runif(10), '*', sclr1=F, sclr2=F), tol)
    expect_lt(dbinary.test(runif(10), runif(10), '^', sclr1=F, sclr2=F), tol)
    expect_lt(dbinary.test(runif(10), runif(10), '/', sclr1=F, sclr2=F), tol)

    expect_lt(dbcast.test(matrix(runif(10), 5, 2), runif(5), '+'), tol)
    expect_lt(dbcast.test(matrix(runif(24), 4, 6), runif(4), '-'), tol)
    expect_lt(dbcast.test(matrix(runif(9), 3, 3), runif(3), '*'), tol)
    expect_lt(dbcast.test(matrix(runif(12), 4, 3), runif(4), '^'), tol)
    expect_lt(dbcast.test(matrix(runif(4), 2, 2), runif(2), '/'), tol)

    expect_lt(dbcast.test(runif(5), matrix(runif(10), 5, 2), '+'), tol)
    expect_lt(dbcast.test(runif(4), matrix(runif(24), 4, 6), '-'), tol)
    expect_lt(dbcast.test(runif(3), matrix(runif(9), 3, 3), '*'), tol)
    expect_lt(dbcast.test(runif(4), matrix(runif(12), 4, 3), '^'), tol)
    expect_lt(dbcast.test(runif(2), matrix(runif(4), 2, 2), '/'), tol)
})


test_that("'dgemm2' dx operator is consistent", {
    expect_lt(dgemm2.test(0, 0, matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm2.test(1, 0, matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm2.test(0, 1, matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm2.test(1, 1, matrix(runif(9), 3), matrix(runif(9), 3)), tol)
})



test_that("'dgemm15' dx operator is consistent", {
    expect_lt(dgemm15.test(0, 0, matrix(runif(9), 3), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm15.test(1, 0, matrix(runif(9), 3), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm15.test(0, 1, matrix(runif(9), 3), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm15.test(1, 1, matrix(runif(9), 3), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
})



test_that("'dgemm1' dx operator is consistent", {
    expect_lt(dgemm1.test(0, 0, runif(1), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm1.test(1, 0, runif(1), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm1.test(0, 1, runif(1), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
    expect_lt(dgemm1.test(1, 1, runif(1), matrix(runif(9), 3), matrix(runif(9), 3)), tol)
})


test_that("'dgemm0' dx operator is consistent", {
    expect_lt(dgemm0.test(0, 0, runif(1), matrix(runif(9), 3), matrix(runif(9), 3), runif(1), matrix(runif(9), 3)), tol)
    expect_lt(dgemm0.test(1, 0, runif(1), matrix(runif(9), 3), matrix(runif(9), 3), runif(1), matrix(runif(9), 3)), tol)
    expect_lt(dgemm0.test(0, 1, runif(1), matrix(runif(9), 3), matrix(runif(9), 3), runif(1), matrix(runif(9), 3)), tol)
    expect_lt(dgemm0.test(1, 1, runif(1), matrix(runif(9), 3), matrix(runif(9), 3), runif(1), matrix(runif(9), 3)), tol)
})



test_that("'dgemv1' dx operator is consistent", {
    expect_lt(dgemv1.test(0, runif(1),  matrix(runif(9), 3), runif(3), runif(1), runif(3)), tol)
    expect_lt(dgemv1.test(1, runif(1),  matrix(runif(9), 3), runif(3), runif(1), runif(3)), tol)
})

test_that("'dgemv2' dx operator is consistent", {
    expect_lt(dgemv2.test(0, runif(1),  matrix(runif(9), 3), runif(3)), tol)
    expect_lt(dgemv2.test(1, runif(1),  matrix(runif(9), 3), runif(3)), tol)
})

test_that("'dgemv3' dx operator is consistent", {
    expect_lt(dgemv3.test(0, matrix(runif(9), 3), runif(3), runif(3)), tol)
    expect_lt(dgemv3.test(1, matrix(runif(9), 3), runif(3), runif(3)), tol)
})

test_that("'dgemv3' dx operator is consistent", {
    expect_lt(dgemv4.test(0, matrix(runif(9), 3), runif(3)), tol)
    expect_lt(dgemv4.test(1, matrix(runif(9), 3), runif(3)), tol)
})


test_that("'dger1' dx operator is consistent", {
    expect_lt(dger1.test(runif(1), runif(3), runif(3), matrix(runif(9), 3, 3)), tol)
})

test_that("'dger2' dx operator is consistent", {
    expect_lt(dger2.test(runif(3), runif(3), matrix(runif(9), 3, 3)), tol)
})


test_that("'dger2' dx operator is consistent", {
    expect_lt(dger3.test(runif(3), runif(3)), tol)
})

test_that("'dger2' dx operator is consistent", {
    expect_lt(ddot.test(runif(3), runif(3)), tol)
})


test_that("'invMat' dx operator is consistent", {
    expect_lt(dinvMat.test(matrix(runif(9), 3, 3)), tol)
})

test_that("'reduction' dx operators are consistent", {
    expect_lt(dreduction.test(rnorm(10), ssq), tol)
    expect_lt(dreduction.test(rnorm(10), sum), tol)
})

test_that("'objective' dx operators are consistent", {
    expect_lt(dobj.test(1, runif(1), bin.entropy), tol)
    expect_lt(dobj.test(c(1, 0), c(0.33, 0.67), cross.entropy), tol)
    expect_lt(dobj.test(runif(10), runif(10), mse), tol)
    expect_lt(dobj.test(runif(10), runif(10), mae), tol)
})

test_that("'ldexp' dx operators are consistent", {
    expect_lt(dldexp.test(runif(5), runif(5), F), tol)
    expect_lt(dldexp.test(runif(5), runif(1), T), tol)
    expect_lt(dldexp.test(runif(1), runif(1), T), tol)
    
})

test_that("'ldbeta' dx operators are consistent", {
    expect_lt(dldbeta.test(runif(5), runif(5), runif(5), F, F), tol)
    expect_lt(dldbeta.test(runif(5), runif(1), runif(5), T, F), tol)
    expect_lt(dldbeta.test(runif(5), runif(5), runif(1), F, T), tol)
    expect_lt(dldbeta.test(runif(1), runif(1), runif(1), T, T), tol)
})

test_that("'ldgamma' dx operators are consistent", {
    expect_lt(dldgamma.test(runif(5), runif(5), runif(5), F, F), tol)
    expect_lt(dldgamma.test(runif(5), runif(1), runif(5), T, F), tol)
    expect_lt(dldgamma.test(runif(5), runif(5), runif(1), F, T), tol)
    expect_lt(dldgamma.test(runif(1), runif(1), runif(1), T, T), tol)
})

test_that("'ldnorm' dx operators are consistent", {
    expect_lt(dldnorm.test(runif(5), runif(5), runif(5), F, F), tol)
    expect_lt(dldnorm.test(runif(5), runif(1), runif(5), T, F), tol)
    expect_lt(dldnorm.test(runif(5), runif(5), runif(1), F, T), tol)
    expect_lt(dldnorm.test(runif(1), runif(1), runif(1), T, T), tol)
})

test_that("'lkh.norm' dx operators are consistent", {
    expect_lt(dlkh.norm.test(rnorm(10), rnorm(10), runif(10), NULL), tol)
    expect_lt(dlkh.norm.test(rnorm(10), rnorm(10), runif(10), runif(10)), tol)
})
