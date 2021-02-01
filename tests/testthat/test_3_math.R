context("math testing")

modello.reset()

a0 = runif(1)
x0 = number(a0, scalar=T)
a1 = array(runif(27), dim=27)
x1 = number(a1)
a1s = runif(3)
x1s = number(a1s)
a2 = matrix(runif(27), 9, 3)
x2 = number(a2)
a2sq = matrix(runif(9), 3, 3)
x2sq = number(a2sq)
a3 = array(runif(27), dim=c(3, 3, 3))
x3 = number(a3)

y1 = number(sample(c(0, 1), 27, replace=TRUE), dx=FALSE)
y2 = number(c(1, rep(0, 26)), dx=FALSE)
y3 = number(runif(27), dx=FALSE)

test_that("'slice' is consistent", {
    expect_equal(x2[2:3,2:3,contiguous=F]$v, a2[2:3,2:3])
    expect_equal(c(x3[1,1,1:3,contiguous=F]$v), a3[1,1,1:3])
})

test_that("'flat_slice' is consistent", {
    expect_equal(x1[c(1, 3, 5), contiguous=F]$v, a1[c(1, 3, 5)])
    expect_equal(c(x2[c(2, 5, 3), contiguous=F]$v), a2[c(2, 5, 3)])
    expect_equal(c(x3[c(6, 3, 9), contiguous=F]$v), a3[c(6, 3, 9)])
})

test_that("'contiguous.slice' is consistent", {
    expect_equal(x1[3,10]$v, a1[3:10])
    expect_equal(x2[1,2]$v, a2[,1:2])
    expect_equal(x3[1,2]$v, a3[,,1:2])
})

bind3 <- function () {ans=c(a3, a3); dim(ans)=c(3, 3, 6);  ans}
test_that("'bind' is consistent", {
    expect_equal(c(x1$bind(x1, 1)$v), c(a1, a1))
    expect_equal(x2$bind(x2, 1)$v, rbind(a2, a2))
    expect_equal(x2$bind(x2, 2)$v, cbind(a2, a2))
    expect_equal(x3$bind(x3, 3)$v, bind3())
})

test_that("'reshape' is consistent", {
    expect_equal(x1$reshape(c(9,3))$v, {dim(a1)=c(9, 3); a1})
    expect_equal(x1$reshape(c(3, 3, 3))$v, {dim(a1)=c(3, 3 ,3); a1})
    expect_equal(x2$reshape(c(3, 9))$v, {dim(a2)=c(3, 9); a2})
    expect_equal(x3$reshape(c(9, 3))$v, {dim(a3)=c(9, 3); a3})
})   

test_that("'add' is consistent", {
    ans0 = x0 + x0
    expect_equal(ans0$get.v(), 2 * a0)
    ans0.1 = x0 + x1
    ans1.0 = x1 + x0
    expect_equal(expect_equal(ans0.1$get.v(), a1 + a0), ans1.0$get.v())
    ans1 = x1 + x1
    expect_equal(ans1$get.v(), 2 * a1)
    ans0.2 = x2 + x0
    ans2.0 = x0 + x2
    expect_equal(expect_equal(ans0.2$get.v(), a2 + a0), ans2.0$get.v())
    ans1.2 = x1 + x2
    ans2.1 = x2 + x1
    expect_equal(expect_equal(ans1.2$get.v(), c(a1) + a2), ans2.1$get.v())
    ans2 = x2 + x2
    expect_equal(ans2$get.v(), 2 * a2)
    ans0.3 = x0 + x3
    ans3.0 = x3 + x0
    expect_equal(expect_equal(ans0.3$get.v(), a0 + a3), ans3.0$get.v())
    ans1.3 = x1 + x3
    ans3.1 = x3 + x1
    expect_equal(expect_equal(ans1.3$get.v(), c(a1) + a3), ans3.1$get.v())
    ans2.3 = x2 + x3
    ans3.2 = x3 + x2
    expect_equal(expect_equal(ans2.3$get.v(), c(a2) + a3), ans3.2$get.v())
    ans3 = x3 + x3
    expect_equal(ans3$get.v(), 2 * a3)
    ans2s.1 = x2 + x1s
    ans2s.2 = x1s + x2
    expect_equal(expect_equal(ans2s.1$get.v(), a2 + a1s), ans2s.2$get.v())
    ans3s.1 = x3 + x1s
    ans3s.2 = x1s + x3
    expect_equal(expect_equal(ans3s.1$get.v(), a3 + a1s), ans3s.2$get.v())
})

test_that("'sub' is consistent", {
    ans0 = x0 - x0
    expect_equal(sum(ans0$get.v()), 0)
    ans0.1 = x0 - x1
    ans1.0 = x1 - x0
    expect_equal(ans0.1$get.v(), a0 - a1)
    expect_equal(ans1.0$get.v(), a1 - a0)
    ans1 = x1 - x1
    expect_equal(sum(ans1$get.v()), 0)
    ans0.2 = x0 - x2
    ans2.0 = x2 - x0
    expect_equal(ans0.2$get.v(), a0 - a2)
    expect_equal(ans2.0$get.v(), a2 - a0)
    ans1.2 = x1 - x2
    ans2.1 = x2 - x1
    expect_equal(ans1.2$get.v(), c(a1) - a2)
    expect_equal(ans2.1$get.v(), a2 - c(a1))
    ans2 = x2 - x2
    expect_equal(sum(ans2$get.v()), 0)
    ans0.3 = x0 - x3
    ans3.0 = x3 - x0
    expect_equal(ans0.3$get.v(), a0 - a3)
    expect_equal(ans3.0$get.v(), a3 - a0)
    ans1.3 = x1 - x3
    ans3.1 = x3 - x1
    expect_equal(ans1.3$get.v(), c(a1) - a3)
    expect_equal(ans3.1$get.v(), a3 - c(a1))
    ans2.3 = x2 - x3
    ans3.2 = x3 - x2
    expect_equal(ans2.3$get.v(), c(a2) - a3)
    expect_equal(ans3.2$get.v(), a3 - c(a2))
    ans3 = x3 - x3
    expect_equal(sum(ans3$get.v()), 0)
    ans2s.1 = x2 - x1s
    ans2s.2 = x1s - x2
    expect_equal(ans2s.1$get.v(), a2 - a1s)
    expect_equal(ans2s.2$get.v(), a1s - a2)
    ans3s.1 = x3 - x1s
    ans3s.2 = x1s - x3
    expect_equal(ans3s.1$get.v(), a3 - a1s)
    expect_equal(ans3s.2$get.v(), a1s - a3)
    ans2s.1 = x2 - x1s
    ans2s.2 = x1s - x2
    expect_equal(ans2s.1$get.v(), a2 - a1s)
    expect_equal(ans2s.2$get.v(), a1s - a2)
    ans3s.1 = x3 - x1s
    ans3s.2 = x1s - x3
    expect_equal(ans3s.1$get.v(), a3 - a1s)
    expect_equal(ans3s.2$get.v(), a1s - a3)
})

test_that("'mult' is consistent", {
    ans0 = x0 * x0
    expect_equal(ans0$get.v(), a0**2)
    ans0.1 = x0 * x1
    ans1.0 = x1 * x0
    expect_equal(expect_equal(ans0.1$get.v(), a1 * a0), ans1.0$get.v())
    ans1 = x1 * x1
    expect_equal(ans1$get.v(), a1**2)
    ans0.2 = x2 * x0
    ans2.0 = x0 * x2
    expect_equal(expect_equal(ans0.2$get.v(), a2 * a0), ans2.0$get.v())
    ans1.2 = x1 * x2
    ans2.1 = x2 * x1
    expect_equal(expect_equal(ans1.2$get.v(), c(a1) * a2), ans2.1$get.v())
    ans2 = x2 * x2
    expect_equal(ans2$get.v(), a2**2)
    ans0.3 = x0 * x3
    ans3.0 = x3 * x0
    expect_equal(expect_equal(ans0.3$get.v(), a0 * a3), ans3.0$get.v())
    ans1.3 = x1 * x3
    ans3.1 = x3 * x1
    expect_equal(expect_equal(ans1.3$get.v(), c(a1) * a3), ans3.1$get.v())
    ans2.3 = x2 * x3
    ans3.2 = x3 * x2
    expect_equal(expect_equal(ans2.3$get.v(), c(a2) * a3), ans3.2$get.v())
    ans3 = x3 * x3
    expect_equal(ans3$get.v(), a3**2)
    
})

test_that("'div' is consistent", {
    ans0 = x0 / x0
    expect_equal(prod(ans0$get.v()), 1)
    ans0.1 = x0 / x1
    ans1.0 = x1 / x0
    expect_equal(ans0.1$get.v(), a0 / a1)
    expect_equal(ans1.0$get.v(), a1 / a0)
    ans1 = x1 / x1
    expect_equal(prod(ans1$get.v()), 1)
    ans0.2 = x0 / x2
    ans2.0 = x2 / x0
    expect_equal(ans0.2$get.v(), a0 / a2)
    expect_equal(ans2.0$get.v(), a2 / a0)
    ans1.2 = x1 / x2
    ans2.1 = x2 / x1
    expect_equal(ans1.2$get.v(), c(a1) / a2)
    expect_equal(ans2.1$get.v(), a2 / c(a1))
    ans2 = x2 / x2
    expect_equal(prod(ans2$get.v()), 1)
    ans0.3 = x0 / x3
    ans3.0 = x3 / x0
    expect_equal(ans0.3$get.v(), a0 / a3)
    expect_equal(ans3.0$get.v(), a3 / a0)
    ans1.3 = x1 / x3
    ans3.1 = x3 / x1
    expect_equal(ans1.3$get.v(), c(a1) / a3)
    expect_equal(ans3.1$get.v(), a3 / c(a1))
    ans2.3 = x2 / x3
    ans3.2 = x3 / x2
    expect_equal(ans2.3$get.v(), c(a2) / a3)
    expect_equal(ans3.2$get.v(), a3 / c(a2))
    ans3 = x3 / x3
    expect_equal(prod(ans3$get.v()), 1)
    ans2s.1 = x2 / x1s
    ans2s.2 = x1s / x2
    expect_equal(ans2s.1$get.v(), a2 / a1s)
    expect_equal(ans2s.2$get.v(), a1s / a2)
    ans3s.1 = x3 / x1s
    ans3s.2 = x1s / x3
    expect_equal(ans3s.1$get.v(), a3 / a1s)
    expect_equal(ans3s.2$get.v(), a1s / a3)
})

test_that("'pow' is consistent", {
    ans0 = x0 ** x0
    expect_equal(ans0$get.v(), a0**a0)
    ans0.1 = x0 ** x1
    ans1.0 = x1 ** x0
    expect_equal(ans0.1$get.v(), a0 ** a1)
    expect_equal(ans1.0$get.v(), a1 ** a0)
    ans1 = x1 ** x1
    expect_equal(ans1$get.v(), a1**a1)
    ans0.2 = x0 ** x2
    ans2.0 = x2 ** x0
    expect_equal(ans0.2$get.v(), a0 ** a2)
    expect_equal(ans2.0$get.v(), a2 ** a0)
    ans1.2 = x1 ** x2
    ans2.1 = x2 ** x1
    expect_equal(ans1.2$get.v(), c(a1) ** a2)
    expect_equal(ans2.1$get.v(), a2 ** c(a1))
    ans2 = x2 ** x2
    expect_equal(ans2$get.v(), a2**a2)
    ans0.3 = x0 ** x3
    ans3.0 = x3 ** x0
    expect_equal(ans0.3$get.v(), a0 ** a3)
    expect_equal(ans3.0$get.v(), a3 ** a0)
    ans1.3 = x1 ** x3
    ans3.1 = x3 ** x1
    expect_equal(ans1.3$get.v(), c(a1) ** a3)
    expect_equal(ans3.1$get.v(), a3 ** c(a1))
    ans2.3 = x2 ** x3
    ans3.2 = x3 ** x2
    expect_equal(ans2.3$get.v(), c(a2) ** a3)
    expect_equal(ans3.2$get.v(), a3 ** c(a2))
    ans3 = x3 ** x3
    expect_equal(ans3$get.v(), a3**a3)
    ans2s.1 = x2 ** x1s
    ans2s.2 = x1s ** x2
    expect_equal(ans2s.1$get.v(), a2 ** a1s)
    expect_equal(ans2s.2$get.v(), a1s ** a2)
    ans3s.1 = x3 ** x1s
    ans3s.2 = x1s ** x3
    expect_equal(ans3s.1$get.v(), a3 ** a1s)
    expect_equal(ans3s.2$get.v(), a1s ** a3)
})

test_that("'abs' is consistent", {
    expect_equal(abs(x0)$get.v(), abs(a0))
    expect_equal(abs(x1)$get.v(), abs(a1))
    expect_equal(abs(x2)$get.v(), abs(a2))
    expect_equal(abs(x3)$get.v(), abs(a3))
})

test_that("'exp' is consistent", {
    expect_equal(exp(x0)$get.v(), exp(a0))
    expect_equal(exp(x1)$get.v(), exp(a1))
    expect_equal(exp(x2)$get.v(), exp(a2))
    expect_equal(exp(x3)$get.v(), exp(a3))
})

test_that("'log' is consistent", {
    expect_equal(log(x0)$get.v(), log(a0))
    expect_equal(log(x1)$get.v(), log(a1))
    expect_equal(log(x2)$get.v(), log(a2))
    expect_equal(log(x3)$get.v(), log(a3))
})

test_that("'sin' is consistent", {
    expect_equal(sin(x0)$get.v(), sin(a0))
    expect_equal(sin(x1)$get.v(), sin(a1))
    expect_equal(sin(x2)$get.v(), sin(a2))
    expect_equal(sin(x3)$get.v(), sin(a3))
})

test_that("'cos' is consistent", {
    expect_equal(cos(x0)$get.v(), cos(a0))
    expect_equal(cos(x1)$get.v(), cos(a1))
    expect_equal(cos(x2)$get.v(), cos(a2))
    expect_equal(cos(x3)$get.v(), cos(a3))
})

test_that("'tan' is consistent", {
    expect_equal(tan(x0)$get.v(), tan(a0))
    expect_equal(tan(x1)$get.v(), tan(a1))
    expect_equal(tan(x2)$get.v(), tan(a2))
    expect_equal(tan(x3)$get.v(), tan(a3))
})

test_that("'sinh' is consistent", {
    expect_equal(sinh(x0)$get.v(), sinh(a0))
    expect_equal(sinh(x1)$get.v(), sinh(a1))
    expect_equal(sinh(x2)$get.v(), sinh(a2))
    expect_equal(sinh(x3)$get.v(), sinh(a3))
})

test_that("'cosh' is consistent", {
    expect_equal(cosh(x0)$get.v(), cosh(a0))
    expect_equal(cosh(x1)$get.v(), cosh(a1))
    expect_equal(cosh(x2)$get.v(), cosh(a2))
    expect_equal(cosh(x3)$get.v(), cosh(a3))
})

test_that("'tanh' is consistent", {
    expect_equal(tanh(x0)$get.v(), tanh(a0))
    expect_equal(tanh(x1)$get.v(), tanh(a1))
    expect_equal(tanh(x2)$get.v(), tanh(a2))
    expect_equal(tanh(x3)$get.v(), tanh(a3))
})

test_that("'sigmoid' is consistent", {
    expect_equal(sigmoid(x0)$get.v(), sigmoid(a0))
    expect_equal(sigmoid(x1)$get.v(), sigmoid(a1))
    expect_equal(sigmoid(x2)$get.v(), sigmoid(a2))
    expect_equal(sigmoid(0), 0.5)
})

test_that("'relu' is consistent", {
    expect_equal(relu(y3)$get.v(), relu(y3$v))
})

test_that("'swish' is consistent", {
    expect_equal(swish(y3)$get.v(), swish(y3$v))
})

test_that("'softmax' is consistent", {
    expect_equal(softmax(x1)$get.v(), softmax(a1))
    expect_equal(softmax(x2, k=1)$get.v(), softmax(a2, k=1))
    expect_equal(softmax(x2, k=2)$get.v(), softmax(a2, k=2))
})

test_that("'ssq' is consistent", {
    expect_equal(ssq(x1)$get.v(), ssq(a1))
    expect_equal(ssq(x2)$get.v(), ssq(a2))
    expect_equal(ssq(x3)$get.v(), ssq(a3))
})

test_that("'bin.entropy' is consistent", {
    expect_equal(bin.entropy(y1, x1)$get.v(), bin.entropy(y1$v, a1))
})

test_that("'cross.entropy' is consistent", {
    expect_equal(cross.entropy(y2, x1)$get.v(), cross.entropy(y2$v, a1))
})

test_that("'mse' is consistent", {
    expect_equal(mse(y3, x1)$get.v(), mse(y3$v, a1))
})

test_that("'mae' is consistent", {
    expect_equal(mae(y3, x1)$get.v(), mae(y3$v, a1))
})

test_that("'gemm' is consistent", {
    expect_equal((x2sq %.% x2sq)$get.v(), a2sq %*% a2sq)
    expect_equal((x2sq %T.% x2sq)$get.v(), t(a2sq) %*% a2sq)
    expect_equal((x2sq %.T% x2sq)$get.v(), a2sq %*% t(a2sq))
    expect_equal((x2sq %T.T% x2sq)$get.v(), t(a2sq) %*% t(a2sq))
    expect_equal(gemm(0, 1, NULL, x2sq, x2sq, NULL, x2sq)$v, a2sq %*% t(a2sq) + a2sq)
    expect_equal(gemm(1, 0, x0, x2sq, x2sq, NULL, NULL)$v, a0 * t(a2sq) %*% a2sq)
    expect_equal(gemm(0, 0, x0, x2sq, x2sq, x0, x2sq)$v, a0 * a2sq %*% a2sq + a0 * a2sq) 
})

test_that("'gemv' is cosistent", {
    expect_equal(c((x2sq %.% x1s)$get.v()), c(a2sq %*% a1s))
    expect_equal(c((x1s %T.% x2sq)$get.v()), c(t(a1s) %*% a2sq))
    expect_equal(c((x2sq %T.% x1s)$get.v()), c(t(a2sq) %*% a1s))
    expect_equal(c(gemv(0, x0, x2sq, x1s, x0, x1s)$get.v()), c(a0 * a2sq %*% a1s + a0 * a1s))
    expect_equal(c(gemv(1, x0, x2sq, x1s, x0, x1s)$get.v()), c(a0 * t(a2sq) %*% a1s + a0 * a1s))
})

test_that("'ger' is consistent", {
    expect_equal((x1s %.T% x1s)$get.v(), a1s %*% t(a1s))
    expect_equal(ger(x0, x1s, x1s, x2sq)$get.v(), a0 * a1s %*% t(a1s) + a2sq) 
})

test_that("'dot' is consistent", {
    expect_equal((x1s %T.% x1s)$get.v(), c(t(a1s) %*% a1s))
})

test_that("'invMat' is consistent", {
    expect_equal(invMat(x2sq)$get.v(), invMat(a2sq))
    #expect_equal(invSymMat(x2sq %.T% x2sq)$get.v(), invMat(a2sq %*% t(a2sq)))
})


test_that("'log-densities' are consistent", {
    expect_equal(c(ldexp(y3, x1)$get.v()), c(dexp(y3$v, a1, log=T)))
    expect_equal(c(ldexp(y3, x0)$get.v()), c(dexp(y3$v, a0, log=T)))

    expect_equal(c(ldbeta(y3, x1, x1)$get.v()), c(dbeta(y3$v, a1, a1, log=T)))
    expect_equal(c(ldbeta(y3, x1, x0)$get.v()), c(dbeta(y3$v, a1, a0, log=T)))
    expect_equal(c(ldbeta(y3, x0, x1)$get.v()), c(dbeta(y3$v, a0, a1, log=T)))
    expect_equal(c(ldbeta(y3, x0, x0)$get.v()), c(dbeta(y3$v, a0, a0, log=T)))

    expect_equal(c(ldgamma(y3, x1, x1)$get.v()), c(dgamma(y3$v, a1, a1, log=T)))
    expect_equal(c(ldgamma(y3, x1, x0)$get.v()), c(dgamma(y3$v, a1, a0, log=T)))
    expect_equal(c(ldgamma(y3, x0, x1)$get.v()), c(dgamma(y3$v, a0, a1, log=T)))
    expect_equal(c(ldgamma(y3, x0, x0)$get.v()), c(dgamma(y3$v, a0, a0, log=T)))
    
    expect_equal(c(ldnorm(y3, x1, x0)$get.v()), c(dnorm(y3$v, a1, a0, log=T)))
    expect_equal(c(ldnorm(y3, x1, x1)$get.v()), c(dnorm(y3$v, a1, a1, log=T)))
    expect_equal(c(ldnorm(y3, x0, x1)$get.v()), c(dnorm(y3$v, a0, a1, log=T)))
    expect_equal(c(ldnorm(y3, x0, x0)$get.v()), c(dnorm(y3$v, a0, a0, log=T)))
})
   
test_that("'lkh.norm' is consistent", {
    expect_equal(lkh.norm(y3, x1, x1)$get.v(), sum(dnorm(y3$v, a1, a1, log=T)))
    expect_equal(lkh.norm(y3, x1, x1, y3)$get.v(), sum(y3$v * dnorm(y3$v, a1, a1, log=T)))
})

test_that("'embeddings' are consistent", {
    ff = sample(1:10, 5)
    xf = number(as.double(ff), dx=F)
    vv = matrix(rnorm(50), 5, 10)
    xv = number(vv)
    xe = embeddings(xf, xv, 5)
    expect_equal(xe$v, vv[,ff])
})
    


