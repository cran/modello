.tol = 1e-4
.tol.opt = 0.001

tol.test <- function (y, yh) {
    eps = 0.000001
    2 * max(abs(y - yh)) / (max(abs(y + yh)) + eps)
}

dunary.test <- function (x, fn, sclr=T) {
    g = graph.open()
    x = number(x, scalar=sclr)
    y = fn(x)
    y$set.dv(y$v**0)
    y$bw()
    tol.test(dfun(x$v, fn), x$dv) 
}

dbinary.test <- function (x1, x2, fn, sclr1=T, sclr2=T) {
    g = graph.open()
    x1 = number(x1, scalar=sclr1)
    x2 = number(x2, scalar=sclr2)
    y = do.call(fn, list(x1, x2))
    y$set.dv(y$v**0)
    y$bw()
    dx1 = dfun(x1$v, function(x)do.call(fn, list(x, x2$v)))
    dx2 = dfun(x2$v, function(x)do.call(fn, list(x1$v, x)))
    tol.test(c(dx1, dx2), c(x1$dv, x2$dv))
}

dbcast.test <- function (x1, x2, fn) {
    g = graph.open()
    x1 = number(x1)
    x2 = number(x2)
    y = do.call(fn, list(x1, x2))
    y$set.dv(y$v**0)
    y$bw()
    dx1 = dfun(x1$v, function(x)do.call(fn, list(c(x), c(x2$v))))
    dx2 = dfun(x2$v, function(x)do.call(fn, list(c(x1$v), c(x))))
    tol.test(c(dx1, dx2), c(x1$dv, x2$dv))
}

dgemm2.test <- function (ta, tb, A, B) {
    fn = function (A, B) {
        if (ta > 0) A = t(A)
        if (tb > 0) B = t(B)
        A %*% B
    }
    g = graph.open()
    x1 = number(A)
    x2 = number(B)
    x3 = gemm(ta=ta, tb=tb, alpha=NULL, A=x1, B=x2, beta=NULL, C=NULL)
    x3$set.dv(x3$v**0)
    x3$bw()
    dA = dfun(A, function(x)fn(x, B))
    dB = dfun(B, function(x)fn(A, x))
    tol.test(c(x1$dv, x2$dv), c(dA, dB))
}

dgemm15.test <- function (ta, tb, A, B, C) {
    fn = function (A, B, C) {
        if (ta > 0) A = t(A)
        if (tb > 0) B = t(B)
        A %*% B + C
    }
    g = graph.open()
    xa = number(A)
    xb = number(B)
    xc = number(C)
    y = gemm(ta=ta, tb=tb, alpha=NULL, A=xa, B=xb, beta=NULL, C=xc)
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(A, function(x)fn(x, B, C))
    dB = dfun(B, function(x)fn(A, x, C))
    dC = dfun(B, function(x)fn(A, B, x))
    tol.test(c(xa$dv, xb$dv, xc$dv), c(dA, dB, dC))
}

dgemm1.test <- function (ta, tb, alpha, A, B) {
    fn = function (alpha, A, B) {
        if (ta > 0) A = t(A)
        if (tb > 0) B = t(B)
        alpha * A %*% B
    }
    g = graph.open()
    xalpha = number(alpha, scalar=T)
    xa = number(A)
    xb = number(B)
    y = gemm(ta=ta, tb=tb, alpha=xalpha, A=xa, B=xb, beta=NULL, C=NULL)
    y$set.dv(y$v**0)
    y$bw()
    dalpha = dfun(alpha, function(x)fn(x, A, B))
    dA = dfun(A, function(x)fn(alpha, x, B))
    dB = dfun(B, function(x)fn(alpha, A, x))
    tol.test(c(xalpha$dv, xa$dv, xb$dv), c(dalpha, dA, dB))
}
dgemm0.test <- function (ta, tb, alpha, A, B, beta, C) {
    fn = function (alpha, A, B, beta, C) {
        if (ta > 0) A = t(A)
        if (tb > 0) B = t(B)
        alpha * A %*% B + beta * C
    }
    g = graph.open()
    xalpha = number(alpha, scalar=T)
    xa = number(A)
    xb = number(B)
    xbeta = number(beta, scalar=T)
    xc = number(C)
    y = gemm(ta=ta, tb=tb, alpha=xalpha, A=xa, B=xb, beta=xbeta, C=xc)
    y$set.dv(y$v**0)
    y$bw()
    dalpha = dfun(alpha, function(x)fn(x, A, B, beta, C))
    dA = dfun(A, function(x)fn(alpha, x, B, beta, C))
    dB = dfun(B, function(x)fn(alpha, A, x, beta, C))
    dbeta = dfun(beta, function(x)fn(alpha, A, B, x, C))
    dC = dfun(C, function(x)fn(alpha, A, B, beta, x))
    tol.test(c(xalpha$dv, xa$dv, xb$dv, xbeta$dv, xc$dv), c(dalpha, dA, dB, dbeta, dC))
}

dgemv1.test <- function (ta, alpha, A, B, beta, C) {
    fn = function (alpha, A, B, beta, C) {
        if (ta > 0) A = t(A)
        alpha * A %*% B + beta * C
    }

    g = graph.open()
    xalpha = number(alpha, scalar=T)
    xa = number(A)
    xb = number(B)
    xbeta = number(beta, scalar=T)
    xc = number(C)
    y = gemv(ta=ta, alpha=xalpha, A=xa, B=xb, beta=xbeta, C=xc)
    y$set.dv(y$v**0)
    y$bw()
    dalpha = dfun(alpha, function(x)fn(x, A, B, beta, C))
    dA = dfun(A, function(x)fn(alpha, x, B, beta, C))
    dB = dfun(as.array(B), function(x)fn(alpha, A, x, beta, C))
    dbeta = dfun(beta, function(x)fn(alpha, A, B, x, C))
    dC = dfun(C, function(x)fn(alpha, A, B, beta, x))
    tol.test(c(xalpha$dv, xa$dv, xb$dv, xbeta$dv, xc$dv), c(dalpha, dA, dB, dbeta, dC))
}

dgemv2.test <- function (ta, alpha, A, B) {
    fn = function (alpha, A, B) {
        if (ta > 0) A = t(A)
        alpha * A %*% B
    }
    g = graph.open()
    xalpha = number(alpha, scalar=T)
    xa = number(A)
    xb = number(B)
    y = gemv(ta=ta, alpha=xalpha, A=xa, B=xb)
    y$set.dv(y$v**0)
    y$bw()
    dalpha = dfun(alpha, function(x)fn(x, A, B))
    dA = dfun(A, function(x)fn(alpha, x, B))
    dB = dfun(as.array(B), function(x)fn(alpha, A, x))
    tol.test(c(xalpha$dv, xa$dv, xb$dv), c(dalpha, dA, dB))
}

dgemv3.test <- function (ta, A, B, C) {
    fn = function (A, B, C) {
        if (ta > 0) A = t(A)
        A %*% B + C
    }
    g = graph.open()
    xa = number(A)
    xb = number(B)
    xc = number(C)
    y = gemv(ta=ta, A=xa, B=xb, C=xc)
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(A, function(x)fn(x, B, C))
    dB = dfun(as.array(B), function(x)fn(A, x, C))
    dC = dfun(C, function(x)fn(A, B, x))
    tol.test(c(xa$dv, xb$dv, xc$dv), c(dA, dB, dC))
}

dgemv4.test <- function (ta, A, B) {
    fn = function (A, B) {
        if (ta > 0) A = t(A)
        A %*% B
    }
    g = graph.open()
    xa = number(A)
    xb = number(B)
    y = gemv(ta=ta, A=xa, B=xb)
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(A, function(x)fn(x, B))
    dB = dfun(as.array(B), function(x)fn(A, x))
    tol.test(c(xa$dv, xb$dv), c(dA, dB))
}


dger1.test <- function (alpha, A, B, C) {
    fn = function (alpha, A, B, C) {
        alpha * A %*% t(B) + C
    }

    g = graph.open()
    xalpha = number(alpha, scalar=T)
    xa = number(A)
    xb = number(B)
    xc = number(C)
    y = ger(alpha=xalpha, A=xa, B=xb, C=xc)
    y$set.dv(y$v**0)
    y$bw()
    dalpha = dfun(alpha, function(x)fn(x, A, B, C))
    dA = dfun(as.matrix(A), function(x)fn(alpha, x, B, C))
    dB = dfun(as.matrix(B), function(x)fn(alpha, A, x, C))
    dC = dfun(C, function(x)fn(alpha, A, B, x))
    tol.test(c(xalpha$dv, xa$dv, xb$dv, xc$dv), c(dalpha, dA, dB, dC))
}

dger2.test <- function (A, B, C) {
    fn = function (A, B, C) {
        A %*% t(B) + C
    }

    g = graph.open()
    xa = number(A)
    xb = number(B)
    xc = number(C)
    y = ger(A=xa, B=xb, C=xc)
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(as.matrix(A), function(x)fn(x, B, C))
    dB = dfun(as.matrix(B), function(x)fn(A, x, C))
    dC = dfun(C, function(x)fn(A, B, x))
    tol.test(c(xa$dv, xb$dv, xc$dv), c(dA, dB, dC))
}

dger3.test <- function (A, B) {
    fn = function (A, B) {
        A %*% t(B)
    }

    g = graph.open()
    xa = number(A)
    xb = number(B)
    y = ger(A=xa, B=xb)
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(as.matrix(A), function(x)fn(x, B))
    dB = dfun(as.matrix(B), function(x)fn(A, x))
    tol.test(c(xa$dv, xb$dv), c(dA, dB))
}

ddot.test <- function (A, B) {
    fn = function (A, B) {
        t(A) %*% B
    }

    g = graph.open()
    xa = number(A)
    xb = number(B)
    y = xa %T.% xb
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(as.matrix(A), function(x)fn(x, B))
    dB = dfun(as.matrix(B), function(x)fn(A, x))
    tol.test(c(xa$dv, xb$dv), c(dA, dB))
}

dinvMat.test <- function (A) {
    fn = function (A) {
        invMat(A)
    }

    g = graph.open()
    xa = number(A)
    y = invMat(xa)
    y$set.dv(y$v**0)
    y$bw()
    dA = dfun(A, fn)
    tol.test(c(xa$dv), c(dA))
}

dreduction.test <- function (x, fn) {
    g = graph.open()
    x = number(x)
    r = fn(x)
    r$dv = r$v**0
    r$bw()
    tol.test(dfun(x$v, fn), x$dv)
}

dobj.test <- function (y, x, fn) {
    g = graph.open()
    y = number(y, dx=F)
    x = number(x)
    obj = fn(y, x)
    obj$set.dv(obj$v**0)
    obj$bw()
    tol.test(dfun(x$v, function(x)fn(y$v, x)), x$dv) 
}

dldexp.test <- function (y, lam, lam.scalar) {
    fn <- function (y, lam) {
        stats::dexp(y, lam, log=TRUE)
    }

    xy = number(y, dx=FALSE)
    xlam = number(lam, scalar=lam.scalar)
    g = graph.open()
    ld = ldexp(xy, xlam)
    ld$set.dv(ld$v**0)
    ld$bw()
    dlam = dfun(lam, function(x)fn(y, x))
    tol.test(c(dlam), c(xlam$dv))
}

dldbeta.test <- function (y, a1, a2, a1.scalar, a2.scalar) {
    fn <- function (y, a1, a2) {
        stats::dbeta(y, a1, a2, log=TRUE)
    }

    xy = number(y, dx=FALSE)
    xa1 = number(a1, scalar=a1.scalar)
    xa2 = number(a2, scalar=a2.scalar)
    g = graph.open()
    ld = ldbeta(xy, xa1, xa2)
    ld$set.dv(ld$v**0)
    ld$bw()
    da1 = dfun(a1, function(x)fn(y, x, a2))
    da2 = dfun(a2, function(x)fn(y, a1, x))
    tol.test(c(da1, da2), c(xa1$dv, xa2$dv))
}

dldgamma.test <- function (y, a, b, a.scalar, b.scalar) {
    fn <- function (y, a, b) {
        stats::dgamma(y, a, b, log=TRUE)
    }

    xy = number(y, dx=F)
    xa = number(a, scalar=a.scalar)
    xb = number(b, scalar=b.scalar)
    g = graph.open()
    ld = ldgamma(xy, xa, xb)
    ld$set.dv(ld$v**0)
    ld$bw()
    da = dfun(a, function(x)fn(y, x, b))
    db = dfun(b, function(x)fn(y, a, x))
    tol.test(c(da, db), c(xa$dv, xb$dv))
}


dldnorm.test <- function (y, mu, s, mu.scalar, s.scalar) {
    fn <- function (y, mu, s) {
        stats::dnorm(y, mu, s, log=TRUE)
    }

    xy = number(y, dx=F)
    xmu = number(mu, scalar=mu.scalar)
    xs = number(s, scalar=s.scalar)
    g = graph.open()
    ld = ldnorm(xy, xmu, xs)
    ld$set.dv(ld$v**0)
    ld$bw()
    dmu = dfun(mu, function(x)fn(y, x, s))
    ds = dfun(s, function(x)fn(y, mu, x))
    tol.test(c(dmu, ds), c(xmu$dv, xs$dv))
}

dldmvnorm.test <- function (y, mu, E) {
    fn <- function (y, mu, E) {
        x = y - mu
        Q = t(x) %*% solve(E) %*% x
        -0.5 * (length(y) * log(2*pi) + log(det(E)) + Q)
    }
    y = number(y)
    mu = number(mu)
    E = number(E)
    g = graph.open()
    ans = ldmvnorm(y, mu, E)
    graph.close()
    ans$dv = 1
    ans$bw()
    dy = dfun(y$v, function (x)fn(x, mu$v, E$v))
    dmu = dfun(mu$v, function (x)fn(y$v, x, E$v))
    dE = dfun(E$v, function (x)fn(y$v, mu$v, x))
    tol.test(c(y$dv, mu$dv, E$dv), c(dy, dmu, dE))
}
       
dmvnorm_posterior.test <- function (a11, e21, e22, y11, mu11, inv) {
    a11 = number(a11)
    e21 = number(e21)
    e22 = number(e22)
    y11 = number(y11)
    mu11 = number(mu11, scalar=TRUE)
    g = graph.open()
    ans = mvnorm_posterior(a11, e21, e22, y11, mu11, inv)
    graph.close()
    ans$mu$dv = 1
    ans$E$dv = 1
    ans$mu$bw()

    da11 = dfun(a11$v, function(x)mvnorm_posterior(x, e21$v, e22$v, y11$v, mu11$v, inv)$mu)
    da11 = da11 + dfun(a11$v, function(x)mvnorm_posterior(x, e21$v, e22$v, y11$v, mu11$v, inv)$E)

    de21 = dfun(e21$v, function(x)mvnorm_posterior(a11$v, x, e22$v, y11$v, mu11$v, inv)$mu) 
    de21 = de21 + dfun(e21$v, function(x)mvnorm_posterior(a11$v, x, e22$v, y11$v, mu11$v, inv)$E)

    de22 = dfun(e22$v, function(x)mvnorm_posterior(a11$v, e21$v, x, y11$v, mu11$v, inv)$mu) 
    de22 = de22 + dfun(e22$v, function(x)mvnorm_posterior(a11$v, e21$v, x, y11$v, mu11$v, inv)$E)

    dy11 = dfun(y11$v, function(x)mvnorm_posterior(a11$v, e21$v, e22$v, x, mu11$v, inv)$mu)
    dy11 = dy11 + dfun(y11$v, function(x)mvnorm_posterior(a11$v, e21$v, e22$v, x, mu11$v, inv)$E)

    dmu11 = dfun(mu11$v, function(x)mvnorm_posterior(a11$v, e21$v, e22$v, y11$v, x, inv)$mu) 
    dmu11 = dmu11 + dfun(mu11$v, function(x)mvnorm_posterior(a11$v, e21$v, e22$v, y11$v, x, inv)$E)

    tol.test(c(a11$dv, e21$dv, e22$dv, y11$dv, mu11$dv), c(da11, de21, de22, dy11, dmu11))
}

dlkh.norm.test <- function (y, mu, s, w) {
    fn <- function (y, mu, s, w) {
        if (is.null(w)) w = 1
        base::sum(w * stats::dnorm(y, mu, s, log=T))
    }
    xy = number(y, dx=F)
    xmu = number(mu)
    xs = number(s)
    if (!is.null(w)) {
        xw = number(w, dx=F)
    } else {
        xw = NULL
    }
    g = graph.open()
    l = lkh.norm(xy, xmu, xs, xw)
    l$set.dv(l$v**0)
    l$bw()
    dmu = dfun(as.array(mu), function(x)fn(y, x, s, w))
    ds = dfun(as.array(s), function(x)fn(y, mu, x, w))
    tol.test(c(dmu, ds), c(xmu$dv, xs$dv)) 
}

test.linreg <- function () {
    fn = function (w, b) {
       mse(c(y$v), c(w %*% t(x$v) + c(b)))
    }

    mdl = module.lm$new(TRUE, 3, 1) 
    x = number(matrix(stats::rnorm(21), 7), dx=FALSE)
    y = number(stats::rnorm(7), dx=FALSE)

    g = graph.open()
    obj = mdl$obj(y, x)
    obj$dv = obj$v**0
    g$bw.zero()
    g$bw()
    
    dW = dfun(mdl$pars()$W$v, function(x)fn(x, mdl$pars()$B$v))
    dB = dfun(mdl$pars()$B$v, function(x)fn(mdl$pars()$W$v, x))
    ANS = tol.test(c(dW, dB), c(mdl$pars()$W$dv, mdl$pars()$B$dv))

    modello.reset()
    return(ANS)
}

test.logisticreg <- function () {
    fn = function (w, b) {
        yh = sigmoid(w %*% t(x$v) + c(b))
        bin.entropy(c(y$v), c(yh))
    }

    mdl = module.logistic$new(TRUE, 3)
    x = number(matrix(stats::rnorm(21), 7), dx=FALSE)
    y = number(base::sample(c(1, 0), 7, replace=T), dx=FALSE)

    g = graph.open()
    obj = mdl$obj(y, x)
    obj$dv = obj$v**0
    g$bw.zero()
    g$bw()

    dW = dfun(mdl$pars()$W$v, function(x)fn(x, mdl$pars()$B$v))
    dB = dfun(mdl$pars()$B$v, function(x)fn(mdl$pars()$W$v, x))
    ANS = tol.test(c(dW, dB), c(mdl$pars()$W$dv, mdl$pars()$B$dv))

    modello.reset()
    return(ANS)
}

test.multinomreg <- function () {
    fn = function (w, b) {
        yh = softmax(w %*% t(x$v) + c(b))
        cross.entropy(c(y$v), c(yh))
    }

    mdl = module.softmax$new(TRUE, 3, 3)
    x = number(matrix(stats::rnorm(21), 7), dx=FALSE)
    y = number(t(sapply(1:7, function(i)sample(c(1, 0, 0)))), dx=FALSE)

    g = graph.open()
    obj = mdl$obj(y, x)
    obj$dv = obj$v**0
    g$bw.zero()
    g$bw()
    
    dW = dfun(mdl$pars()$W$v, function(x)fn(x, mdl$pars()$B$v))
    dB = dfun(mdl$pars()$B$v, function(x)fn(mdl$pars()$W$v, x))
    ANS = tol.test(c(dW, dB), c(mdl$pars()$W$dv, mdl$pars()$B$dv))

    modello.reset()
    return(ANS)
}

test.opt<- function (opt, ...) {
    x = matrix(stats::rnorm(100), nrow=20, ncol=5)
    w = matrix(1, nrow = 5, ncol = 1)
    y = x %*% w + stats::rnorm(20, 0, 0.001)

    mdl = module.lm$new(TRUE, 5, 1, b=FALSE)
    xx = number(x, dx=FALSE)
    yy = number(y, dx=FALSE)

    opt = opt(mdl$pars())
    g = graph.open()
    j = mdl$obj(yy, xx)
    graph.close()

    opt$step(g, j, ...)

    tol.test(c(w), c(mdl$pars()$W$v))
}

test.dksqexp <-function (x1, x2, a, b) {
    x1 = number(x1)
    x2 = number(x2)
    a = number(a, scalar=TRUE)
    b = number(b, scalar=length(b)==1)
    graph.open()
    k = ksqexp(x1, x2, a, b)
    k$dv = 1
    k$bw()
    graph.close()
    dx1 = t(dfun(t(x1$v), function(x)ksqexp(x, t(x2$v), a$v, b$v)))
    dx2 = t(dfun(t(x2$v), function(x)ksqexp(t(x1$v), x, a$v, b$v)))
    da = dfun(a$v, function(x)ksqexp(t(x1$v), t(x2$v), x, b$v))
    db = dfun(b$v, function(x)ksqexp(t(x1$v), t(x2$v), a$v, x))
    tol.test(c(x1$dv, x1$dv, a$dv, b$dv), c(dx1, dx2, da, db))
}
    
