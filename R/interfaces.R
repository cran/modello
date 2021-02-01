.intrf.allocate_numbers <- function (n) {
    .Call(intrf_c__allocate_numbers, as.integer(n), PACKAGE="modello")
}

.intrf.deallocate_numbers <- function () {
    .Call(intrf_c__deallocate_numbers, PACKAGE="modello")
}

.intrf.number__rank <- function (id) {
    .Call(intrf_c__number__rank, as.integer(id), PACKAGE="modello")
}

.intrf.numbers__size <- function (id) {
    .Call(intrf_c__number__size, as.integer(id), PACKAGE="modello")
}

.intrf.numbers__shape <- function (id) {
    .Call(intrf_c__number__shape, as.integer(id), PACKAGE="modello")
}
.intrf.number__is_allocated <- function (id) {
    .Call(intrf_c__number__is_allocated, as.integer(id), PACKAGE="modello") > 0
}
.intrf.number__has_dx <- function (id) {
    .Call(intrf_c__number__has_dx, as.integer(id), PACKAGE="modello") > 0
}
.intrf.number__append <-  function (shp, dx) {
    .Call(intrf_c__number__append, as.integer(shp), as.integer(dx), PACKAGE="modello")
}

.intrf.number__set_v <-  function (id, x) {
    .Call(intrf_c__number__set_v, as.integer(id), as.double(x), PACKAGE="modello")
}

.intrf.number__set_dv <-  function (id, x) {
    .Call(intrf_c__number__set_dv, as.integer(id), as.double(x), PACKAGE="modello")
}

.intrf.number__set_slice_v <- function (id, v, s) {
    storage.mode(s) = "integer"
    .Call(intrf_c__number__set_slice_v, as.integer(id), as.double(v), s) 
}

.intrf.number__set_slice_dv <- function (id, dv, s) {
    storage.mode(s) = "integer"
    .Call(intrf_c__number__set_slice_dv, as.integer(id), as.double(dv), s) 
}

.intrf.number__set_flat_slice_v <- function (id, v, s) {
    .Call(intrf_c__number__set_flat_slice_v, as.integer(id), as.double(v), as.integer(s)) 
}

.intrf.number__set_flat_slice_dv <- function (id, dv, s) {
    .Call(intrf_c__number__set_flat_slice_dv, as.integer(id), as.double(dv), as.integer(s)) 
}

.intrf.number__get_v <- function (id) {
    .Call(intrf_c__number__get_v, as.integer(id), PACKAGE="modello")
}

.intrf.number__get_dv <- function (id) {
    .Call(intrf_c__number__get_dv, as.integer(id), PACKAGE="modello")
}

.intrf.number__inlock_free <- function (id) {
    .Call(intrf_c__number__inlock_free, as.integer(id), PACKAGE="modello") > 0
}

.intrf.number__pop <- function (id) {
    .Call(intrf_c__number__pop, as.integer(id), PACKAGE="modello")
}

.intrf.number__gc <- function () {
    .Call(intrf_c__number__gc)
}

.intrf.nodes__allocate <- function (n) {
    .Call(intrf_c__nodes__allocate, as.integer(n), PACKAGE="modello")
}

.intrf.deallocate_nodes <- function () {
    .Call(intrf_c__deallocate_nodes)
}

.intrf.graphs__allocate <- function (n) {
    .Call(intrf_c__graphs__allocate, as.integer(n), PACKAGE="modello")
}

.intrf.deallocate_graphs <- function () {
    .Call(intrf_c__deallocate_graphs)
}
.intrf.graph__pop <- function (gi) {
    .Call(intrf_c__graph__pop, as.integer(gi), PACKAGE="modello")
}

.intrf.graph__gc <- function () {
    .Call(intrf_c__graph__gc)
}

.intrf.graph__open <- function (i=0) {
    .Call(intrf_c__graph__open, as.integer(i), PACKAGE="modello")
}

.intrf.graph__close <- function () {
    .Call(intrf_c__graph__close)
}

.intrf.ggg <- function (i) {
    .Call(intrf_c__ggg, as.integer(i), PACKAGE="modello")
}

.intrf.graphi__get <- function () {
    .Call(intrf_c__get_graphi, PACKAGE="modello")
}

.intrf.number__op <- function (ndi) {
    .Call(intrf_c__number__op, as.integer(ndi), PACKAGE="modello")
}

.intrf.number__bw <- function (ndi) {
    .Call(intrf_c__number__bw, as.integer(ndi), PACKAGE="modello")
}

.intrf.number__bw_zero <- function (ndi) {
    .Call(intrf_c__number__bw_zero, as.integer(ndi), PACKAGE="modello")
}

.intrf.graph__op <- function (g) {
    .Call(intrf_c__graph__op, as.integer(g), PACKAGE="modello")
}

.intrf.graph__bw <- function (g) {
    .Call(intrf_c__graph__bw, as.integer(g), PACKAGE="modello")
}

.intrf.graph__bw_zero <- function (g) {
    .Call(intrf_c__graph__bw_zero, as.integer(g), PACKAGE="modello")
}

.intrf.number__abs <- function (id) {
    .Call(intrf_c__number__abs, as.integer(id),  PACKAGE="modello")
}

.intrf.number__exp <- function (id) {
    .Call(intrf_c__number__exp, as.integer(id),  PACKAGE="modello")
}

.intrf.number__log <- function (id) {
    .Call(intrf_c__number__log, as.integer(id),  PACKAGE="modello")
}

.intrf.number__sin <- function (id) {
    .Call(intrf_c__number__sin, as.integer(id),  PACKAGE="modello")
}

.intrf.number__cos <- function (id) {
    .Call(intrf_c__number__cos, as.integer(id),  PACKAGE="modello")
}

.intrf.number__tan <- function (id) {
    .Call(intrf_c__number__tan, as.integer(id),  PACKAGE="modello")
}

.intrf.number__sinh <- function (id) {
    .Call(intrf_c__number__sinh, as.integer(id),  PACKAGE="modello")
}

.intrf.number__cosh <- function (id) {
    .Call(intrf_c__number__cosh, as.integer(id),  PACKAGE="modello")
}

.intrf.number__tanh <- function (id) {
    .Call(intrf_c__number__tanh, as.integer(id),  PACKAGE="modello")
}

.intrf.number__sigmoid <- function (id) {
    .Call(intrf_c__number__sigmoid, as.integer(id),  PACKAGE="modello")
}

.intrf.number__relu <- function (id) {
    .Call(intrf_c__number__relu, as.integer(id),  PACKAGE="modello")
}

.intrf.number__swish <- function (id) {
    .Call(intrf_c__number__swish, as.integer(id),  PACKAGE="modello")
}

.intrf.number__softmax <- function (id, k) {
    .Call(intrf_c__number__softmax, as.integer(id), as.integer(k),  PACKAGE="modello")
}

.intrf.number__slice <- function (id, s) {
    .Call(intrf_c__number__slice, as.integer(id), as.integer(s),  PACKAGE="modello")
}

.intrf.number__flat_slice <- function (id, s) {
    .Call(intrf_c__number__flat_slice, as.integer(id), as.integer(s),  PACKAGE="modello")
}

.intrf.number__contiguous_slice <- function (id, s1, s2) {
    .Call(intrf_c__number__contiguous_slice, as.integer(id),
          as.integer(s1), as.integer(s2),  PACKAGE="modello")
}

.intrf.number__reshape <- function (id, shp) {
    .Call(intrf_c__number__reshape, as.integer(id), as.integer(shp),  PACKAGE="modello")
}

.intrf.number__drop_shape <- function (id) {
    .Call(intrf_c__number__drop_shape, as.integer(id),  PACKAGE="modello")
}

.intrf.number__bind <- function (id1, id2, k) {
    .Call(intrf_c__number__bind, as.integer(id1), as.integer(id2), as.integer(k),  PACKAGE="modello")
}

.intrf.number__embeddings <- function (idf, idx, n) {
    .Call(intrf_c__number__embeddings, as.integer(idf), as.integer(idx), as.integer(n),  PACKAGE="modello")
}

.intrf.number__add <- function (id1, id2) {
    .Call(intrf_c__number__add, as.integer(id1), as.integer(id2),  PACKAGE="modello")
}

.intrf.number__sub <- function (id1, id2) {
    .Call(intrf_c__number__sub, as.integer(id1), as.integer(id2),  PACKAGE="modello")
}

.intrf.number__mult <- function (id1, id2) {
    .Call(intrf_c__number__mult, as.integer(id1), as.integer(id2),  PACKAGE="modello")
}

.intrf.number__div <- function (id1, id2) {
    .Call(intrf_c__number__div, as.integer(id1), as.integer(id2),  PACKAGE="modello")
}

.intrf.number__pow <- function (id1, id2) {
    .Call(intrf_c__number__pow, as.integer(id1), as.integer(id2),  PACKAGE="modello")
}

.intrf.number__sum <- function (id, k) {
    .Call(intrf_c__number__sum, as.integer(id), as.integer(k),  PACKAGE="modello")
}

.intrf.number__ssq <- function (id) {
    .Call(intrf_c__number__ssq, as.integer(id),  PACKAGE="modello")
}

.intrf.number__bin_entropy <- function (id1, id2, k=0) {
    .Call(intrf_c__number__bin_entropy, as.integer(id1), as.integer(id2),
          as.integer(k),  PACKAGE="modello")
}

.intrf.number__cross_entropy <- function (id1, id2, k=0) {
    .Call(intrf_c__number__cross_entropy, as.integer(id1), as.integer(id2),
          as.integer(k),  PACKAGE="modello")
}

.intrf.number__mse <- function (idy, idyh) {
    .Call(intrf_c__number__mse, as.integer(idy), as.integer(idyh),  PACKAGE="modello")
}

.intrf.number__mae <- function (idy, idyh) {
    .Call(intrf_c__number__mae, as.integer(idy), as.integer(idyh),  PACKAGE="modello")
}

.intrf.number__dgemm0 <- function (ta, tb, a, A, B, b, C) {
    .Call(intrf_c__number__dgemm0,
          as.integer(ta),
          as.integer(tb),
          as.integer(a),
          as.integer(A),
          as.integer(B),
          as.integer(b),
          as.integer(C),
          PACKAGE="modello")
}

.intrf.number__dgemm1 <- function (ta, tb, a, A, B) {
    .Call(intrf_c__number__dgemm1,
          as.integer(ta),
          as.integer(tb),
          as.integer(a),
          as.integer(A),
          as.integer(B),
          PACKAGE="modello")
}

.intrf.number__dgemm15 <- function (ta, tb, A, B, C) {
    .Call(intrf_c__number__dgemm15,
          as.integer(ta),
          as.integer(tb),
          as.integer(A),
          as.integer(B),
          as.integer(C),
          PACKAGE="modello")
}

.intrf.number__dgemm2 <- function (ta, tb, A, B) {
    .Call(intrf_c__number__dgemm2,
          as.integer(ta),
          as.integer(tb),
          as.integer(A),
          as.integer(B),
          PACKAGE="modello")
}

.intrf.number__dp_gemv__1 <- function (ta, a, A, x, b, y) {
    .Call(intrf_c__number__dp_gemv__1,
          as.integer(ta),
          as.integer(a),
          as.integer(A),
          as.integer(x),
          as.integer(b),
          as.integer(y),
          PACKAGE="modello")
}

.intrf.number__dp_gemv__2 <- function (ta, a, A, x) {
    .Call(intrf_c__number__dp_gemv__2,
          as.integer(ta),
          as.integer(a),
          as.integer(A),
          as.integer(x),
          PACKAGE="modello")
}

.intrf.number__dp_gemv__3 <- function (ta, A, x, y) {
    .Call(intrf_c__number__dp_gemv__3,
          as.integer(ta),
          as.integer(A),
          as.integer(x),
          as.integer(y),
          PACKAGE="modello")
}

.intrf.number__dp_gemv__4 <- function (ta, A, x) {
    .Call(intrf_c__number__dp_gemv__4,
          as.integer(ta),
          as.integer(A),
          as.integer(x),
          PACKAGE="modello")
}

.intrf.number__dp_ger__1 <- function (a, x, y, z) {
    .Call(intrf_c__number__dp_ger__1,
          as.integer(a),
          as.integer(x),
          as.integer(y),
          as.integer(z),
          PACKAGE="modello")
}

.intrf.number__dp_ger__2 <- function (x, y, z) {
    .Call(intrf_c__number__dp_ger__2,
          as.integer(x),
          as.integer(y),
          as.integer(z),
          PACKAGE="modello")
}

.intrf.number__dp_ger__3 <- function (x, y) {
    .Call(intrf_c__number__dp_ger__3,
          as.integer(x),
          as.integer(y),
          PACKAGE="modello")
}

.intrf.number__dp_dot <- function (x, y) {
    .Call(intrf_c__number__dp_dot,
          as.integer(x),
          as.integer(y),
          PACKAGE="modello")
}

.intrf.number__ldexp <- function (y, lam) {
    .Call(intrf_c__number__ldexp, as.integer(y), as.integer(lam),
          PACKAGE="modello")
}

.intrf.number__ldlaplace <- function (y, mu, lam) {
    .Call(intrf_c__number__ldlaplace, as.integer(y), as.integer(mu),
          as.integer(lam), PACKAGE="modello")
}

.intrf.number__ldbeta <- function (y, a1, a2) {
    .Call(intrf_c__number__ldbeta, as.integer(y), as.integer(a1),
          as.integer(a2), PACKAGE="modello")
}

.intrf.number__ldgamma <- function (y, a, b) {
    .Call(intrf_c__number__ldgamma, as.integer(y), as.integer(a),
          as.integer(b), PACKAGE="modello")
}

.intrf.number__ldnorm <- function (y, mu, s) {
    .Call(intrf_c__number__ldnorm, as.integer(y), as.integer(mu),
          as.integer(s), PACKAGE="modello")
}

.intrf.number__ldmvnorm__1 <- function (y, mu, E) {
    .Call(intrf_c__number__ldmvnorm__1, as.integer(y), as.integer(mu),
          as.integer(E), PACKAGE="modello")
}

.intrf.number__lkh_norm <- function (y, mu, s, w) {
    .Call(intrf_c__number__lkh_norm, as.integer(y), as.integer(mu),
          as.integer(s), as.integer(w), PACKAGE="modello")
}

## .intrf.number__ksqexp <- function (x1, x2, a, b) {
##     .Call(intrf_c__number__ksqexp, as.integer(x1), as.integer(x2),
##           as.integer(a), as.integer(b), PACKAGE="modello")
## }

.intrf.number__invMat <- function (id) {
    .Call(intrf_c__number__invMat, as.integer(id),  PACKAGE="modello")
}

.intrf.allocate_gopts <- function (n) {
    .Call(intrf_c__allocate_gopts, as.integer(n), PACKAGE="modello")
}

.intrf.deallocate_gopts <- function () {
    .Call(intrf_c__deallocate_gopts, PACKAGE="modello")
}

.intrf.sgd__append <- function (xin) {
    .Call(intrf_c__sgd__append, as.integer(xin), PACKAGE="modello")
}

.intrf.sgd__step <- function (xoi, gi, xout, lr, niter) {
    .Call(intrf_c__sgd__append,
          as.integer(xoi),
          as.integer(gi),
          as.integer(xout),
          as.integer(lr),
          as.integer(niter),
          PACKAGE="modello")
}

.intrf.sgdwm__append <- function (xin) {
    .Call(intrf_c__sgdwm__append, as.integer(xin), PACKAGE="modello")
}

.intrf.sgdwm__step <- function (xoi, gi, xout, lr, alpha, niter) {
    .Call(intrf_c__sgd__append,
          as.integer(xoi),
          as.integer(gi),
          as.integer(xout),
          as.integer(lr),
          as.integer(alpha),
          as.integer(niter),
          PACKAGE="modello")
}

.intrf.adam__append <- function (xin) {
    .Call(intrf_c__adam__append, as.integer(xin), PACKAGE="modello")
}

.intrf.sgd__step <- function (xoi, gi, xout, lr, b1, b2, niter) {
    .Call(intrf_c__sgd__append,
          as.integer(xoi),
          as.integer(gi),
          as.integer(xout),
          as.integer(lr),
          as.integer(b1),
          as.integer(b2),
          as.integer(niter),
          PACKAGE="modello")
}

.intrf.gopt__pop <- function (i) {
    .Call(intrf_c__gopt__pop, as.integer(i), PACKAGE="modello")
}

.intrf.sgd__step <- function (xoi, gi, xout, lr, niter) {
    .Call(intrf_c__sgd__step,
          as.integer(xoi),
          as.integer(gi),
          as.integer(xout),
          as.double(lr),
          as.integer(niter),
          PACKAGE="modello")
}

.intrf.sgdwm__step <- function (xoi, gi, xout, lr, alpha, niter) {
    .Call(intrf_c__sgdwm__step,
          as.integer(xoi),
          as.integer(gi),
          as.integer(xout),
          as.double(lr),
          as.double(alpha),
          as.integer(niter),
          PACKAGE="modello")
}

.intrf.adam__step <- function (xoi, gi, xout, lr, beta1, beta2, niter) {
    .Call(intrf_c__adam__step,
          as.integer(xoi),
          as.integer(gi),
          as.integer(xout),
          as.double(lr),
          as.double(beta1),
          as.double(beta2),
          as.integer(niter),
          PACKAGE="modello")
}




