#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void F77_NAME(intrf_f__number__rank)(int *id, int *r);
extern SEXP intrf_c__number__rank (SEXP id) {
  SEXP r;
  PROTECT(r=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__rank)(INTEGER(id), INTEGER(r));
  UNPROTECT(1);
  return(r);
}
void F77_NAME(intrf_f__allocate_numbers)(int *n);
extern SEXP intrf_c__allocate_numbers (SEXP n) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__allocate_numbers)(INTEGER(n));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__is_allocated)(int *id, int *ans);
extern SEXP intrf_c__number__is_allocated (SEXP id) {
  SEXP ans;
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__is_allocated)(INTEGER(id), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__has_dx)(int *id, int *ans);
extern SEXP intrf_c__number__has_dx (SEXP id) {
  SEXP ans;
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__has_dx)(INTEGER(id), INTEGER(ans));
  unprotect(1);
  return(ans);
}
void F77_NAME(intrf_f__deallocate_numbers)();
extern SEXP intrf_c__deallocate_numbers () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__deallocate_numbers)();
  UNPROTECT(1);
  return(ans);
} 
void F77_NAME(intrf_f__number__append)(int *id, int *r, int *shp, int *dx);
extern SEXP intrf_c__number__append (SEXP shp, SEXP dx) {
  SEXP ans;
  int r = length(shp);
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__append)(INTEGER(ans), &r, INTEGER(shp), INTEGER(dx));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__size)(int *i, int *sz);
extern SEXP intrf_c__number__size (SEXP id) {
  SEXP ans;
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__size)(INTEGER(id), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
} 
void F77_NAME(intrf_f__number__shape)(int* id, int *r, int *shp);
extern SEXP intrf_c__number__shape (SEXP id) {
  SEXP r, ans;
  r = intrf_c__number__rank(id);
  PROTECT(ans=allocVector(INTSXP, INTEGER(r)[0]));
  F77_CALL(intrf_f__number__shape)(INTEGER(id), INTEGER(r), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__set_v)(int *id, int *n, double *x);
extern SEXP intrf_c__number__set_v (SEXP id, SEXP x) {
  SEXP ans;
  int n = length(x);
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__set_v)(INTEGER(id), &n, REAL(x));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__set_dv)(int *i, int *n, double *x);
extern SEXP intrf_c__number__set_dv (SEXP i, SEXP x) {
  SEXP ans;
  int n = length(x);
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__set_dv)(INTEGER(i), &n, REAL(x));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__set_slice_v)(int *id, double *v, int *s, int *m, int *n, int *l);
extern SEXP intrf_c__number__set_slice_v (SEXP id, SEXP v, SEXP s) {
  SEXP ans;
  int l = length(v);
  int m = nrows(s);
  int n = ncols(s);
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__set_slice_v)(INTEGER(id), REAL(v), INTEGER(s), &m, &n, &l);
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__set_slice_dv)(int *id, double *dv, int *s, int *m, int *n, int *l);
extern SEXP intrf_c__number__set_slice_dv (SEXP id, SEXP dv, SEXP s) {
  SEXP ans;
  int l = length(dv);
  int m = nrows(s);
  int n = ncols(s);
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__set_slice_dv)(INTEGER(id), REAL(dv), INTEGER(s), &m, &n, &l);
  UNPROTECT(1);
  return(ans);
}

void F77_NAME(intrf_f__number__set_flat_slice_v)(int *id, double *v, int *s, int *m, int *l);
extern SEXP intrf_c__number__set_flat_slice_v (SEXP id, SEXP v, SEXP s) {
  SEXP ans;
  int l = length(v);
  int m = length(s);
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__set_flat_slice_v)(INTEGER(id), REAL(v), INTEGER(s), &m, &l);
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__set_flat_slice_dv)(int *id, double *dv, int *s, int *m, int *l);
extern SEXP intrf_c__number__set_flat_slice_dv (SEXP id, SEXP dv, SEXP s) {
  SEXP ans;
  int l = length(dv);
  int m = length(s);
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__set_flat_slice_dv)(INTEGER(id), REAL(dv), INTEGER(s), &m, &l);
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__get_v)(int *id, int *n, double *x);
extern SEXP intrf_c__number__get_v (SEXP id) {
  SEXP r, sz, shp, ans;
  r = intrf_c__number__rank(id);
  sz = intrf_c__number__size(id);
  shp = intrf_c__number__shape(id);
  PROTECT(ans=allocVector(REALSXP, INTEGER(sz)[0]));
  F77_CALL(intrf_f__number__get_v)(INTEGER(id), INTEGER(sz), REAL(ans));
  if (INTEGER(r)[0] > 0) setAttrib(ans, R_DimSymbol, shp);
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__get_dv)(int *id, int *n, double *x);
extern SEXP intrf_c__number__get_dv (SEXP id) {
  SEXP r, sz, shp, ans;
  r = intrf_c__number__rank(id);
  sz = intrf_c__number__size(id);
  shp = intrf_c__number__shape(id);
  PROTECT(ans=allocVector(REALSXP, INTEGER(sz)[0]));
  F77_CALL(intrf_f__number__get_dv)(INTEGER(id), INTEGER(sz), REAL(ans));
  if (INTEGER(r)[0] > 0) setAttrib(ans, R_DimSymbol, shp);
  UNPROTECT(1);
  return(ans);
}

void F77_NAME(intrf_f__number__inlock_free)(int * id, int *ans);
extern SEXP intrf_c__number__inlock_free (SEXP id) {
  SEXP ans;
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__inlock_free)(INTEGER(id), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}

void F77_NAME(intrf_f__number__pop)(int *id);
extern SEXP intrf_c__number__pop (SEXP id) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__pop)(INTEGER(id));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__gc)();
extern SEXP intrf_c__number__gc () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__gc)();
  UNPROTECT(1);
  return(ans);
}

//> NODES
void F77_NAME(intrf_f__nodes__allocate)(int *n);
extern SEXP intrf_c__nodes__allocate (SEXP n) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__nodes__allocate)(INTEGER(n));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__deallocate_nodes)();
extern SEXP intrf_c__deallocate_nodes () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__deallocate_nodes)();
  UNPROTECT(1);
  return(ans);
} 
void F77_NAME(intrf_f__graphs__allocate)(int *n);
extern SEXP intrf_c__graphs__allocate (SEXP n) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graphs__allocate)(INTEGER(n));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__is_allocated)(int *id, int *ans);
extern SEXP intrf_c__graph__is_allocated (SEXP id) {
  SEXP ans;
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__graph__is_allocated)(INTEGER(id), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__deallocate_graphs)();
extern SEXP intrf_c__deallocate_graphs () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__deallocate_graphs)();
  UNPROTECT(1);
  return(ans);
} 
void F77_NAME(intrf_f__graph__open)(int *i);
extern SEXP intrf_c__graph__open (SEXP i) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__open)(INTEGER(i));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__close)();
extern SEXP intrf_c__graph__close () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__close)();
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__ggg)(int *i, int *n, int *x, int *sz);
extern SEXP intrf_c__ggg (SEXP i) {
  SEXP ans;
  int n = 0, m = 1, sz1 = 1, sz0 = 0;
  F77_CALL(intrf_f__ggg)(INTEGER(i), &m, &n, &sz1);
  PROTECT(ans=allocVector(INTSXP, n));
  F77_CALL(intrf_f__ggg)(INTEGER(i), &n, INTEGER(ans), &sz0);
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__get_graphi)(int *i);
extern SEXP intrf_c__get_graphi () {
  SEXP ans;
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__get_graphi)(INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__pop)(int *gi);
extern SEXP intrf_c__graph__pop (SEXP gi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__pop)(INTEGER(gi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__gc)();
extern SEXP intrf_c__graph__gc () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__gc)();
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__op)(int *ndi);
extern SEXP intrf_c__number__op (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__op)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__fw)(int *ndi);
extern SEXP intrf_c__number__fw (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__fw)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__bw_zero)(int *ndi);
extern SEXP intrf_c__number__bw_zero (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__bw_zero)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__bw)(int *ndi);
extern SEXP intrf_c__number__bw (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__number__bw)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__op)(int *ndi);
extern SEXP intrf_c__graph__op (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__op)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__fw)(int *ndi);
extern SEXP intrf_c__graph__fw (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__fw)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__bw_zero)(int *ndi);
extern SEXP intrf_c__graph__bw_zero (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__bw_zero)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__graph__bw)(int *ndi);
extern SEXP intrf_c__graph__bw (SEXP ndi) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__graph__bw)(INTEGER(ndi));
  UNPROTECT(1);
  return(ans);
}
//> NUMBERS_MATH
void F77_NAME(intrf_f__number__slice)(int *id, int *n, int *s, int *idout);
extern SEXP intrf_c__number__slice (SEXP id, SEXP s) {
  SEXP ans;
  int n = length(s);
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__slice)(INTEGER(id), &n, INTEGER(s), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__flat_slice)(int *id, int *n, int *s, int *idout);
extern SEXP intrf_c__number__flat_slice (SEXP id, SEXP s) {
  SEXP ans;
  int n = length(s);
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__flat_slice)(INTEGER(id), &n, INTEGER(s), INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__number__contiguous_slice)(int *id, int *s1, int *s2, int *idout);
extern SEXP intrf_c__number__contiguous_slice (SEXP id, SEXP s1, SEXP s2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__contiguous_slice)(INTEGER(id), INTEGER(s1), INTEGER(s2),
					      INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__reshape)(int *id, int *n, int *shp, int *idout);
extern SEXP intrf_c__number__reshape (SEXP id, SEXP shp) {
  SEXP idout;
  int n = length(shp);
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__reshape)(INTEGER(id), &n, INTEGER(shp), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__drop_shape)(int *id, int *idout);
extern SEXP intrf_c__number__drop_shape (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__drop_shape)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__bind)(int *id1, int *id2, int *k, int *idout);
extern SEXP intrf_c__number__bind (SEXP id1, SEXP id2, SEXP k) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__bind)(INTEGER(id1), INTEGER(id2), INTEGER(k), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__embeddings)(int *idf, int *idx, int *n, int*idout);
extern SEXP intrf_c__number__embeddings (SEXP idf, SEXP idx, SEXP n) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__embeddings)(INTEGER(idf), INTEGER(idx), INTEGER(n), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__add)(int *id1, int *id2, int *idout);
extern SEXP intrf_c__number__add (SEXP id1, SEXP id2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__add)(INTEGER(id1), INTEGER(id2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__sub)(int *id1, int *id2, int *idout);
extern SEXP intrf_c__number__sub (SEXP id1, SEXP id2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__sub)(INTEGER(id1), INTEGER(id2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__mult)(int *id1, int *id2, int *idout);
extern SEXP intrf_c__number__mult (SEXP id1, SEXP id2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__mult)(INTEGER(id1), INTEGER(id2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__pow)(int *id1, int *id2, int *idout);
extern SEXP intrf_c__number__pow (SEXP id1, SEXP id2, SEXP typ1, SEXP typ2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__pow)(INTEGER(id1), INTEGER(id2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__div)(int *id1, int *id2, int *idout);
extern SEXP intrf_c__number__div (SEXP id1, SEXP id2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__div)(INTEGER(id1), INTEGER(id2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__bin_entropy)(int *id1, int *id2, int *idout);
extern SEXP intrf_c__number__bin_entropy (SEXP id1, SEXP id2, SEXP k) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__bin_entropy)(INTEGER(id1), INTEGER(id2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__cross_entropy)(int *id1, int *id2, int *k, int *idout);
extern SEXP intrf_c__number__cross_entropy (SEXP id1, SEXP id2, SEXP k) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__cross_entropy)(INTEGER(id1), INTEGER(id2), INTEGER(k), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__mse)(int *idy, int *idyh, int *idout);
extern SEXP intrf_c__number__mse (SEXP idy, SEXP idyh) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__mse)(INTEGER(idy), INTEGER(idyh), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__mae)(int *idy, int *idyh, int *idout);
extern SEXP intrf_c__number__mae (SEXP idy, SEXP idyh) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__mae)(INTEGER(idy), INTEGER(idyh), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}  

void F77_NAME(intrf_f__number__abs)(int *id, int *idout);
extern SEXP intrf_c__number__abs (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__abs)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__exp)(int *id, int *idout);
extern SEXP intrf_c__number__exp (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__exp)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__log)(int *id, int *idout);
extern SEXP intrf_c__number__log (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__log)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__sin)(int *id, int *idout);
extern SEXP intrf_c__number__sin (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__sin)(INTEGER(id), INTEGER(idout));
  //SET_VECTOR_ELT(ans, 0, idout);
  //SET_VECTOR_ELT(ans, 1, typout);
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__cos)(int *id, int *idout);
extern SEXP intrf_c__number__cos (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__cos)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__tan)(int *id, int *idout);
extern SEXP intrf_c__number__tan (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__tan)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__sinh)(int *id, int *idout);
extern SEXP intrf_c__number__sinh (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__sinh)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__cosh)(int *id, int *idout);
extern SEXP intrf_c__number__cosh (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__cosh)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__tanh)(int *id, int *idout);
extern SEXP intrf_c__number__tanh (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__tanh)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__sigmoid)(int *id, int *idout);
extern SEXP intrf_c__number__sigmoid (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__sigmoid)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__relu)(int *id, int *idout);
extern SEXP intrf_c__number__relu (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__relu)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__swish)(int *id, int *idout);
extern SEXP intrf_c__number__swish (SEXP id) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__swish)(INTEGER(id), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__elu)(int *id, int *ida, int *idout);
extern SEXP intrf_c__number__elu (SEXP id, SEXP ida) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__elu)(INTEGER(id), INTEGER(ida), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__softmax)(int *id, int *k, int *idout);
extern SEXP intrf_c__number__softmax (SEXP id, SEXP k) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__softmax)(INTEGER(id), INTEGER(k), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}

//> DGEMM
void F77_NAME(intrf_f__number__dgemm0)(int *transA, int *transB, int *idAlpha, int *idA,
					int *idB, int *idBeta, int *idC, int *idout);
extern SEXP intrf_c__number__dgemm0 (SEXP transA, SEXP transB,
				      SEXP idAlpha, SEXP idA, SEXP idB, SEXP idBeta, SEXP idC) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dgemm0)(INTEGER(transA), INTEGER(transB), INTEGER(idAlpha), INTEGER(idA),
				     INTEGER(idB), INTEGER(idBeta), INTEGER(idC), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dgemm1)(int *transA, int *transB, int *idAlpha, int *idA,
					int *idB, int *idout);
extern SEXP intrf_c__number__dgemm1 (SEXP transA, SEXP transB, SEXP idAlpha, SEXP idA, SEXP idB) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dgemm1)(INTEGER(transA), INTEGER(transB), INTEGER(idAlpha),
				     INTEGER(idA), INTEGER(idB), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dgemm15)(int *transA, int *transB, int *idA, int *idB, int *idC, int *idout);
extern SEXP intrf_c__number__dgemm15 (SEXP transA, SEXP transB, SEXP idA, SEXP idB, SEXP idC) {
  SEXP idout;;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dgemm15)(INTEGER(transA), INTEGER(transB), INTEGER(idA),
				     INTEGER(idB), INTEGER(idC), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dgemm2)(int *transA, int *transB, int *idA, int *idB, int *idout);
extern SEXP intrf_c__number__dgemm2 (SEXP transA, SEXP transB, SEXP idA, SEXP idB) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dgemm2)(INTEGER(transA), INTEGER(transB), INTEGER(idA), INTEGER(idB),
				     INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_gemv__1)(int *trans, int *idAlpha, int *idA, int *idx,
					   int *idBeta, int *idy, int *idout);
extern SEXP intrf_c__number__dp_gemv__1 (SEXP trans, SEXP idAlpha, SEXP idA, SEXP idx,
					 SEXP idBeta, SEXP idy) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_gemv__1)(INTEGER(trans), INTEGER(idAlpha), INTEGER(idA),
					INTEGER(idx), INTEGER(idBeta), INTEGER(idy), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_gemv__2)(int *trans, int *idAlpha, int *idA, int *idx, int *idout);
extern SEXP intrf_c__number__dp_gemv__2 (SEXP trans, SEXP idAlpha, SEXP idA, SEXP idx) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_gemv__2)(INTEGER(trans), INTEGER(idAlpha), INTEGER(idA),
					INTEGER(idx), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_gemv__3)(int *trans, int *idA, int *idx, int *idy, int *idout);
extern SEXP intrf_c__number__dp_gemv__3 (SEXP trans, SEXP idA, SEXP idx, SEXP idy) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_gemv__3)(INTEGER(trans), INTEGER(idA), INTEGER(idx),
					INTEGER(idy), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_gemv__4)(int *trans, int *idA, int *idx, int *idout);
extern SEXP intrf_c__number__dp_gemv__4 (SEXP trans, SEXP idA, SEXP idx) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_gemv__4)(INTEGER(trans), INTEGER(idA), INTEGER(idx), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_ger__1)(int *idAlpha, int *idx, int *idy, int *idz, int *idout);
extern SEXP intrf_c__number__dp_ger__1 (SEXP idAlpha, SEXP idx, SEXP idy, SEXP idz) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_ger__1)(INTEGER(idAlpha), INTEGER(idx), INTEGER(idy), INTEGER(idz),
				       INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_ger__2)(int *idx, int *idy, int *idz, int *idout);
extern SEXP intrf_c__number__dp_ger__2 (SEXP idx, SEXP idy, SEXP idz) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_ger__2)(INTEGER(idx), INTEGER(idy), INTEGER(idz), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_ger__3)(int *idx, int *idy, int *idout);
extern SEXP intrf_c__number__dp_ger__3 (SEXP idx, SEXP idy) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_ger__3)(INTEGER(idx), INTEGER(idy), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__dp_dot)(int *idx, int *idy, int *idout);
extern SEXP intrf_c__number__dp_dot (SEXP idx, SEXP idy) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__dp_dot)(INTEGER(idx), INTEGER(idy), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}

//> MATRIX INVERSION

void F77_NAME(intrf_f__number__invMat)(int *idx, int *idout);
extern SEXP intrf_c__number__invMat(SEXP idx) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__invMat)(INTEGER(idx), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__sum)(int *idx, int *k, int *idout);
extern SEXP intrf_c__number__sum (SEXP idx, SEXP k) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__sum)(INTEGER(idx), INTEGER(k), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
//> @name SSQ
void F77_NAME(intrf_f__number__ssq)(int *idx, int *idout);
extern SEXP intrf_c__number__ssq (SEXP idx) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ssq)(INTEGER(idx), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__ldexp)(int *idy, int *idlam, int *idout);
extern SEXP intrf_c__number__ldexp (SEXP idy, SEXP idlam) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ldexp)(INTEGER(idy), INTEGER(idlam), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__ldlaplace)(int *idy, int *idmu, int *idlam, int *idout);
extern SEXP intrf_c__number__ldlaplace (SEXP idy, SEXP idmu, SEXP idlam) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ldlaplace)(INTEGER(idy), INTEGER(idmu),
				       INTEGER(idlam), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__ldbeta)(int *idy, int *a1, int *a2, int *idout);
extern SEXP intrf_c__number__ldbeta (SEXP idy, SEXP ida1, SEXP ida2) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ldbeta)(INTEGER(idy), INTEGER(ida1),
				    INTEGER(ida2), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__ldgamma)(int *idy, int *ida, int *idb, int *idout);
extern SEXP intrf_c__number__ldgamma (SEXP idy, SEXP ida, SEXP idb) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ldgamma)(INTEGER(idy), INTEGER(ida),
				     INTEGER(idb), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__ldnorm)(int *idy, int *idmu, int *ids, int *idout);
extern SEXP intrf_c__number__ldnorm (SEXP idy, SEXP idmu, SEXP ids) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ldnorm)(INTEGER(idy), INTEGER(idmu),
				    INTEGER(ids), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}
void F77_NAME(intrf_f__number__ldmvnorm__1)(int *idy, int *idmu, int *idE, int *idout);
extern SEXP intrf_c__number__ldmvnorm__1 (SEXP idy, SEXP idmu, SEXP idE) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ldmvnorm__1)(INTEGER(idy), INTEGER(idmu),
					 INTEGER(idE), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}  
void F77_NAME(intrf_f__number__lkh_norm)(int *idy, int *idmu, int *ids, int *idw, int *idout);
extern SEXP intrf_c__number__lkh_norm (SEXP idy, SEXP idmu, SEXP ids, SEXP idw) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__lkh_norm)(INTEGER(idy), INTEGER(idmu), INTEGER(ids),
				      INTEGER(idw), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}

void F77_NAME(intrf_f__number__ksqexp)(int *idx1, int *idx2, int *ida, int *idb, int *idout);
extern SEXP intrf_c__number__ksqexp (SEXP idx1, SEXP idx2, SEXP ida, SEXP idb) {
  SEXP idout;
  PROTECT(idout=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__number__ksqexp)(INTEGER(idx1), INTEGER(idx2),
				    INTEGER(ida), INTEGER(idb), INTEGER(idout));
  UNPROTECT(1);
  return(idout);
}

/* void F77_NAME(intrf_f__amoeba_opt)(int *g, int *nin, int *xin, int *xout, int *iter, */
/* 				   double *ftol, int *itmax, int *ndim, double *sx); */
/* extern SEXP intrf_c__amoeba_opt (SEXP g, SEXP xin, SEXP xout, SEXP ftol, SEXP itmax, SEXP sx) { */
/*   SEXP iter; */
/*   int nin = length(xin); */
/*   int ndim = length(sx); */
/*   PROTECT(iter=allocVector(INTSXP, 1)); */
/*   F77_CALL(intrf_f__amoeba_opt)(INTEGER(g), &nin, INTEGER(xin), INTEGER(xout), INTEGER(iter), REAL(ftol), */
/* 				INTEGER(itmax), &ndim, REAL(sx)); */
/*   UNPROTECT(1); */
/*   return(iter); */
/* } */
/* void F77_NAME(intrf_f__brent_opt)(int *g, int *xin, int *xout, double *tol, double *lw, double *up, int *dx); */
/* extern SEXP intrf_c__brent_opt (SEXP g, SEXP xin, SEXP xout, SEXP tol, SEXP lw, SEXP up, SEXP dx) { */
/*   SEXP ans; */
/*   PROTECT(ans=allocVector(NILSXP, 1)); */
/*   F77_CALL(intrf_f__brent_opt)(INTEGER(g), INTEGER(xin), INTEGER(xout), REAL(tol), REAL(lw), REAL(up), INTEGER(dx)); */
/*   UNPROTECT(1); */
/*   return(ans); */
/* } */
/* void F77_NAME(intrf_f__frprmn_opt)(int *g, int *nin, int *xin, int *xout, int *iter, */
/* 				   double *ftol, int *itmax, int *dx, int *ng); */
/* extern SEXP intrf_c__frprmn_opt (SEXP g, SEXP xin, SEXP xout, SEXP ftol, SEXP itmax, SEXP dx, SEXP ng) { */
/*   SEXP iter; */
/*   int nin = length(xin); */
/*   PROTECT(iter=allocVector(INTSXP, 1)); */
/*   F77_CALL(intrf_f__frprmn_opt)(INTEGER(g), &nin, INTEGER(xin), INTEGER(xout), INTEGER(iter), */
/* 				REAL(ftol), INTEGER(itmax), INTEGER(dx), INTEGER(ng)); */
/*   UNPROTECT(1); */
/*   return(iter); */
/* } */
/* void F77_NAME(intrf_f__dfpmin_opt)(int *g, int *nin, int *xin, int *xout, int *iter, */
/* 				   double *gtol, int *itmax, int *ng); */
/* extern SEXP intrf_c__dfpmin_opt (SEXP g, SEXP xin, SEXP xout, SEXP gtol, SEXP itmax, SEXP ng) { */
/*   SEXP iter; */
/*   int nin = length(xin); */
/*   PROTECT(iter=allocVector(INTSXP, 1)); */
/*   F77_CALL(intrf_f__dfpmin_opt)(INTEGER(g), &nin, INTEGER(xin), INTEGER(xout), INTEGER(iter), */
/* 				REAL(gtol), INTEGER(itmax), INTEGER(ng)); */
/*   UNPROTECT(1); */
/*   return(iter); */
/* } */
/* void F77_NAME(intrf_f__sgd_opt)(int *g, int *nin, int *xin, int *xout, double *lr, int *iter, */
/* 				int *epchmax, int *itmax, int *gti, int *xtout); */
/* extern SEXP intrf_c__sgd_opt (SEXP g, SEXP xin, SEXP xout, SEXP lr, SEXP epchmax, SEXP itmax, */
/* 			      SEXP gti, SEXP xtout) { */
/*   SEXP iter; */
/*   int nin = length(xin); */
/*   PROTECT(iter=allocVector(INTSXP, 1)); */
/*   F77_CALL(intrf_f__sgd_opt)(INTEGER(g), &nin, INTEGER(xin), INTEGER(xout), REAL(lr), INTEGER(iter), */
/* 			     INTEGER(epchmax), INTEGER(itmax), INTEGER(gti), INTEGER(xtout)); */
/*   UNPROTECT(1); */
/*   return(iter); */
/* } */

/* void F77_NAME(intrf_f__sgdwm_opt)(int *g, int *nin, int *xin, int *xout, double *lr, double *alpha, */
/* 				  int *iter, int *epchmax, int *itmax, int *gti, int * xtout); */
/* extern SEXP intrf_c__sgdwm_opt (SEXP g, SEXP xin, SEXP xout, SEXP lr, SEXP alpha, */
/* 				SEXP epchmax, SEXP itmax, SEXP gti, SEXP xtout) { */
/*   SEXP iter; */
/*   int nin = length(xin); */
/*   PROTECT(iter=allocVector(INTSXP, 1)); */
/*   F77_CALL(intrf_f__sgdwm_opt)(INTEGER(g), &nin, INTEGER(xin), INTEGER(xout), REAL(lr), REAL(alpha), */
/* 			       INTEGER(iter), INTEGER(epchmax), INTEGER(itmax), */
/* 			       INTEGER(gti), INTEGER(xtout)); */
/*   UNPROTECT(1); */
/*   return(iter); */
/* } */
/* void F77_NAME(intrf_f__adam_opt)(int *g, int *nin, int *xin, int *xout, double *lr, double *beta1, */
/* 				 double *beta2, int *iter, int *epchmax, int *itmax, int *gti, int *xtout); */
/* extern SEXP intrf_c__adam_opt (SEXP g, SEXP xin, SEXP xout, SEXP lr, SEXP beta1, SEXP beta2, */
/* 			       SEXP epchmax, SEXP itmax, SEXP gti, SEXP xtout) { */
/*   SEXP iter; */
/*   int nin = length(xin); */
/*   PROTECT(iter=allocVector(INTSXP, 1)); */
/*   F77_CALL(intrf_f__adam_opt)(INTEGER(g), &nin, INTEGER(xin), INTEGER(xout), REAL(lr), REAL(beta1), */
/* 			      REAL(beta1), INTEGER(iter), INTEGER(epchmax), INTEGER(itmax), */
/* 			      INTEGER(gti), INTEGER(xtout)); */
/*   UNPROTECT(1); */
/*   return(iter); */
/* } */

void F77_NAME(intrf_f__allocate_gopts)(int *n);
extern SEXP intrf_c__allocate_gopts (SEXP n) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__allocate_gopts)(INTEGER(n));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__deallocate_gopts)();
extern SEXP intrf_c__deallocate_gopts () {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__deallocate_gopts)();
  UNPROTECT(1);
  return(ans);
} 
void F77_NAME(intrf_f__gopt__pop)(int *i);
extern SEXP intrf_c__gopt__pop (SEXP i) {
  SEXP ans;
  PROTECT(ans = allocVector(NILSXP, 1));
  F77_CALL(intrf_f__gopt__pop)(INTEGER(i));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__sgd__append)(int *i, int *nin, int *xin);
extern SEXP intrf_c__sgd__append (SEXP xin) {
  SEXP ans;
  int nin = length(xin);
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__sgd__append)(INTEGER(ans), &nin, INTEGER(xin));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__sgd__step)(int *xoi, int *gi, int *xout, double *lr, int *niter);
extern SEXP intrf_c__sgd__step (SEXP xoi, SEXP gi, SEXP xout, SEXP lr, SEXP niter) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__sgd__step)(INTEGER(xoi), INTEGER(gi), INTEGER(xout), REAL(lr), INTEGER(niter));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__sgdwm__append)(int *i, int *nin, int *xin);
extern SEXP intrf_c__sgdwm__append (SEXP xin) {
  SEXP ans;
  int nin = length(xin);
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__sgdwm__append)(INTEGER(ans), &nin, INTEGER(xin));
  UNPROTECT(1);
  return(ans);
} 
void F77_NAME(intrf_f__sgdwm__step)(int *xoi, int *gi, int *xout, double *lr, double *alpha, int *niter);
extern SEXP intrf_c__sgdwm__step (SEXP xoi, SEXP gi, SEXP xout, SEXP lr, SEXP alpha, SEXP niter) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__sgdwm__step)(INTEGER(xoi), INTEGER(gi), INTEGER(xout), REAL(lr), REAL(alpha), INTEGER(niter));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__adam__append)(int *i, int *nin, int *xin);
extern SEXP intrf_c__adam__append (SEXP xin) {
  SEXP ans;
  int nin = length(xin);
  PROTECT(ans=allocVector(INTSXP, 1));
  F77_CALL(intrf_f__adam__append)(INTEGER(ans), &nin, INTEGER(xin));
  UNPROTECT(1);
  return(ans);
}
void F77_NAME(intrf_f__adam__step)(int *xoi, int *gi, int *xout, double *lr,
				   double *beta, double *beta2, int *niter);
extern SEXP intrf_c__adam__step (SEXP xoi, SEXP gi, SEXP xout, SEXP lr, SEXP beta1, SEXP beta2, SEXP niter) {
  SEXP ans;
  PROTECT(ans=allocVector(NILSXP, 1));
  F77_CALL(intrf_f__adam__step)(INTEGER(xoi), INTEGER(gi), INTEGER(xout), REAL(lr),
				REAL(beta1), REAL(beta2), INTEGER(niter));
  UNPROTECT(1);
  return(ans);
}
  
static const R_CallMethodDef interfaces[] =
  {
   {"intrf_c__number__rank", (DL_FUNC) &intrf_c__number__rank, 1},
   {"intrf_c__allocate_numbers", (DL_FUNC) &intrf_c__allocate_numbers, 1},
   {"intrf_c__number__is_allocated", (DL_FUNC) &intrf_c__number__is_allocated, 1},
   {"intrf_c__number__has_dx", (DL_FUNC) &intrf_c__number__has_dx, 1},
   {"intrf_c__deallocate_numbers", (DL_FUNC) &intrf_c__deallocate_numbers, 0},
   {"intrf_c__number__append", (DL_FUNC) &intrf_c__number__append, 2},
   {"intrf_c__number__size", (DL_FUNC) &intrf_c__number__size, 1},
   {"intrf_c__number__shape", (DL_FUNC) &intrf_c__number__shape, 1},
   {"intrf_c__number__set_v", (DL_FUNC) &intrf_c__number__set_v, 2},
   {"intrf_c__number__set_dv", (DL_FUNC) &intrf_c__number__set_dv, 2},
   {"intrf_c__number__set_slice_v", (DL_FUNC) &intrf_c__number__set_slice_v, 3},
   {"intrf_c__number__set_slice_dv", (DL_FUNC) &intrf_c__number__set_slice_dv, 3},
   {"intrf_c__number__set_flat_slice_v", (DL_FUNC) &intrf_c__number__set_flat_slice_v, 3},
   {"intrf_c__number__set_flat_slice_dv", (DL_FUNC) &intrf_c__number__set_flat_slice_dv, 3},
   {"intrf_c__number__get_v", (DL_FUNC) &intrf_c__number__get_v, 1},
   {"intrf_c__number__get_dv", (DL_FUNC) &intrf_c__number__get_dv, 1},
   {"intrf_c__number__inlock_free", (DL_FUNC) &intrf_c__number__inlock_free, 1},
   {"intrf_c__number__pop", (DL_FUNC) &intrf_c__number__pop, 1},
   {"intrf_c__number__gc", (DL_FUNC) &intrf_c__number__gc, 0},
   {"intrf_c__nodes__allocate", (DL_FUNC) &intrf_c__nodes__allocate, 1},
   {"intrf_c__deallocate_nodes", (DL_FUNC) &intrf_c__deallocate_nodes, 0},
   {"intrf_c__graphs__allocate", (DL_FUNC) &intrf_c__graphs__allocate, 1},
   {"intrf_c__graph__is_allocated", (DL_FUNC) &intrf_c__graph__is_allocated, 1},
   {"intrf_c__deallocate_graphs", (DL_FUNC) &intrf_c__deallocate_graphs, 0},
   {"intrf_c__graph__open", (DL_FUNC) &intrf_c__graph__open, 1},
   {"intrf_c__graph__close", (DL_FUNC) &intrf_c__graph__close, 0},
   {"intrf_c__ggg", (DL_FUNC) &intrf_c__ggg, 1},
   {"intrf_c__get_graphi", (DL_FUNC) &intrf_c__get_graphi, 0},
   {"intrf_c__graph__pop", (DL_FUNC) &intrf_c__graph__pop, 1},
   {"intrf_c__graph__gc", (DL_FUNC) &intrf_c__graph__gc, 0},
   {"intrf_c__number__op", (DL_FUNC) &intrf_c__number__op, 1},
   //{"intrf_c__number__fw", (DL_FUNC) &intrf_c__number__fw, 1},
   {"intrf_c__number__bw_zero", (DL_FUNC) &intrf_c__number__bw_zero, 1},
   {"intrf_c__number__bw", (DL_FUNC) &intrf_c__number__bw, 1},
   {"intrf_c__graph__op", (DL_FUNC) &intrf_c__graph__op, 1},
   //{"intrf_c__graph__fw", (DL_FUNC) &intrf_c__graph__fw, 1},
   {"intrf_c__graph__bw_zero", (DL_FUNC) &intrf_c__graph__bw_zero, 1},
   {"intrf_c__graph__bw", (DL_FUNC) &intrf_c__graph__bw, 1},
   {"intrf_c__number__slice", (DL_FUNC) &intrf_c__number__slice, 2},
   {"intrf_c__number__flat_slice", (DL_FUNC) &intrf_c__number__flat_slice, 2},
   {"intrf_c__number__contiguous_slice", (DL_FUNC) &intrf_c__number__contiguous_slice, 3},
   {"intrf_c__number__reshape", (DL_FUNC) &intrf_c__number__reshape, 2},
   {"intrf_c__number__drop_shape", (DL_FUNC) &intrf_c__number__drop_shape, 1},
   {"intrf_c__number__bind", (DL_FUNC) &intrf_c__number__bind, 3},
   {"intrf_c__number__embeddings", (DL_FUNC) &intrf_c__number__embeddings, 3},
   {"intrf_c__number__add", (DL_FUNC) &intrf_c__number__add, 2},
   {"intrf_c__number__sub", (DL_FUNC) &intrf_c__number__sub, 2},
   {"intrf_c__number__mult", (DL_FUNC) &intrf_c__number__mult, 2},
   {"intrf_c__number__pow", (DL_FUNC) &intrf_c__number__pow, 2},
   {"intrf_c__number__div", (DL_FUNC) &intrf_c__number__div, 2},
   {"intrf_c__number__bin_entropy", (DL_FUNC) &intrf_c__number__bin_entropy, 3},
   {"intrf_c__number__cross_entropy", (DL_FUNC) &intrf_c__number__cross_entropy, 3},
   {"intrf_c__number__mse", (DL_FUNC) &intrf_c__number__mse, 2},
   {"intrf_c__number__mae", (DL_FUNC) &intrf_c__number__mae, 2},
   {"intrf_c__number__abs", (DL_FUNC) &intrf_c__number__abs, 1},
   {"intrf_c__number__exp", (DL_FUNC) &intrf_c__number__exp, 1},
   {"intrf_c__number__log", (DL_FUNC) &intrf_c__number__log, 1},
   {"intrf_c__number__sin", (DL_FUNC) &intrf_c__number__sin, 1},
   {"intrf_c__number__cos", (DL_FUNC) &intrf_c__number__cos, 1},
   {"intrf_c__number__tan", (DL_FUNC) &intrf_c__number__tan, 1},
   {"intrf_c__number__sinh", (DL_FUNC) &intrf_c__number__sinh, 1},
   {"intrf_c__number__cosh", (DL_FUNC) &intrf_c__number__cosh, 1},
   {"intrf_c__number__tanh", (DL_FUNC) &intrf_c__number__tanh, 1},
   {"intrf_c__number__sigmoid", (DL_FUNC) &intrf_c__number__sigmoid, 1},
   {"intrf_c__number__relu", (DL_FUNC) &intrf_c__number__relu, 1},
   {"intrf_c__number__swish", (DL_FUNC) &intrf_c__number__swish, 1},
   {"intrf_c__number__elu", (DL_FUNC) &intrf_c__number__elu, 1},
   {"intrf_c__number__softmax", (DL_FUNC) &intrf_c__number__softmax, 2},
   {"intrf_c__number__dgemm0", (DL_FUNC) &intrf_c__number__dgemm0, 7},
   {"intrf_c__number__dgemm1", (DL_FUNC) &intrf_c__number__dgemm1, 5},
   {"intrf_c__number__dgemm15", (DL_FUNC) &intrf_c__number__dgemm15, 5},
   {"intrf_c__number__dgemm2", (DL_FUNC) &intrf_c__number__dgemm2, 4},
   {"intrf_c__number__dp_gemv__1", (DL_FUNC) &intrf_c__number__dp_gemv__1, 6},
   {"intrf_c__number__dp_gemv__2", (DL_FUNC) &intrf_c__number__dp_gemv__2, 4},
   {"intrf_c__number__dp_gemv__3", (DL_FUNC) &intrf_c__number__dp_gemv__3, 4},
   {"intrf_c__number__dp_gemv__4", (DL_FUNC) &intrf_c__number__dp_gemv__4, 3},
   {"intrf_c__number__dp_ger__1", (DL_FUNC) &intrf_c__number__dp_ger__1, 4},
   {"intrf_c__number__dp_ger__2", (DL_FUNC) &intrf_c__number__dp_ger__2, 3},
   {"intrf_c__number__dp_ger__3", (DL_FUNC) &intrf_c__number__dp_ger__3, 2},
   {"intrf_c__number__dp_dot", (DL_FUNC) &intrf_c__number__dp_dot, 2},
   {"intrf_c__number__invMat", (DL_FUNC) &intrf_c__number__invMat, 1},
   {"intrf_c__number__sum", (DL_FUNC) &intrf_c__number__sum, 2},
   {"intrf_c__number__ssq", (DL_FUNC) &intrf_c__number__ssq, 1},
   {"intrf_c__number__ldexp", (DL_FUNC) &intrf_c__number__ldexp, 2},
   {"intrf_c__number__ldlaplace", (DL_FUNC) &intrf_c__number__ldlaplace, 3},
   {"intrf_c__number__ldbeta", (DL_FUNC) &intrf_c__number__ldbeta, 3},
   {"intrf_c__number__ldgamma", (DL_FUNC) &intrf_c__number__ldgamma, 3},
   {"intrf_c__number__ldnorm", (DL_FUNC) &intrf_c__number__ldnorm, 3},
   {"intrf_c__number__ldmvnorm__1", (DL_FUNC) &intrf_c__number__ldmvnorm__1, 3},
   {"intrf_c__number__lkh_norm", (DL_FUNC) &intrf_c__number__lkh_norm, 4},
   //{"intrf_c__number__ksqexp", (DL_FUNC) &intrf_c__number__ksqexp, 4},
   {"intrf_c__allocate_gopts", (DL_FUNC) &intrf_c__allocate_gopts, 1},
   {"intrf_c__deallocate_gopts", (DL_FUNC) &intrf_c__deallocate_gopts, 0},
   {"intrf_c__gopt__pop", (DL_FUNC) &intrf_c__gopt__pop, 1},
   {"intrf_c__sgd__append", (DL_FUNC) &intrf_c__sgd__append, 1},
   {"intrf_c__sgd__step", (DL_FUNC) &intrf_c__sgd__step, 5},
   {"intrf_c__sgdwm__append", (DL_FUNC) &intrf_c__sgdwm__append, 1},
   {"intrf_c__sgdwm__step", (DL_FUNC) &intrf_c__sgdwm__step, 6},
   {"intrf_c__adam__append", (DL_FUNC) &intrf_c__adam__append, 1},
   {"intrf_c__adam__step", (DL_FUNC) &intrf_c__adam__step, 7},
   {NULL, NULL, 0}
  };

void R_init_modello (DllInfo *info) {
  R_registerRoutines(info, NULL, interfaces, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
