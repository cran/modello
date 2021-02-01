!  This file is part of Modello.
!
!  Modello is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 2 of the License, or
!  (at your option) any later version.
!  
!  Modello is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
!  MA 02110-1301, USA.

module math

  use omp_lib
  use env
  use errwarn
  use utils

  implicit none

  character(len=*), parameter :: mod_math_name_ = 'math'

  interface
     pure subroutine DGETRF(M, N, A, LDA, IPIV, INFO)
       use env, only: dp_
       implicit none
       integer, intent(in) :: M, N, LDA
       real(kind=dp_), intent(inout) :: A(LDA,*)
       integer, intent(out) :: IPIV(*), INFO
     end subroutine DGETRF
     pure subroutine DGETRI (N, A, LDA, IPIV, WORK, LWORK, INFO)
       use env, only: dp_
       implicit none
       integer, intent(in) :: N, LDA, LWORK, IPIV(*)
       real(kind=dp_), intent(inout) :: A(LDA,*)
       real(kind=dp_), intent(out) :: work(*)
       integer, intent(out) :: INFO
     end subroutine DGETRI
     pure subroutine DGESV (N, NRHS, A, LDA, IPIV, B, LDB, INFO)
       use env, only: dp_
       implicit none
       integer, intent(in) :: N, NRHS, LDA, LDB
       real(kind=dp_), intent(inout) :: A(LDA,*), B(LDB,*)
       integer, intent(out) :: IPIV(*), INFO
     end subroutine DGESV
     pure subroutine DPOTRF (UPLO, N, A, LDA, INFO)
       use env, only: dp_
       implicit none
       character, intent(in) :: UPLO(*)
       integer, intent(in) :: N, LDA
       real(kind=dp_), intent(inout) :: A(LDA,*)
       integer, intent(out) :: INFO
     end subroutine DPOTRF
     pure subroutine DPOTRI (UPLO, N, A, LDA, INFO)
       use env, only: dp_
       implicit none
       character, intent(in) :: UPLO
       integer, intent(in) :: N, LDA
       real(kind=dp_), intent(inout) :: A(LDA,*)
       integer, intent(out) :: INFO
     end subroutine DPOTRI
     pure subroutine DPOSV (UPLO, N, NRHS, A, LDA, B, LDB, INFO)
       use env, only: dp_
       implicit none
       character(len = 1), intent(in) :: UPLO
       integer, intent(in) :: N, NRHS, LDA, LDB
       integer, intent(out) :: INFO
       real(kind=dp_), intent(inout) :: A(LDA,N), B(LDB,NRHS)
     end subroutine DPOSV
     pure subroutine DGEMM (TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
       use env, only: dp_
       implicit none
       character, intent(in) :: TRANSA, TRANSB
       integer, intent(in) :: M, N, K, LDA, LDB, LDC
       real(kind=dp_), intent(in) :: A(LDA,*), B(LDB,*), ALPHA, BETA
       real(kind=dp_), intent(inout) :: C(LDC,*)
     end subroutine DGEMM
     pure subroutine DSYMM(SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
       use env, only: dp_
       implicit none
       character, intent(in) :: SIDE, UPLO
       integer, intent(in) :: M, N, LDA, LDB, LDC
       real(kind=dp_), intent(in) :: ALPHA, A(LDA,*), B(LDB,N), BETA, C(LDC,N)
     end subroutine DSYMM
     pure subroutine DGEMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY)
       use env, only: dp_
       implicit none
       character, intent(in) :: TRANS
       integer, intent(in) :: M, N, LDA, INCX, INCY
       real(kind=dp_), intent(in):: ALPHA, A(LDA,N), X(*), BETA
       real(kind=dp_), intent(inout) :: Y(*)
     end subroutine DGEMV
     pure subroutine DGER (M, N, alpha, X, INCX, Y, INCY, A, LDA)
       use env, only: dp_
       implicit none
       integer, intent(in) :: M, N, INCX, INCY, LDA
       real(kind=dp_), intent(in) :: alpha, X(*), Y(*)
       real(kind=dp_), intent(inout) :: A(LDA,*)
     end subroutine DGER
     pure function DDOT (N, DX, INCX, DY, INCY)
       use env, only: dp_
       integer, intent(in) :: N, INCX, INCY
       real(kind=dp_), intent(in) :: DX(N), DY(N)
       real(kind=dp_) :: DDOT
     end function DDOT
  end interface

  interface
     real(c_double) function r__beta (a, b) bind(c)
       use iso_c_binding
       real(c_double) :: a, b
     end function r__beta
     real(c_double) function r__lbeta (a, b) bind(c)
       use iso_c_binding
       real(c_double) :: a, b
     end function r__lbeta
     real(c_double) function r__gamma (a) bind(c)
       use iso_c_binding
       real(c_double) :: a
     end function r__gamma
     real(c_double) function r__lgamma (a) bind(c)
       use iso_c_binding
       real(c_double) :: a
     end function r__lgamma
  end interface

  !* PROBABILITY DISTRIBUTIONS FROM R (see file useR.c)

  !** UNIFORM DISTRIBUTION

  interface
     real(c_double) function r__runif (a, b) bind(c)
       use iso_c_binding
       real(c_double) :: a, b
     end function r__runif
     real(c_double) function r__dunif (x, a, b, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, a, b
       integer(c_int) :: ln
     end function r__dunif
     real(c_double) function r__punif (x, a, b, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, a, b
       integer(c_int) :: lwt, ln
     end function r__punif
     real(c_double) function r__qunif (x, a, b, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, a, b
       integer(c_int) :: lwt, ln
     end function r__qunif
  end interface

  !** BETA DISTRIBUTION

  interface
     real (c_double) function r__rbeta (shp1, shp2) bind(c)
       use iso_c_binding
       real(c_double) :: shp1, shp2
     end function r__rbeta
     real(c_double) function r__dbeta (x, shp1, shp2, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, shp1, shp2
       integer(c_int) :: ln
     end function r__dbeta
     real(c_double) function r__pbeta (x, shp1, shp2, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, shp1, shp2
       integer(c_int) :: lwt, ln
     end function r__pbeta
     real(c_double) function r__qbeta (x, shp1, shp2, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, shp1, shp2
       integer(c_int) :: lwt, ln
     end function r__qbeta
  end interface

  !** GAMMA DISTRIBUTION

  interface
     real(c_double) function r__rgamma (shp, rt) bind(c)
       use iso_c_binding
       real(c_double) :: shp, rt
     end function r__rgamma
     real(c_double) function r__dgamma (x, shp, rt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, shp, rt
       integer(c_int) :: ln
     end function r__dgamma
     real(c_double) function r__pgamma (x, shp, rt, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, shp, rt
       integer(c_int) :: lwt, ln
     end function r__pgamma
     real(c_double) function r__qgamma (x, shp, rt, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, shp, rt
       integer(c_int) :: lwt, ln
     end function r__qgamma
  end interface

  !** NORNAL DISTRIBUTION

  interface
     real(c_double) function r__rnorm (mu, sigma) bind(c)
       use iso_c_binding
       real(c_double) :: mu, sigma
     end function r__rnorm
     real(c_double) function r__dnorm (x, mu, sigma, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, mu, sigma
       integer(c_int) :: ln
     end function r__dnorm
     real(c_double) function r__pnorm (x, mu, sigma, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, mu, sigma
       integer(c_int) :: lwt, ln
     end function r__pnorm
     real(c_double) function r__qnorm (x, mu, sigma, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, mu, sigma
       integer(c_int) :: lwt, ln
     end function r__qnorm
  end interface

  !** EXPONENTIAL DISTRIBUTION

  interface
     real(c_double) function r__rexp (rt) bind(c)
       use iso_c_binding
       real(c_double) :: rt
     end function r__rexp
     real(c_double) function r__dexp (x, rt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, rt
       integer(c_int) :: ln
     end function r__dexp
     real(c_double) function r__pexp (x, rt, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, rt
       integer(c_int) :: lwt, ln
     end function r__pexp
     real(c_double) function r__qexp (x, rt, lwt, ln) bind(c)
       use iso_c_binding
       real(c_double) :: x, rt
       integer(c_int) :: lwt, ln
     end function r__qexp
  end interface
 
  !> Sum of squares
  !! @author Filippo Monari
  !! @param[in] x double precision array with up to two dimensions
  !! @param[in] w double precision vector with weights
  !! @param[in] k integer, dimension to be wieghted
  interface SSQ
     module procedure SSQ__1
     !module procedure wssq__1
     module procedure SSQ__2
     !module procedure wssq__2
  end interface SSQ

  !> L2-Norm
  !! @author Filippo Monari
  !! @param[in] x double precision, rank 1 or 2 array
  interface L2NORM
     module procedure L2NORM__1
     module procedure L2NORM__2
  end interface L2NORM

  !> Product derivative
  !! @param[in] x input array (rank 1 or 2)
  interface dx_product
     module procedure dx_product__1
     module procedure dx_product__2
  end interface
  
  !> Normal likelihood
  interface ldnorm
     module procedure ldnorm__1
     module procedure ldnorm__2
  end interface ldnorm

  !> Normal Likelihood - Derivative
  !! w.r.t. s (standard deviation)
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  !! @param[in] w double precision, weights
  interface dx_ldnorm_s
     module procedure dx_ldnorm_s__1
     module procedure dx_ldnorm_s__2
  end interface dx_ldnorm_s

  !> Normal Likelihood - Derivative
  !! w.r.t. mu (mean)
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  !! @param[in] w double precision, weights
  interface dx_ldnorm_mu
     module procedure dx_ldnorm_mu__1
     module procedure dx_ldnorm_mu__2
  end interface dx_ldnorm_mu

  !> Squared Exponential Kernel
  !! @param[in] x1,x2 input vectors
  !! @param[in] a,b kernel parameters
  interface ksqexp
     module procedure ksqexp__1
     module procedure ksqexp__2
  end interface ksqexp

  !> Suqared Exponential Kernel - Derivative
  !! w.r.t. the a paramter.
  !! @param[in] x1,x2 input vectors
  !! @param[in] a,b kernel parameters
  interface dx_ksqexp_a
     module procedure dx_ksqexp_a__1
     module procedure dx_ksqexp_a__2
  end interface dx_ksqexp_a

  !> Suqared Exponential Kernel - Derivative
  !! w.r.t. the b paramter.
  !! @param[in] x1,x2 input vectors
  !! @param[in] a,b kernel parameters
  interface dx_ksqexp_b
     module procedure dx_ksqexp_b__1
     module procedure dx_ksqexp_b__2
  end interface dx_ksqexp_b

  !> Suqared Exponential Kernel - Derivative
  !! w.r.t. the x1 input vector.
  !! @param[in] x1,x2 input vectors
  !! @param[in] a,b kernel parameters
  interface dx_ksqexp_x1
     module procedure dx_ksqexp_x1__1
     module procedure dx_ksqexp_x1__2
  end interface dx_ksqexp_x1

  !> Suqared Exponential Kernel - Derivative
  !! w.r.t. the x2 input vector.
  !! @param[in] x1,x2 input vectors
  !! @param[in] a,b kernel parameters
  interface dx_ksqexp_x2
     module procedure dx_ksqexp_x2__1
     module procedure dx_ksqexp_x2__2
  end interface dx_ksqexp_x2  
     
contains

  !> Creates an identity matrix of shape [n,n]
  !! @author Filippo Monari
  !! @param[in] n integer 
  function IM (n)
    implicit none
    integer, intent(in) :: n
    double precision :: im(n,n)
    integer :: i
    !$omp parallel
    !$omp workshare
    im = 0._dp_
    !$omp end workshare
    !$omp do
    do i = 1, n
       im(i,i) = 1._dp_
    end do
    !$omp end do
    !$omp end parallel
  end function IM

  !> Kronecker Delta.
  !! i == j -> 1; i /= j -> 0
  !! @author Filippo Monari
  !! @param[in] i,j integer, indexes
  elemental function deltaij (i, j) result(k)
    implicit none
    integer, intent(in) :: i, j
    real(kind=dp_) :: k
    if (i == j) then
       k = 1._dp_
    else
       k = 0._dp_
    end if
  end function deltaij

  !> Digamma function
  !! Derivative of the log-gamma function
  !! @param[in] x double precision
  !! @reference
  !! Jose Bernardo, Algorithm AS 103: Psi ( Digamma ) Function,
  !! Applied Statistics, Volume 25, Number 3, 1976, pages 315-317.
  elemental function psi (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans, xx, r
    real(kind=dp_), parameter :: c = 8.5_dp_ 
    ! Approximate for smal x
    if (x < tiny_dp_) then
       ans = - euler_mascheroni_dp_ - x**(-1) + ReimannAt2_dp_ * x
       return
    end if
    ! Reduce to psi(X + N)
    xx = x
    ans = 0
    do while (xx < c)
       ans = ans - 1._dp_ / xx
       xx = xx + 1._dp_
    end do
    ! Use Stirling's (actually de Moivre's) expansion.
    r = 1._dp_ / xx
    ans = ans + log(xx) - 0.5_dp_ * r
    r = r**2
    ans = ans &
         - r * (12._dp_**(-1) &
         - r * (120._dp_**(-1) &
         - r * (252._dp_**(-1) &
         - r * (240._dp_**(-1) &
         - r * (132._dp_**(-1))))))
  end function psi

  !> log-beta function
  !! @param[in] a,b double precision
  elemental function log_beta (a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: a, b
    real(kind=dp_) :: ans
    ans = log_gamma(a) + log_gamma(b) - log_gamma(a + b)
  end function log_beta

  !> Calculates the derivative of the log-beta function
  !! w.r.t. a
  !! @param[in] a,b double precision
  elemental function dx_log_beta_a (a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: a, b
    real(kind=dp_) :: ans
    ans = psi(a) - psi(a + b)
  end function dx_log_beta_a

  !> calculates the derivative of the log-beta function
  !! w.r.t. b 
  !! @param[in] a,b double precision
  elemental function dx_log_beta_b (a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: a, b
    real(kind=dp_) :: ans
    !ans = (log_beta(a, b + tol_dp_) - log_beta(a, b - tol_dp_)) / (2 * tol_dp_)
    ans = psi(b) - psi(a + b)
  end function dx_log_beta_b
  
  !> Calculates the outer product of two vectors
  !! @author Filippo Monari
  !! @param[in] x1,x2 double precision vectors
  function OUTERPROD (x1, x2) result(ans)
    implicit none
    double precision, intent(in) :: x1(:), x2(:)
    double precision :: ans(size(x1),size(x2))
    integer :: i, j
    if (size(x1) >= size(x2)) then
       call outer_do
    else
       call inner_do
    end if
  contains
    subroutine outer_do
      !$omp parallel do
      do i = 1, size(x1)
         do j = 1, size(x2)
            ans(i,j) = x1(i) * x2(j)
         end do
      end do
      !$omp end parallel do
    end subroutine outer_do
    subroutine inner_do
      !$omp parallel do
      do j = 1, size(x2)
         do i = 1, size(x1)
            ans(i,j) = x1(i) * x2(j)
         end do
      end do
      !$omp end parallel do
    end subroutine inner_do
  end function OUTERPROD
  
  !> Calculates the trace of a square matrix
  !! @author Filippo Monari
  !! @param[in] A real(:,:)
  !! @todo move to impure math
  function TRACE (A) result(ans)
    implicit none
    real(kind=dp_), intent (in) :: A(:,:)
    real(kind=dp_) :: ans
    integer :: i
    ans = 0
    !$omp parallel do
    do i = 1, size(A, 1)
       ans = ans + A(i,i)
    end do
    !$omp end parallel do
  end function TRACE
  
  !> Sums the log of the diagonal elements of a matrix
  !! @author Filippo Monari
  !! @param[in] A double precision matrix
  !! @todo move to impure math
  function LOGTRACE (A) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: A(:,:)
    real(kind=dp_) :: ans
    integer :: i
    ans = 0
    !$omp parallel do
    do i = 1, size(A, 1)
       ans = ans + log(A(i,i))
    end do
    !$omp end parallel do
  end function LOGTRACE

  !> @defgroup math_intrnsic_dx_ Derivative of Intrinsic Functions
  !! @{
  
  !> Derivative of x^n
  !! @param[in] x double precision scalar or array
  !! @param[in] n double precision scalar or array with same shape of 'x'
  elemental double precision function dx_xpow (x, n)
    implicit none
    double precision, intent(in) :: x, n
    dx_xpow = n * x**(n-1)
  end function dx_xpow
  
  !> Derivative of a^x
  !! @param[in] a double precision scalar or array
  !! @param[in] x double precision scalar or array with same shape as 'a'
  elemental double precision function dx_powx (a, x)
    implicit none
    double precision, intent(in) :: a, x
    dx_powx = a**x * log(a)
  end function dx_powx

  !> Derivtive of abs(x)
  !! @param[in] x double precision scalar or array
  elemental function dx_abs (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans
    ans = sign(1._dp_, x)
  end function dx_abs
 
  !> Derivative of exp(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_exp (x)
    implicit none
    double precision, intent(in) :: x
    dx_exp = exp(x)
  end function dx_exp

  !> Derivative of log(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_log (x)
    implicit none
    double precision, intent(in) :: x
    dx_log = 1/x
  end function dx_log

  !> Derivative of sin(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_sin (x)
    implicit none
    double precision, intent(in) :: x
    dx_sin = cos(x)
  end function dx_sin

  !> Derivative of cos(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_cos (x)
    implicit none
    double precision, intent(in) :: x
    dx_cos = -sin(x)
  end function dx_cos

  !> Derivative of tan(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_tan (x)
    implicit none
    double precision, intent(in) :: x
    dx_tan = (1/cos(x))**2
  end function dx_tan

  !> Derivative of sinh(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_sinh (x)
    implicit none
    double precision, intent(in) :: x
    dx_sinh = cosh(x)
  end function dx_sinh

  !> Derivative of cosh(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_cosh (x)
    implicit none
    double precision, intent(in) :: x
    dx_cosh = sinh(x)
  end function dx_cosh

  !> Derivative of tanh(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_tanh (x)
    implicit none
    double precision, intent(in) :: x
    dx_tanh = (1/cosh(x))**2
  end function dx_tanh
  !> @}

  !> @defgroup math_solve_ Matrix Inversion and System of Linear Equations
  !! @{


  subroutine dgetrf_det (A, ipv, D)
    real(kind=dp_), intent(in) :: A(:,:)
    integer :: ipv(:)
    real(kind=dp_), intent(out) :: D
    integer :: i
    D = 1
    !$omp parallel do
    do i = 1, size(A, 1)
       D = D * A(i,i) * (-1)**ipv(i)
    end do
    !$omp end parallel do
  end subroutine dgetrf_det
  
  !> Inverts the given generic matrix.
  !! @author Filippo Monari
  !! @param[inout] A double precsion matrix to invert
  !! @param[out] D double precision scalar, optional, determinat of the matrix
  !! @todo implement determinant calculation
  subroutine INVMAT (A, info, D)
    implicit none
    real(kind=dp_), intent(inout) :: A(:,:)
    integer, intent(out) :: info
    real(kind=dp_), intent(out), optional :: D
    integer :: m, n, lda
    integer :: ipv(minval(shape(A)))
    real(kind=dp_) :: lwork(1)
    real(kind=dp_), allocatable :: work(:)
    info = 0
    m = size(A, 1)
    n = size(A, 2)
    lda = m
    info = 0
    call DGETRF(m, n, A, lda, ipv, info)
    if (info == 0 .and. present(D)) call dgetrf_det(A, ipv, D)
    if (info == 0) call DGETRI(n, A, lda, ipv, lwork, -1, info)
    if (info == 0) allocate(work(int(lwork(1))), stat=info)
    if (info == 0) call DGETRI(n, A, lda, ipv, work, int(lwork(1)), info)
  end subroutine INVMAT

  !> Forward differentiation for the genral matrix inversion.
  !! @param[in] dA double precision matrix, derivative of the original matrix
  !! @param[in] C double precision matrix, inverse
  !! @param[inout] dC double precision matrix, derivative of the inverse
  subroutine dx_fw_invMat (dA, C, dC)
    implicit none
    real(kind=dp_), intent(in) :: dA(:,:), C(:,:)
    real(kind=dp_), intent(inout) :: dC(:,:)
    real(kind=dp_) :: B(size(dC, 1),size(dA, 2))
    call dp_gemm(0, 0, -1._dp_, C, dA, 0._dp_, B)
    call dp_gemm(0, 0,  1._dp_, B, C, 0._dp_, dC) 
  end subroutine dx_fw_invMat

  !> Bakward differentiation of the general matrix inversion
  !! @param[in] dC double precision matrix, derivative of the inverse
  !! @param[in] C double preicision matrix, inverse
  !! @param[inout] dA double precision matrix, derivative of the orignal matrix
  subroutine dx_bw_invMat (dC, C, dA)
    implicit none
    real(kind=dp_), intent(in) :: dC(:,:), C(:,:)
    real(kind=dp_), intent(inout) :: dA(:,:)
    real(kind=dp_) :: B(size(C, 2),size(dC, 2))
    call dp_gemm(1, 0, -1._dp_, C, dC, 0._dp_, B)
    call dp_gemm(0, 1, 1._dp_, B, C, 0._dp_, dA)
  end subroutine dx_bw_invMat

  !> Inverts a given symmetric and positive defined matrix.
  !! @author Filippo Monari
  !! @param[inout] A double precision matrix to invert
  !! @param[out] D double precision scalar, optional, determinant of the matrix
  subroutine INVSYMMAT (A, info, lnD)
    implicit none
    real(kind=dp_), intent(inout) :: A(:,:)
    integer, intent(out) :: info
    real(kind=dp_), intent(out), optional :: lnD
    integer :: n, lda
    info = 0
    n = size(A, 1)
    lda = max(1, n)
    call DPOTRF('L', n, A, lda, info)
    if (present(lnD)) lnD = 2 * LOGTRACE(A)
    call DPOTRI('L', n, A, lda, info)
    if (info == 0) call private_fill
  contains
    subroutine private_fill
      integer :: i, j
      n = size(A, 1) 
      !$omp parallel do
      do j = 1, n - 1 
         do i = j+1, n
            A(j,i) = A(i,j)
         end do
      end do
      !$omp end parallel do
    end subroutine private_fill
  end subroutine INVSYMMAT

  !> Solves the system of linear equations A.X = B
  !! @author Filippo Monari
  !! @param[inout] A double precision matrix, (in) rhs coefficient matrix, (out) ...
  !! @param[inout] B double precision matrix, (in) lhs matrix, (out) solution of the system
  subroutine SOLVE (A, B, info, D)
    implicit none
    real(kind=dp_), intent(inout) :: A(:,:), B(:,:)
    integer, intent(out) :: info
    real(kind=dp_), intent(out), optional :: D
    integer :: n, lda, nrhs, ldb, ipv(size(A, 2))
    info = 0
    n = size(A, 1)
    lda = max(1, n)
    nrhs = size(B, 2)
    ldb = max(1, n)
    ipv = 0
    call DGESV(n, nrhs, A, lda, ipv, B, ldb, info)
    if (present(D)) call dgetrf_det(A, ipv, D)
  end subroutine SOLVE

  !> Solves the system of linear equations A.x = B, where A is symmetric positive defined matrix
  !! @author Filippo Monari
  !! @param[inout] A double precision matrix, (in) rhs coefficient matrix, (out)
  !! LT matrix from Cholesky decomposition
  !! @param[input] B double precision matrix, (in) lhs matrx, (out) solution of the system
  subroutine SOLVESYM (A, B, info, lnD)
    implicit none
    real(kind=dp_), intent(inout) :: A(:,:), B(:,:)
    integer, intent(out) :: info
    real(kind=dp_), intent(out), optional :: lnD
    integer :: n, lda, nrhs, ldb
    info = 0
    n = size(A, 1)
    lda = max(1, n)
    nrhs = size(B, 2)
    ldb = max(1, n)
    call DPOSV('L', n, nrhs, A, lda, B, ldb, info)
    if (info == 0 .and. present(lnD)) lnD = 2 * LOGTRACE(A)
  end subroutine SOLVESYM

  !> Backward differentiation of the dterminant(A).
  !! @param[in] A double precision(:,:)
  !! @param[in] D double precision, determinant
  !! @param[in] dD double precision, determinant derivative
  function DX_BW_DET (IA, D, dD, sym) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: IA(:,:), D, dD
    real(kind=dp_) :: ans(size(IA,1),size(IA,2))
    logical, intent(in) :: sym
    if (sym) then
       !$omp parallel workshare
       ans = dD * D * IA
       !$omp end parallel workshare
    else
       !$omp parallel workshare
       ans = dD * D * transpose(IA)
       !$omp end parallel workshare
    end if
  end function DX_BW_DET
  !> @}

  !> @defgroup math_mm_mv_ Matrix/Vector Multiplication
  !! @{
  
  !> General matrix multiplication C := alpha * op(A).op(B) + beta * C
  !! @param[in] transA integer, 1 indicates op(A) = A**T, 0 indicates op(A) = A
  !! @param[in] transB integer, 0 indicates op(B) = B**T, 0 indicates op(B) = B
  !! @param[in] alpha double precision scalar
  !! @param[in] A double precision matrix
  !! @param[in] B double precision matrix
  !! @param[in] beta double precision scalar
  !! @param[inout] C double precision matrix, (out) result of the matrix multilication operation
  pure subroutine dp_gemm (transA, transB, alpha, A, B, beta, C)
    implicit none
    integer, intent(in) :: transA 
    integer, intent(in) :: transB 
    double precision, intent(in) :: alpha, A(:,:), B(:,:), beta
    double precision, intent(inout) :: C(:,:)
    integer :: LDA, MC, LDB, N, M, K
    character(len=1) :: tA, tB
    M = merge(size(A, 1), size(A, 2), transA < 1)
    K = merge(size(A, 2), size(A, 1), transA < 1)
    LDA = merge(M, K, transA < 1)
    ta = merge('N', 'T', transA < 1)
    LDB = merge( &
         merge(size(B, 1), size(B, 2), transB < 1), &
         merge(size(B, 2), size(B, 1), transB < 1), &
         transB < 1)
    tB = merge('N', 'T', transB < 1)
    MC = size(C, 1)
    N = size(C, 2)
    call DGEMM(tA, tB, M, N, K, alpha, A, LDA, B, LDB, beta, C, MC)  
  end subroutine dp_gemm

  !> Bakward differentiation of 'dp_gemm' w.r.t. A
  !! Parameters share the name and meaning with 'dp_gemm'
  !! @param[in] dC double precision matrix, derivative of C
  !! @param[inout] dA double precision matrix, derivative of A
  pure subroutine dx_bw_dp_gemm_A (transA, transB, alpha, B, dC, dA)
    implicit none
    integer, intent(in) :: transA, transB
    real(kind=dp_), intent(in) :: alpha, B(:,:), dC(:,:)
    real(kind=dp_), intent(inout) :: dA(:,:)
    if (transA > 0) then
       !A%dx = B x t(C%dx) => leave B as it was in the original operation
       call dp_gemm(transB, 1, alpha, B, dC, 1._dp_, dA)
    else
       !A%dx = C%dx x t(B%dx) => transpose B w.r.t. the original operation
       call dp_gemm(0, merge(0, 1, transB > 0), alpha, dC, B, 1._dp_, dA)
    end if
  end subroutine dx_bw_dp_gemm_A
  
  !> Bakward differentiation of 'dgemm' w.r.t. B
  !! Parameters share the name and meaning with 'dgemm'
  !! @param[in] dC double precision matrix, derivative of C
  !! @param[inout] dB double precision matrix, derivative of B
  pure subroutine dx_bw_dp_gemm_B (transA, transB, alpha, A, dC, dB)
    implicit none
    integer, intent(in) :: transA, transB
    real(kind=dp_), intent(in) :: alpha, A(:,:), dC(:,:)
    real(kind=dp_), intent(inout) :: dB(:,:)
    if (transB > 0) then
       !B%dx = t(C%dx) x A => leave A as it was in the original operation
       call dp_gemm(1, transA, alpha, dC, A, 1._dp_, dB)
    else
       !B%dx = t(A) x C => transpose A w.r.t. the original operation
       call dp_gemm(merge(0, 1, transA > 0), 0, alpha, A, dC, 1._dp_, dB)
    end if
  end subroutine dx_bw_dp_gemm_B
  
  !> Bakward differentiation of dgemm w.r.t. alpha
  !! Parameters share the name and meaning with 'dgemm'
  !! @param[in] dC double precision matrix, derivative of C
  !! @param[inout] dAlpha double precision matrix, derivative of alpha
  subroutine DX_BW_DP_GEMM_ALPHA (transa, transB, A, B, dC, dAlpha)
    implicit none
    integer, intent(in) :: transA, transB
    real(kind=dp_), intent(in) :: A(:,:), B(:,:), dC(:,:)
    real(kind=dp_), intent(inout) :: dAlpha
    real(kind=dp_), allocatable :: X(:,:)
    integer :: m, n
    m = merge(size(A, 2), size(A, 1), transA > 0)
    n = merge(size(B, 1), size(B, 2), transB > 0)
    allocate(X(m,n))
    X = 0
    call dp_gemm(transA, transB, 1._dp_, A, B, 0._dp_, X)
    !$omp parallel workshare
    dAlpha = dAlpha + sum(X * dC)
    !$omp end parallel workshare
  end subroutine DX_BW_DP_GEMM_ALPHA
  
  !> General matrix multiplication for symmetric matrices
  !! Performs one of the matrix-matrix operations:
  !! 1. C := alpha*A*B + beta*C,
  !! or
  !! 2. C := alpha*B*A + beta*C,
  !! where alpha and beta are scalars,  A is a symmetric matrix and  B and
  !! C are m by n matrices.
  !! @param[in] side integer, if > 0 preforms (2) else (1)
  !! @param[in] uplo integer, if > 0 reference to UT of A else reference to LT of A
  !! @param[in] alpha double precision
  !! @param[in] A double precision(:,:), symmetric matrix
  !! @param[in] B double precision(:,:)
  !! @param[in] beta double precision
  !! @param[inout] C double precision(:,:)
  pure subroutine dp_symm (side, uplo, alpha, A, B, beta, C)
    implicit none
    integer, intent(in) :: side
    integer, intent(in) :: uplo
    double precision, intent(in) :: alpha, A(:,:), B(:,:), beta
    double precision, intent(inout) :: C(:,:)
    integer :: M, N, LDA, LDB, LDC
    character(len=1) :: sd, ul
    M = size(C, 1)
    N = size(C, 2)
    LDA = size(A, 1)
    LDB = size(B, 1)
    LDC = size(C, 1)
    sd = merge('R', 'L', side > 0)
    ul = merge('L', 'U', uplo > 0)
    call DSYMM(sd, ul, M, N, alpha, A, LDA, B, LDB, beta, C, LDC)
  end subroutine dp_symm

  !> General matrix vector multiplication y := alpha * op(A).x + beta * y
  !! @param[in] trans integer, 1 indicated op(A) = T(A), 0 indicates op(A) = A
  !! @param[in] alpha dpuble precision scalar
  !! @param[in] A double precision matrix
  !! @param[in] x double precision vector
  !! @param[in] beta double precision scalar
  !! @param[inout] y double precision vector
  pure subroutine dp_gemv (trans, alpha, A, x, beta, y)
    implicit none
    integer, intent(in) :: trans
    real(kind=dp_), intent(in) :: alpha, A(:,:), x(:), beta
    real(kind=dp_), intent(inout) :: y(:)
    character :: ta
    integer :: M, N, LDA
    M = size(A, 1)
    N = size(A, 2) 
    LDA = M
    ta = merge('N', 'T', trans < 1)
    call DGEMV(ta, M, N, alpha, A, LDA, x, 1, beta, y, 1)
  end subroutine dp_gemv

  !> Bakward differentiation of dp_gemv w.r.t. A
  !! Variables share the same meaning as in 'dp_gemv'.
  !! @param[in] dy double precision vector, derivative of y
  !! @param[inout] dA double precision vector derivative of A
  pure subroutine dx_bw_dp_gemv_A (trans, alpha, x, dy, dA)
    implicit none
    integer, intent(in) :: trans
    real(kind=dp_), intent(in) :: alpha, x(:), dy(:)
    real(kind=dp_), intent(inout) :: dA(:,:)
    if (trans > 0) then
       call dp_ger(alpha, x, dy, dA)
    else
       call dp_ger(alpha, dy, x, dA)
    end if
  end subroutine dx_bw_dp_gemv_A

  !> Bakward differentiation of 'dp_gemv' w.r.t. x
  !! Parameters share the same name and meaning as in 'dp_gemv'.
  !! @param[in] dy doauble preciaion vector, derivative of y
  !! @param[in] dx douvle precision vector, derivative of x
  pure subroutine dx_bw_dp_gemv_x (trans, alpha, A, dy, dx)
    implicit none
    integer, intent(in) :: trans
    real(kind=dp_), intent(in) :: alpha, A(:,:), dy(:)
    real(kind=dp_), intent(inout) :: dx(:)
    call dp_gemv(merge(0, 1, trans > 0), alpha, A, dy, 1._dp_, dx)
  end subroutine dx_bw_dp_gemv_x

  !> Bakward differentiation of 'dp_gemv' w.r.t. alpha.
  !! Parametrs share the name and meaning with 'dp_gemv'.
  !! @param[in] dy double precision, derivative of y
  !! @param[inout] dAlpha derivative of alpha
  subroutine DX_BW_DP_GEMV_ALPHA (trans, A, x, dy, dAlpha)
    implicit none
    integer, intent(in) :: trans
    real(kind=dp_), intent(in) :: A(:,:), x(:), dy(:)
    real(kind=dp_), intent(inout) :: dAlpha
    real(kind=dp_), allocatable :: yy(:)
    allocate(yy(size(dy)))
    yy = 0
    call dp_gemv(trans, 1._dp_, A, x, 0._dp_, yy)
    !$omp parallel workshare
    dAlpha = dAlpha + sum(yy * dy)
    !$omp end parallel workshare
  end subroutine DX_BW_DP_GEMV_ALPHA

  !> General outer product of two vectors: A := alpha * x.y**T + A
  !! @param[in] alpha double precision scalar
  !! @param[in] x double precision vector
  !! @param[in] y double precision vector
  !! @param[in] A double precision matrix
  pure subroutine dp_ger (alpha, x, y, A)
    implicit none
    real(kind=dp_), intent(in) :: alpha, x(:), y(:)
    real(kind=dp_), intent(inout) :: A(:,:)
    call DGER(size(x), size(y), alpha, x, 1, y, 1, A, size(x))
  end subroutine dp_ger

  !> Inner product of two vectors: x**T.y
  !! @param[in] x doauble precision vector
  !! @param[in] y double precision vector
  pure function dp_dot (x, y) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x(:), y(:)
    real(kind=dp_) :: ans
    ans = DDOT(size(x), x, 1, y, 1) 
  end function dp_dot

  !> @defgroup math_mm_mv_quad_ Quadaratic Form Derivatives
  !! @{

  !> Forward differentiation of the quadratic form:
  !! C = B**T.A.B
  !! @param[in] A double precision(:,:)
  !! @param[in] B double precision(:,:)
  !! @param[in] dA double precision(:,:), derivative of A
  !! @param[in] dB double precision(:,:), derivative of B
  !! @param[in] dC double precision(:,:), derivative of C
  !! @param[inout] dA double precision(:,:), derivative of A
  subroutine DX_FW_QUADRATIC1 (A, B, dA, dB, dC)
    implicit none
    real(kind=dp_), intent(in), dimension(:,:) :: A, B, dA, dB
    real(kind=dp_), intent(inout) :: dC(:,:)
    real(kind=dp_) :: X(size(dB,2),size(A,2))
    real(kind=dp_) :: Y(size(B,2),size(dA,2))
    real(kind=dp_) :: Z(size(B,2),size(A,2))
    !$omp parallel sections
    !$omp section
    call dp_gemm(1, 0, 1._dp_, dB, A, 0._dp_, X)
    !$omp section
    call dp_gemm(1, 0, 1._dp_, B, dA, 0._dp_, Y)
    !$omp section
    call dp_gemm(1, 0, 1._dp_, B, A, 0._dp_, Z)
    !$omp end parallel sections
    call dp_gemm(0, 0, 1._dp_, X, B, 0._dp_, dC)
    call dp_gemm(0, 0, 1._dp_, Y, B, 1._dp_, dC)
    call dp_gemm(0, 0, 1._dp_, Z, dB, 1._dp_, dC)
  end subroutine DX_FW_QUADRATIC1
  
  !> Bakward differentiation w.r.t. A and B of the quadratic form:
  !! C = B**T.A.B
  !! @param[in] A double precision(:,:)
  !! @param[in] B double precision(:,:)
  !! @param[in] dC double precision(:,:), derivative of C
  !! @param[inout] dA double precision(:,:), optional, derivative of A
  !! @param[inout] dB double precision(:,:), oprional, derivative of B
  subroutine DX_BW_QUADRATIC1 (A, B, dC, dA, dB)
    implicit none
    real(kind=dp_), intent(in), dimension(:,:) :: A, B, dC
    real(kind=dp_), intent(inout), optional :: dA(:,:), dB(:,:)
    real(kind=dp_) :: BdC(size(B,1),size(dC,2))
    real(kind=dp_) :: AB(size(A,1),size(B,2))
    call dp_gemm(0, 0, 1._dp_, B, dC, 0._dp_, BdC)
    !$omp parallel sections
    !$omp section
    if (present(dA)) then
       call dp_gemm(0, 1, 1._dp_, BdC, B, 1._dp_, dA)
    end if
    !$omp section
    if (present(dB)) then
       call dp_gemm(0, 0, 1._dp_, A, B, 0._dp_, AB)
       call dp_gemm(0, 1, 1._dp_, AB, dC, 1._dp_, dB)
       call dp_gemm(1, 0, 1._dp_, A, BdC, 1._dp_, dB)
    end if
    !$omp end parallel sections
  end subroutine DX_BW_QUADRATIC1

  !> Bakward differentiation w.r.t. A and B of the quadratic form:
  !! C = B**T.A.B
  !! For the case when A is symmetric, calculation can be speed up
  !! with dp_symm.
  !! @param[in] A double precision(:,:)
  !! @param[in] B double precision(:,:)
  !! @param[in] dC double precision(:,:), derivative of C
  !! @param[inout] dA double precision(:,:), optional, derivative of A
  !! @param[inout] dB double precision(:,:), oprional, derivative of B
  subroutine DX_BW_QUADRATIC1_SYM (A, B, dC, dA, dB)
    implicit none
    real(kind=dp_), intent(in), dimension(:,:) :: A, B, dC
    real(kind=dp_), intent(inout), optional :: dA(:,:), dB(:,:)
    real(kind=dp_) :: BdC(size(B,1),size(dC,2))
    real(kind=dp_) :: AB(size(A,1),size(B,2))
    call dp_gemm(0, 0, 1._dp_, B, dC, 0._dp_, BdC)
    !$omp parallel sections
    !$omp section
    if (present(dA)) then
       call dp_gemm(0, 1, 1._dp_, BdC, B, 1._dp_, dA)
    end if
    !$omp section
    if (present(dB)) then
       call dp_symm(0, 0, 1._dp_, A, B, 0._dp_, AB)
       call dp_gemm(0, 1, 1._dp_, AB, dC, 1._dp_, dB)
       call dp_symm(0, 0, 1._dp_, A, BdC, 1._dp_, dB)
    end if
    !$omp end parallel sections
  end subroutine DX_BW_QUADRATIC1_SYM

  !> Bakward differentiation w.r.t. A and B of the quadratic form:
  !! C = B**T.A**(-1).B
  !! @param[in] IA double precision(:,:), inverse of A
  !! @param[in] B double precision(:,:)
  !! @param[in] dC double precision(:,:), derivative of C
  !! @param[inout] dA double precision(:,:), optional, derivative of A
  !! @param[inout] dB double precision(:,:), oprional, derivative of B
  subroutine DX_BW_QUADRATIC2 (IA, B, dC, dA, dB)
    implicit none
    real(kind=dp_), intent(in), dimension(:,:) :: IA, B, dC
    real(kind=dp_), intent(inout), optional :: dA(:,:), dB(:,:)
    real(kind=dp_) :: IATB(size(IA,2),size(B,2))
    real(kind=dp_) :: IAB(size(IA,1),size(B,2))
    real(kind=dp_) :: BTIAT(size(B,2),size(IA,1))
    real(kind=dp_) :: IATBdC(size(IA,2),size(dC,2))
    call dp_gemm(1, 0, 1._dp_, IA, B, 0._dp_, IATB)
    call dp_gemm(0, 0, 1._dp_, IATB, dC, 0._dp_, IATBdC)
    !$omp parallel sections
    !$omp section
    if (present(dA)) then
       call dp_gemm(1, 1, 1._dp_, B, IA, 0._dp_, BTIAT)
       call dp_gemm(0, 0, -1._dp_, IATBdC, BTIAT, 1._dp_, dA)
    end if
    !$omp section
    if (present(dB)) then
       dB = dB + IATBdC
       call dp_gemm(0, 0, 1._dp_, IA, B, 0._dp_, IAB)
       call dp_gemm(0, 1, 1._dp_, IAB, dC, 1._dp_, dB)
    end if
    !$omp end parallel sections
  end subroutine DX_BW_QUADRATIC2

  !> Calculates the quadratic form:
  !! Q = B**T . A**(-1) . B
  !! where A is symmetric and positive defined matrix.
  !! This function does not invert A but solve the following system of
  !! linear equations:
  !! A . X = B => X = A**(-1) . B
  !! Therefore, the quadaratic becomee equal to: 
  !! Q = B**T . X
  !! @param[in] A double precision(:,:), symmetric positive defined matrx
  !! @param[in] B double precision(:,:)
  !! @param[inout] Q double precision(:,:)
  !! @param[out] info integer, error flag
  !! @param[inout] lnD double precision, option, log-detrminant of A
  subroutine QUADRATIC2_SYM (A, B, Q, info, lnD)
    implicit none
    real(kind=dp_), intent(inout) :: A(:,:) 
    real(kind=dp_), intent(in) :: B(:,:)
    real(kind=dp_), intent(inout) :: Q(:,:)
    integer, intent(out) :: info
    real(kind=dp_), intent(inout), optional :: lnD
    real(kind=dp_) :: BB(size(B,1),size(B,2))
    !$omp parallel workshare
    BB = B
    !$omp end parallel workshare
    if (present(lnD)) then
       call solveSym(A, BB, info, lnD)
    else
       call solveSym(A, BB, info)
    end if
    if (info==0) call dp_gemm(1, 0, 1._dp_, BB, B, 0._dp_, Q)
  end subroutine QUADRATIC2_SYM

  !> Bakward differentiation w.r.t. A and B of the quadratic form:
  !! C = B**T .A**(-1) . B
  !! where A is symmetrix and positive defined.
  !! @param[in] A double precision(:,:)
  !! @param[in] B double precision(:,:)
  !! @param[in] dC double precision(:,:), derivative of C
  !! @param[inout] dA double precision(:,:), optional, derivative of A
  !! @param[inout] dB double precision(:,:), oprional, derivative of B
  subroutine DX_BW_QUADRATIC2_SYM (A, B, dC, dA, dB, lnD, info)
    implicit none
    real(kind=dp_), intent(inout), dimension(:,:) :: A, B
    real(kind=dp_), intent(in) :: dC(:,:)
    real(kind=dp_), intent(inout), optional :: dA(:,:), dB(:,:), lnD
    integer, intent(out) :: info
    real(kind=dp_) :: X(size(A,2),size(dC,2))
    !call dp_symm(0, 0, 1._dp_, IA, B, 0._dp_, IAB)
    if (present(lnd)) then
       call SOLVESYM(A, B, info, lnD)
    else
       call SOLVESYM(A, B, info)
    end if
    call dp_gemm(0, 0, 1._dp_, B, dC, 0._dp_, X)
    !$omp parallel sections
    !$omp section
    if (present(dA)) then
       call dp_gemm(0, 1, -1._dp_, X, B, 1._dp_, dA)
    end if
    !$omp section
    if (present(dB)) then
       dB = dB + X
       call dp_gemm(0, 1, 1._dp_, B, dC, 1._dp_, dB)
    end if
    !$omp end parallel sections
  end subroutine DX_BW_QUADRATIC2_SYM
  !> @}
  !> @}

  !> @defgroup math_sigmoid_ Sigmoid Function
  !! @{
  
  !> Sigmoid function
  !! @author Filippo Monari
  !! @param[in] x double precision
  elemental function sigmoid (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans
    ans = 1 / (1 + exp(-x))
  end function sigmoid

  !> Derivative of sigmoid(x)
  !! @param[in] x double precision scalar or array
  elemental double precision function dx_sigmoid (x)
    implicit none
    real(kind=dp_), intent(in) :: x
    dx_sigmoid = exp(-x) / (1 + exp(-x))**2
  end function dx_sigmoid
  !> @}

  !> @defgroup math_relu_ Rectified Liner Unit (ReLU) Function
  !! @{

  !> ReLu function
  !! @author Filippo Monari
  !! @param[in] x double preicision
  elemental function relu (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans
    ans = max(0._dp_, x)
  end function relu

  !> Derivative of relu(x)
  !! @param[in] x double precision
  elemental function dx_relu (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans
    ans = merge(1._dp_, 0._dp_, x > 0)
  end function dx_relu
  !> @}

  !> @defgroup math_elu_ Exponential Linear Unit (ELU) Function
  !! @{
  
  !> ELU function
  !! @author Filippo Monari
  !! @param[in] x double precision
  !! @param[in] a double precision
  elemental function elu (x, a) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x, a
    real(kind=dp_) :: ans
    ans = merge(x, a * (exp(-x) -1), x > 0) 
  end function elu

  !> Derivative w.r.t. x of elu(x, a)
  !! @param[in] x double precision
  !! @param[in] a double precision
  elemental function dx_elu (x, a) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x, a
    real(kind=dp_) :: ans
    ans = merge(1._dp_, -a * exp(-x), x > 0)
  end function dx_elu
  !> @}

  !> @defgroup math_swish_ Swish Function
  !! @{

  !> Swish function
  !! @author Filippo Monari
  !! @param[in] x double precision
  elemental function swish (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans
    ans = x  * sigmoid(x)
  end function swish

  !> Deirvative of swish(x)
  !! @param[in] x double precision
  elemental function dx_swish (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x
    real(kind=dp_) :: ans
    ans = 1 / (1+exp(-x)) + x*exp(-x) / (1+exp(-x))**2
  end function dx_swish
  !> @}

  !> @defgroup math_softmax_ Softmax Function
  !! @{
  
  !> Softmax fucntion
  !! @author Filippo Monari
  !! @param[in] x double precision(:)
  function SOFTMAX (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x(:)
    real(kind=dp_) :: ans(size(x)), s, m
    !$omp parallel workshare
    m = maxval(x)
    ans = exp(x - m + tol_dp_)
    s = sum(ans)
    ans = ans / s
    !$omp end parallel workshare
  end function SOFTMAX

  !> Derivative of softmax(x)
  !! @param[in] x double preicision (:)
  function DX_SOFTMAX (x, s) result(dx) 
    implicit none
    real(kind=dp_), intent(in) :: x(:), s(:)
    real(kind=dp_) :: dx(size(x),size(x))
    integer :: i, j, n
    n = size(x)
    !omp parallel do
    do j = 1, n
       do i = 1, n
          dx(i, j) = s(i) * (deltaij(i, j) - s(j))
       end do
    end do
    !$end omp parallel do
  end function DX_SOFTMAX
  !> @}

  !> @defgroup math_redux_ Reduction Functions
  !! @{
  
  !> Sum of Squares for rank 1 double precisiona arrays
  !! @param[in] x double precision(:)
  function SSQ__1 (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x(:)
    real(kind=dp_) :: ans
    !$omp parallel workshare
    ans = sum(x**2)
    !$omp end parallel workshare
  end function SSQ__1

  !> Derivative for the ssq for double precision vectors
  elemental function dx_ssq (x) result(ans)
    implicit none
    double precision, intent(in) :: x
    double precision :: ans
    ans = 2 * x
  end function dx_ssq
  
  !> Sum of Squares for rank 2 double precisiona arrays
  !! @param[in] x double precision(:,:)
  function SSQ__2 (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x(:,:)
    real(kind=dp_) :: ans
    !$omp parallel workshare
    ans = sum(x**2)
    !$omp end parallel workshare
  end function SSQ__2
  
  !> L2-Norm for double precision rank 1 arrays
  !! @param[in] x double precision(:)
  function L2NORM__1 (x) result(ans)
    implicit none
    real(kind=dp_), intent (in) :: x(:)
    real(kind=dp_) :: ans
    ans = sqrt(ssq(x))
  end function L2NORM__1

  !> L2-Norm for double precision rank 2 arrays
  !! @param[in] x double precision(:,:)
  function L2NORM__2 (x) result(ans)
    implicit none
    real(kind=dp_), intent (in) :: x(:,:)
    real(kind=dp_) :: ans
    ans = sqrt(ssq(x))
  end function L2NORM__2

  !> Derivative of the product fucntion for vectors
  !! @param[in] x double precision(:)
  function DX_PRODUCT__1 (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x(:)
    real(kind=dp_) :: ans(size(x))
    !$omp parallel workshare
    ans = product(x) / x
    !$omp end parallel workshare
  end function dx_product__1

  !> Derivative of the product function for matrices
  !! @param[in] x double precision(:,:)
  function DX_PRODUCT__2 (x) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x(:,:)
    real(kind=dp_) :: ans(size(X,1),size(X,2))
    !$omp parallel workshare
    ans = product(x) / x
    !$omp end parallel workshare
  end function DX_PRODUCT__2
  !> @}
  

  !> @defgroup math_entropy_ Entropy Functions
  !! @{
  
  !> Binary Entropy
  !! @author Filippo Monari
  !! @param[in] x double precision, target probability
  !! @param[in] p double precision, probability
  elemental function bin_entropy (x, p) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x, p
    real(kind=dp_) :: ans
    ans = - x * log(p) - (1 - x) * log(1 - p)
  end function bin_entropy

  !> Derivative of bin_entropy(x, p) w.r.t. 'p'.
  !! @param[in] x double precision, target probability
  !! @param[in] p double precision, probability
  elemental function dx_bin_entropy (x, p) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x, p
    real(kind=dp_) :: ans
    ans = -x / p + (1 - x) / (1 - p)
  end function dx_bin_entropy

  !> Cross-entropy
  !! @author Filippo Monari
  !! @param[in] x double precision (:), target probability
  !! @param[in] p double precision (:), probability
  elemental function cross_entropy (x, p) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x, p
    real(kind=dp_) :: ans
    ans = -x * log(p)
  end function cross_entropy

  !> Derivative of cross_entropy(x, p) w.r.t. 'p'.
  !! @param[in] x double precision, target probability
  !! @param[in] p double precision, probability
  elemental function dx_cross_entropy (x, p) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x, p
    real(kind=dp_) :: ans
    ans = -x / p
  end function dx_cross_entropy
  !> @} 

  !> @defgroup math_kernels_ Kernel Functions
  !! @{

  !> Isotropic Square Exponential Kernel
  !! All the feature share the same rate paramter.
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  pure function KSQEXP__1 (x1, x2, a, b) result (ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b
    real(kind=dp_) :: ans
    !omp parallel workshare
    ans = product(sqexp(x1, x2, a, b))
    !omp end parallel workshare
  end function KSQEXP__1

  !> Derivative of the isotropic square expoential kernel
  !! w.r.t. a.
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  function DX_KSQEXP_A__1 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b
    real(kind=dp_) :: ans, s(size(x1))
    !omp parallel workshare
    s = sqexp(x1, x2, a, b)
    ans = sum(product(s)/s * dx_sqexp_a(x1, x2, b))
    !omp end parallel workshare
  end function DX_KSQEXP_A__1

  !> Derivative of the isotropic square expoential kernel
  !! w.r.t. b.
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  function DX_KSQEXP_B__1 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b
    real(kind=dp_) :: ans, s(size(x1))
    !$omp parallel workshare
    s = sqexp(x1, x2, a, b)
    ans = sum(product(s)/s * dx_sqexp_b(x1, x2, a, b))
    !$omp end parallel workshare
  end function DX_KSQEXP_B__1

  !> Derivative of the isotropic square expoential kernel
  !! w.r.t. x1
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  !! @todo move to unpure math module
  function DX_KSQEXP_X1__1 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b
    real(kind=dp_) :: ans(size(x1))
    !ans = dx_product(sqexp(x1, x2, a, b)) * dx_sqexp_x1(x1, x2, a, b)
    !$omp parallel workshare
    ans = sqexp(x1, x2, a, b)
    ans = product(ans) / ans * dx_sqexp_x1(x1, x2, a, b)
    !$omp end parallel workshare
  end function DX_KSQEXP_X1__1

  !> Derivative of the isotropic square expoential kernel
  !! w.r.t. x2
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  !! @todo move to unpure math
  function DX_KSQEXP_X2__1 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b
    real(kind=dp_) :: ans(size(x2))
    !$omp parallel workshare
    ans = sqexp(x1, x2, a, b)
    ans = product(ans) / ans * dx_sqexp_x2(x1, x2, a, b)
    !$omp end parallel workshare
  end function DX_KSQEXP_X2__1

  !> Anisotropic Square Exponential Kernel
  !! The feature have different the same rate paramter.
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision(:), ampliutude parameter
  !! @param[in] b double precision, rate parameter
  !! @todo move to unpure math
  function KSQEXP__2 (x1, x2, a, b) result (ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b(:)
    real(kind=dp_) :: ans
    !$omp parallel workshare
    ans = product(sqexp(x1, x2, a, b))
    !$omp end parallel workshare
  end function KSQEXP__2

  !> Derivative of the anisotropic square expoential kernel
  !! w.r.t. a
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  !! @todo move to unpure math
  function DX_KSQEXP_A__2 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b(:)
    real(kind=dp_) :: ans, s(size(x1))
    !$omp parallel workshare
    s = sqexp(x1, x2, a, b)
    ans = sum(product(s) / s * dx_sqexp_a(x1, x2, b))
    !$omp end parallel workshare
  end function DX_KSQEXP_A__2

  !> Derivative of the anisotropic square expoential kernel
  !! w.r.t. b
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  !! @todo move to unpure math
  function DX_KSQEXP_B__2 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b(:)
    real(kind=dp_) :: ans(size(b))
    !$omp parallel workshare
    ans = sqexp(x1, x2, a, b)
    ans = product(ans) / ans * dx_sqexp_b(x1, x2, a, b)
    !$omp end parallel workshare
  end function DX_KSQEXP_B__2

  !> Derivative of the anisotropic square expoential kernel
  !! w.r.t. x1
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  function DX_KSQEXP_X1__2 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b(:)
    real(kind=dp_) :: ans(size(x1))
    !$omp parallel workshare
    ans = sqexp(x1, x2, a, b)
    ans = product(ans)/ans * dx_sqexp_x1(x1, x2, a, b)
    !$omp end parallel workshare
  end function DX_KSQEXP_X1__2

  !> Derivative of the anisotropic square expoential kernel
  !! w.r.t. x2
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  function DX_KSQEXP_X2__2 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1(:), x2(:), a, b(:)
    real(kind=dp_) :: ans(size(x2))
    !$omp parallel workshare
    ans = sqexp(x1, x2, a, b)
    ans = product(ans)/ans * dx_sqexp_x2(x1, x2, a, b)
    !$omp end parallel workshare
  end function DX_KSQEXP_X2__2

  !> Square Exponeitial Function (SE)
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  elemental function sqexp (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1, x2, a, b
    real(kind=dp_) :: ans
    ans = a * exp(-(x1 - x2)**2 * b)
  end function sqexp

  !> SE derivative w.r.t. a
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  elemental function dx_sqexp_a (x1, x2, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1, x2, b
    real(kind=dp_) :: ans
    ans = exp(-(x1 - x2)**2 * b)
  end function dx_sqexp_a

  !> SE derivative w.r.t. b
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  elemental function dx_sqexp_b (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1, x2, a, b
    real(kind=dp_) :: ans
    ans = -a * (x1 - x2)**2 * exp(-(x1 - x2)**2 * b)
  end function dx_sqexp_b

  !> SE derivative w.r.t. x1
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  elemental function dx_sqexp_x1 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1, x2, a, b
    real(kind=dp_) :: ans
    ans = -2 * b * (x1 - x2) * a * exp(-(x1 - x2)**2 * b)
  end function dx_sqexp_x1

  !> SE derivative w.r.t. x2
  !! @param[in] x1 double precision(:), feature vector
  !! @param[in] x2 double precision(:), feature vector
  !! @param[in] a double precision, ampliutude parameter
  !! @param[in] b double precision, rate parameter
  elemental function dx_sqexp_x2 (x1, x2, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: x1, x2, a, b
    real(kind=dp_) :: ans
    ans = 2 * b * (x1 - x2) * a * exp(-(x1 - x2)**2 * b)
  end function dx_sqexp_x2
  !> @}

  !> @defgroup math_probdist_ Probability Distributions
  !!@ {
  
  !> log-desity for the unifirom distribution.
  !! @param[in] a double precision, lower bound
  !! @param[in] b double precision, upper bound
  elemental function ldunif (a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: a, b
    real(kind=dp_) :: ans
    ans = 1 / (b - a)
  end function ldunif

  !> log-density for the exponential distribution.
  !! @param[in] y double precision, observations
  !! @param[in] lam double precision, rate parameter
  elemental function ldexp (y, lam) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, lam
    real(kind=dp_) :: ans
    ans = merge(log(lam) - lam * y, 0._dp_, y > 0)
  end function ldexp

  !> Derivative of ldexp w.r.t. lam
  !! @param[in] y double precision, observations
  !! @param[in] lam double precision, rate paramter
  elemental function dx_ldexp (y, lam) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, lam
    real(kind=dp_) :: ans
    ans = merge(lam**(-1) - y, 0._dp_, y > 0)
  end function dx_ldexp
  
  !> log-density for the laplace distribution.
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, location parameter
  !! @param[in] lam double precision, rate parameter
  elemental function ldlaplace (y, mu, lam) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, lam, mu
    real(kind=dp_) :: ans
    ans = log(0.5) + log(lam) - lam * abs(y - mu)
  end function ldlaplace

  !> Derivative of ldlaplace w.r.t. mu
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, location paramter
  !! @param[in] lam double precision, rate parameter
  elemental function dx_ldlaplace_mu (y, mu, lam) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, lam
    real(kind=dp_) :: ans
    ans = -dx_abs(y - mu) * lam
  end function dx_ldlaplace_mu

  !> Derivative of ldlaplace w.r.t. lam
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, location paramter
  !! @param[in] lam double precision, rate parameter
  elemental function dx_ldlaplace_lam (y, mu, lam) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, lam
    real(kind=dp_) :: ans
    ans = lam**(-1) - abs(y - mu)
  end function dx_ldlaplace_lam
  
  !> log-density for the beta distribution.
  !! @param[in] y double precision, observations
  !! @param[in] a1,a2 double precision, shape parameters
  elemental function ldbeta (y, a1, a2) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, a1, a2
    real(kind=dp_) :: ans
    ans = merge(((a1 - 1) * log(y) + (a2 - 1) * log(1 - y)) - log_beta(a1, a2), 0._dp_, &
         y > 0 .and. y < 1) 
  end function ldbeta

  !> Derivative of lsbeta w.r.t. a1
  !! @param[in] y double precision, observations
  !! @param[in] a1,a2 double precision, shape parameters
  elemental function dx_ldbeta_a1 (y, a1, a2) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, a1, a2
    real(kind=dp_) :: ans
    ans = merge(log(y) - dx_log_beta_a(a1, a2), 0._dp_, y > 0 .and. y < 1)
  end function dx_ldbeta_a1

  !> Derivative of ldbeta w.r.t. a2
  !! @param[in] y double precision, observations
  !! @param[in] a1,a2 double precision, shape parameters
  elemental function dx_ldbeta_a2 (y, a1, a2) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, a1, a2
    real(kind=dp_) :: ans
    ans = merge(log(1 - y) - dx_log_beta_b(a1, a2), 0._dp_, y > 0 .and. y < 1)
  end function dx_ldbeta_a2
  
  !> log-density for the gamma distribution
  !! @param[in] y double precision, observations
  !! @param[in] a double precision, shape parameter
  !! @param[in] b double precision, rate parameter
  elemental function ldgamma (y, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, a, b
    real(kind=dp_) :: ans
    ans = merge(a * log(b) - log_gamma(a) + (a - 1) * log(y) - b * y, 0._dp_, y > 0) 
  end function ldgamma

  !> Derivative of ldgamma w.r.t. a
  !! @param[in] y double precision, observations
  !! @param[in] a double precision, shape parameter
  !! @param[in] b double precision, rate parameter
  elemental function dx_ldgamma_a (y, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, a, b
    real(kind=dp_) :: ans
    !ans = merge(log(b) - dx_log_gamma(a) + log(y), 0._dp_, y > 0)
    ans = merge(log(b) - psi(a) + log(y), 0._dp_, y > 0)
  end function dx_ldgamma_a

  !> Derivative of ldgamma w.r.t. b
  !! @param[in] y double precision, observations
  !! @param[in] a double precision, shape parameter
  !! @param[in] b double precision, rate parameter
  elemental function dx_ldgamma_b (y, a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, a, b
    real(kind=dp_) :: ans
    ans = merge(a * b**(-1) - y, 0._dp_, y > 0)
  end function dx_ldgamma_b
  
  !> ldnorm__1
  !! Calculates the log-likelihood of the observations y accroding to the
  !! normal distribution with mean mu and starndard deviation s
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  elemental function ldnorm__1 (y, mu, s) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, s
    real(kind=dp_) :: ans
    ans = -log(s) - 0.5 * log(2 * pi_dp_) - 0.5 * ((y - mu) / s)**2 
  end function ldnorm__1

  !> Derivative of ldnorm__1 w.r.t. the standard deviation (s)
  !! @param[in] y double pecision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  elemental function dx_ldnorm_s__1 (y, mu, s) result (ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, s
    real(kind=dp_) :: ans
    ans = -s**(-1) + s**(-3) * (y - mu)**2
  end function dx_ldnorm_s__1

  !> Derivative of ldnorm__1 w.r.t. the mean (mu)
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  elemental function dx_ldnorm_mu__1 (y, mu, s) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, s
    real(kind=dp_) :: ans
    ans = s**(-2) * (y - mu)
  end function dx_ldnorm_mu__1
  
  !> ldnorm__2, weighted normal likelihood
  !! Calculates the weighted log-likelihood of the observations y accroding to the
  !! normal distribution with mean mu and starndard deviation s
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  !! @param[in] w duble precision, weights
  elemental function ldnorm__2 (y, mu, s, w) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, s, w
    real(kind=dp_) :: ans
    ans = w * ldnorm__1(y, mu, s)
  end function ldnorm__2

  !> Derivative of ldnorm__2 w.r.t. the standard deviation (s).
  !! @param[in] y double precision, observations
  !! @param[in] mu double precision, mean
  !! @param[in] s double precision, standard deviation
  !! @param[in] w double precision, weights
  elemental function dx_ldnorm_s__2 (y, mu, s, w) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, s, w
    real(kind=dp_) :: ans
    ans = w * dx_ldnorm_s__1(y, mu, s)
  end function dx_ldnorm_s__2

  !> Derivative of ldnorm__2 w.r.t. the mean (mu).
  !! @param[in] y double precision, observations
  !! @param[in] mu double ecision, mean
  !! @param[in] s double precision, standard deviation
  !! @param[in] w double precision, weights
  elemental function dx_ldnorm_mu__2 (y, mu, s, w) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: y, mu, s, w
    real(kind=dp_) :: ans
    ans = w * dx_ldnorm_mu__1(y, mu, s)
  end function dx_ldnorm_mu__2

  !> Multivariate Normal log-density
  !! @param[in] y double precision(:), observations
  !! @param[in] mu double precision(:), mean vector
  !! @param[in] E double precision(:,:), covariance matrix
  !! @param[out] info integer, error flag
  !! @param[inout] ld double precision, log-density
  subroutine LDMVNORM__1 (y, mu, E, ld, info) 
    implicit none
    real(kind=dp_), intent(in) :: y(:), mu(:), E(:,:)
    integer, intent(out) :: info
    real(kind=dp_), intent(inout) :: ld
    real(kind=dp_) :: lnD, EE(size(E,1),size(E,2))
    real(kind=dp_) :: Q(1,1), x(size(y), 1)
    info = 0
    !$omp parallel workshare
    x(:,1) = y - mu
    EE = E
    !$omp end parallel workshare
    call QUADRATIC2_SYM(EE, x, Q, info, lnD)
    if (info == 0) ld = -0.5_dp_ * (size(y) * log(2 * pi_dp_) + lnD + Q(1,1))
  end subroutine LDMVNORM__1
  
  !> ldmvnorm__1 backward differentiation
  !! @param[in] y double precision(:), observations
  !! @param[in] mu double precision(:), mean
  !! @param[in] E double precision(:,:), covariance matrix
  !! @param[in] dld double precision, derivative of the log-density
  !! @param[out] info integer, error flag
  !! @param[inout] dmu double precision(:), optional, derivative of the mean
  !! @paarm[inout] dE double precision(:,:), oprional, derivative of the covariance matrix
  subroutine DX_BW_LDMVNORM__1 (y, mu, E, dld, info, dmu, dE)
    implicit none
    real(kind=dp_), intent(in) :: y(:),  mu(:), E(:,:), dld
    integer, intent(out) :: info
    real(kind=dp_), intent(inout), optional :: dmu(:), dE(:,:)
    real(kind=dp_) :: x(size(y),1), dx(size(y),1), dQ(1,1)
    real(kind=dp_) :: IE(size(E,1),size(E,2)), lnD, D, dD
    info = 0
    IE = E
    x(:,1) = y - mu
    dQ = -0.5_dp_
    call INVSYMMAT(IE, info, lnD)
    if (info == 0) then
       if (present(dmu) .and. present(dE)) then
          dx = 0
          call DX_BW_QUADRATIC2_SYM(IE, x, dQ, dE, dx, info=info)
          ! w.r.t. mu
          dmu = -dx(:,1) * dld
          ! w.r.t. E
          D = exp(lnD)
          dD = dx_log(D) 
          dE = dE - 0.5_dp_ * DX_BW_DET(IE, D, dD, .true.)
          dE = dE * dld
       else if (present(dmu) .and. .not. present(dE)) then
          dx = 0
          call DX_BW_QUADRATIC2_SYM(IE, x, dQ, dB=dx, info=info)
          dmu = -dx(:,1) * dld
       else if (present(dE) .and. .not. present(dmu)) then
          call DX_BW_QUADRATIC2_SYM(IE, x, dQ, dE, info=info)
          D = exp(lnD)
          dD = dx_log(D)
          dE = dE - 0.5_dp_ * DX_BW_DET(IE, D, dD, .true.)
          dE = dE * dld
       else
          info = err_missingArg_
       end if
    end if
  end subroutine DX_BW_LDMVNORM__1
  
  !> Calculates the log-likelihood of the observation y according to a normal distribution
  !! with precision matrix A, and mean vector mu.
  !! @param[in] y double precision(:), observation vector
  !! @param[in] mu double precision(:), mean vector
  !! @param[in] A double precision(:,:), precision matrix
  !! @param[in] lnD double precision, determinant of A**-1 (covariance matrix)
  pure function ldnorm__4 (y, mu, A, lnD) result(ans)
    implicit none
    !in/out
    real(kind=dp_), intent(in) :: y(:), mu(:), A(:,:), lnD
    real(kind=dp_) :: ans
    !auxiliaryiliary
    real(kind=dp_) :: x(size(y)), Ay(size(y))
    x = y - mu
    call dp_gemv(0, 1._dp_, A, x, 0._dp_, Ay)
    ans = -0.5 * (size(y) * log(2 * pi_dp_) - 0.5 * lnD + dp_dot(x, Ay))
  end function ldnorm__4

  subroutine MVNORM_POSTERIOR (A11, E12, y, mu, E22)
    implicit none
    double precision, intent(inout), optional :: mu(:), E22(:,:)
    real(kind=dp_), intent(in), optional :: y(:)
    double precision, intent(in) :: A11(:,:), E12(:,:)
    double precision :: X(size(A11, 2), size(E12, 2))
    call dp_symm(0, 0, dble(1), A11, E12, dble(0), X)
    !$omp parallel sections
    !$omp section
    if (present(y) .and. present(mu)) call dp_gemv(1, 1._dp_, X, y - mu, dble(1), mu)
    !$omp section
    if (present(E22)) call dp_gemm(1, 0, -1._dp_, E12, X, 1._dp_, E22)
    !$omp end parallel sections
  end subroutine MVNORM_POSTERIOR
 
  !> @}
  
  

    ! subroutine ginv (A, lnD, info)	!R
  !   !DOC
  !   !Calculates the general inverse and an approximation of the log determinant of a mtrix
  !   !approximates the inverse of an singular (or computationally singular)
  !   !matrix through eigen-decomposition (Moore - Penrose pseudoinverse)
  !   !VARIABLES
  !   implicit none
  !   !in/out	
  !   double precision, intent (inout) :: A(:,:), lnD			!(m,m)
  !   integer, intent(out) :: info
  !   !auxiliary
  !   double precision :: U(size(A, 1),size(A, 2))	!eigen vector matrix
  !   double precision :: VT(size(A,2),size(A,1))
  !   double precision :: D(size(A,1)), DD(size(A,1),size(A,1)) !eigen values
  !   double precision :: tol
  !   !counters
  !   integer :: i
  !   !EXECUTION
  !   ! if (.not. isSquare(A)) then
  !   !    call raiseError('ginvMat,matrix is not square,', 1)
  !   !    return
  !   ! else if (size(A,1) /= size(B,1) .or. size(A,2) /= size(B,2)) then
  !   !    call raiseError('ginvMat,matrices are not compatible', 2)
  !   !    return
  !   ! end if
  !   call svd(U, D, VT, A, info)
  !   lnD = 0; tol = eps_dbl_ * D(1)
  !   do i = 1, size(D)
  !      if (D(i) < tol) then
  !         D(i) = 0
  !      else
  !         D(i) = 1/D(i)
  !      end if
  !   end do
  !   call diag(DD, D)
  !   A = matmul(transpose(VT), matmul(DD, transpose(U)))	!@toDo: this can be done without creating diagonal matrix
  !   lnD = sum(log(D))
  ! end subroutine ginv

  ! subroutine svd (U, D, VT, A, info)
  !   implicit none
  !   double precision, intent (out) :: U(:,:), D(:), VT(:,:)
  !   integer, intent(out) :: info
  !   double precision, intent (in) :: A(:,:)

  !   double precision, allocatable :: work(:)
  !   double precision :: D_dm(1,size(D))
  !   integer, parameter :: lwmax = 10**3
  !   integer :: lwork

  !   allocate(work(lwmax))
  !   lwork = -1
  !   !DGESVD(JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO)	 
  !   call DGESVD('A', 'A', size(A,1), size(A,2), A, size(A,1), D, U, size(A,1), VT, size(A,1), &
  !        work, lwork, info)
  !   lwork = int(work(1))
  !   if (lwork > lwmax) then
  !      deallocate(work)
  !      allocate(work(lwork))
  !   end if
  !   call DGESVD('A', 'A', size(A,1), size(A,2), A, size(A,1), D, U, size(A,1), VT, size(A,1), & 
  !        work, lwork, info)
  !   D_dm(1,:) = D
  ! end subroutine svd

  ! subroutine wrp_dposv (uplo, A, B, lnD, info)
  !   !_DOC_
  !   !Wrap to LAPCK subrotuine dposv.
  !   !_VARIABLES_
  !   implicit none
  !   !in/out
  !   character(len = 1), intent(in) :: uplo !L for lower triangular, U for upper triangular 
  !   double precision, intent(inout) :: B(:,:) !(in) right side of the sysntem of equations / (out) solution
  !   double precision, intent(in) :: A(:,:) !left side of the systemof equations
  !   double precision, intent(out) :: lnD   !log det of A
  !   integer, intent(out) :: info           !error flag
  !   !local
  !   double precision :: A1(size(A, 1),size(A, 2)) !to copy A
  !   !INTERFACES
  !   interface
  !      subroutine DPOSV (UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  !        character(len = 1), intent(in) :: UPLO
  !        integer, intent(in) :: N, NRHS, LDA, LDB
  !        integer, intent(out) :: INFO
  !        double precision, intent(inout) :: A(LDA,N), B(LDB,NRHS)
  !      end subroutine DPOSV
  !   end interface
  !   !MAIN
  !   A1 = A
  !   call DPOSV(uplo, size(A, 1), size(B, 2), A1, size(A, 1), B, size(A, 1), info)
  !   lnD = 2 * lntr(A1)
  ! end subroutine wrp_dposv

  ! subroutine wrp_dsymm (side, uplo, alpha, A, B, beta, C)
  !   !_DOC_
  !   !Wrap to BLAS DSYMM subroutine.
  !   !DSYMM performs one of the matrix-matrix operations:
  !   !C := alpha*A*B + beta*C,
  !   !or
  !   !C := alpha*B*A + beta*C,
  !   !where alpha and beta are scalars,  A is a symmetric matrix and  B and
  !   !C are  m by n matrices.
  !   implicit none
  !   integer, intent(in) :: side	!0 => C := alpha*A*B + beta*C || 1 => C := alpha*B*A + beta*C
  !   integer, intent(in) :: uplo	!0 => Only the upper triangular part of A is to be referenced || 1 => Only the lower triangular part of A is to be referenced.
  !   double precision, intent(in) :: alpha, A(:,:), B(:,:), beta
  !   double precision, intent(inout) :: C(:,:)	
  !   integer :: M, N, LDA, LDB, LDC
  !   character(len=1) :: sd, ul
  !   external DSYMM
  !   !MAIN					
  !   M = size(C, 1)
  !   N = size(C, 2)
  !   LDA = size(A, 1)
  !   LDB = size(B, 1)
  !   LDC = size(C, 1)
  !   if (M /= LDB) print *, 'error1'
  !   if (N /= size(B, 2)) print *, 'error2'
  !   if (side > 0) then
  !      if (N /= size(A, 1) .or. N /= size(A,2)) print *, 'error3'
  !      sd = 'R'
  !   else
  !      if (M /= size(A, 1) .or. M /= size(A,2)) print *, 'error4'
  !      sd = 'L'
  !   end if
  !   if (uplo > 0) then
  !      ul = 'L'
  !   else
  !      ul = 'U'
  !   end if
  !   call DSYMM(sd, ul, M, N, alpha, A, LDA, B, LDB, beta, C, LDC)
  ! end subroutine wrp_dsymm
  
  ! subroutine wrp_dsyrk (uplo, trans, alpha, A, beta, C)
  !   !_DOC_
  !   !DSYRK  performs one of the symmetric rank k operations
  !   !C := alpha*A*A**T + beta*C,
  !   !or
  !   !C := alpha*A**T*A + beta*C,
  !   !where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
  !   !and  A  is an  n by k  matrix in the first case and a  k by n  matrix
  !   !in the second case.
  !   !_VARIABLES_
  !   implicit none
  !   !in/out
  !   integer, intent(in) :: uplo !0 => 'U', 1 => 'L'
  !   integer, intent(in) :: trans	!0 => C := alpha*A*A**T + beta*C || 1 =>  C := alpha*A**T*A + beta*C 
  !   double precision, intent(in) :: alpha, A(:,:), beta
  !   double precision, intent(inout) :: C(:,:)
  !   !local
  !   integer :: N, K, LDA, LDC
  !   character(len=1) :: ul, trn
  !   !MAIN
  !   N  = size(C, 1)
  !   if (trans == 0) then
  !      K = size(A, 2)
  !      LDA = size(A, 1)
  !      trn = 'N'
  !   else
  !      K = size(A, 1)
  !      LDA = size(A, 2)
  !      trn = 'T'
  !   end if
  !   if (uplo > 0) then
  !      ul = 'L'
  !   else
  !      ul = 'U'
  !   end if
  !   LDC = size(C, 1)
  !   call DSYRK(ul, trn, N, K, alpha, A, LDA, beta, C, LDC)
  ! end subroutine wrp_dsyrk

  ! !* CALCULATION OF COMMON MATRIX ENTITIES
  
  ! !* MATRIX DECOMPOSITION
  
  ! subroutine wrp_dpotrf (uplo, A, info)
  !   !_DOC_
  !   !Wrao to the LAPACK subroutine DPOTRF (Cholesky decomposition)
  !   !_VARIABLES_
  !   implicit none
  !   !in/out
  !   character(len = 1), intent(in) :: uplo		!L for lower triangular, U for upper triangular
  !   double precision, intent(inout) :: A(:,:)	!Matrix to decompose
  !   integer, intent(out) :: info 				!error flag
  !   integer :: i, j
  !   !INTERFACES
  !   interface
  !      subroutine DPOTRF (UPLO, N, A, LDA, INFO)
  !        character(len=1), intent(in) :: UPLO
  !        integer, intent(in) :: N, LDA
  !        integer, intent(out) :: INFO
  !        double precision, intent(inout) :: A(LDA,N)
  !      end subroutine DPOTRF
  !   end interface
  !   !MAIN
  !   call DPOTRF(uplo, size(A, 1), A, size(A, 1), info)
  !   if (uplo == 'L') then
  !      do i = 1, size(A, 1)
  !         do j = i, size(A, 2)
  !            if (i /= j) A(i,j) = 0
  !         end do
  !      end do
  !   else if (uplo == 'U') then
  !      do i = 1, size(A, 1)
  !         do j = 1, i
  !            if (i/=j) A(i,j) = 0
  !         end do
  !      end do
  !   end if
  ! end subroutine wrp_dpotrf
  

  
  ! !* STATISTICAL FUNCTIONS
  
  ! double precision function mean (A)
  !   !_DOC_
  !   !Mean of a vector
  !   double precision, intent (in) :: A(:)
  !   mean = sum(A) / size(A)
  ! end function mean
  
  ! double precision function var (A)
  !   !_DOC_
  !   !Variance of a vector
  !   !_VARIABLES_
  !   implicit none
  !   !in/out
  !   double precision, intent (in) :: A(:)
  !   !MAIN
  !   var = sum((A - mean(A))**2) / (size(A) - 1)		
  ! end function var
  
  ! double precision function sd (A)
  !   !_DOC_
  !   !Standard deviation of a vector
  !   !_VARIABLES_
  !   implicit none
  !   !in/out
  !   double precision, intent (in) :: A(:)
  !   !MAIN
  !   sd = sqrt(var(A))
  ! end function sd
  
  ! ! double precision function quantile (x, p, srt)
  ! !   !_DOC_
  ! !   !Calculates the q quantile of the vector x
  ! !   !srt is logical: if .true. the vector x is sorted.
  ! !   !This is done so that in case of recursive calls of the function the vector x can be sorted only once.
  ! !   !_VARIABLES_
  ! !   implicit none
  ! !   !in/out
  ! !   double precision, intent(inout) :: x(:)
  ! !   double precision, intent(in) :: p
  ! !   logical, intent(in) :: srt
  ! !   !local
  ! !   double precision :: i
  ! !   integer :: l, h
  ! !   !MAIN
  ! !   if (srt) call sort(x)
  ! !   i = 1 + (size(x) - 1) * p
  ! !   l = floor(i)
  ! !   h = ceiling(i)
  ! !   quantile = x(l) * (h - i) + x(h) * (i - l) 
  ! ! end function quantile
  
  ! !** PROBABILITY DENSITY FUCNTIONS
  ! !Due infitnity handling in the calculations the probability density function from R have been wraped so to return
  ! !a predefined constant intesad of infinity.
  ! !At the moment this seems to be the most reasonalble thing to do in the future I'll think for a better solution.
  
  ! !* LIKELIHOOD FUNCTIONS
  
  ! double precision function fdiff1 (fn, z, i)
  !   implicit none
  !   integer, intent(in) :: i
  !   double precision, intent(in) :: z(:)
  !   procedure(dpfun_1dp) :: fn
  !   double precision :: h(size(z))
  !   h = 0
  !   h(i) = tiny_
  !   fdiff1 = (fn(z + h) - fn(z)) / h(i)
  ! end function fdiff1
  
  ! double precision function bdiff1 (fn, z, i)
  !   implicit none
  !   integer, intent(in) :: i
  !   double precision, intent(in) :: z(:)
  !   procedure(dpfun_1dp) :: fn
  !   double precision :: h(size(z))
  !   h = 0
  !   h(i) = tiny_
  !   bdiff1 = (fn(z) - fn(z - h)) / h(i)
  ! end function bdiff1
  
  ! double precision function rdiff1 (fn, z, i)
  !   implicit none
  !   integer, intent(in) :: i
  !   double precision, intent(in) :: z(:)
  !   procedure(dpfun_1dp) :: fn
  !   if (c_runif(dble(0), dble(1)) < 0.5) then
  !      rdiff1 = fdiff1(fn, z, i)
  !   else
  !      rdiff1 = bdiff1(fn, z, i)
  !   end if
  ! end function rdiff1
  

  
  ! double precision function cdiff2 (fn, z, i, j)
  !   implicit none
  !   integer, intent (in) :: i, j
  !   procedure(dpfun_1dp) :: fn
  !   double precision, intent (in) :: z(:)
  !   double precision :: h(size(z))
  !   h = 0
  !   h(j) = tiny_
  !   cdiff2 = (cdiff1(fn, z + h, i) - cdiff1(fn, z - h, i)) / (2 * h(j)) 
  ! end function cdiff2
  
  ! subroutine grad (d, fn, z, dfn1)
  !   !DOC
  !   !Gradient approximation by central difference
  !   implicit none
  !   double precision, intent(out) :: d(:)
  !   double precision, intent(in) :: z(:) 
  !   procedure(dpfun_1dp) :: fn
  !   integer :: i
  
  !   interface
  !      double precision function dfn1 (fn, z, i)
  !        procedure(dpfun_1dp) :: fn
  !        double precision, intent(in) :: z(:)
  !        integer, intent(in) :: i
  !      end function dfn1
  !   end interface
  
  !   !_MAIN_
  !   d = 0
  !   do i = 1, size(z)
  !      d(i) = dfn1(fn, z, i) 
  !   end do
  ! end subroutine grad
  
  ! subroutine gradu (d, fn, z, dfn1)
  !    implicit none
  !   double precision, intent(out) :: d(:,:)
  !   double precision, intent(in) :: z(:)
  !   procedure(dpfun_1dp) :: fn
  !   integer :: i
  !   interface
  !      double precision function dfn1 (fn, z, i)
  !        procedure(dpfun_1dp) :: fn
  !        double precision, intent(in) :: z(:)
  !        integer, intent(in) :: i
  !      end function dfn1
  !   end interface
  !   call grad(d(:,1), fn, z, dfn1)
  !   d = d / L2norm(d)
  ! end subroutine gradu
  
  ! subroutine hessian (H, fn, z, dfn2)
  !   implicit none
  !   double precision, intent(in) :: z(:)
  !   procedure(dpfun_1dp) :: fn
  !   double precision, intent(out) :: H(size(z),size(z))
  !   integer :: i, j
  
  !   interface
  !      double precision function dfn2 (fn, z, i, j)
  !        procedure(dpfun_1dp) :: fn
  !        double precision, intent(in) :: z(:)
  !        integer, intent(in) :: i, j
  !      end function dfn2
  !   end interface
  
  
  !   do i = 1, size(z)
  !      do j = i, size(z)
  !         H(i,j) = dfn2(fn, z, i, j)
  !         if (i /= j) H(j,i) = H(i,j)
  !      end do
  !   end do
  
  ! end subroutine hessian
  
  

  ! !* VARIABLE TRANSFORMATION


  
  ! !** BETWEEN 0 AND 1
  ! !_DOC_

  ! double precision function Rto01_1 (x)
  !   implicit none
  !   double precision, intent(in) :: x

  !   Rto01_1 = (tanh(x) + 1) / 2

  ! end function Rto01_1

  ! double precision function Rfrom01_1 (x)
  !   implicit none
  !   double precision, intent(in) :: x

  !   Rfrom01_1 = atanh(2 * x - 1)

  ! end function Rfrom01_1

  ! double precision function Rto01_2 (x)
  !   implicit none
  !   double precision, intent(in) :: x

  !   Rto01_2 = 1 / (1 + exp(-x))

  ! end function Rto01_2

  ! double precision function Rfrom01_2 (x)
  !   implicit none
  !   double precision, intent(in) :: x

  !   Rfrom01_2 = log(x) - log(1 - x)

  ! end function Rfrom01_2
 
end module math
