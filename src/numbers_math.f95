module numbers_math

  use env
  use types
  use registers
  use errwarn
  use numbers_utils
  use numbers
  use math
  use nodes_utils
  use nodes
  use operators

  implicit none

  private
  public &
       operator(+), &
       operator(-), &
       operator(*), &
       operator(**), &
       operator(/), &
       abs, &
       exp, &
       log, &
       sin, &
       cos, &
       tan, &
       sinh, &
       cosh, &
       tanh, &
       number__dgemm, &
       number__dp_gemv, &
       number__dp_ger, &
       number__dp_dot, &
       number__invMat, &
       number__sum, &
       number__product, &
       number__ssq, &
       number__sigmoid, &
       number__relu, &
       number__swish, &
       number__elu, &
       number__softmax, &
       number__bin_entropy, &
       number__cross_entropy, &
       number__mse, &
       number__mae, &
       number__ldexp, &
       number__ldlaplace, &
       number__ldbeta, &
       number__ldgamma, &
       number__ldnorm, &
       number__ldmvnorm__1, &
       number__lkh_norm, &
       number__ksqexp

  character(len=*), parameter :: mod_numbers_math_name_ = 'numbers_math'

  !> Returns the addition between two 'numbers'
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  interface operator (+)
     module procedure number__add
  end interface operator (+)

  !> Returns the subtraction between two 'numbers'
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  interface operator (-)
     module procedure number__sub
  end interface operator (-)

  !> Returns the multiplication between two 'numbers'
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  interface operator (*)
     module procedure number__mult
  end interface operator (*)

  !> Returns the power between two 'numbers'
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  interface operator (**)
     module procedure number__pow
  end interface operator (**)

  !> Returns the division between two 'numbers'
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  interface operator (/)
     module procedure number__div
  end interface operator (/)

  !> Returns the absolute value of a 'number'
  !! @param[in] x 'number'
  interface abs
     module procedure number__abs
  end interface abs
  
  !> Returns the exponential of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface exp
     module procedure number__exp
  end interface exp

  !> Returns the log of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface log
     module procedure number__log
  end interface log

  !> Returns the sin of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface sin
     module procedure number__sin
  end interface sin

  !> Returns the cosine of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface cos
     module procedure number__cos
  end interface cos

  !> Returns the tangent of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface tan
     module procedure number__tan
  end interface tan

  !> Returns the hyperbolic sine of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface sinh
     module procedure number__sinh
  end interface sinh

  !> Returns the hyperbolic cosine of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface cosh
     module procedure number__cosh
  end interface cosh

  !> Returns the hyperbolic tangent of a 'number'
  !! @author Filippo Monari
  !! @param[in] x1 'number'
  interface tanh
     module procedure number__tanh
  end interface tanh

  interface number__sum
     module procedure number__sum__1
     module procedure number__sum__2
  end interface number__sum

  interface number__product
     module procedure number__product__1
     module procedure number__product__2
  end interface number__product

  !> Returns the softmax of a 'number' of 0 > rank <= 0.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @param[in] k integer, dimension along which calculate the softmax (only rank ==2)
  interface number__softmax
     module procedure number__softmax__1
     module procedure number__softmax__2
  end interface number__softmax

  !> Returns the cross-entropy between two 'numbers' of equal 0 > rank <= 2
  !! @param[in] x 'number' target probability
  !! @param[in] p 'number' probability
  !! @param[in] k integer, dimension along which calculate the cross-entropy (only rank == 2)
  interface number__cross_entropy
     module procedure number__cross_entropy__1
     !module procedure number__cross_entropy__2
  end interface number__cross_entropy

  !> Genral matrix multiplication
  !! @author Filippo Monari
  !! @param[in] transA,transB integer, transposition flags
  !! @param[in] alpha,beta 'numbers' of rank 1
  !! @param[in] A,B,C 'numbers' of rank 2
  interface number__dgemm
     module procedure number__dgemm__1
     module procedure number__dgemm__2
     module procedure number__dgemm__3
     module procedure number__dgemm__4
  end interface number__dgemm

  !> Genral matrix vector multiplication
  !! @author Filippo Monari
  !! @param[in] trans integer, transposition flags
  !! @param[in] alpha,beta 'numbers' of rank 1
  !! @param[in] A,B,C 'numbers' of rank 2
  interface number__dp_gemv
     module procedure number__dp_gemv__1
     module procedure number__dp_gemv__2
     module procedure number__dp_gemv__3
     module procedure number__dp_gemv__4
  end interface number__dp_gemv

  !> General outer product between vectors.
  !! alpha * x . y **T + A
  !! @param[in] alpha double precision scalar
  !! @param[in] x,y 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  interface number__dp_ger
     module procedure number__dp_ger__1
     module procedure number__dp_ger__2
     module procedure number__dp_ger__3
  end interface number__dp_ger

  interface number__lkh_norm
     module procedure number__lkh_norm__1
     module procedure number__lkh_norm__2
  end interface number__lkh_norm
  
contains

  !> @defgroup numbers_math_binary_ Binary Operations Between 'numbers'.
  !! Addition, subtraction, multiplication, power, and division
  !! between 'numbers'.
  !! @details Broadcasting is implmented by trying to spread the lower shape
  !! 'number' over the higher shape 'number'.
  !! To see the logic behind the definition of lower and higher shape,
  !! please refer to the function 'higher_shape' in the 'numbers_utils' module.
  !! @{
  
  !> Checks that the inputs of a binary operation are consistent.
  !! @author Filippo Monary
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  subroutine binary_check_args (x1, x2)
    implicit none
    type(number), intent(in) :: x1, x2
    call do_safe_within('op_binary_check', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(x1), err_notAlloc_, 'x1')
      call assert(is_allocated(x2), err_notAlloc_, 'x2')
      call assert(mod(product(higher_shape(x1, x2)), product(lower_shape(x1, x2))) == 0, &
           err_wrngSz_, 'x1 or x2')
    end subroutine private_check
  end subroutine binary_check_args
  
  !> Addition of two 'numbers'.
  !! ans = x1 + x2
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function number__add (x1, x2) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), pointer :: ans
    call do_safe_within('number__add', mod_numbers_math_name_, private_add)
  contains
    subroutine private_add
      call binary_check_args(x1, x2)
      call number__append(ans, higher_shape(x1, x2), has_dx(x1) .or. has_dx(x2))
      call node__append(op_add_id_, x1, x2, ans, [integer::])
      call graph__append
      if (err_free()) call op_add(x1, x2, ans)
    end subroutine private_add
  end function number__add

  !> Subtraction of two 'numbers'.
  !! ans = x1 - x2
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function number__sub (x1, x2) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), pointer :: ans
    call do_safe_within('number__sub', mod_numbers_math_name_, private_sub)
  contains
    subroutine private_sub
      call binary_check_args(x1, x2)
      call number__append(ans, higher_shape(x1, x2), has_dx(x1) .or. has_dx(x2))
      call node__append(op_sub_id_, x1, x2, ans, [integer::])
      call graph__append
      if (err_free()) call op_sub(x1, x2, ans)
    end subroutine private_sub
  end function number__sub

  !> Multiplication of two 'numbers'.
  !! ans = x1 * x2
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function number__mult (x1, x2) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), pointer :: ans
    call do_safe_within('number__mult', mod_numbers_math_name_, private_mult)
  contains
    subroutine private_mult
      call binary_check_args(x1, x2)
      call number__append(ans, higher_shape(x1, x2), has_dx(x1) .or. has_dx(x2))
      call node__append(op_mult_id_, x1, x2, ans, [integer::])
      call graph__append
      if (err_free()) call op_mult(x1, x2, ans)
    end subroutine private_mult
  end function number__mult

  !> Power of two 'numbers'.
  !! ans = x1 ** x2
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function number__pow (x1, x2) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), pointer :: ans
    call do_safe_within('number__pow', mod_numbers_math_name_, private_pow)
  contains
    subroutine private_pow
      call binary_check_args(x1, x2)
      call number__append(ans, higher_shape(x1, x2), has_dx(x1) .or. has_dx(x2))
      call node__append(op_pow_id_, x1, x2, ans, [integer::])
      call graph__append
      if (err_free()) call op_pow(x1, x2, ans)
    end subroutine private_pow
  end function number__pow

  !> Division of two 'numbers'.
  !! ans = x1 / x2
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function number__div (x1, x2) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), pointer :: ans
    call do_safe_within('number__div', mod_numbers_math_name_, private_div)
  contains
    subroutine private_div
      call binary_check_args(x1, x2)
      call number__append(ans, higher_shape(x1, x2), has_dx(x1) .or. has_dx(x2))
      call node__append(op_div_id_, x1, x2, ans, [integer::])
      call graph__append
      if (err_free()) call op_div(x1, x2, ans)
    end subroutine private_div
  end function number__div
  !> @}

  !> @defgroup numbers_math_unary_ Unary Operation on 'numbers'.
  !! Abs, exp, log, sin, cos, tan, sinh, cosh, tanh, relu, sigmoid,
  !! and swish.
  !! @{

  !> Absolute value
  !! ans = if (x > 0) x else -x
  !! @param[in] x 'number'
  function number__abs (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__abs", mod_numbers_math_name_, private_abs)
  contains
    subroutine private_abs
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_abs_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_abs(x, ans)
    end subroutine private_abs
  end function number__abs

  !> Exponential
  !! ans = e ** x
  !! @param[in] x 'number'
  function number__exp (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__exp', mod_numbers_math_name_, private_exp)
  contains
    subroutine private_exp
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_exp_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_exp(x, ans)
    end subroutine private_exp
  end function number__exp

  !> Natural Logarithm
  !! ans = log_e(x)
  !! @param[in] x 'number'
  function number__log (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__log', mod_numbers_math_name_, private_log)
  contains
    subroutine private_log
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_log_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_log(x, ans)
    end subroutine private_log
  end function number__log

  !> Sine
  !! ans = sin(x)
  !! @param[in] x 'number'
  function number__sin (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__sin', mod_numbers_math_name_, private_sin)
  contains
    subroutine private_sin
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_sin_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_sin(x, ans)
    end subroutine private_sin
  end function number__sin

  !> Cosine
  !! ans = cos(x)
  !! @param[in] x 'number'
  function number__cos (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__cos', mod_numbers_math_name_, private_cos)
  contains
    subroutine private_cos
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_cos_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_cos(x, ans)
    end subroutine private_cos
  end function number__cos

  !> Tangent
  !! ans = tan(x)
  !! @param[in] x 'number'
  function number__tan (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__tan', mod_numbers_math_name_, private_tan)
  contains
    subroutine private_tan
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_tan_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_tan(x, ans)
    end subroutine private_tan
  end function number__tan

  !> Hyperbolic Sine
  !! ans = sinh(x)
  !! @param[in] x 'number'
  function number__sinh (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__sinh_dpn0', mod_numbers_math_name_, private_sinh)
  contains
    subroutine private_sinh
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_sinh_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_sinh(x, ans)
    end subroutine private_sinh
  end function number__sinh

  !> Hyperbolic Cosine
  !! ans = cosh(x)
  !! @param[in] x 'number'
  function number__cosh (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__cosh_dpn0', mod_numbers_math_name_, private_cosh)
  contains
    subroutine private_cosh
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_cosh_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_cosh(x, ans)
    end subroutine private_cosh
  end function number__cosh

  !> Hyperbolic Tangent
  !! ans = tanh(x)
  !! @param[in] x 'number'
  function number__tanh (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__tanh_dpn0', mod_numbers_math_name_, private_tanh)
  contains
    subroutine private_tanh
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_tanh_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_tanh(x, ans)
    end subroutine private_tanh
  end function number__tanh

  !> Sigmoid
  !! ans = 1 / (1 - e ** -x)
  !! @param[in] x 'number'
  function number__sigmoid (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__sigmoid", mod_numbers_math_name_, private_sigmoid)
  contains
    subroutine private_sigmoid
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_sigmoid_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_sigmoid(x, ans)
    end subroutine private_sigmoid
  end function number__sigmoid

  !> ReLU
  !! ans = if (x > 0) x else 0
  !! @param[in] x 'number'
  function number__relu (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__relu", mod_numbers_math_name_, private_relu)
  contains
    subroutine private_relu
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_relu_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_relu(x, ans)
    end subroutine private_relu
  end function number__relu

  !> Swish
  !! ans = 1 / sigmoid(x)
  !! @param[in] x 'number'
  function number__swish (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__swish", mod_numbers_math_name_, private_swish)
  contains
    subroutine private_swish
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_swish_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_swish(x, ans)
    end subroutine private_swish
  end function number__swish
  
  !> ELU
  !! @todo create the unary parametersied group
  function number__elu (x, a) result(ans)
    implicit none
    type(number), intent(in) :: x, a
    type(number), pointer :: ans
    call do_safe_within("number__swish", mod_numbers_math_name_, private_elu)
  contains
    subroutine private_elu
      call assert(is_allocated(a), err_notAlloc_, 'a')
      call assert(mtrnk(a) == 0, err_wrngSz_, 'a')
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_elu_id_, x, a, ans, [integer::])
      call graph__append
      if (err_free()) call op_elu(x, ans, a)
    end subroutine private_elu
  end function number__elu
  
  !> Softmax
  !! ans = e ** x_i / sum_i(e ** x_i)
  !! @param[in] x 'number'
  function number__softmax__1 (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__softmax__2", mod_numbers_math_name_, private_softmax)
  contains
    subroutine private_softmax
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_softmax1_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_softmax(x, ans)
    end subroutine private_softmax
  end function number__softmax__1

  !> Softmax along the dimension k
  !! ans = [softmax(x_1), ..., softmax(x_k)]
  !! @param[in] x 'number'
  !! @param[in] k integer, dimension index
  function number__softmax__2 (x, k) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in) :: k
    type(number), pointer :: ans
    call do_safe_within("number__softmax__2", mod_numbers_math_name_, private_softmax)
  contains
    subroutine private_softmax
      call assert(mtrnk(x) > 1 .and. k <= mtrnk(x), err_wrngSz_, 'x')
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call number__append(ans, x%shp, has_dx(x))
      call node__append(op_softmax2_id_, x, ans, [k])
      call graph__append
      if (err_free()) call op_softmax(x, ans, k)
    end subroutine private_softmax
  end function number__softmax__2
  !> @}

  !> @defgroup numbers_math_gemm_ General Matrix Multiplication Operations
  !! @{

  !> Checks that the inputs of a gemm operation are consistent.
  !! @author Filippo Monary
  !! @param[out] shpout integer(2), shape of the result
  !! @param[in] ta,tb integer, transposition flags
  !! @param[in] A,B 'numbers' of rank 2
  !! @param[in] alpha,beta 'numbers' of rank 0, optional
  !! @param[in] C 'number' of rank 2, optional
  subroutine gemm_check_args (shpout, ta, tb, A, B, C, alpha, beta)
    implicit none
    integer, intent(out) :: shpout(2)
    integer, intent(in) :: ta, tb
    type(number), intent(in) :: A, B
    type(number), intent(in), optional :: alpha, beta, C
    shpout = 1
    call do_safe_within('gemm_check_args', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(A), err_notAlloc_, 'A')
      call assert(mtrnk(A) == 2, err_generic_, 'A rank /= 2')
      call assert(is_allocated(B), err_notAlloc_, 'B')
      call assert(mtrnk(B) == 2, err_generic_, 'B rank /= 2')
      if (present(alpha)) then
         call assert(is_allocated(alpha), err_alloc_, 'alpha')
         call assert(mtrnk(alpha) == 0, err_generic_, 'alpha rank /= 0')
      end if
      if (present(beta)) then
         call assert(is_allocated(beta), err_alloc_, 'alpha')
         call assert(mtrnk(beta) == 0, err_generic_, 'beta rank /= 0')
      end if
      if (present(C)) then
         call assert(is_allocated(C), err_notAlloc_, 'C')
         call assert(mtrnk(C) == 2, err_generic_, 'C rank /= 2')
         if (err_free()) call private_check_shape(C)
      else
         if (err_free()) call private_check_shape
      end if
    end subroutine private_check
    subroutine private_check_shape (C)
      type(number), intent(in), optional :: C
      integer :: mA, kA, nB, kB
      mA = merge(A%shp(1), A%shp(2), ta < 1)
      kA = merge(A%shp(2), A%shp(1), ta <1)
      nB = merge(B%shp(2), B%shp(1), tB < 1)
      kB = merge(B%shp(1), B%shp(2), tB < 1)
      call assert(kA == kB, err_wrngSz_, 'A or B')
      if (present(C)) call assert(mA == C%shp(1) .and. nB == C%shp(2), err_wrngSz_, 'C')
      if (err_free()) shpout = [mA, nB]
    end subroutine private_check_shape
  end subroutine gemm_check_args
  
  !> ans = alpha * op(A).op(B) + beta * C
  !! @param[in] transA integer, if > 1 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 1 op(B) = B**T else op(B) = B
  !! @param[in] alpha 'number' wirh rank 0
  !! @param[in] A 'number' with rank 2
  !! @param[in] B 'number' with rank 2
  !! @param[in] beta 'number' with rank 0
  !! @param[in] C 'number' with rank 2
  function number__dgemm__1 (transA, transB, alpha, A, B, beta, C) result(CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: alpha, beta
    type(number), intent(in) :: A, B, C
    type(number), pointer :: CC
    call do_safe_within('number__dgemm__1', mod_numbers_math_name_, private_dgemm)
  contains
    subroutine private_dgemm
      integer :: shpout(2)
      call gemm_check_args(shpout, transA, transB, A, B, C, alpha, beta)
      call number__append(CC, shpout, has_dx(A) .or. has_dx(B) .or. has_dx(alpha) .or. has_dx(beta))
      call node__append(op_dgemm1_id_, alpha, A, B, beta, C, CC, [transA, transB])
      call graph__append
      if (err_free()) call op_dgemm(transA, transB, alpha, A, B, beta, C, CC)
    end subroutine private_dgemm
  end function number__dgemm__1

  !> ans = alpha * op(A).op(B)
  !! @param[in] transA integer, if > 1 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 1 op(B) = B**T else op(B) = B
  !! @param[in] alpha 'number' wirh rank 0
  !! @param[in] A 'number' with rank 2
  !! @param[in] B 'number' with rank 2
  function number__dgemm__2 (alpha, transA, transB, A, B) result(C)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: alpha
    type(number), intent(in) :: A, B
    type(number), pointer :: C
    call do_safe_within('number__dgemm__2', mod_numbers_math_name_, private_dgemm)
  contains
    subroutine private_dgemm
      integer :: shpout(2)
      call gemm_check_args(shpout, transA, transB, A, B, alpha=alpha)
      call number__append(C, shpout, has_dx(A) .or. has_dx(B) .or. has_dx(alpha))
      !call err_safe(private_append)
      call node__append(op_dgemm2_id_, alpha, A, B, C, [transA, transB])
      call graph__append
      if (err_free()) call op_dgemm(alpha, transA, transB, A, B, C)
    end subroutine private_dgemm    
  end function number__dgemm__2

  !> Returns op(A).op(B) + C
  !! @param[in] transA integer, if > 1 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 1 op(B) = B**T else op(B) = B
  !! @param[in] A 'number' with rank 2
  !! @param[in] B 'number' with rank 2
  !! @param[in] C 'number' with rank 2
  function number__dgemm__3 (transA, transB, A, B, C) result(CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B, C
    type(number), pointer :: CC
    call do_safe_within('number__dgemm__3', mod_numbers_math_name_, private_dgemm)
  contains
    subroutine private_dgemm
      integer :: shpout(2)
      call gemm_check_args(shpout, transA, transB, A, B, C)
      call number__append(CC, shpout, has_dx(A) .or. has_dx(B) .or. has_dx(C))
      call node__append(op_dgemm3_id_, A, B, C, CC, [transA, transB])
      call graph__append
      if (err_free()) call op_dgemm(transA, transB, A, B, C, CC)
    end subroutine private_dgemm    
  end function number__dgemm__3

  !> ans = op(A).op(B)
  !! @param[in] transA integer, if > 1 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 1 op(B) = B**T else op(B) = B
  !! @param[in] A 'number' with rank 2
  !! @param[in] B 'number' with rank 2
  function number__dgemm__4 (transA, transB, A, B) result(C)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B
    type(number), pointer :: C
    call do_safe_within('number__dgemm__4', mod_numbers_math_name_, private_dgemm)
  contains
    subroutine private_dgemm
      integer :: shpout(2)
      call gemm_check_args(shpout, transA, transB, A, B)
      call number__append(C, shpout, has_dx(A) .or. has_dx(B))
      !call err_safe(private_append)
      call node__append(op_dgemm4_id_, A, B, C, [transA, transB])
      call graph__append
      if (err_free()) call op_dgemm(transA, transB, A, B, C)
    end subroutine private_dgemm
  end function number__dgemm__4
  !> @}

  !> @defgroup numbers_math_gemv_ General Matrix Vector Operations
  !! @{

  !> Checks that the input of a gemv operation are consistent.
  !! @param[out] shpout integer(1), shape of the output 'number'
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] A 'number', with rank 2
  !! @param[in] x 'number', with rank 1
  !! @param[in] y 'number', with y
  !! @param[in] alpha 'number', with rank 0
  !! @param[in] beta 'number', with rank 0
  subroutine gemv_check_args (shpout, trans, A, x, y, alpha, beta)
    implicit none
    integer, intent(out) :: shpout(1)
    integer, intent(in) :: trans
    type(number), intent(in) :: A, x
    type(number), intent(in), optional :: y, alpha, beta
    shpout = 1
    call do_safe_within('gemv_check_args', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(A), err_notAlloc_, 'A')
      call assert(mtrnk(A) == 2, err_generic_, 'A rank /= 2')
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(mtrnk(x) == 1, err_generic_, 'x rank /= 1')
      if (present(alpha)) then
         call assert(is_allocated(alpha), err_notAlloc_, 'alpha')
         call assert(mtrnk(alpha) == 0, err_generic_, 'alpha rank /= 0')
      end if
      if (present(beta)) then
         call assert(is_allocated(beta), err_notAlloc_, 'beta')
         call assert(mtrnk(beta) == 0, err_generic_, 'beta rank /= 0')
      end if
      if (present(y)) then
         call assert(is_allocated(y), err_notAlloc_, 'y')
         call assert(mtrnk(y) == 1, err_generic_, 'y rank /= 1')
         if(err_free()) call private_check_shape(y)
      else
         if (err_free()) call private_check_shape
      end if
    end subroutine private_check
    subroutine private_check_shape (y)
      type(number), intent(in), optional :: y
      integer :: n, m
      m = merge(A%shp(1), A%shp(2), trans < 1)
      n = merge(A%shp(2), A%shp(1), trans < 1)
      call assert(n == mtsz(x), err_wrngSz_, 'A or x')
      if (present(y)) call assert(m == mtsz(y), err_wrngSz_, 'y')
      if (err_free()) shpout = m
    end subroutine private_check_shape
  end subroutine gemv_check_args
  
  !> ans = alpha * op(A).x + beta * y
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] alpha 'number' with rank 0
  !! @param[in] A 'number' with rank 2
  !! @param[in] x 'number' with rank 1
  !! @param[in] beta 'number' with rank 0
  !! @param[in] y 'number' with rank 1
  function number__dp_gemv__1 (trans, alpha, A, x, beta, y) result(yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: alpha, beta
    type(number), intent(in) :: A, x, y
    type(number), pointer :: yy
    call do_safe_within('number__dp_gemv__1', mod_numbers_math_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      integer :: shpout(1)
      call gemv_check_args(shpout, trans, A, x, y, alpha, beta)
      call number__append(yy, shpout, has_dx(A) .or. has_dx(x) .or. has_dx(alpha) .or. has_dx(beta))
      call node__append(op_dp_gemv1_id_, alpha, x, A, beta, y, yy, [trans])
      call graph__append
      if (err_free()) call op_dp_gemv(trans, alpha, x, A, beta, y, yy)
    end subroutine private_dp_gemv
  end function number__dp_gemv__1

  !> ans = alpha * op(A).x
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] alpha 'number' with rank 0
  !! @param[in] A 'number' with rank 2
  !! @param[in] x 'number' with rank 1
  function number__dp_gemv__2 (alpha, trans, A, x) result(y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: alpha
    type(number), intent(in) :: A, x
    type(number), pointer :: y
    call do_safe_within('number__dp_gemv__2', mod_numbers_math_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      integer :: shpout(1)
      call gemv_check_args(shpout, trans, A, x, alpha=alpha)
      call number__append(y, shpout, has_dx(A) .or. has_dx(x) .or. has_dx(alpha))
      call node__append(op_dp_gemv2_id_, alpha, x, A, y, [trans])
      call graph__append
      if (err_free()) call op_dp_gemv(alpha, trans, x, A, y)
    end subroutine private_dp_gemv
  end function number__dp_gemv__2

  !> ans = op(A).x + y
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] A 'number' with rank 2
  !! @param[in] x 'number' with rank 1
  !! @param[in] y 'number' with rank 1
  function number__dp_gemv__3 (trans, A, x, y) result(yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: A, x, y
    type(number), pointer :: yy
    call do_safe_within('number__dgemm__3', mod_numbers_math_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      integer :: shpout(1)
      call gemv_check_args(shpout, trans, A, x, y)
      call number__append(yy, shpout, has_dx(A) .or. has_dx(x) .or. has_dx(y))
      call node__append(op_dp_gemv3_id_, x, A, y, yy, [trans])
      call graph__append
      if (err_free()) call op_dp_gemv(trans, x, A, y, yy)
    end subroutine private_dp_gemv
  end function number__dp_gemv__3

  !> ans = op(A).x
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] A 'number' with rank 2
  !! @param[in] x 'number' with rank 1
  function number__dp_gemv__4 (trans, A, x) result(y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: A, x
    type(number), pointer :: y
    call do_safe_within('number__dgemm__4', mod_numbers_math_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      integer :: shpout(1)
      call gemv_check_args(shpout, trans, A, x)
      call number__append(y, shpout, has_dx(A) .or. has_dx(x))
      call node__append(op_dp_gemv4_id_, x, A, y, [trans])
      call graph__append
      if (err_free()) call op_dp_gemv(trans, x, A, y)
    end subroutine private_dp_gemv
  end function number__dp_gemv__4
  !> @}

  !> @defgroup numbers_math_ger_ General Outer Product Operations
  !! @{

  !> Checks that the input to a ger operatio are consistent.
  !! @param[in] shpout integer(2), shape of the output 'number'
  !! @param[in] x 'number' with rank 1
  !! @param[in] y 'number' with rank 1
  !! @param[in] z 'number' with rank 2
  !! @param[in] alpha 'number' with rank 0
  subroutine ger_check_args (shpout, x, y, z, alpha)
    implicit none
    integer, intent(out) :: shpout(2)
    type(number), intent(in) :: x, y
    type(number), intent(in), optional :: z, alpha
    shpout = 1
    call do_safe_within('ger_check_args', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(mtrnk(x) == 1, err_generic_, 'x rank /= 1')
      call assert(is_allocated(y), err_notAlloc_, 'y')
      call assert(mtrnk(y) == 1, err_generic_, 'y rank /= 1')
      if (present(alpha))  then
         call assert(is_allocated(alpha), err_notAlloc_, 'alpha')
         call assert(mtrnk(alpha) == 0, err_generic_, 'alpha rank /= 0')
      end if
      if (present(z)) then
         call assert(is_allocated(z), err_notAlloc_, 'z')
         call assert(mtrnk(z) == 2, err_generic_, 'z rank /= 2')
         if (err_free()) call private_check_shape(z)
      else
         if (err_free()) call private_check_shape
      end if
    end subroutine private_check
    subroutine private_check_shape (z)
      type(number), intent(in), optional :: z
      if (present(z)) call assert(z%shp(1) == mtsz(x) .and. z%shp(2) == mtsz(y), err_wrngSz_, 'z')
      if (err_free()) shpout = [mtsz(x), mtsz(y)]
    end subroutine private_check_shape
  end subroutine ger_check_args

  !> ans = alpha * x.y**T + z
  !! @param[in] alpha 'number' with rank 0
  !! @param[in] x 'number' with rank 1
  !! @param[in] y 'number' with rank 1
  !! @param[in] z 'number' with rank 2
  function number__dp_ger__1 (alpha, x, y, z) result(A)
    implicit none
    type(number), intent(in) :: alpha, x, y, z
    type(number), pointer :: A
    call do_safe_within('number__dp_ger__1', mod_numbers_math_name_, private_ger)
  contains
    subroutine private_ger
      integer :: shpout(2)
      call ger_check_args(shpout, x, y, z, alpha)
      call number__append(A, shpout, has_dx(alpha) .or. has_dx(x) .or. has_dx(y) .or. has_dx(z))
      call node__append(op_dp_ger1_id_, alpha, x, y, z, A, [integer::])
      call graph__append
      if (err_free()) call op_dp_ger(alpha, x, y, z, A)
    end subroutine private_ger
  end function number__dp_ger__1

  !> ans = x.y**T + z
  !! @param[in] x 'number' with rank 1
  !! @param[in] y 'number' with rank 1
  !! @param[in] z 'number' with rank 2
  function number__dp_ger__2 (x, y, z) result(A)
    implicit none
    type(number), intent(in) :: x, y, z
    type(number), pointer :: A
    call do_safe_within('number__dp_ger__2', mod_numbers_math_name_, private_ger)
  contains
    subroutine private_ger
      integer :: shpout(2)
      call ger_check_args(shpout, x, y, z)
      call number__append(A, z%shp, has_dx(x) .or. has_dx(y) .or. has_dx(z))
      call node__append(op_dp_ger2_id_, x, y, z, A, [integer::])
      call graph__append
      if (err_free()) call op_dp_ger(x, y, z, A)
    end subroutine private_ger
  end function number__dp_ger__2

  !> ans = x.y**T
  !! @param[in] x 'number' with rank 1
  !! @param[in] y 'number' with rank 1
  function number__dp_ger__3 (x, y) result(A)
    implicit none
    type(number), intent(in) :: x, y
    type(number), pointer :: A
    call do_safe_within('number__dp_ger__3', mod_numbers_math_name_, private_ger)
  contains
    subroutine private_ger
      integer :: shpout(2)
      call ger_check_args(shpout, x, y)
      call number__append(A, [mtsz(x),mtsz(y)], has_dx(x) .or. has_dx(y))
      call node__append(op_dp_ger3_id_, x, y, A, [integer::])
      call graph__append
      if (err_free()) call op_dp_ger(x, y, A)
    end subroutine private_ger
  end function number__dp_ger__3
  !> @}

  !> Inner product betwee vectors.
  !! ans = x**T . y
  !! @param[in] x, 'number' of rank 1
  !! @param[in] y 'number' of rank 1
  function number__dp_dot (x, y) result(z)
    implicit none
    type(number), intent(in) :: x, y
    type(number), pointer :: z
    call do_safe_within("number__dp_dot", mod_numbers_math_name_, private_dot)
  contains
    subroutine private_dot
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(mtrnk(x) == 1, err_generic_, 'x rank /= 1')
      call assert(is_allocated(y), err_notAlloc_, 'y')
      call assert(mtrnk(y) == 1, err_generic_, 'y rank /= 1')
      call assert(mtsz(x) == mtsz(y), err_wrngSz_, 'x or y')
      call number__append(z, [integer::], has_dx(x) .or. has_dx(y))
      call node__append(op_dp_dot_id_, x, y, z, [integer::])
      call graph__append
      if (err_free()) call op_dp_dot(x, y, z)
    end subroutine private_dot
  end function number__dp_dot


  !> @defgroup numbers_math_invamt_ Matrix Inversion Operations
  !! @{

  !> Checks that the input of an invMat operations are consistent
  !! @param[in] x 'number' with rank 2 and square shape
  subroutine invMat_check_args (x)
    implicit none
    type(number), intent(in) :: x
    call do_safe_within("invMat_check_args", mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(mtrnk(x) == 2, err_generic_, 'x has not rank 2')
      call assert(x%shp(1) == x%shp(2), err_generic_, 'x is not square')
    end subroutine private_check
  end subroutine invMat_check_args
  
  !> Genreal Matrix inversion.
  !! @param[in] x 'number' with rank 2 and square shape
  function number__invMat (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__invMat_dpn2', mod_numbers_math_name_, private_invMat)
  contains
    subroutine private_invMat
      call invMat_check_args(x)
      call number__append(ans, [x%shp(1), x%shp(2)], has_dx(x))
      call node__append(op_invMat_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_invMat(x, ans)
    end subroutine private_invMat    
  end function number__invMat
  !> @}

  !> @defgroup number_math_reductions_ Reduction Operations
  !! @{
  
  !> Checks that the inputs of a reduction operation are consistent.
  !! @author Filippo Monary
  !! @param[in] x1 'number' with rank > 0 
  subroutine red_check_args (x1)
    implicit none
    type(number), intent(in) :: x1
    call do_safe_within('red_check_args', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(x1), err_notAlloc_, 'x1')
      call assert(mtrnk(x1) > 0, err_generic_, 'x1 is scalar')
    end subroutine private_check
  end subroutine red_check_args
  
  !> Summation.
  !! @param[in] x 'number' of rank > 0
  function number__sum__1 (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__sum', mod_numbers_math_name_, private_sum)
  contains
    subroutine private_sum
      call red_check_args(x)
      call number__append(ans, [integer::], has_dx(x))
      call node__append(op_sum_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_sum(x, ans)
    end subroutine private_sum
  end function number__sum__1

  !> Summation along a dimension.
  !! @param[in] x 'number' of rank > 0
  function number__sum__2 (x, k) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in) :: k
    type(number), pointer :: ans
    call do_safe_within("number__sum__2", mod_numbers_math_name_, private_sum)
  contains
    subroutine private_sum
      integer :: i
      call red_check_args(x)
      call number__append(ans, [pack(x%shp, [(i, i=1, mtrnk(x))] /= k)], has_dx(x))
      call node__append(op_sum2_id_, x, ans, [k])
      call graph__append
      if (err_free()) call op_sum(x, ans, k)
    end subroutine private_sum
  end function number__sum__2

  function number__product__1 (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__product__1", mod_numbers_math_name_, private_product)
  contains
    subroutine private_product
      call red_check_args(x)
      call number__append(ans, [integer::], has_dx(x))
      call node__append(op_product1_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_product(x, ans)
    end subroutine private_product
  end function number__product__1

  function number__product__2 (x, k) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in) :: k
    type(number), pointer :: ans
    call do_safe_within("number__sum__2", mod_numbers_math_name_, private_product)
  contains
    subroutine private_product
      integer :: i
      call red_check_args(x)
      call number__append(ans, [pack(x%shp, [(i, i=1, mtrnk(x))] /= k)], has_dx(x))
      call node__append(op_product2_id_, x, ans, [k])
      call graph__append
      if (err_free()) call op_product(x, ans, k)
    end subroutine private_product
  end function number__product__2
  
  !> Sum of squares.
  !! @param[in] x 'number' of rank > 0
  function number__ssq (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within('number__ssq', mod_numbers_math_name_, private_ssq)
  contains
    subroutine private_ssq
      call red_check_args(x)
      call number__append(ans, [integer::], has_dx(x))
      call node__append(op_ssq_id_, x, ans, [integer::])
      call graph__append
      if (err_free()) call op_ssq(x, ans)
    end subroutine private_ssq
  end function number__ssq
  !> @}

  !> @defgroup number_math_dprob Probability Densities
  !! @{

  !> Checks that the arguments of a dprob operations are consistent
  !! @param[in] y 'number', observations
  !! @param[in] a 'number', proibability density paramter
  !! @param[in] aname character, name identifying the a paramter
  !! @param[in] b 'number', proibability density paramter
  !! @param[in] bname character, name identifying the b paramter
  !! @param[in] c 'number', proibability density paramter
  !! @param[in] cname character, name identifying the c paramter
  subroutine dprob_check_args (y, a, aname, b, bname, c, cname)
    implicit none
    type(number), intent(in) :: y, a
    character(len=*), intent(in) :: aname
    type(number), intent(in), optional :: b, c
    character(len=*), intent(in), optional :: bname, cname
    call do_safe_within('op_dprob_check', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(y), err_notAlloc_, 'y')
      call warn(has_dx(y), warn_hasdx_, 'y')
      call assert(is_allocated(a), err_notAlloc_, aname)
      if (mtrnk(a) > 0) call assert(all(shape(y) == shape(a)), err_wrngSz_, aname)
      if (present(b)) then
         call assert(present(bname), err_generic_, 'bname, missing')
         call assert(is_allocated(b), err_notAlloc_, bname)
         if (mtrnk(b) > 0) call assert(all(shape(y) == shape(b)), err_wrngSz_, bname)
      end if
      if (present(c)) then
         call assert(present(cname), err_generic_, 'cname, missing')
         call assert(is_allocated(c), err_notAlloc_, cname)
         if (mtrnk(c) > 0) call assert(all(shape(y) == shape(c)), err_wrngSz_, cname)
      end if
    end subroutine private_check
  end subroutine dprob_check_args
  
  !> Exponential distribution - log-density
  !! @param[in] y 'number', observations
  !! @param[in] lam 'number', rate parameter
  function number__ldexp (y, lam) result(ans)
    implicit none
    type(number), intent(in) :: y, lam
    type(number), pointer :: ans
    call do_safe_within('number__ldexp', mod_numbers_math_name_, private_ldexp)
  contains
    subroutine private_ldexp
      call dprob_check_args(y, lam, 'lam')
      call number__append(ans, y%shp, has_dx(lam))
      call node__append(op_ldexp_id_, y, lam, ans, [integer::])
      call graph__append
      if (err_free()) call op_ldexp(y, lam, ans)
    end subroutine private_ldexp
  end function number__ldexp

  !> Laplace distribution - log-density
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', location parameter
  !! @param[in] lam 'number', rate parameter
  function number__ldlaplace (y, mu, lam) result(ans)
    implicit none
    type(number), intent(in) :: y, mu, lam
    type(number), pointer :: ans
    call do_safe_within('number__ldlaplace', mod_numbers_math_name_, private_ldlaplace)
  contains
    subroutine private_ldlaplace
      call dprob_check_args(y, mu, 'mu', lam, 'lam')
      call number__append(ans, y%shp, has_dx(mu) .or. has_dx(lam))
      call node__append(op_ldlaplace_id_, y, mu, lam, ans, [integer::])
      call graph__append
      if (err_free()) call op_ldlaplace(y, mu, lam, ans)
    end subroutine private_ldlaplace
  end function number__ldlaplace

  !> Beta distribution - log-density
  !! @param[in] y 'number', observations
  !! @param[in] a1 'number', shape 1 parameter
  !! @param[in] a2 'number', shape 2 parameter
  function number__ldbeta (y, a1, a2) result(ans)
    implicit none
    type(number), intent(in) :: y, a1, a2
    type(number), pointer :: ans
    call do_safe_within('number__ldbeta', mod_numbers_math_name_, private_ldbeta)
  contains
    subroutine private_ldbeta
      call dprob_check_args(y, a1, 'a1', a2, 'a2')
      call number__append(ans, y%shp, has_dx(a1) .or. has_dx(a2))
      call node__append(op_ldbeta_id_, y, a1, a2, ans, [integer::])
      call graph__append
      if (err_free()) call op_ldbeta(y, a1, a2, ans)
    end subroutine private_ldbeta
  end function number__ldbeta

  !> Gamma distribution - log-density
  !! @param[in] y 'number', observations
  !! @param[in] a 'number', shape parameter
  !! @param[in] b 'number', rate parameter
  function number__ldgamma (y, a, b) result(ans)
    implicit none
    type(number), intent(in) :: y, a, b
    type(number), pointer :: ans
    call do_safe_within('number__ldgamma', mod_numbers_math_name_, private_ldgamma)
  contains
    subroutine private_ldgamma
      call dprob_check_args(y, a, 'a', b, 'b')
      call number__append(ans, y%shp, has_dx(a) .or. has_dx(b))
      call node__append(op_ldgamma_id_, y, a, b, ans, [integer::])
      call graph__append
      if (err_free()) call op_ldgamma(y, a, b, ans)
    end subroutine private_ldgamma
  end function number__ldgamma

  !> Normal distribution - log-density
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean parameter
  !! @param[in] s 'number', standard deviation parameter
  function number__ldnorm (y, mu, s) result(ans)
    implicit none
    type(number), intent(in) :: y, mu, s
    type(number), pointer :: ans
    call do_safe_within('number__ldnorm', mod_numbers_math_name_, private_ldnorm)
  contains
    subroutine private_ldnorm
      call dprob_check_args(y, mu, 'mu', s, 's')
      call number__append(ans, y%shp, has_dx(mu) .or. has_dx(s))
      call node__append(op_ldnorm_id_, y, mu, s, ans, [integer::])
      call graph__append
      if (err_free()) call op_ldnorm(y, mu, s, ans)
    end subroutine private_ldnorm
  end function number__ldnorm

  !> Multivariante Normal Distribution - log-density
  !! @param[in] y 'number, observations
  !! @param[in] mu 'number', mean
  !! @param[in] E 'number', covariance matrix
  function number__ldmvnorm__1 (y, mu, E) result(ans)
    implicit none
    type(number), intent(in) :: y, mu, E
    type(number), pointer :: ans
    call do_safe_within('number__ldmvnorm__1', mod_numbers_math_name_, private_do)
  contains
    subroutine private_do
      call assert(is_allocated(y), err_notAlloc_, 'y')
      call assert(is_allocated(mu), err_notAlloc_, 'mu')
      call assert(is_allocated(E), err_notAlloc_, 'E')
      call assert(mtrnk(y) == 1, err_generic_, 'y has rank /= 1')
      call assert(mtrnk(mu) == 1, err_generic_, 'mu has rank /= 1')
      call assert(mtsz(y) == mtsz(mu), err_wrngSz_, 'y or mu')
      call assert(mtrnk(E) == 2, err_generic_, 'E has rank /= 2')
      call assert(E%shp(1) == E%shp(2), err_generic_, 'E is not square')
      call assert(mtsz(y) == E%shp(1), err_wrngSz_, 'y or E')
      call number__append(ans, [integer::], has_dx(mu) .or. has_dx(E))
      call node__append(op_ldmvnorm1_id_, y, mu, E, ans, [integer::])
      call graph__append
      if (err_free()) call op_ldmvnorm__1(y, mu, E, ans)
    end subroutine private_do
  end function number__ldmvnorm__1
  
  !> @}

  !> @defgroup number_math_lkh Log-Likelihoods
  !! @{
  
  !> Normal likelihood for a set of independent variables.
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number' mean parameters
  !! @param[in[ s 'number', standard deviation parameters
  function number__lkh_norm__1 (y, mu, s) result(ans)
    implicit none
    type(number), intent(in) :: y, mu, s
    type(number), pointer :: ans
    call do_safe_within('number__lkh_norm__1', mod_numbers_math_name_, private_lkh)
  contains
    subroutine private_lkh
      call assert(is_allocated(y), err_notAlloc_, 'y')
      call assert(is_allocated(mu), err_notAlloc_, 'mu')
      call assert(is_allocated(s), err_notAlloc_, 's')
      call warn(has_dx(y), warn_hasdx_, 'y')
      call assert(all(y%shp == mu%shp) .and. all(y%shp == s%shp), &
           err_wrngSz_, 'y, or mu or s')
      call number__append(ans, [integer::], has_dx(mu) .or. has_dx(s))
      call node__append(op_lkh_norm1_id_, y, mu, s, ans, [integer::])
      call graph__append
      if (err_free()) call op_lkh_norm(y, mu, s, ans)
    end subroutine private_lkh
  end function number__lkh_norm__1

  !> Weighted Normal likelihood for a set of independent variables.
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean parameters
  !! @param[in] s 'number', standard deviation parameters
  !! @param[in] w 'number', weight parameters
  function number__lkh_norm__2 (y, mu, s, w) result(ans)
    implicit none
    type(number), intent(in) :: y, mu, s, w
    type(number), pointer :: ans
    call do_safe_within('number__lkh_norm__1', mod_numbers_math_name_, private_lkh)
  contains
    subroutine private_lkh
      call assert(is_allocated(y), err_notAlloc_, 'y')
      call assert(is_allocated(mu), err_notAlloc_, 'mu')
      call assert(is_allocated(s), err_notAlloc_, 's')
      call assert(is_allocated(w), err_notAlloc_, 'w')
      call warn(has_dx(y), warn_hasdx_, 'y')
      call warn(has_dx(w), warn_hasdx_, 'w')
      call assert(all(y%shp == mu%shp) .and. all(y%shp == s%shp) .and. &
           all(y%shp == w%shp), err_wrngSz_, 'y, or mu or s or w')
      call number__append(ans, [integer::], has_dx(mu) .or. has_dx(s))
      call node__append(op_lkh_norm2_id_, y, mu, s, w, ans, [integer::])
      call graph__append
      if (err_free()) call op_lkh_norm(y, mu, s, w, ans)
    end subroutine private_lkh
  end function number__lkh_norm__2
  !> @}

  !> @defgroup numbers_math_objectives_ Objective Operations Between Numbers
  !! These procedures define the objective functions suitable for optimisation.
  !! @details No broadcasting is allowed and the shape of the target and prediction
  !! must be the same.
  
  !> Checks that the inputs of an objective operation are consistent.
  !! @author Filippo Monary
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  subroutine obj_check_args (y, yh)
    implicit none
    type(number), intent(in) :: y, yh
    call do_safe_within('obj_check', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(y), err_notAlloc_, 'x1')
      call assert(is_allocated(yh), err_notAlloc_, 'x2')
      call warn(has_dx(y), warn_generic_, "y has dx")
      call assert(mtsz(y) == mtsz(yh), err_wrngSz_, 'x1 or x2')
    end subroutine private_check
  end subroutine obj_check_args

  !> Binary Entropy - cross-entropy for a binary variable.
  !! @param[in] y 'number', target
  !! @param[in] yh 'number', prediction
  function number__bin_entropy (y, yh) result(ans)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), pointer :: ans
    call do_safe_within('number__bin_entropy', mod_numbers_math_name_, private_bin_entropy)
  contains
    subroutine private_bin_entropy
      call obj_check_args(y, yh)
      call number__append(ans, [integer::], has_dx(yh))
      call node__append(op_bin_entropy_id_, y, yh, ans, [integer::])
      call graph__append
      if (err_free()) call op_bin_entropy(y, yh, ans)
    end subroutine private_bin_entropy
  end function number__bin_entropy

  !> Cross-entropy
  !! @param[in] y 'number', target
  !! @param[in] yh 'number', prediction
  function number__cross_entropy__1 (y, yh) result(ans)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), pointer :: ans
    call do_safe_within('number__corss_entropy__1', mod_numbers_math_name_, private_cross_entropy)
  contains
    subroutine private_cross_entropy
      call obj_check_args(y, yh)
      call number__append(ans, [integer::], has_dx(yh))
      call node__append(op_cross_entropy1_id_, y, yh, ans, [integer::])
      call graph__append
      if (err_free()) call op_cross_entropy(y, yh, ans)
    end subroutine private_cross_entropy
  end function number__cross_entropy__1

  !> Mean Squared Error
  !! @param[in] y 'number', target
  !! @param[in] yh 'number', prediction
  function number__mse (y, yh) result(ans)
    type(number), intent(in) :: y, yh
    type(number), pointer :: ans
    call do_safe_within("number__mse", mod_numbers_math_name_, private_mse)
  contains
    subroutine private_mse
      call obj_check_args(y, yh)
      call number__append(ans, [integer::], has_dx(yh))
      call node__append(op_mse_id_, y, yh, ans, [integer::])
      call graph__append
      if (err_free()) call op_mse(y, yh, ans)
    end subroutine private_mse
  end function number__mse

  !> Mean Absolute Error
  !! @param[in] y 'number', target
  !! @param[in] yh 'number', prediction
  function number__mae (y, yh) result(ans)
    type(number), intent(in) :: y, yh
    type(number), pointer :: ans
    call do_safe_within("number__mae", mod_numbers_math_name_, private_mae)
  contains
    subroutine private_mae
      call obj_check_args(y, yh)
      call number__append(ans, [integer::], has_dx(yh))
      call node__append(op_mae_id_, y, yh, ans, [integer::])
      call graph__append
      if (err_free()) call op_mae(y, yh, ans)
    end subroutine private_mae
  end function number__mae
  !> @}

  !> @defgroup number_math_kernels_ Kernel Functions
  !! @{

  !> Checks the input arguments of a kernel
  subroutine krn_check_args (shpout, x1, x2, a, b)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(in) :: a, b
    integer, intent(out), allocatable :: shpout(:)
    call do_safe_within('krn_check_args', mod_numbers_math_name_, private_check)
  contains
    subroutine private_check
      call assert(is_allocated(x1), err_notAlloc_, 'x1')
      call assert(is_allocated(x2), err_notAlloc_, 'x2')
      call assert(is_allocated(a), err_notAlloc_, 'a')
      call assert(is_allocated(b), err_notAlloc_, 'b')
      call assert(mtrnk(a) == 0 .or. mtsz(a) == mtsz(x1), err_wrngSz_, 'a')
      call err_safe(private_check_shape)
    end subroutine private_check
    subroutine private_check_shape
      if (mtrnk(x1) == 1 .and. mtrnk(x2) == 1) then
         call assert(mtsz(x1) == mtsz(x2), err_wrngSz_, 'x1 or x2')
         call assert(mtrnk(b) == 0 .or. mtsz(b) == mtsz(x1), err_wrngSz_, 'b')
         if (err_free()) call alloc(shpout, [integer::], 'shoput')
      else if (mtrnk(x1) == 1 .and. mtrnk(x2) == 2) then
         call assert(mtsz(x1) == x2%shp(2), err_wrngSz_, 'x1 or x2')
         call assert(mtrnk(b) == 0 .or. mtsz(b) == mtsz(x1), err_wrngSz_, 'b') 
         if (err_free()) call alloc(shpout, [x2%shp(1)], 'shpout')
      else if (mtrnk(x1) == 2 .and. mtrnk(x2) == 2) then
         call assert(x1%shp(2) == x2%shp(2), err_wrngSz_, 'x1, or x2')
         call assert(mtrnk(b) == 0 .or. mtsz(b) == x1%shp(2), err_wrngSz_, 'b')
         if (err_free()) call alloc(shpout, [x1%shp(1), x2%shp(1)], 'shpout')
      else
         call raise_error('x1 or x2 has wrong rank', err_generic_)
      end if
    end subroutine private_check_shape
  end subroutine krn_check_args
  
  !> Squared exponential kernel
  !! @param[in] x1 'number', feature vector
  !! @param[in] x2 'number', feature vector
  !! @param[in] a 'number', amplitude parameter
  !! @param[in] b 'number', rate parameter
  function number__ksqexp (x1, x2, a, b) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2, a, b
    type(number), pointer :: ans
    call do_safe_within('number__ksqexp', mod_numbers_math_name_, private_ksqexp)
  contains
    subroutine private_ksqexp
      integer, allocatable :: shpout(:)
      call krn_check_args(shpout, x1, x2, a, b)
      call number__append(ans, shpout, has_dx(x1) .or. has_dx(x2) .or. &
           has_dx(a) .or. has_dx(b))
      call node__append(op_ksqexp_id_, x1, x2, a, b, ans, [integer::])
      call graph__append
      if (err_free()) call op_ksqexp(x1, x2, a, b, ans)
    end subroutine private_ksqexp
  end function number__ksqexp

  !> @}
  
  
end module numbers_math
