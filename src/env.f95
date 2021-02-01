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

module env

  implicit none

  abstract interface 
     subroutine sbr0_
     end subroutine sbr0_
  end interface
  
  !> @defgroup env_precisionKinds_ Precision Kinds
  !! Constants defining the different kind of numerical precision.
  !! @{
  integer, parameter :: sp_ = kind(1.0)  !< single precision
  integer, parameter :: dp_ = kind(1.d0) !< double precision
  !> @}
  
  !> @defgroup env_numericalConstants_ Numerical Constants
  !! Useful numerical constants.
  !! @{
  real(kind=dp_), parameter :: eps_dp_ = epsilon(1._dp_) !< machine double precision
  real(kind=sp_), parameter :: eps_sp_ = epsilon(1._sp_) !< macine single precision
  real(kind=dp_), parameter :: pi_dp_ = acos(-1._dp_)    !< pi greek double precision
  real(kind=sp_), parameter :: pi_sp_ = acos(-1._sp_)    !< pi greek single precision
  real(kind=dp_), parameter :: OO_dp_ = huge(1._dp_)     !< double precision infinite (max dp number)
  real(kind=sp_), parameter :: OO_sp_ = huge(1._sp_)     !< single precision infinire (max sp number)
  real(kind=dp_), parameter :: tol_dp_ = sqrt(eps_dp_) !< double precision tollerance for calculations (about 1.49e-8)
  real(kind=sp_), parameter :: tol_sp_ = sqrt(eps_sp_) !< single precision tollerance for calculations
  real(kind=dp_), parameter :: tiny_dp_ = 1e-6_dp_     !< double precision threshold for tiny numbers
  real(kind=sp_), parameter :: tiny_sp_ = 1e-6_sp_     !< single precision threshold for tiny numbers
  real(kind=dp_), parameter :: euler_mascheroni_dp_ = 0.57721566490153286060_dp_ !< double precision Euler-Mascheroni constant
  real(kind=dp_), parameter :: ReimannAt2_dp_ = pi_dp_**2 / 6 !< double precision value of Reimann Z function at 2
  !> @}

  !> @defgroup env_specialFileNames Special File Names
  !! Names of special files
  character(len=*), parameter :: data_ = 'data.modello' !< file containing the data source schema
  !> @}
  
  !> @defgroup  env_errorCodes_ Error Codes
  !! Integers defining different available error types
  !! @{
  integer, parameter :: err_alloc_ = 1        !< allocation 
  integer, parameter :: err_dealloc_ = 2      !< deallocation 
  integer, parameter :: err_unknwnVal_ = 3    !< unknwon value 
  integer, parameter :: err_wrngSz_ = 4       !< wrong size or shape 
  integer, parameter :: err_oorng_ = 5        !< out of range 
  integer, parameter :: err_alreadyAlloc_ = 6 !< alredy allocated 
  integer, parameter :: err_notAlloc_ = 7     !< not allocated 
  integer, parameter :: err_alreadyAssoc_ = 8 !< already associated 
  integer, parameter :: err_notAssoc_ = 9     !< not associated 
  integer, parameter :: err_alreadyInit_ = 10 !< already initialised 
  integer, parameter :: err_notInit_ = 11     !< not initialised
  integer, parameter :: err_missingArg_ = 12  !< missing argument
  integer, parameter :: err_wrngArg_ = 13     !< wrong argument
  integer, parameter :: err_wrngTyp_ = 14     !< wrong type
  integer, parameter :: err_generic_ = 15     !< generic error
  integer, parameter :: err_lapack_ = 16 !< lapack subroutine error
  !> @}

  !> @defgroup env_warningCodes_ Warning Codes
  !! Integers defining different available warning types
  !! @{
  integer, parameter :: warn_generic_ = 1 !< generic warning
  integer, parameter :: warn_hasdx_ = 2   !< number has gradient
  !> @}
  
  !> @defgroup env_numberParameters_ Number Parameters
  !! Parameters relative to numbers types
  !! @{
  
  !> @defgroup env_numberParameters_initRegisterParameters_ Init register parameters
  !! Parameters defining the size and the slot of the init number 'init' register.
  !! @{
  integer, parameter :: number_init_sz_ = 4 !< size of the init register
  integer, parameter :: number_init_idi_ = 1 !< index of the init register for x%id
  integer, parameter :: number_init_shpi_ = 2 !< index of the init register for x%shp
  integer, parameter :: number_init_vi_ = 3   !< index of the init register for x%v
  integer, parameter :: number_init_dvi_ = 4  !< index of the init register for x%dv
  !> @}
  
  !> @defgroup env_numberParameters_initFalgs Initialisation flags
  !! Falgs signaling the initialitisation status for 'number' elements.
  !! @{
  integer, parameter :: init_null_ = 0  !< null, not allocated
  integer, parameter :: init_alloc_ = 1 !< allocated
  integer, parameter :: init_assoc_ = 2 !< associated
  integer, parameter :: init_vals_(3) = [init_null_, init_alloc_, init_assoc_] !< set of defined initialisation values
  !> @}
  !! @}
  
  !> @defgroup ev_operatorIds_ Operator IDs
  !! Integer parameters defininig the operators applied.
  !! @{
  
  !> @defgroup env_operatorIds_unaryOperators_ Unary Operators
  !! Take a number as input, and returs a number of the same rank and shappe.
  !! @{
  integer, parameter :: op_unary_start_ = 0 !< startgin offset for numbering unary operators
  integer, parameter :: op_exp_id_ = 1 + op_unary_start_ !< exponential id
  integer, parameter :: op_log_id_ = 2 + op_unary_start_ !< log id
  integer, parameter :: op_sin_id_ = 3 + op_unary_start_ !< sin id
  integer, parameter :: op_cos_id_ = 4 + op_unary_start_ !< cos id
  integer, parameter :: op_tan_id_ = 5 + op_unary_start_ !< tan id
  integer, parameter :: op_sinh_id_ = 6 + op_unary_start_ !< sinh id
  integer, parameter :: op_cosh_id_ = 7 + op_unary_start_ !< cosh id
  integer, parameter :: op_tanh_id_ = 8 + op_unary_start_ !< tanh id
  integer, parameter :: op_invMat_id_ = 9 + op_unary_start_ !< matrix inverstion id
  integer, parameter :: op_invSymMat_id_ = 10 + op_unary_start_ !< symmetric matrix inversion id
  integer, parameter :: op_sigmoid_id_ = 11 + op_unary_start_   !< sigmoid id
  integer, parameter :: op_softmax1_id_ = 12 + op_unary_start_ !< softmax id
  integer, parameter :: op_softmax2_id_ = 13 + op_unary_start_ !< softmax by axis id
  integer, parameter :: op_relu_id_ = 14 + op_unary_start_ !< relu id
  integer, parameter :: op_swish_id_ = 15 + op_unary_start_ !< swish id
  integer, parameter :: op_elu_id_ = 16 + op_unary_start_ !< elu id
  integer, parameter :: op_abs_id_ = 17 + op_unary_start_ !< abs id
  !> @}
  
  !> @defgroup env_operatorIds_binaryOpertors_ Binary Operators
  !! Take two 'numbers' as inputs, and return a number with
  !! rank the higer rank and with shape the shape of the higher rank number. 
  !! @{
  integer, parameter :: op_binary_start_ = 100            !< starting offset for numbering binary operators
  integer, parameter :: op_add_id_ = 1 + op_binary_start_ !< addition 
  integer, parameter :: op_sub_id_ = 2 + op_binary_start_ !< subtraction 
  integer, parameter :: op_mult_id_ = 3 + op_binary_start_ !< multiplication 
  integer, parameter :: op_pow_id_ = 4 + op_binary_start_  !< power 
  integer, parameter :: op_div_id_ = 5 + op_binary_start_  !< division 
  !> @}
  
  !> @defgroup env_operatorsIds_reductionOperators Reduction Operators
  !! Take a 'number' with rank > 0 and return a 'number' with rank = 0.
  !! @{
  integer, parameter :: op_reduction_start_ = 200            !< starting offset for numbering reduction operators
  integer, parameter :: op_sum_id_ = 1 + op_reduction_start_ !< sum along all dimensions 
  integer, parameter :: op_sum2_id_ = 2 + op_reduction_start_ !< sum along one dimension
  integer, parameter :: op_product1_id_ = 3 + op_reduction_start_
  integer, parameter :: op_product2_id_ = 4 + op_reduction_start_
  integer, parameter :: op_ssq_id_ = 5 + op_reduction_start_ !< sum of squares
  !> @}
  
  !> @defgroup env_operatorIds_matrixMultiplication Matrix / Vector Multiplication Operators
  !! Upper case letters are matrices, lower case letters are vectors. 
  !! @{
  integer, parameter :: op_mm_start_ = 300 !< starting offset for numbering matrix/vector multiplication  operators
  integer, parameter :: op_dgemm1_id_ = 1 + op_mm_start_ !< dgemm (alpha * op(A).op(B) + beta * C) id
  integer, parameter :: op_dgemm2_id_ = 2 + op_mm_start_ !< dgemm (alpha * op(A).op(B) + C) id
  integer, parameter :: op_dgemm3_id_ = 3 + op_mm_start_ !< dgemm (op(A).op(B) + beta * C) id
  integer, parameter :: op_dgemm4_id_ = 4 + op_mm_start_ !< dgemm (op(A).op(B)) id
  integer, parameter :: op_dp_gemv1_id_ = 5 + op_mm_start_ !< dgemv (alpha op(A).x + beta * y) id
  integer, parameter :: op_dp_gemv2_id_ = 6 + op_mm_start_ !< dgemv (op(A).x + y) id
  integer, parameter :: op_dp_gemv3_id_ = 7 + op_mm_start_ !< dgemv (alpha op(A).x) id
  integer, parameter :: op_dp_gemv4_id_ = 8 + op_mm_start_ !< dgemv (op(A).x) id
  integer, parameter :: op_dp_dot_id_ = 9 + op_mm_start_ !< ddot (x**T.y) id
  integer, parameter :: op_dp_ger1_id_ = 10 + op_mm_start_ !<dger (alpha * x.y**T + A) id
  integer, parameter :: op_dp_ger2_id_ = 11 + op_mm_start_ !<dger (x.y**T + A) id
  integer, parameter :: op_dp_ger3_id_ = 12 + op_mm_start_ !<dger (x.y**T) id
  integer, parameter :: op_solve_id_ = 13 + op_mm_start_ !< solve of linear euqations ig
  integer, parameter :: op_solveSym_id_ = 14 + op_mm_start_ !< solve symmetric system of linear equations id
  !> @}
  
  !> @defgroup env_operatorIds_modifiers Modifiers Operators
  !! Slincing, binding and reshaping.
  !! @{
  integer, parameter :: op_modif_start_ = 400 !< starting offset for modifiers operators
  integer, parameter :: op_slice_id_ = 1 + op_modif_start_ !< generic slice id
  integer, parameter :: op_flat_slice_id_ = 2 + op_modif_start_ !< flat slice id
  integer, parameter :: op_contiguous_slice_id_ = 3 + op_modif_start_ !< contiguous (with pointer) slice id
  integer, parameter :: op_reshape_id_ = 4 + op_modif_start_ !< reshape id
  integer, parameter :: op_drop_shape_id_ = 5 + op_modif_start_ !< drop id
  integer, parameter :: op_bind_id_ = 6 + op_modif_start_       !< bind id
  integer, parameter :: op_embeddings_id_ = 7 + op_modif_start_ !< embeddings id
  integer, parameter :: op_feed_id_ = 8 + op_modif_start_
  !> @}

  !> @defgroup env_operatorIds_objectives Objective IDs
  !! Operators implementing objective functions
  !! @{
  integer, parameter :: op_obj_start_ = 500
  integer, parameter :: op_bin_entropy_id_ = 1 + op_obj_start_ !< binary entropy id
  integer, parameter :: op_cross_entropy1_id_ = 2 + op_obj_start_ !< cross-entropy id
  integer, parameter :: op_cross_entropy2_id_ = 3 + op_obj_start_ !< cross-entropy by axis id
  integer, parameter :: op_mse_id_ = 4 + op_obj_start_            !< mean squared error id
  integer, parameter :: op_mae_id_ = 5 + op_obj_start_            !< mean absolute error id
  !> @}

  !> @defgroup env_operatorIds_ldprob_ log-probability Density Distributions
  !! Opertrors implementing the natural logarithm of probability density distributions
  integer, parameter :: op_ldprob_start_ = 600 !< starting offset for log-probability density distributions
  integer, parameter :: op_ldexp_id_= 1 + op_ldprob_start_ !< exponential
  integer, parameter :: op_ldlaplace_id_ = 2 + op_ldprob_start_ !< laplace 
  integer, parameter :: op_ldbeta_id_ = 3 + op_ldprob_start_    !< beta
  integer, parameter :: op_ldgamma_id_ = 4 + op_ldprob_start_   !< gamma
  integer, parameter :: op_ldnorm_id_ = 5 + op_ldprob_start_    !< normal
  integer, parameter :: op_ldmvnorm1_id_ = 6 + op_ldprob_start_ !< multivariate normal
  !> @}
  
  !> @defgroup env_operatorIds_lkh_ Likelihood Operator IDs
  !! Operators implementing likleihood functions
  integer, parameter :: op_lkh_start_ = 700 !< start offset for log-likelihoods
  integer, parameter :: op_lkh_norm1_id_ = 1 + op_lkh_start_ !< normal likelihood for i.i.d. variables 
  integer, parameter :: op_lkh_norm2_id_ = 2 + op_lkh_start_ !< weighted normal likelihood for i.i.d. variables
  ! integer, parameter :: op_lkh_norm3_id_ = 3 + op_lkh_start_
  ! integer, parameter :: op_lkh_norm4_id_ = 4 + op_lkh_start_
  !> @}

  !> @defgroup env_operatorsIds_kernels Kernel Operator IDs
  !! Operators implementing kernel functions
  !! @{
  integer, parameter :: op_krn_start_ = 800 !< starting offset for kernel functions
  integer, parameter :: op_ksqexp_id_ = 1 + op_krn_start_ !< square exponential kernel
  !! @}

  !> @}

end module env



	
