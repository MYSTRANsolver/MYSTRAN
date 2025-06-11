! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
! associated documentation files (the "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
! the following conditions:                                                                              
                                                                                                         
! The above copyright notice and this permission notice shall be included in all copies or substantial   
! portions of the Software and documentation.                                                                              
                                                                                                         
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
! OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
! THE SOFTWARE.                                                                                          
! _______________________________________________________________________________________________________
                                                                                                        
! End MIT license text.                                                                                      

   MODULE MITC8_B_Interface

   INTERFACE

      SUBROUTINE MITC8_B ( R, S, T, INLAYER, SHEAR, B )

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP
      USE CONSTANTS_1, ONLY           :  ZERO, QUARTER, ONE, TWO, THREE

      USE SHP2DQ_Interface
      USE MITC_COVARIANT_BASIS_Interface
      USE MITC_CONTRAVARIANT_BASIS_Interface
      USE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface
      USE MITC_ADD_TO_B_Interface
      USE OUTER_PRODUCT_Interface
      USE MITC8_CARTESIAN_LOCAL_BASIS_Interface
      USE MITC_TRANSFORM_B_Interface

      IMPLICIT NONE 

      LOGICAL, INTENT(IN)             :: INLAYER           ! TRUE for in-layer rows (1-4)
      LOGICAL, INTENT(IN)             :: SHEAR             ! TRUE for transverse shear rows (5-6)

      INTEGER(LONG)                   :: I, J              ! Basis vector indices. 1=R, 2=S
      INTEGER(LONG)                   :: K, L              ! DO loop indices
      INTEGER(LONG)                   :: ROW               ! A row of B. 1-6.
      INTEGER(LONG)                   :: COL               ! A column (element DOF) of B. 1-48.
      INTEGER(LONG)                   :: POINT             ! Sampling point number
      INTEGER(LONG)                   :: POINT_A           ! Corner sampling point number adjacent to midside one.
      INTEGER(LONG)                   :: POINT_B           ! Corner sampling point number adjacent to midside one.

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix
      REAL(DOUBLE)                    :: E(6, 6*ELGP)      ! Strain-displacement matrix directly interpolated
      REAL(DOUBLE)                    :: A
      REAL(DOUBLE)                    :: POINT_R(8)        ! Sampling point isoparametric R coordinates
      REAL(DOUBLE)                    :: POINT_S(8)        ! Sampling point isoparametric S coordinates
      REAL(DOUBLE)                    :: POINT_COORDS_1(8) ! Shear sampling point isoparametric R or S coordinates
      REAL(DOUBLE)                    :: POINT_COORDS_2(8) ! Shear sampling point isoparametric S or R coordinates
      REAL(DOUBLE)                    :: PSH(ELGP)         ! Shape functions (interpolation functions)
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: H_IS(8)           ! Interpolation functions indexed by in-layer sampling point number.
      REAL(DOUBLE)                    :: H_IT(5)           ! Interpolation functions indexed by shear sampling point number.
      REAL(DOUBLE)                    :: G(3,3)            ! Array of 3 covariant basis vectors in basic coordinates
      REAL(DOUBLE)                    :: G_CONTRA(3,3)     ! Array of 3 contravariant basis vectors in basic coordinates
      REAL(DOUBLE)                    :: G_J_NORMALIZED(3)
      REAL(DOUBLE)                    :: GG(3,3)           ! Outer product of two contravariant basis vectors
      REAL(DOUBLE)                    :: B_1(6,6*ELGP,4)   ! Part of strain-displacement matrix for each of 4 sampling points.
      REAL(DOUBLE)                    :: B_2(6,6*ELGP,1)   ! Part of strain-displacement matrix for a sampling point.
      REAL(DOUBLE)                    :: E_AVERAGE(6,6*ELGP)
      REAL(DOUBLE)                    :: EJJ(6,6*ELGP)     ! Part of strain-displacement matrix with only row J used.
      REAL(DOUBLE)                    :: EIT(6,6*ELGP)     ! Part of strain-displacement matrix with only row RT or ST.
      REAL(DOUBLE)                    :: EIT_RA(6,6*ELGP)  ! Part of strain-displacement matrix with only row RT or ST at RA.
      REAL(DOUBLE)                    :: EIT_RB(6,6*ELGP)  ! Part of strain-displacement matrix with only row RT or ST at RB.
      REAL(DOUBLE)                    :: G_JJ
      REAL(DOUBLE)                    :: G_RS
      REAL(DOUBLE)                    :: SCALAR
      REAL(DOUBLE)                    :: DUM1(3)
      REAL(DOUBLE)                    :: COORD_1           ! R or S coordinate of a sampling point.
      REAL(DOUBLE)                    :: COORD_2           ! S or R coordinate of a sampling point.
      REAL(DOUBLE)                    :: TRANSFORM(3,3)    ! Transformation matrix

      INTRINSIC                       :: DSQRT

      END SUBROUTINE MITC8_B

   END INTERFACE

   END MODULE MITC8_B_Interface

