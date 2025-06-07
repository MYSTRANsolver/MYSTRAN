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

   MODULE QUAD8_B_Interface

   INTERFACE

      SUBROUTINE QUAD8_B ( R, S, T, INLAYER, SHEAR, B )

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, THREE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE OUTA_HERE_Interface
      USE SHP2DQ_Interface
      USE MITC_COVARIANT_BASIS_Interface
      USE MITC_CONTRAVARIANT_BASIS_Interface
      USE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface
      USE OUTER_PRODUCT_Interface

      IMPLICIT NONE 

      LOGICAL, INTENT(IN)             :: INLAYER           ! TRUE for in-layer rows (1-4)
      LOGICAL, INTENT(IN)             :: SHEAR             ! TRUE for transverse shear rows (5-6)

      INTEGER(LONG)                   :: I, J              ! DO loop indices
      INTEGER(LONG)                   :: POINT             ! Sampling point number

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix in cartesian local coordinates
      REAL(DOUBLE)                    :: BB(6, 6*ELGP)     ! Strain-displacement matrix in basic coordinates
      REAL(DOUBLE)                    :: E(6, 6*ELGP)      ! Strain-displacement matrix directly interpolated
      REAL(DOUBLE)                    :: A
      REAL(DOUBLE)                    :: POINT_R(8)        ! Sampling point isoparametric R coordinates
      REAL(DOUBLE)                    :: POINT_S(8)        ! Sampling point isoparametric S coordinates
      REAL(DOUBLE)                    :: PSH(ELGP)         ! Shape functions (interpolation functions)
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: H_IS(8)           ! Interpolation functions indexed by sampling point number.
      REAL(DOUBLE)                    :: G_R(3)            ! Covariant basis vector g_r in basic coordinates
      REAL(DOUBLE)                    :: G_S(3)            ! Covariant basis vector g_s in basic coordinates
      REAL(DOUBLE)                    :: G_T(3)            ! Covariant basis vector g_t in basic coordinates
      REAL(DOUBLE)                    :: G_1(3)            ! Contravariant basis vector g^1 in basic coordinates
      REAL(DOUBLE)                    :: G_2(3)            ! Contravariant basis vector g^2 in basic coordinates
      REAL(DOUBLE)                    :: G_3(3)            ! Contravariant basis vector g^3 in basic coordinates
      REAL(DOUBLE)                    :: GG(3,3)           ! Outer product of two contravariant basis vectors

      INTRINSIC                       :: DSQRT

      END SUBROUTINE QUAD8_B

   END INTERFACE

   END MODULE QUAD8_B_Interface

