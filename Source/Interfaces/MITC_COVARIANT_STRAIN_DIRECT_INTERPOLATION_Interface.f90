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

   MODULE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface

   INTERFACE

      SUBROUTINE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION ( R, S, T, ROW_FROM, ROW_TO, B )
      
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, EPROP, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, FOUR
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE SHP2DQ_Interface
      USE OUTA_HERE_Interface
      USE MITC_COVARIANT_BASIS_Interface
      USE MITC_GP_RS_Interface
      USE MITC_DIRECTOR_VECTOR_Interface

      IMPLICIT NONE 
      
      REAL(DOUBLE) , INTENT(IN)       :: R,S,T             ! Isparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix.
      REAL(DOUBLE)                    :: PSH(ELGP)         ! Shape functions
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: DPSHG3(3,ELGP)    ! Derivatives of shape functions with respect to R, S, T.
      REAL(DOUBLE)                    :: G_R(3)            ! g_r vector in basic coordinates
      REAL(DOUBLE)                    :: G_S(3)            ! g_s vector in basic coordinates
      REAL(DOUBLE)                    :: G_T(3)            ! g_t vector in basic coordinates
      REAL(DOUBLE)                    :: JAC(3,3)          ! Jacobian matrix in basic coordinates
      REAL(DOUBLE)                    :: GP_RS(2,ELGP)     ! Isoparametric coordinates of the nodes
      REAL(DOUBLE)                    :: DIRECTOR(3)       ! Director vector
      REAL(DOUBLE)                    :: THICKNESS(ELGP)   ! Element thicknesses at grid points

      INTEGER(LONG), INTENT(IN)       :: ROW_FROM          ! First row of B to generate. 1-6.
      INTEGER(LONG), INTENT(IN)       :: ROW_TO            ! Last row of B to generate. 1-6.
      INTEGER(LONG)                   :: GP                ! Element grid point number
      INTEGER(LONG)                   :: I,J,K,L           ! Loop and tensor indices
      INTEGER(LONG)                   :: ROW      

      END SUBROUTINE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION

   END INTERFACE

   END MODULE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface

