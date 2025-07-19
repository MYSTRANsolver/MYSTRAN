! #################################################################################################################################
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
      SUBROUTINE MITC_TRANSFORM_CONTRAVARIANT_TO_LOCAL ( R, S, T, B )

! Transform covariant strain components from the contravariant basis to the cartesian local basis.
!   ε_kl = ε~_ij (g^i dot e_k)(g^j dot e_l)

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP
      USE CONSTANTS_1, ONLY           :  ZERO, QUARTER, ONE, TWO, THREE

      USE MITC_COVARIANT_BASIS_Interface
      USE MITC_CONTRAVARIANT_BASIS_Interface
      USE MITC4_CARTESIAN_LOCAL_BASIS_Interface
      USE MITC_TRANSFORM_B_Interface
      USE MATMULT_FFF_Interface
      

      IMPLICIT NONE 

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix
      REAL(DOUBLE)                    :: TRANSFORM(3,3)    ! Transformation matrix
      REAL(DOUBLE)                    :: G(3,3)            ! Array of 3 covariant basis vectors in basic coordinates
      REAL(DOUBLE)                    :: G_CONTRA(3,3)     ! Array of 3 contravariant basis vectors in basic coordinates
      REAL(DOUBLE)                    :: E(3,3)            ! Basis vectors

! **********************************************************************************************************************************


      CALL MITC_COVARIANT_BASIS( R, S, T, G )
      CALL MITC_CONTRAVARIANT_BASIS( G, G_CONTRA )
      E = MITC4_CARTESIAN_LOCAL_BASIS(R, S, T)

      ! The 3x3 transformation matrix A is defined by
      !    A_xy = g^y dot e_x
      ! or two transformation matrices multiplied
      CALL MATMULT_FFF(TRANSPOSE(E), G_CONTRA, 3, 3, 3, TRANSFORM)

      CALL MITC_TRANSFORM_B( TRANSFORM, B)


      RETURN

! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_TRANSFORM_CONTRAVARIANT_TO_LOCAL
