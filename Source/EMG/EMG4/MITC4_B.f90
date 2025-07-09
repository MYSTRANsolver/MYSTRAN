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
      SUBROUTINE MITC4_B ( R, S, T, B )
 
! Calculates the strain-displacement matrix in the cartesian local coordinate system
! for MITC4 shell at one point in isoparametric coordinates.
!
! Reference [1]:
!  "A new MITC4+ shell element" by Ko, Lee, Bathe, 2016
!
! Reference [2]:
!  MITC4 paper "A continuum mechanics based four-node shell element for general nonlinear analysis" 
!     by Dvorkin and Bathe


      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE

      USE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface
      USE MITC_TRANSFORM_CONTRAVARIANT_TO_LOCAL_Interface

      IMPLICIT NONE 

      INTEGER(LONG)                   :: COL               ! A column (element DOF) of B. 1-24.

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix
      REAL(DOUBLE)                    :: E(6, 6*ELGP)      ! Strain-displacement matrix directly interpolated
      REAL(DOUBLE)                    :: B_SHEAR_A(6, 6*ELGP)
      REAL(DOUBLE)                    :: B_SHEAR_B(6, 6*ELGP)
      REAL(DOUBLE)                    :: B_SHEAR_C(6, 6*ELGP)
      REAL(DOUBLE)                    :: B_SHEAR_D(6, 6*ELGP)
      
! **********************************************************************************************************************************
! Initialize empty matrix

      B(:,:) = ZERO
 

! **********************************************************************************************************************************
! Add in-layer strain-displacement terms

      IF(.TRUE.) THEN
                                                           ! MITC4+ form of MITC4 according to ref [1]
         CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, .TRUE., .TRUE., E )
      ELSE
                                                           ! MITC4 according to ref [2]
         CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, 1, 4, E )
      ENDIF

      B(1:4,:) = B(1:4,:) + E(1:4,:)


! **********************************************************************************************************************************
! Add transverse shear strain-displacement terms

      ! According to ref [2]. Tying point labels are different from ref [1] but it's otherwise equivalent.

      !
      !Tying points A,B,C,D are the same as in Bathe wrt R and S (Bathe's r_1 and r_2) and same node numbering:
      ! 2     A     1
      !  +----o----+
      !  |    ^s   |
      !  |    |    |
      !B o    +->r o D
      !  |         |
      !  |         |
      !  +----o----+
      ! 3     C     4
      !

      CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO, ONE,  ZERO, 5, 6, B_SHEAR_A )
      CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION(-ONE,  ZERO, ZERO, 5, 6, B_SHEAR_B )
      CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO,-ONE,  ZERO, 5, 6, B_SHEAR_C )
      CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ONE,  ZERO, ZERO, 5, 6, B_SHEAR_D )

      DO COL=1,6*ELGP
        !e_st
        B(5, COL) = HALF * (ONE + R) * B_SHEAR_D(5, COL) + HALF * (ONE - R) * B_SHEAR_B(5, COL)
        !e_rt
        B(6, COL) = HALF * (ONE + S) * B_SHEAR_A(6, COL) + HALF * (ONE - S) * B_SHEAR_C(6, COL)
      ENDDO



! **********************************************************************************************************************************
! Transform covariant strain components from the contravariant basis to the cartesian local basis.

      CALL MITC_TRANSFORM_CONTRAVARIANT_TO_LOCAL( R, S, T, B )


      ! Double shear terms because it's now treated as vectors instead of tensors.
      B(4:6,:) = B(4:6,:) * 2

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC4_B
