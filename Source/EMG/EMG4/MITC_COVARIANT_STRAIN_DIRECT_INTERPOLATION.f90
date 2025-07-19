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
      SUBROUTINE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION ( R, S, T, ROW_FROM, ROW_TO, B )
 
! Covariant strain-displacement components at point (R,S,T) directly evaluated from the displacement and rotation interpolations
!
!        Grid point 1        Grid point 2      ...  
!      ux uy uz rx ry rz   ux uy uz rx ry rz
! 11 [ #  #  #  #  #  #  | #  #  #  #  #  #  |     ]
! 22 [ #  #  #  #  #  #  | #  #  #  #  #  #  |     ]
! 33 [ 0  0  0  0  0  0  | 0  0  0  0  0  0  |     ]
! 12 [ #  #  #  #  #  #  | #  #  #  #  #  #  | ... ]
! 23 [ #  #  #  #  #  #  | #  #  #  #  #  #  |     ]
! 13 [ #  #  #  #  #  #  | #  #  #  #  #  #  |     ]


      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, FOUR
      USE MITC_STUF, ONLY             :  DIRECTOR, DIR_THICKNESS

      USE MITC_SHAPE_FUNCTIONS_Interface
      USE MITC_COVARIANT_BASIS_Interface

      IMPLICIT NONE 
      
      REAL(DOUBLE) , INTENT(IN)       :: R,S,T             ! Isparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix.
      REAL(DOUBLE)                    :: PSH(ELGP)         ! Shape functions
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: DPSHG3(3,ELGP)    ! Derivatives of shape functions with respect to R, S, T.
      REAL(DOUBLE)                    :: G(3,3)            ! Covariant basis vectors (Jacobian matrix) in basic coordinates

      INTEGER(LONG), INTENT(IN)       :: ROW_FROM          ! First row of B to generate. Strain component index 1-6.
      INTEGER(LONG), INTENT(IN)       :: ROW_TO            ! Last row of B to generate. Strain component index 1-6.
      INTEGER(LONG)                   :: GP                ! Element grid point number
      INTEGER(LONG)                   :: I,J,K,L           ! Loop and tensor indices
      INTEGER(LONG)                   :: ROW               
      
! **********************************************************************************************************************************

! Reference [2]:
!  MITC4 paper "A continuum mechanics based four-node shell element for general nonlinear analysis" 
!     by Dvorkin and Bathe

                                                           ! Shape function derivatives at R,S
      CALL MITC_SHAPE_FUNCTIONS(R, S, PSH, DPSHG)

                                                           ! Extend shape function derivates to include 
                                                           ! thickness direction (0) for convenience.
      DO GP=1,ELGP
        DO J=1,2
          DPSHG3(J,GP) = DPSHG(J,GP)
        ENDDO
        DPSHG3(3,GP) = ZERO
      ENDDO


! Jacobian matrix
      CALL MITC_COVARIANT_BASIS( R, S, T, G )

! Build strain-displacement matrix
! From "A continuum mechanics based four-node shell element for general nonlinear analysis" by Dvorkin and Bathe
! equations (21a), (22a), (23a), (24a)

      DO ROW=1,6
        DO GP=1,ELGP

          K = (GP-1) * 6

          IF ((ROW >= ROW_FROM) .AND. (ROW <= ROW_TO) .AND. ROW /= 3) THEN

                                                           ! Tensor indices for the row
            SELECT CASE (ROW)
              CASE (1); I=1; J=1                           ! In-layer normal strain
              CASE (2); I=2; J=2                           ! In-layer normal strain
              CASE (4); I=1; J=2                           ! In-layer shear strain
              CASE (5); I=2; J=3                           ! Transverse shear strain
              CASE (6); I=1; J=3                           ! Transverse shear strain
            END SELECT

                                                ! e_ij = sum over nodes [ 1/2 d/di u_0 dot g_j  +  1/2 d/dj u_0 dot g_i  + ...
            B(ROW, K+1) = (DPSHG3(I,GP) * G(1,J) + DPSHG3(J,GP) * G(1,I)) / TWO
            B(ROW, K+2) = (DPSHG3(I,GP) * G(2,J) + DPSHG3(J,GP) * G(2,I)) / TWO
            B(ROW, K+3) = (DPSHG3(I,GP) * G(3,J) + DPSHG3(J,GP) * G(3,I)) / TWO
                                                           !... 1/4 d/di (t h phi) dot g_j  + ...
            B(ROW, K+4) = DIR_THICKNESS(GP) * T * DPSHG3(I,GP) * (G(3,J) * DIRECTOR(GP,2) - G(2,J) * DIRECTOR(GP,3)) / FOUR
            B(ROW, K+5) = DIR_THICKNESS(GP) * T * DPSHG3(I,GP) * (G(1,J) * DIRECTOR(GP,3) - G(3,J) * DIRECTOR(GP,1)) / FOUR
            B(ROW, K+6) = DIR_THICKNESS(GP) * T * DPSHG3(I,GP) * (G(2,J) * DIRECTOR(GP,1) - G(1,J) * DIRECTOR(GP,2)) / FOUR
                                                           !... 1/4 d/dj (t h phi) dot g_i  ]
            IF (J == 3) THEN
                                                           ! Transverse shear rows are special. Eqn. (23a), (24a)
                                                           ! 1/4 d/dt (t h phi) = 1/4 h phi
              B(ROW, K+4) = B(ROW, K+4) + DIR_THICKNESS(GP) * PSH(GP) * (G(3,I) * DIRECTOR(GP,2) - G(2,I) * DIRECTOR(GP,3)) / FOUR
              B(ROW, K+5) = B(ROW, K+5) + DIR_THICKNESS(GP) * PSH(GP) * (G(1,I) * DIRECTOR(GP,3) - G(3,I) * DIRECTOR(GP,1)) / FOUR
              B(ROW, K+6) = B(ROW, K+6) + DIR_THICKNESS(GP) * PSH(GP) * (G(2,I) * DIRECTOR(GP,1) - G(1,I) * DIRECTOR(GP,2)) / FOUR
            ELSE            
                                                           ! 1/4 d/dr (t h phi) = 1/4 t h dN/dr phi
                                                           ! 1/4 d/ds (t h phi) = 1/4 t h dN/ds phi
              B(ROW, K+4) = B(ROW, K+4) +                                                                                          &
                DIR_THICKNESS(GP) * T * DPSHG3(J,GP) * (G(3,I) * DIRECTOR(GP,2) - G(2,I) * DIRECTOR(GP,3)) / FOUR
              B(ROW, K+5) = B(ROW, K+5) +                                                                                          &
                DIR_THICKNESS(GP) * T * DPSHG3(J,GP) * (G(1,I) * DIRECTOR(GP,3) - G(3,I) * DIRECTOR(GP,1)) / FOUR
              B(ROW, K+6) = B(ROW, K+6) +                                                                                          &
                DIR_THICKNESS(GP) * T * DPSHG3(J,GP) * (G(2,I) * DIRECTOR(GP,1) - G(1,I) * DIRECTOR(GP,2)) / FOUR
            ENDIF


          ELSE
                                                           ! Zero row
            DO L=1,6
              B(ROW, K+L) = 0
            ENDDO

          ENDIF

        ENDDO
      ENDDO



      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION
