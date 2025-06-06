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
      REAL(DOUBLE)                    :: THICKNESS         ! Uniform element thickness

      INTEGER(LONG), INTENT(IN)       :: ROW_FROM          ! First row of B to generate. 1-6.
      INTEGER(LONG), INTENT(IN)       :: ROW_TO            ! Last row of B to generate. 1-6.
      INTEGER(LONG)                   :: GP                ! Element grid point number
      INTEGER(LONG)                   :: I,J,K,L           ! Loop and tensor indices
      INTEGER(LONG)                   :: ROW               

! **********************************************************************************************************************************

! Thickness is treated as uniform.
! To allow grid point thicknesses, a different value should be used for each node, interpolating to midside nodes.
      THICKNESS = EPROP(1)
!victor todo see if the thicknessses used below are at the nodes, and if so, make this an array GP_THICKNESS like in MITC_COVARIANT_BASIS

! Shape function derivatives at R,S
      IF (TYPE(1:5) == 'QUAD8') THEN

        CALL SHP2DQ ( 0, 0, ELGP, 'MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION', '', 0, R, S, 'N', PSH, DPSHG )

      ELSE

        WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
        WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF
                                                           ! Extend shape function derivates to include 
                                                           ! thickness direction (0) for convenience.
      DO GP=1,ELGP
        DO J=1,2
          DPSHG3(J,GP) = DPSHG(J,GP)
        ENDDO
        DPSHG3(3,GP) = ZERO
      ENDDO


! Jacobian matrix
      CALL MITC_COVARIANT_BASIS( R, S, T, G_R, G_S, G_T )
      DO I=1,3
        JAC(I,1) = G_R(I)
        JAC(I,2) = G_S(I)
        JAC(I,3) = G_T(I)
      ENDDO

! Build strain-displacement matrix

      GP_RS = MITC_GP_RS()

      DO ROW=1,6
        DO GP=1,ELGP
        
          DIRECTOR = MITC_DIRECTOR_VECTOR( GP_RS(1,GP), GP_RS(2,GP) )

          K = (GP-1) * 6

          IF ((ROW >= ROW_FROM) .AND. (ROW <= ROW_TO) .AND. ROW /= 3) THEN

                                                           ! Tensor indices for the row
            SELECT CASE (ROW)
              CASE (1); I=1; J=1
              CASE (2); I=2; J=2
              CASE (4); I=1; J=2
              CASE (5); I=2; J=3
              CASE (6); I=1; J=3
            END SELECT

                                                ! e_ij = sum over nodes [ 1/2 d/di u_0 dot g_j  +  1/2 d/dj u_0 dot g_i  + ...
            B(ROW, K+1) = (DPSHG3(I,GP) * JAC(1,J) + DPSHG3(J,GP) * JAC(1,I)) / TWO
            B(ROW, K+2) = (DPSHG3(I,GP) * JAC(2,J) + DPSHG3(J,GP) * JAC(2,I)) / TWO
            B(ROW, K+3) = (DPSHG3(I,GP) * JAC(3,J) + DPSHG3(J,GP) * JAC(3,I)) / TWO
                                                           !... 1/4 d/di (t h phi) dot g_j  + ...
            B(ROW, K+4) = THICKNESS * T * DPSHG3(I,GP) * (JAC(3,J) * DIRECTOR(2) - JAC(2,J) * DIRECTOR(3)) / FOUR
            B(ROW, K+5) = THICKNESS * T * DPSHG3(I,GP) * (JAC(1,J) * DIRECTOR(3) - JAC(3,J) * DIRECTOR(1)) / FOUR
            B(ROW, K+6) = THICKNESS * T * DPSHG3(I,GP) * (JAC(2,J) * DIRECTOR(1) - JAC(1,J) * DIRECTOR(2)) / FOUR
                                                           !... 1/4 d/dj (t h phi) dot g_i  ]
            IF (J == 3) THEN
                                                           ! Differentiating in the thickness direction is special
                                                           ! 1/4 d/dt (t h phi) = 1/4 h phi
              B(ROW, K+4) = B(ROW, K+4) + THICKNESS * PSH(GP) * (JAC(3,I) * DIRECTOR(2) - JAC(2,I) * DIRECTOR(3)) / FOUR
              B(ROW, K+5) = B(ROW, K+5) + THICKNESS * PSH(GP) * (JAC(1,I) * DIRECTOR(3) - JAC(3,I) * DIRECTOR(1)) / FOUR
              B(ROW, K+6) = B(ROW, K+6) + THICKNESS * PSH(GP) * (JAC(2,I) * DIRECTOR(1) - JAC(1,I) * DIRECTOR(2)) / FOUR
            ELSE            
                                                           ! 1/4 d/dr (t h phi) = 1/4 t h dN/dr phi
                                                           ! 1/4 d/ds (t h phi) = 1/4 t h dN/ds phi
              B(ROW, K+4) = B(ROW, K+4) + THICKNESS * T * DPSHG3(J,GP) * (JAC(3,I) * DIRECTOR(2) - JAC(2,I) * DIRECTOR(3)) / FOUR
              B(ROW, K+5) = B(ROW, K+5) + THICKNESS * T * DPSHG3(J,GP) * (JAC(1,I) * DIRECTOR(3) - JAC(3,I) * DIRECTOR(1)) / FOUR
              B(ROW, K+6) = B(ROW, K+6) + THICKNESS * T * DPSHG3(J,GP) * (JAC(2,I) * DIRECTOR(1) - JAC(1,I) * DIRECTOR(2)) / FOUR
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
