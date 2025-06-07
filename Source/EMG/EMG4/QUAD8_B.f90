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
      SUBROUTINE QUAD8_B ( R, S, T, INLAYER, SHEAR, B )
 
! Calculates the strain-displacement matrix in the cartesian local coordinate system
! for MITC8 shell at one point in isoparametric coordinates.

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
      
! **********************************************************************************************************************************
! Initialize empty matrix

      DO I=1,6                                             
        DO J=1,6*ELGP
          BB(I,J) = ZERO
        ENDDO 
      ENDDO 
 

! **********************************************************************************************************************************
! Add in-layer strain-displacement terms

      IF ( INLAYER ) THEN                                  

                                                           ! 8 sampling points
        A = ONE / DSQRT(THREE)
        POINT_R = (/ A, -A, -A,  A, ZERO, -A,     ZERO, A    /)
        POINT_S = (/ A,  A, -A, -A, A,     ZERO, -A,    ZERO /)

                                                           ! Interpolation functions for the 8 sampling points.
        IF (TYPE(1:5) == 'QUAD8') THEN

          CALL SHP2DQ ( 0, 0, ELGP, 'QUAD8_B', '', 0, R/A, S/A, 'N', PSH, DPSHG )

        ELSE

          WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
          WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
          FATAL_ERR = FATAL_ERR + 1
          CALL OUTA_HERE ( 'Y' )

        ENDIF

                                                            ! Convert interpolation functions from native node numbering
                                                            ! to Bathe interpolation point/node numbering.
        H_IS(1) = PSH(3)
        H_IS(2) = PSH(4)
        H_IS(3) = PSH(1)
        H_IS(4) = PSH(2)
        H_IS(5) = PSH(7)
        H_IS(6) = PSH(8)
        H_IS(7) = PSH(5)
        H_IS(8) = PSH(6)

        DO POINT=1,4
            ! The contribution to ε in the global (basic) basis is
            ! = hIS(i) * ( ε~_rr g^r g^r|_i  +  ε~_ss g^s g^s|_i  +  ε~_rs [g^r g^s|_i  + g^r g^s|_i^T] )

            CALL MITC_COVARIANT_BASIS( POINT_R(POINT), POINT_S(POINT), T, G_R, G_S, G_T )
            CALL MITC_CONTRAVARIANT_BASIS( G_R, G_S, G_T, G_1, G_2, G_3 )
            CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( POINT_R(POINT), POINT_S(POINT), T, 1, 4, E )

                                                           ! ε~_rr g^r g^r|_i
            CALL OUTER_PRODUCT( G_1, G_1, 3, 3, GG )
            DO I=1,6*ELGP
              !victor todo AddtoB and multiple by H_IS(POINT) on the way.
            ENDDO

            !victor todo 2 other terms


        ENDDO

        DO POINT=5,8

!victor todo

        ENDDO

      ENDIF

! **********************************************************************************************************************************
! Add transverse shear strain-displacement terms

      IF ( SHEAR ) THEN                                    

!victor todo

      ENDIF


! **********************************************************************************************************************************
! Transform from the global cartesian basis (basic) to the cartesian local basis.

! Victor todo


      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************
  
      END SUBROUTINE QUAD8_B
