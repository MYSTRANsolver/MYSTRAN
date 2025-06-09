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
! Based on
! MITC8 paper "A FORMULATION OF GENERAL SHELL ELEMENTS-THE USE OF MIXED INTERPOLATION OF TENSORIAL COMPONENTS" 
!   by Dvorkin and Bathe, 1986


      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, QUARTER, ONE, TWO, THREE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE OUTA_HERE_Interface
      USE SHP2DQ_Interface
      USE MITC_COVARIANT_BASIS_Interface
      USE MITC_CONTRAVARIANT_BASIS_Interface
      USE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface
      USE QUAD8_ADD_TO_B_Interface
      USE OUTER_PRODUCT_Interface

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
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix in cartesian local coordinates
      REAL(DOUBLE)                    :: BB(6, 6*ELGP)     ! Strain-displacement matrix in basic coordinates
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
      REAL(DOUBLE)                    :: BB_1(6,6*ELGP,4)  ! Part of strain-displacement matrix for each of 4 sampling points.
      REAL(DOUBLE)                    :: BB_2(6,6*ELGP,1)  ! Part of strain-displacement matrix for a sampling point.
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

      INTRINSIC                       :: DSQRT
      
! **********************************************************************************************************************************
! Initialize empty matrix

      BB(:,:) = ZERO
 

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

          BB_1(:,:,POINT) = ZERO

          ! The contribution to ε in the global (basic) basis is
          ! = hIS(i) * ( ε~_rr g^r g^r|_i  +  ε~_ss g^s g^s|_i  +  ε~_rs [g^r g^s|_i  + g^r g^s|_i^T] )

          CALL MITC_COVARIANT_BASIS( POINT_R(POINT), POINT_S(POINT), T, G )
          CALL MITC_CONTRAVARIANT_BASIS( G, G_CONTRA )
          CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( POINT_R(POINT), POINT_S(POINT), T, 1, 4, E )

                                                           ! ε~_rr g^r g^r|_i
          CALL OUTER_PRODUCT( G_CONTRA(:,1), G_CONTRA(:,1), 3, 3, GG )
          DO COL=1,6*ELGP
            CALL QUAD8_ADD_TO_B( BB_1, POINT, COL, E(1, COL), GG )
          ENDDO

                                                           ! ε~_ss g^s g^s|_i
          CALL OUTER_PRODUCT( G_CONTRA(:,2), G_CONTRA(:,2), 3, 3, GG )
          DO COL=1,6*ELGP
            CALL QUAD8_ADD_TO_B( BB_1, POINT, COL, E(2, COL), GG )
          ENDDO

                                                           ! ε~_rs [g^r g^s|_i  + g^r g^s|_i^T]
          CALL OUTER_PRODUCT( G_CONTRA(:,1), G_CONTRA(:,2), 3, 3, GG )
          GG = GG + TRANSPOSE(GG)
          DO COL=1,6*ELGP
            CALL QUAD8_ADD_TO_B( BB_1, POINT, COL, E(4, COL), GG )
          ENDDO

          BB = BB + BB_1(:,:,POINT) * H_IS(POINT)

        ENDDO




        DO POINT=5,8
                                                           
          SELECT CASE (POINT)
            CASE (5); I=1; J=2; POINT_A=1; POINT_B=2
            CASE (6); I=2; J=1; POINT_A=2; POINT_B=3
            CASE (7); I=1; J=2; POINT_A=3; POINT_B=4
            CASE (8); I=2; J=1; POINT_A=4; POINT_B=1
          END SELECT

                                                           ! ε_ss g^s g^s |_SamplingPoint if sampling point is 5 or 7
                                                           ! ε_rr g^r g^r |_SamplingPoint if sampling point is 6 or 8
          CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, J, J, EJJ )
          CALL MITC_COVARIANT_BASIS(R, S, T, G)

          !              _
          ! Convert g to g
          !
          ! This only has an effect for non-rectangular elements where r and s aren't perpendicular.
          ! I assume g_rs means the component of g_r in the s direction.
          ! G_JJ = |G_J| where J is R or S.
          G_JJ = DSQRT( G(1,J)*G(1,J) + G(2,J)*G(2,J) + G(3,J)*G(3,J) )

          ! I assume g_rs in both formulas is a mistake and it should be g_sr in the other one.
          ! This is because g_rs in both wouldn't be symmetric between the two sets of sampling points.
          ! I decided which should be g_rs and which g_sr from slightly better convergence in a skewed element test
          ! and in a skewed curved thin strip test.
          ! This choice was also slightly better than making no adjustments to g at all.
          G_J_NORMALIZED(:) = G(:,J) / G_JJ
          G_RS = G(1,I)*G_J_NORMALIZED(1) + G(2,I)*G_J_NORMALIZED(2) + G(3,I)*G_J_NORMALIZED(3)
          G(:,I) = G(:,I) - G(:,J) * G_RS / G_JJ

          CALL MITC_CONTRAVARIANT_BASIS( G, G_CONTRA )

          BB_2(:,:,1) = ZERO
          
          CALL OUTER_PRODUCT( G_CONTRA(:,J), G_CONTRA(:,J), 3, 3, GG )
          DO COL=1,6*ELGP
            CALL QUAD8_ADD_TO_B( BB_2, 1, COL, EJJ(J, COL), GG )
          ENDDO
          
                                                           ! [1/2 (ε|_A + ε|_B)]
          E_AVERAGE = (BB_1(:,:,POINT_A) + BB_1(:,:,POINT_B)) / TWO

                      ! {g_r · [1/2 (ε|_A + ε|_B)] · g_r} g^r g^r |_SamplingPoint if sampling point is 5 or 7
                      ! {g_s · [1/2 (ε|_A + ε|_B)] · g_s} g^s g^s |_SamplingPoint if sampling point is 6 or 8
          CALL OUTER_PRODUCT( G_CONTRA(:,I), G_CONTRA(:,I), 3, 3, GG )
          DO COL=1,6*ELGP
                                                           ! g_i · [...]
            DUM1(1) = G(1,I) * E_AVERAGE(1,COL) + G(2,I) * E_AVERAGE(4,COL) + G(3,I) * E_AVERAGE(6,COL)
            DUM1(2) = G(1,I) * E_AVERAGE(4,COL) + G(2,I) * E_AVERAGE(2,COL) + G(3,I) * E_AVERAGE(5,COL)
            DUM1(3) = G(1,I) * E_AVERAGE(6,COL) + G(2,I) * E_AVERAGE(5,COL) + G(3,I) * E_AVERAGE(3,COL)
                                                           ! [...] · g_i
            SCALAR = DUM1(1) * G(1,I) + DUM1(2) * G(2,I) + DUM1(3) * G(3,I)
            CALL QUAD8_ADD_TO_B( BB_2, 1, COL, SCALAR, GG )
          ENDDO        
        
                      ! {g_r · [1/2 (ε|_A + ε|_B)] · g_s} (g^r g^s + [g^r g^s]^T |_SamplingPoint
          CALL OUTER_PRODUCT( G_CONTRA(:,1), G_CONTRA(:,2), 3, 3, GG )
          GG = GG + TRANSPOSE(GG)
          DO COL=1,6*ELGP
                                                           ! g_r · [...]
            DUM1(1) = G(1,1) * E_AVERAGE(1,COL) + G(2,1) * E_AVERAGE(4,COL) + G(3,1) * E_AVERAGE(6,COL)
            DUM1(2) = G(1,1) * E_AVERAGE(4,COL) + G(2,1) * E_AVERAGE(2,COL) + G(3,1) * E_AVERAGE(5,COL)
            DUM1(3) = G(1,1) * E_AVERAGE(6,COL) + G(2,1) * E_AVERAGE(5,COL) + G(3,1) * E_AVERAGE(3,COL)
                                                           ! [...] · g_s
            SCALAR = DUM1(1) * G(1,2) + DUM1(2) * G(2,2) + DUM1(3) * G(3,2)
            CALL QUAD8_ADD_TO_B( BB_2, 1, COL, SCALAR, GG )
          ENDDO   
        
          BB = BB + BB_2(:,:,1) * H_IS(POINT)

        ENDDO

      ENDIF




! **********************************************************************************************************************************
! Add transverse shear strain-displacement terms

      IF ( SHEAR ) THEN                                    

        DO I=1,2

          SELECT CASE (I)
            CASE (1) ; COORD_1 = R; COORD_2 = S; ROW = 6   ! ε_rt
            CASE (2) ; COORD_1 = S; COORD_2 = R; ROW = 5   ! ε_st
          END SELECT


          ! 7 sampling points for ε_rt (I=1)
          !         ^ s
          !         |
          !  4      7      3          o sampling point
          !   +-o---+---o-+           + node
          !   | 2       1 |
          !   |           |
          ! 8 + o   o   o + 6 --> r
          !   | RB  5  RA |
          !   | 3       4 |
          !   +-o---+---o-+
          !  1      5      2
          ! 

          ! 7 sampling points for ε_st (I=2)
          ! Points 2 and 4 are interchanged compared to Bathe because this makes it symmetric with ε_rt to reuse code.
          !         ^ s
          !         |
          !  7      6      5          o sampling point
          !   +-----+-----+           + node
          !   o4  SAo    1o
          !   |           |
          ! 8 +   5 o     + 4 --> r
          !   |           |
          !   o3  SBo    2o
          !   +-----+-----+
          !  1      2      3
          ! 

          ! Points 6 and 7 are called RA and RB respectively in Bathe.
          A = ONE / DSQRT(THREE)
          POINT_COORDS_1 = (/ A,   -A,   -A,    A,   ZERO, A,   -A,    ZERO /)
          POINT_COORDS_2 = (/ ONE,  ONE, -ONE, -ONE, ZERO, ZERO, ZERO, ZERO /)
          SELECT CASE (I)
            CASE (1) ; POINT_R = POINT_COORDS_1; POINT_S = POINT_COORDS_2
            CASE (2) ; POINT_R = POINT_COORDS_2; POINT_S = POINT_COORDS_1
          END SELECT

          ! Interpolation functions for the 5 numbered sampling points.
          H_IT(5) = (ONE - (COORD_1 / A) ** TWO) * (ONE - COORD_2 ** TWO)
          H_IT(1) = QUARTER * (ONE + COORD_1 / A) * (ONE + COORD_2) - QUARTER * H_IT(5)
          H_IT(2) = QUARTER * (ONE - COORD_1 / A) * (ONE + COORD_2) - QUARTER * H_IT(5)
          H_IT(3) = QUARTER * (ONE - COORD_1 / A) * (ONE - COORD_2) - QUARTER * H_IT(5)
          H_IT(4) = QUARTER * (ONE + COORD_1 / A) * (ONE - COORD_2) - QUARTER * H_IT(5)


          ! The contribution to ε in the global basis is
          !   ε~_rt (g^r g^t + g^t g^r) + ε~_st (g^s g^t + g^t g^s)
          ! = ε~_rt (g^r g^t + [g^r g^t]^T) + ε~_st (g^s g^t + [g^s g^t]^T)
          ! ε~_rt g^r g^t = sum over sampling points 1-4 of (h ε~_rt g^r g^t) evaluated at the sampling point
          ! plus a special term for sampling point 5.

          DO POINT=1,5

            ! I guess that the transverse shear sampling points are at the nodal surface (t=0, T=0). Same as MITC4.
            CALL MITC_COVARIANT_BASIS( POINT_R(POINT), POINT_S(POINT), ZERO, G )
            CALL MITC_CONTRAVARIANT_BASIS( G, G_CONTRA )
            
            IF(POINT == 5) THEN
              CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( POINT_R(6), POINT_S(6), ZERO, ROW, ROW, EIT_RA )
              CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( POINT_R(7), POINT_S(7), ZERO, ROW, ROW, EIT_RB )
              EIT = (EIT_RA + EIT_RB) / TWO
            ELSE
              CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( POINT_R(POINT), POINT_S(POINT), ZERO, ROW, ROW, EIT )
            ENDIF

            BB_2(:,:,1) = ZERO
            
            CALL OUTER_PRODUCT( G_CONTRA(:,I), G_CONTRA(:,3), 3, 3, GG )
            ! Evaluate both the g^r g^t term and the [g^r g^t]^T term together so their sum is symmetric since only
            ! the unique elements are stored in the B matrix.
            GG = GG + TRANSPOSE(GG)
            DO COL=1,6*ELGP
              CALL QUAD8_ADD_TO_B( BB_2, 1, COL, EIT(ROW, COL), GG )
            ENDDO
            
            BB = BB + BB_2(:,:,1) * H_IT(POINT)

          ENDDO
        
        
        
        
        ENDDO



      ENDIF


! **********************************************************************************************************************************
! Transform from the global cartesian basis (basic) to the cartesian local basis.

      B = BB !victor todo temporary shortcut

      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************
  
      END SUBROUTINE QUAD8_B
