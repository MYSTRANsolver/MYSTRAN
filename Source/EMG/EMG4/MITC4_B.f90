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
      SUBROUTINE MITC4_B ( R, S, T, MEMBRANE, BENDING, SHEAR, B )
 
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
      USE MODEL_STUF, ONLY            :  ELGP, XEB
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, QUARTER, TWO, FOUR
      USE PARAMS, ONLY                :  QUAD4TYP
      USE MITC_STUF, ONLY             :  GP_RS

      USE CROSS_Interface
      USE MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface
      USE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface
      USE MITC_TRANSFORM_CONTRAVARIANT_TO_LOCAL_Interface

      IMPLICIT NONE 

      INTEGER(LONG)                   :: COL               ! A column (element DOF) of B. 1-24.
      INTEGER(LONG)                   :: GP                ! Grid point number. 1-4.

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix
      REAL(DOUBLE)                    :: X_R(3)            ! Characteristic geometry vector x_r
      REAL(DOUBLE)                    :: X_S(3)            ! Characteristic geometry vector x_s
      REAL(DOUBLE)                    :: X_D(3)            ! Characteristic geometry vector x_d (distortion vector)
      REAL(DOUBLE)                    :: BM(6, 6*ELGP)     ! Strain-displacement matrix for membrane
      REAL(DOUBLE)                    :: BB(6, 6*ELGP)     ! Strain-displacement matrix for bending
      REAL(DOUBLE)                    :: BS(6, 6*ELGP)     ! Strain-displacement matrix for shear
      REAL(DOUBLE)                    :: BM_A(6, 6*ELGP)
      REAL(DOUBLE)                    :: BM_B(6, 6*ELGP)
      REAL(DOUBLE)                    :: BM_C(6, 6*ELGP)
      REAL(DOUBLE)                    :: BM_D(6, 6*ELGP)
      REAL(DOUBLE)                    :: BM_E(6, 6*ELGP)
      REAL(DOUBLE)                    :: E(6, 6*ELGP)      ! Strain-displacement matrix directly interpolated
      REAL(DOUBLE)                    :: BS_A(6, 6*ELGP)
      REAL(DOUBLE)                    :: BS_B(6, 6*ELGP)
      REAL(DOUBLE)                    :: BS_C(6, 6*ELGP)
      REAL(DOUBLE)                    :: BS_D(6, 6*ELGP)
      REAL(DOUBLE)                    :: XRxXS(3)
      REAL(DOUBLE)                    :: MR(3)
      REAL(DOUBLE)                    :: MS(3)
      REAL(DOUBLE)                    :: DUM1(3)
      REAL(DOUBLE)                    :: c_r, c_s, d       ! Distortion variables used in ref [1]
                                                           ! Intermediate variables used in ref [1]
      REAL(DOUBLE)                    :: a_A, a_B, a_C, a_D, a_E

      LOGICAL      , INTENT(IN)       :: MEMBRANE          ! If true, generate membrane parts of B (rows 1,2,4)
      LOGICAL      , INTENT(IN)       :: BENDING           ! If true, generate bending parts of B (rows 1,2,4)
      LOGICAL      , INTENT(IN)       :: SHEAR             ! If true, generate shear parts of B (rows 5,6)

      
! **********************************************************************************************************************************
! Initialize empty matrix

      B(:,:) = ZERO
 

! **********************************************************************************************************************************
! Add in-layer strain-displacement terms

                                                           ! Characteristic geometry vectors
      X_R(:) = ZERO
      X_S(:) = ZERO
      X_D(:) = ZERO
      DO GP=1,ELGP
         X_R(:) = X_R(:) + QUARTER * GP_RS(1, GP)                * XEB(GP, :)
         X_S(:) = X_S(:) + QUARTER *                GP_RS(2, GP) * XEB(GP, :)
         X_D(:) = X_D(:) + QUARTER * GP_RS(1, GP) * GP_RS(2, GP) * XEB(GP, :)
      ENDDO

     
      IF(QUAD4TYP == 'MITC4+') THEN
                                                           ! MITC4+ according to ref [1]

         IF(MEMBRANE) THEN
                                                           ! BM at each membrane strain tying point.
            !
            !Membrane strain tying points A,B,C,D,E
            ! 2     A     1
            !  +----o----+
            !  |    ^s   |
            !  |    |    |
            !D o    +->r o C
            !  |   E     |
            !  |         |
            !  +----o----+
            ! 3     B     4
            !
            CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO,  ONE , T, X_R, X_S, X_D, .TRUE., .FALSE., 1, 1, BM_A )
            CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO, -ONE , T, X_R, X_S, X_D, .TRUE., .FALSE., 1, 1, BM_B )
            CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ONE ,  ZERO, T, X_R, X_S, X_D, .TRUE., .FALSE., 2, 2, BM_C )
            CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION(-ONE ,  ZERO, T, X_R, X_S, X_D, .TRUE., .FALSE., 2, 2, BM_D )
            CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO,  ZERO, T, X_R, X_S, X_D, .TRUE., .FALSE., 4, 4, BM_E )

                                                           ! Dual basis vectors m^r and m^s to the characteristic
                                                           ! geometry vectors x_r and x_s. From eqn (11) in ref [1].
            CALL CROSS(X_R, X_S, XRxXS)
            CALL CROSS(X_S, XRxXS, DUM1)
            MR = DUM1 / DOT_PRODUCT(X_R, DUM1)
            CALL CROSS(XRxXS, X_R, DUM1)
            MS = DUM1 / DOT_PRODUCT(X_S, DUM1)

                                                           ! c_r, c_s, d from eqn (24) in ref [1].
            c_r = DOT_PRODUCT(X_D, MR)
            c_s = DOT_PRODUCT(X_D, MS)
            d = c_r * c_r + c_s * c_s - ONE

            a_A = c_r * (c_r - 1) / (TWO * d)
            a_B = c_r * (c_r + 1) / (TWO * d)
            a_C = c_s * (c_s - 1) / (TWO * d)
            a_D = c_s * (c_s + 1) / (TWO * d)
            a_E = 2 * c_r * c_s / d

                                                           ! Eqn (27a) in ref [1]
            BM(1,:) = HALF * (ONE - TWO * a_A + S + 2 * a_A * S*S ) * BM_A(1,:)                                                    &
                    + HALF * (ONE - TWO * a_B - S + 2 * a_B * S*S ) * BM_B(1,:)                                                    &
                    + a_C * (-ONE + S*S) * BM_C(2,:)                                                                               &
                    + a_D * (-ONE + S*S) * BM_D(2,:)                                                                               &
                    + a_E * (-ONE + S*S) * BM_E(4,:)
                                                           ! Eqn (27b) in ref [1]
            BM(2,:) = a_A * (-ONE + R*R) * BM_A(1,:)                                                                               &
                    + a_B * (-ONE + R*R) * BM_B(1,:)                                                                               &
                    + HALF * (ONE - TWO * a_C + R + 2 * a_C * R*R ) * BM_C(2,:)                                                    &
                    + HALF * (ONE - TWO * a_D - R + 2 * a_D * R*R ) * BM_D(2,:)                                                    &
                    + a_E * (-ONE + R*R) * BM_E(4,:)

            BM(3,:) = ZERO
                                                           ! Eqn (27c) in ref [1]
            BM(4,:) = QUARTER * ( R + FOUR * a_A * R * S) * BM_A(1,:)                                                              &
                    + QUARTER * (-R + FOUR * a_B * R * S) * BM_B(1,:)                                                              &
                    + QUARTER * ( S + FOUR * a_C * R * S) * BM_C(2,:)                                                              &
                    + QUARTER * (-S + FOUR * a_D * R * S) * BM_D(2,:)                                                              &
                    + (1 + a_E * R * S) * BM_E(4,:)

            B(1:4,:) = B(1:4,:) + BM(1:4,:)

         ENDIF

         IF(BENDING) THEN
                                                          ! Bending is the same as the MITC4+ form of MITC4
            CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, X_R, X_S, X_D, .FALSE., .TRUE., 1, 4, BB )
            B(1:4,:) = B(1:4,:) + BB(1:4,:)
         ENDIF

      ELSEIF(QUAD4TYP == 'MITC4 ') THEN
      
         IF(.TRUE.) THEN
         
            IF(MEMBRANE) THEN
                                                           ! MITC4+ form of MITC4 according to ref [1]
               CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, X_R, X_S, X_D, .TRUE., .FALSE., 1, 4, BM )
               B(1:4,:) = B(1:4,:) + BM(1:4,:)
            ENDIF

            IF(BENDING) THEN
               CALL MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, X_R, X_S, X_D, .FALSE., .TRUE., 1, 4, BB )
               B(1:4,:) = B(1:4,:) + BB(1:4,:)
            ENDIF
            
         ELSE

                                                           ! MITC4 according to ref [2]
                                                           ! Equivalent to the MITC4+ form of MITC4
                                                           ! but can't separate membrane and bending.
                                                           ! Could be removed and this branch is never reached.
            IF(MEMBRANE .AND. BENDING) THEN

               CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( R, S, T, 1, 4, E )
               B(1:4,:) = B(1:4,:) + E(1:4,:)

            ENDIF

         ENDIF

      ENDIF



! **********************************************************************************************************************************
! Add transverse shear strain-displacement terms

      IF(SHEAR) THEN

         ! According to ref [2]. Tying point labels are different from ref [1] but it's otherwise equivalent.
         ! The same in MITC4 and MITC4+.

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

         CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO, ONE,  ZERO, 5, 6, BS_A )
         CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION(-ONE,  ZERO, ZERO, 5, 6, BS_B )
         CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ZERO,-ONE,  ZERO, 5, 6, BS_C )
         CALL MITC_COVARIANT_STRAIN_DIRECT_INTERPOLATION( ONE,  ZERO, ZERO, 5, 6, BS_D )

         DO COL=1,6*ELGP
           !e_st
           B(5, COL) = HALF * (ONE + R) * BS_D(5, COL) + HALF * (ONE - R) * BS_B(5, COL)
           !e_rt
           B(6, COL) = HALF * (ONE + S) * BS_A(6, COL) + HALF * (ONE - S) * BS_C(6, COL)
         ENDDO

      ENDIF

! **********************************************************************************************************************************
! Transform covariant strain components from the contravariant basis to the cartesian local basis.

      CALL MITC_TRANSFORM_CONTRAVARIANT_TO_LOCAL( R, S, T, B )


      ! Double shear terms because it's now treated as vectors instead of tensors.
      B(4:6,:) = B(4:6,:) * 2

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC4_B
