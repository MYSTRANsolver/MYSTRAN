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
      SUBROUTINE MITC4 ( OPT, INT_ELEM_ID )

! Calculates, or calls subr's to calculate, quadrilateral element matrices:

!  1) ME        = element mass matrix                  , if OPT(1) = 'Y'
!  2) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  3) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  4) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  5) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  6) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y'

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS, NTSUB, NSUB
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  NUM_EMG_FATAL_ERRS, PCOMP_PROPS, ELGP, ES, KE, EM, EB, ET, BE1, BE2, BE3, PHI_SQ,         &
                                         FCONV, EPROP, PTE, ALPVEC, TREF, DT, PPE, PRESS, MASS_PER_UNIT_AREA,                      &
                                         NUM_PLIES, PCOMP_LAM, PLY_NUM, TPLY, STRESS, KED
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, FOUR

      USE MITC_INITIALIZE_Interface
      USE ORDER_GAUSS_Interface
      USE OUTA_HERE_Interface
      USE MATMULT_FFF_Interface
      USE MATMULT_FFF_T_Interface
      USE MITC_DETJ_Interface
      USE MITC4_B_Interface
      USE MITC4_CARTESIAN_LOCAL_BASIS_Interface
      USE MITC_TRANSFORM_B_Interface
      USE PLANE_COORD_TRANS_21_Interface
      USE MATL_TRANSFORM_MATRIX_Interface
      USE MATMULT_FFF_Interface
      USE MATMULT_FFF_T_Interface
      USE MITC_ELASTICITY_Interface
      USE CROSS_Interface
      USE MITC_SHAPE_FUNCTIONS_Interface
      USE MITC_COVARIANT_BASIS_Interface
      USE LUMP_MASS_Interface

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MITC4'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID
      INTEGER(LONG), PARAMETER        :: IORD_IJ = 2       ! Integration order for stiffness matrix
      INTEGER(LONG), PARAMETER        :: IORD_K = 2        ! Integration order for stiffness matrix in thickness direction
      INTEGER(LONG), PARAMETER        :: IORD_STRESS_Q4 = 2! Gauss integration order for stress/strain recovery matrices
      INTEGER(LONG)                   :: I,J,K,L,M         ! DO loop indices
      INTEGER(LONG)                   :: STR_PT_NUM        ! Stress recovery point number
      INTEGER(LONG)                   :: GP                ! Element grid point number

      REAL(DOUBLE)                    :: HH_IJ(MAX_ORDER_GAUSS) ! Gauss weights for integration in in-layer directions
      REAL(DOUBLE)                    :: SS_IJ(MAX_ORDER_GAUSS) ! Gauss abscissa's for integration in in-layer directions
      REAL(DOUBLE)                    :: HH_K(MAX_ORDER_GAUSS)  ! Gauss weights for integration in thickness direction
      REAL(DOUBLE)                    :: SS_K(MAX_ORDER_GAUSS)  ! Gauss abscissa's for integration in thickness direction
      REAL(DOUBLE)                    :: R, S, T                ! Isoparametric coordinates of a point
      REAL(DOUBLE)                    :: BI(6,6*ELGP)      ! Strain-displ matrix for this element for one Gauss point
      REAL(DOUBLE)                    :: BI1(6,6*ELGP)     ! Strain-displ matrix for this element for one Gauss point bottom
      REAL(DOUBLE)                    :: BI2(6,6*ELGP)     ! Strain-displ matrix for this element for one Gauss point top
      REAL(DOUBLE)                    :: DUM1(6,6*ELGP)    ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM2(6*ELGP,6*ELGP)    ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM3(6*ELGP,6)    ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM4(3)           ! Intermediate matrix
      REAL(DOUBLE)                    :: INTFAC            ! An integration factor (constant multiplier for the Gauss integration)
      REAL(DOUBLE)                    :: DETJ              ! Jacobian determinant
      REAL(DOUBLE)                    :: E2(6,6)           ! Membrane and shear elasticity matrix in the element coordinate system.
      REAL(DOUBLE)                    :: E3(6,6)           ! Membrane and shear elasticity matrix in the cartesian local coordinate system.
      REAL(DOUBLE)                    :: EM2(6,6)          ! Membrane elasticity matrix in the element coordinate system.
      REAL(DOUBLE)                    :: EM3(6,6)          ! Membrane elasticity matrix in the cartesian local coordinate system.
      REAL(DOUBLE)                    :: EB2(6,6)          ! Bending elasticity matrix in the element coordinate system.
      REAL(DOUBLE)                    :: EB3(6,6)          ! Bending elasticity matrix in the cartesian local coordinate system.
      REAL(DOUBLE)                    :: CLB(3,3)          ! Cartesian local basis basis vectors
      REAL(DOUBLE)                    :: MATL_AXES_ROTATE
      REAL(DOUBLE)                    :: TRANSFORM(3,3)
      REAL(DOUBLE)                    :: DUM66(6,6)        ! Intermediate matrix in calculating outputs
      REAL(DOUBLE)                    :: T66(6,6)          ! 6x6 transformation matrix for elasticity
      REAL(DOUBLE)                    :: CTE(6)            ! Coefficient of thermal expansion vector
      REAL(DOUBLE)                    :: THERMAL_STRAIN(6) ! Thermal strain vector
      REAL(DOUBLE)                    :: TBAR              ! Average elem temperature
      REAL(DOUBLE)                    :: UNIT_PTE(6*ELGP)  ! Thermal load vector for unit temperature change.
      REAL(DOUBLE)                    :: UNIT_PPE(6*ELGP)  ! Pressure load vector for unit pressure.
      REAL(DOUBLE)                    :: PSH(ELGP)
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: G(3,3)
      REAL(DOUBLE)                    :: NORMAL(3)

      REAL(DOUBLE)                    :: BMI(6,6*ELGP)     ! Strain-displ matrix for membrane for one Gauss point
      REAL(DOUBLE)                    :: BBI(6,6*ELGP)     ! Strain-displ matrix for bending for one Gauss point
      REAL(DOUBLE)                    :: BSI(6,6*ELGP)     ! Strain-displ matrix for shear for one Gauss point
      REAL(DOUBLE)                    :: M_1DOF(ELGP,ELGP) ! Consistent mass matrix with 1 DOF per node.
      REAL(DOUBLE)                    :: DENSITY
      REAL(DOUBLE)                    :: FORCEx(IORD_STRESS_Q4*IORD_STRESS_Q4) ! Engineering force in the elem x direction at Gauss points
      REAL(DOUBLE)                    :: FORCEy(IORD_STRESS_Q4*IORD_STRESS_Q4) ! Engineering force in the elem x direction at Gauss points
      REAL(DOUBLE)                    :: FORCExy(IORD_STRESS_Q4*IORD_STRESS_Q4)! Engineering force in the elem xy direction at Gauss points
      REAL(DOUBLE)                    :: KS(ELGP,ELGP)     ! KED matrix for one DOF
      REAL(DOUBLE)                    :: DUM12(ELGP,2)     ! Intermediate matrix used in solving for KED matrices
      INTEGER(LONG)                   :: GAUSS_PT          ! Gauss point number (used for output in subr SHP2DQ
                                                           ! An output from subr ORDER, called herein.  Gauss weights.
      REAL(DOUBLE)                    :: HHH(MAX_ORDER_GAUSS)
      INTEGER(LONG)                   :: JPLY              ! PLY_NUM in a DO loop
      REAL(DOUBLE)                    :: SSS(MAX_ORDER_GAUSS)
      REAL(DOUBLE)                    :: DUM11(2,2)        ! Intermediate matrix used in solving for KED matrices
      INTEGER(LONG)                   :: KI, KJ            ! For converting grid point number to element DOF number
      REAL(DOUBLE)                    :: DUM13(ELGP,ELGP)  ! Intermediate matrix used in solving for KED matrices
      REAL(DOUBLE)                    :: DPSHX(2,4)        ! Derivatives of PSH wrt elem x, y coords.
      REAL(DOUBLE)                    :: JACI(2,2)         ! An output from subr JAC2D, called herein. 2 x 2 Jacobian inverse.
      REAL(DOUBLE)                    :: DUM14(3,3)
      REAL(DOUBLE)                    :: DUM33(3,3)
      REAL(DOUBLE)                    :: JAC2x2(2,2)

! **********************************************************************************************************************************

! COORDINATE SYSTEMS
! ==================
!
! Basic
!  SNORM vector components are stored in this and transformed to element coordinates before use.
!
! Cartesian local
!  e^_1, e^_2, e^_3 in Bathe.
!  e^_3 is parallel to the director vector.
!  Used for strain in the strain-displacement matrix and the material elasticity matrix is transformed to this to integrate KE.
!  Orthogonal
!
! Element
!  x_element, y_element, z_element
!  Used for the grid point DOFs of the strain-displacement and the element stiffness matrices.
!  Used for extrapolating stress and strain from Gauss points to corners.
!  Used for element stress, strain, and force outputs
!  Defined the same way as MSC (element coordinate system).
!  Defined by x_element being the bisection of the diagonals and z_element being normal to both diagonals.
!  It's flat even when the element is warped.
!  Orthogonal
!
! Material
!  Used for material elasticity read from the input file.
!  Orthogonal
!
! Isoparametric (natural)
!  R, S, T in code. r_1, r_2, r_3 in Bathe.
!  Each coordinate has range [-1,1].
!  T is parallel to the director vector.
!  Not orthogonal
!
! Covariant
!  g_r, g_s, g_t. g_1, g_2, g_3 in Bathe.
!  Parallel to the isoparametric coordinates but scaled by the element size. Eg. |g_t| = half thickness in the direction of
!  the director vector (SNORM).
!  Not orthogonal
!
! Contravariant
!  g^r, g^s, g^t. g^1, g^2, g^3 in Bathe.
!  Contravariant to the covariant.
!  Not orthogonal
!
! V1, V2, Vn
! Used to express node rotations when building the strain-displacement matrix before being transformed to the element
! coordinate system.
! Vn is the director vector. V1 and V2 are in arbitrary orthogonal directions.
! Orthogonal

! **********************************************************************************************************************************

! Initialize
      PHI_SQ  = ONE                                        ! Not used for this element
      CALL MITC_INITIALIZE ()

      IF (PCOMP_PROPS == 'Y') THEN
        WRITE(ERR,*) ' *ERROR: Code not written for composite material with MITC4'
        WRITE(F06,*) ' *ERROR: Code not written for composite material with MITC4'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
      ENDIF



! **********************************************************************************************************************************
! Generate the mass matrix for this element.

      IF (OPT(1) == 'Y') THEN

         ! Consistent mass matrix
         ! ME = ∫ N' ρ N det(J) dv

         M_1DOF(:,:) = ZERO

         DENSITY = MASS_PER_UNIT_AREA / EPROP(1)

         CALL ORDER_GAUSS ( IORD_IJ, SS_IJ, HH_IJ )
         CALL ORDER_GAUSS ( IORD_K, SS_K, HH_K )

                                                           ! Make mass matrix with 1 DOF per node
         DO I=1,IORD_IJ
            DO J=1,IORD_IJ
               DO K=1,IORD_K
                  R = SS_IJ(I)
                  S = SS_IJ(J)
                  T = SS_K(K)

                  CALL MITC_SHAPE_FUNCTIONS(R, S, PSH, DPSHG)

                  DETJ = MITC_DETJ ( R, S, T )
                  INTFAC = DETJ*HH_IJ(I)*HH_IJ(J)*HH_K(K)  ! det(J) * Gauss point weight

                  DO L=1,ELGP
                     DO M=1,ELGP
                        M_1DOF(L,M) = M_1DOF(L,M) + PSH(L) * PSH(M) * DENSITY * INTFAC
                     ENDDO
                  ENDDO

               ENDDO
            ENDDO
         ENDDO

         CALL LUMP_MASS( M_1DOF )


      ENDIF



! **********************************************************************************************************************************
! Calculate element thermal loads.

      IF (OPT(2) == 'Y') THEN

         E2 = MITC_ELASTICITY()

         UNIT_PTE(:) = ZERO

         CALL ORDER_GAUSS ( IORD_IJ, SS_IJ, HH_IJ )
         CALL ORDER_GAUSS ( IORD_K, SS_K, HH_K )

         ! Force = ∫ B' E ε_thermal dv

         DO I=1,IORD_IJ
            DO J=1,IORD_IJ
               DO K=1,IORD_K
                  R = SS_IJ(I)
                  S = SS_IJ(J)
                  T = SS_K(K)

                                                           ! Find the angle of the cartesian local coordinate
                                                           ! system's x axis projected onto the element coordinate system's
                                                           ! xy plane. Since the basis vectors are already expressed in
                                                           ! element coordinates, projection is simply ignoring the z component.
                  CLB = MITC4_CARTESIAN_LOCAL_BASIS( R, S, T )
                  MATL_AXES_ROTATE = -ATAN2(CLB(2,1), CLB(1,1))
                                                           ! Rotate the material elasticity matrix from element coordinates
                                                           ! to projected cartesian local coordinates. When the elasticity
                                                           ! matrix is used, it will be assumed to be in the non-projected
                                                           ! cartesian local coordinate system but pretend they're the same
                                                           ! so that material properties follow the curved surface of warped
                                                           ! elements.
                  CALL PLANE_COORD_TRANS_21 ( MATL_AXES_ROTATE, TRANSFORM, SUBR_NAME )
                  CALL MATL_TRANSFORM_MATRIX ( TRANSFORM, T66 )
                  T66 = TRANSPOSE(T66)
                  CALL MATMULT_FFF   ( E2  , T66   , 6, 6, 6, DUM66 )
                  CALL MATMULT_FFF_T ( T66 , DUM66 , 6, 6, 6, E3    )

                                                           ! Transform membrane thermal expansion coefficient vector
                                                           ! the same way as the elasticity matrix.
                  CTE(:) = ALPVEC(:,1)
                  CTE(4:6) = CTE(4:6) / TWO                ! Remove shear factor of 2 to transform.
                  CTE = MATMUL(TRANSPOSE(T66), CTE)
                  CTE(4:6) = CTE(4:6) * TWO                ! Reinstate shear factor of 2 after transform.

                  DETJ = MITC_DETJ ( R, S, T )
                  INTFAC = DETJ*HH_IJ(I)*HH_IJ(J)*HH_K(K)  ! det(J) * Gauss point weight
                  CALL MITC4_B( R, S, T, .TRUE., .TRUE., .TRUE., BI)

                                                           ! DUM3 = BI^T * E3
                  CALL MATMULT_FFF_T ( BI, E3, 6, 6*ELGP, 6, DUM3 )

                                                           ! PTE += DUM3 * unit_ε_thermal * det(J) * GaussWeight
                  UNIT_PTE = UNIT_PTE + MATMUL ( DUM3, CTE ) * INTFAC

               ENDDO
            ENDDO
         ENDDO

                                                  ! Scale the thermal load vector by the temperature differences in each
                                                  ! subcase with thermal load.
         DO J=1,NTSUB
                                                  ! Use constant temperature to match the strain field of
                                                  ! linear elements.
            TBAR = (DT(1,J) + DT(2,J) + DT(3,J) + DT(4,J))/FOUR

            PTE(1:6*ELGP,J) = UNIT_PTE(1:6*ELGP) * (TBAR - TREF(1))
         ENDDO


      ENDIF

! **********************************************************************************************************************************


      IF ((OPT(3) == 'Y') .OR. (OPT(6) == 'Y')) THEN

         STR_PT_NUM = 0

         CALL ORDER_GAUSS ( IORD_STRESS_Q4, SS_IJ, HH_IJ )

         DO STR_PT_NUM = 1,5

                                                           ! Account for Bathe's R,S coordinates vs node numbering being different
                                                           ! from Mystran's
               SELECT CASE (STR_PT_NUM)
                  CASE (1); R=ZERO    ; S=ZERO             ! Center
                  CASE (2); R=SS_IJ(2); S=SS_IJ(2)         ! Gauss point 1
                  CASE (3); R=SS_IJ(2); S=SS_IJ(1)         ! Gauss point 2
                  CASE (4); R=SS_IJ(1); S=SS_IJ(2)         ! Gauss point 3
                  CASE (5); R=SS_IJ(1); S=SS_IJ(1)         ! Gauss point 4
               END SELECT

                                                           ! Get the strain-displacement matrix at top and bottom
                                                           ! and transform strain terms to the element coordinate system.
                                                           !
                                                           ! This might not be quite right because the element
                                                           ! coordinate system is flat even when the element is warped
                                                           ! so the cartesian local coordinate system's normal (z) is
                                                           ! different to the element coordinate system's z.
               CALL MITC4_B( R, S, -ONE, .TRUE., .TRUE., .TRUE., BI1)
               TRANSFORM = MITC4_CARTESIAN_LOCAL_BASIS( R, S, -ONE )
               BI1(4:6,:) = BI1(4:6,:) / 2
               CALL MITC_TRANSFORM_B( TRANSFORM, BI1 )
               BI1(4:6,:) = BI1(4:6,:) * 2

               CALL MITC4_B( R, S, +ONE, .TRUE., .TRUE., .TRUE., BI2)
               TRANSFORM = MITC4_CARTESIAN_LOCAL_BASIS( R, S, +ONE )
               BI2(4:6,:) = BI2(4:6,:) / 2
               CALL MITC_TRANSFORM_B( TRANSFORM, BI2 )
               BI2(4:6,:) = BI2(4:6,:) * 2

                                                           ! Membrane strain is the average of the strains at the two t points.
               BE1(1,1:6*ELGP,STR_PT_NUM) = (BI2(1,:) + BI1(1,:)) / TWO           ! xx
               BE1(2,1:6*ELGP,STR_PT_NUM) = (BI2(2,:) + BI1(2,:)) / TWO           ! yy
               BE1(3,1:6*ELGP,STR_PT_NUM) = (BI2(4,:) + BI1(4,:)) / TWO           ! xy

                                                           ! Curvature is (strain_top - strain_bottom) / thickness
                                                           ! To allow grid point thicknesses, this should be the thickness
                                                           ! interpolated at the Gauss point.
!victor todo EPROP(1) might be supposed to be the magnitude of [(DIR_THICKESS * DIRECTOR) interpolated at the Gauss point]
! similar to how MITC_COVARIANT_BASIS does it. Maybe even use the covariant basis 3rd vector times 2?
               BE2(1,1:6*ELGP,STR_PT_NUM) = (BI2(1,:) - BI1(1,:)) / EPROP(1)      ! xx
               BE2(2,1:6*ELGP,STR_PT_NUM) = (BI2(2,:) - BI1(2,:)) / EPROP(1)      ! yy
               BE2(3,1:6*ELGP,STR_PT_NUM) = (BI2(4,:) - BI1(4,:)) / EPROP(1)      ! xy

                                                           ! Transverse shear strain. Note reversed order of rows.
               BE3(1,1:6*ELGP,STR_PT_NUM) = (BI2(6,:) + BI1(6,:)) / TWO           ! zx
               BE3(2,1:6*ELGP,STR_PT_NUM) = (BI2(5,:) + BI1(5,:)) / TWO           ! yz

         ENDDO


      ENDIF

! **********************************************************************************************************************************
! Calculate element stiffness matrix KE.

      IF(OPT(4) == 'Y') THEN

! Based on
! MITC4 paper "A continuum mechanics based four-node shell element for general nonlinear analysis"
!   by Dvorkin and Bathe


         ! K = int( [B]^T [E3] [B] dV )
         !    dV = |det(J)|dr ds dt
         ! K = int( [B]^T [E3] [B] |det(J)| dr ds dt )
         !
         ! [E3] is material elasticity matrix in cartesian local coordinates
         ! stress = [E3] * strain
         ! strain = [B] * displacement
         ! K is in the basic coordinate system


         E2 = MITC_ELASTICITY()                            ! Not used. Delete this.

                                                           ! Membrane
         EM2(:,:) = ZERO
         EM2(1,1) = EM(1,1)
         EM2(1,2) = EM(1,2)
         EM2(1,4) = EM(1,3)
         EM2(2,2) = EM(2,2)
         EM2(2,4) = EM(2,3)
         EM2(4,4) = EM(3,3)

                                                           ! Bending and transverse shear
         EB2(:,:) = ZERO
         EB2(1,1) = EB(1,1)
         EB2(1,2) = EB(1,2)
         EB2(1,4) = EB(1,3)
         EB2(2,2) = EB(2,2)
         EB2(2,4) = EB(2,3)
         EB2(4,4) = EB(3,3)
         EB2(5,5) = ET(2,2) * EPROP(3)
         EB2(5,6) = ET(2,1) * EPROP(3)
         EB2(6,6) = ET(1,1) * EPROP(3)

         DO I=2,6                                          ! Copy UT to LT because it's symmetric.
            DO J=1,I-1
               EM2(I,J) = EM2(J,I)
               EB2(I,J) = EB2(J,I)
            ENDDO
         ENDDO


         KE(1:6*ELGP,1:6*ELGP) = ZERO

         CALL ORDER_GAUSS ( IORD_IJ, SS_IJ, HH_IJ )
         CALL ORDER_GAUSS ( IORD_K, SS_K, HH_K )

         DO I=1,IORD_IJ
            DO J=1,IORD_IJ
               DO K=1,IORD_K
                  R = SS_IJ(I)
                  S = SS_IJ(J)
                  T = SS_K(K)

                                                           ! Find the angle of the cartesian local coordinate
                                                           ! system's x axis projected onto the element coordinate system's
                                                           ! xy plane. Projection is simply ignoring the z component.
                  CLB = MITC4_CARTESIAN_LOCAL_BASIS( R, S, T )
                  MATL_AXES_ROTATE = -ATAN2(CLB(2,1), CLB(1,1))
                                                           ! Rotate the material elasticity matrix so it's expressed in projected
                                                           ! cartesian local coordinates. When the elasticity matrix is used
                                                           ! it will be assumed to be in the non-projected cartesial local
                                                           ! coordinate system but pretend they're the same so that material
                                                           ! properties follow the curved surface of warped elements.
                  CALL PLANE_COORD_TRANS_21 ( MATL_AXES_ROTATE, TRANSFORM, SUBR_NAME )
                  CALL MATL_TRANSFORM_MATRIX ( TRANSFORM, T66 )
                  T66 = TRANSPOSE(T66)


                  DETJ = MITC_DETJ ( R, S, T )
                  INTFAC = DETJ*HH_IJ(I)*HH_IJ(J)*HH_K(K)

                  IF(.FALSE.) THEN
                                                           ! Single material for membrane and bending
                                                           ! Not used. Delete this.
                     CALL MATMULT_FFF   ( E2  , T66   , 6, 6, 6, DUM66 )
                     CALL MATMULT_FFF_T ( T66 , DUM66 , 6, 6, 6, E3    )

                     CALL MITC4_B( R, S, T, .TRUE., .TRUE., .TRUE., BI)
                     CALL MATMULT_FFF ( E3, BI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                  ELSEIF(.TRUE.) THEN

                     CALL MATMULT_FFF   ( EM2 , T66   , 6, 6, 6, DUM66 )
                     CALL MATMULT_FFF_T ( T66 , DUM66 , 6, 6, 6, EM3   )

                     CALL MATMULT_FFF   ( EB2 , T66   , 6, 6, 6, DUM66 )
                     CALL MATMULT_FFF_T ( T66 , DUM66 , 6, 6, 6, EB3   )

                                                           ! Membrane
                                                           ! ∫ B_m' E_m B_m dv
                     CALL MITC4_B( R, S, T, .TRUE., .FALSE., .FALSE., BMI)
                     CALL MATMULT_FFF ( EM3, BMI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BMI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Bending and transverse shear
                                                           ! including bending-shear coupling
                                                           ! ∫ B_b' (E_b + E_t) B_b dv
                     CALL MITC4_B( R, S, T, .FALSE., .TRUE., .TRUE., BBI)
                     CALL MATMULT_FFF ( EB3, BBI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BBI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Geometric bending-membrane coupling
                                                           ! This is coupling due to the geometry of the element (warped or
                                                           ! with SNORM), not unsymmetric composites or MID4 on PSHELL.
                                                           ! ∫ B_m' E_m B_b dv + ∫ B_b' E_m' B_m dv
                     CALL MATMULT_FFF ( EM3, BBI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BMI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC
                     CALL MATMULT_FFF_T ( EM3, BMI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BBI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                  ELSE
                                                           ! All the phenomena fully separated.
                                                           ! Not used. Delete this.
                     CALL MATMULT_FFF   ( EM2 , T66   , 6, 6, 6, DUM66 )
                     CALL MATMULT_FFF_T ( T66 , DUM66 , 6, 6, 6, EM3   )

                     CALL MATMULT_FFF   ( EB2 , T66   , 6, 6, 6, DUM66 )
                     CALL MATMULT_FFF_T ( T66 , DUM66 , 6, 6, 6, EB3   )

                                                           ! Membrane
                     CALL MITC4_B( R, S, T, .TRUE., .FALSE., .FALSE., BMI)
                     CALL MATMULT_FFF ( EM3, BMI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BMI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Bending
                     CALL MITC4_B( R, S, T, .FALSE., .TRUE., .FALSE., BBI)
                     CALL MATMULT_FFF ( EB3, BBI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BBI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Transverse shear
                                                           ! Use bending+transverse shear elasticity
                                                           ! because MID3 requires MID2 but not MID1.
                     CALL MITC4_B( R, S, T, .FALSE., .FALSE., .TRUE., BSI)
                     CALL MATMULT_FFF ( EB3, BSI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BSI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Bending-membrane coupling
                     CALL MATMULT_FFF ( EM3, BBI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BMI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC
                     CALL MATMULT_FFF ( TRANSPOSE(EM3), BMI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BBI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Membrane-shear coupling
                     CALL MATMULT_FFF ( EM3, BSI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BMI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC
                     CALL MATMULT_FFF ( TRANSPOSE(EM3), BMI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BSI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC

                                                           ! Bending-shear coupling
                     CALL MATMULT_FFF ( EB3, BSI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BBI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC
                     CALL MATMULT_FFF ( TRANSPOSE(EB3), BBI, 6, 6, 6*ELGP, DUM1 )
                     CALL MATMULT_FFF_T ( BSI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                     KE(1:6*ELGP,1:6*ELGP) = KE(1:6*ELGP,1:6*ELGP) + DUM2(:,:)*INTFAC


                  ENDIF



               ENDDO
            ENDDO
         ENDDO





      ENDIF


! **********************************************************************************************************************************
! Determine element pressure loads

      IF (OPT(5) == 'Y') THEN

         UNIT_PPE(:) = ZERO

         CALL ORDER_GAUSS ( IORD_IJ, SS_IJ, HH_IJ )

         DO I=1,IORD_IJ
            DO J=1,IORD_IJ
               R = SS_IJ(I)
               S = SS_IJ(J)

               CALL MITC_SHAPE_FUNCTIONS(R, S, PSH, DPSHG)
                                                           ! Normalized normal vector at R,S
                                                           ! This is the interpolated director vector
                                                           ! and follows SNORM if specified.
               CALL MITC_COVARIANT_BASIS( R, S, ZERO, G )
               NORMAL(:) = G(:,3) / SQRT(DOT_PRODUCT(G(:,3), G(:,3)))

               CALL CROSS(G(:,1),G(:,2),DUM4)
               DETJ = SQRT(DOT_PRODUCT(DUM4, DUM4))
               INTFAC = DETJ * HH_IJ(I) * HH_IJ(J)         ! Contribution to area for this Gauss point

               DO GP=1,ELGP
                  K = (GP-1) * 6
                  UNIT_PPE(K+1:K+3) = UNIT_PPE(K+1:K+3) + NORMAL * PSH(GP) * INTFAC
               ENDDO

            ENDDO
         ENDDO

                                                           ! Scale the unit pressure load vector by the pressure in each subcase.
         DO J=1,NSUB
            PPE(1:6*ELGP,J) = UNIT_PPE(1:6*ELGP) * PRESS(3,J)
         ENDDO



      ENDIF

! **********************************************************************************************************************************
! Calculate linear differential stiffness matrix

      IF ((OPT(6) == 'Y') .AND. (LOAD_ISTEP > 1)) THEN

                                                           ! Find membrane engineering forces at each Gauss point
                                                           ! by summing the forces in each ply.
         FORCEx(:)  = 0
         FORCEy(:)  = 0
         FORCExy(:) = 0

         CALL GET_ELEM_NUM_PLIES ( INT_ELEM_ID )           ! Get NUM_PLIES

         DO JPLY=1,NUM_PLIES
                                                           ! Get UEL, EM, TPLY for this ply.
            IF (PCOMP_PROPS == 'N') THEN
                                                           ! EM is already set above.

               TPLY = EPROP(1)                             ! Element thickness
               CALL ELMDIS

            ELSE


               IF (PCOMP_LAM == 'NON') THEN                ! Delete this IF block to allow the LAM field set to nonsymmetric.
                  FATAL_ERR          = FATAL_ERR + 1
                  NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
                  WRITE(ERR,*) ' *ERROR: Code not written for non-symmetric composite buckling or differential stiffness.'
                  WRITE(F06,*) ' *ERROR: Code not written for non-symmetric composite buckling or differential stiffness.'
                  CALL OUTA_HERE ( 'Y' )
               ENDIF

               PLY_NUM = JPLY                              ! Used by SHELL_ABD_MATRICES
               CALL SHELL_ABD_MATRICES ( INT_ELEM_ID, 'N' )! Get EM, ZPLY, TPLY, ALPVEC for this ply
               CALL ELMDIS
               CALL ELMDIS_PLY                             ! Adjust UEL using ZPLY

            ENDIF

            GAUSS_PT = 0
            DO I=1,IORD_STRESS_Q4
               DO J=1,IORD_STRESS_Q4
                  GAUSS_PT = GAUSS_PT + 1

                                                           ! Stress at this Gauss point using UEL, BE1, EM, ALPVEC, DT
                  CALL ELEM_STRE_STRN_ARRAYS ( GAUSS_PT+1 )

                  FORCEx(GAUSS_PT)  = FORCEx(GAUSS_PT)  + TPLY*STRESS(1)
                  FORCEy(GAUSS_PT)  = FORCEy(GAUSS_PT)  + TPLY*STRESS(2)
                  FORCExy(GAUSS_PT) = FORCExy(GAUSS_PT) + TPLY*STRESS(3)
               ENDDO
            ENDDO

         ENDDO


                                                           ! Transform force from element coordinates to cartesian local
                                                           ! This isn't quite right for non-flat elements becuase all the z
                                                           ! components are omitted in both coordinate systems.
                                                           ! It would be better to obtain force directly in cartesian local
                                                           ! coordinates instead.
         CALL ORDER_GAUSS ( IORD_STRESS_Q4, SSS, HHH )

         DO GAUSS_PT = 1,4
                                                           ! 3x3 force tensor
            DUM14(:,:) = ZERO
            DUM14(1,1) = FORCEx(GAUSS_PT)
            DUM14(2,2) = FORCEy(GAUSS_PT)
            DUM14(1,2) = FORCExy(GAUSS_PT)
            DUM14(2,1) = FORCExy(GAUSS_PT)

            SELECT CASE (GAUSS_PT)
               CASE (1); R=SSS(2); S=SSS(2)             ! Gauss point 1
               CASE (2); R=SSS(2); S=SSS(1)             ! Gauss point 2
               CASE (3); R=SSS(1); S=SSS(2)             ! Gauss point 3
               CASE (4); R=SSS(1); S=SSS(1)             ! Gauss point 4
            END SELECT

            CLB = MITC4_CARTESIAN_LOCAL_BASIS( R, S, ZERO )

            CALL MATMULT_FFF (DUM14, CLB, 3, 3, 3, DUM33 )
            CALL MATMULT_FFF_T (CLB, DUM33, 3, 3, 3, DUM14 )

            FORCEx(GAUSS_PT) = DUM14(1,1)
            FORCEy(GAUSS_PT) = DUM14(2,2)
            FORCExy(GAUSS_PT) = DUM14(1,2)
         ENDDO


! Accoring to:
!   Robert D. Cook, David S. Malkus, Michael E. Plesha Concepts and Applications of Finite Element Analysis, 3rd Edition  1989
!   Section 14.3 Stress Stiffness Matrix Of A Plate Element

!         +1  +1
! [  ]   ⌠   ⌠  [   ]T [   ]-T [ Nx  Nxy ] [   ]-1 [   ]
! [k ] = |   |  [ G ]  [ J ]   [ Nxy Ny  ] [ J ]   [ G ]  |J|  dξ dη
! [ σ]   ⌡   ⌡  [  I]  [   ]               [   ]   [  I]
!        -1  -1

! k_σ is the stress stiffness (differential stiffness) matrix
! Nx, Ny, Nxy are membrane engineering forces
! |J| is the Jacobian determinant
! G_I is a 2xELGP matrix of shape function derivatives with respect to isoparametric coordinates ξ  and η.

! DPSHX = J^-1 G_I is the 2 x ELGP matrix of shape function derivatives with respect to element coordinates x and y.

!        +1  +1
! [k ]   ⌠   ⌠        T [ Nx  Nxy ]
! [ σ] = ⌡   ⌡   DPSHX  [ Nxy Ny  ]  DPSHX  |J|  dξ dη
!        -1  -1

         KS(:,:) = ZERO



         GAUSS_PT = 0
         DO I=1,IORD_STRESS_Q4
            DO J=1,IORD_STRESS_Q4
               GAUSS_PT = GAUSS_PT + 1

                                                           ! Account for Bathe's R,S coordinates vs node numbering being different
                                                           ! from Mystran's
               SELECT CASE (GAUSS_PT)
                  CASE (1); R=SSS(2); S=SSS(2)             ! Gauss point 1
                  CASE (2); R=SSS(2); S=SSS(1)             ! Gauss point 2
                  CASE (3); R=SSS(1); S=SSS(2)             ! Gauss point 3
                  CASE (4); R=SSS(1); S=SSS(1)             ! Gauss point 4
               END SELECT

               DUM11(1,1) = FORCEx(GAUSS_PT)  ; DUM11(1,2) = FORCExy(GAUSS_PT)
               DUM11(2,1) = FORCExy(GAUSS_PT) ; DUM11(2,2) = FORCEy(GAUSS_PT)


                                                           ! 2D inverse Jacobian
               CALL MITC_COVARIANT_BASIS ( R, S, ZERO, G )
               CLB = MITC4_CARTESIAN_LOCAL_BASIS( R, S, ZERO )
               DUM14 = MATMUL(TRANSPOSE(G), CLB)
               JAC2x2(1:2,1:2) = DUM14(1:2,1:2)
               DETJ = JAC2x2(1,1)*JAC2x2(2,2) - JAC2x2(1,2)*JAC2x2(2,1)
               JACI(1,1) =  JAC2x2(2,2)/DETJ
               JACI(1,2) = -JAC2x2(1,2)/DETJ
               JACI(2,1) = -JAC2x2(2,1)/DETJ
               JACI(2,2) =  JAC2x2(1,1)/DETJ
               CALL MITC_SHAPE_FUNCTIONS ( R, S, PSH, DPSHG )
                                                           ! Shape function derivatives at this Gauss point.
               CALL MATMULT_FFF ( JACI, DPSHG, 2, 2, 4, DPSHX )

               CALL MATMULT_FFF_T ( DPSHX, DUM11, 2, ELGP, 2, DUM12 )
               CALL MATMULT_FFF ( DUM12, DPSHX, ELGP, 2, ELGP, DUM13 )

               INTFAC = DETJ*HHH(I)*HHH(J)

                                                           ! Accumulate integrand into the result
               DO K=1,ELGP
                  DO L=1,ELGP
                     KS(K,L) = KS(K,L) + DUM13(K,L) * INTFAC
                  ENDDO
               ENDDO

            ENDDO
         ENDDO


                                                           ! Copy KS into KED for each translational DOF.
         KED(1:6*ELGP,1:6*ELGP) = 0
         DO I=1,ELGP
            DO J=1,ELGP
               KI = (I-1) * 6
               KJ = (J-1) * 6
               KED(KI + 1, KJ + 1) = KS(I,J)
               KED(KI + 2, KJ + 2) = KS(I,J)
               KED(KI + 3, KJ + 3) = KS(I,J)
            ENDDO
         ENDDO


      ENDIF




      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************

      END SUBROUTINE MITC4
