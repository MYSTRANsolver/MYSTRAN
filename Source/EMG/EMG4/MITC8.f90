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
      SUBROUTINE MITC8 ( OPT, INT_ELEM_ID )
 
! Calculates, or calls subr's to calculate, quadrilateral element matrices:

!  1) ME        = element mass matrix                  , if OPT(1) = 'Y'
!  2) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  3) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  4) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  5) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  6) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  NUM_EMG_FATAL_ERRS, PCOMP_PROPS, ELGP, ES, KE, EM, ET, BE1, BE2, BE3, PHI_SQ, FCONV,      &
                                         EPROP, SHELL_STR_ANGLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, FOUR
      USE PARAMS, ONLY                :  TSTM_DEF

      USE ORDER_GAUSS_Interface
      USE OUTA_HERE_Interface
      USE MATMULT_FFF_Interface
      USE MATMULT_FFF_T_Interface
      USE MITC_DETJ_Interface
      USE MITC8_B_Interface
      USE MITC_GP_RS_Interface
      USE MITC8_CARTESIAN_LOCAL_BASIS_Interface
      USE MITC8_ELEMENT_CS_BASIS_Interface
      
      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MITC8'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID
      INTEGER(LONG), PARAMETER        :: IORD_IJ = 3       ! Integration order for stiffness matrix
      INTEGER(LONG), PARAMETER        :: IORD_K = 2        ! Integration order for stiffness matrix in thickness direction
      INTEGER(LONG), PARAMETER        :: IORD_STRESS_Q8 = 2! Gauss integration order for stress/strain recovery matrices
      INTEGER(LONG)                   :: I,J,K,L,M         ! DO loop indices
      INTEGER(LONG)                   :: STR_PT_NUM        ! Stress recovery point number

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
      REAL(DOUBLE)                    :: INTFAC            ! An integration factor (constant multiplier for the Gauss integration)
      REAL(DOUBLE)                    :: DETJ              ! Jacobian determinant
      REAL(DOUBLE)                    :: E(6,6)            ! Elasticity matrix in the material coordinate system.
      REAL(DOUBLE)                    :: EE(6,6)           ! Elasticity matrix in the cartesian local coordinate system.
      REAL(DOUBLE)                    :: GP_RS(2,ELGP)     ! Isoparametric coordinates of grid points
      REAL(DOUBLE)                    :: LOCAL_BASIS(3,3)  ! Cartesian local basis
      REAL(DOUBLE)                    :: ELEMENT_BASIS(3,3)! Element coordinate system basis
      REAL(DOUBLE)                    :: XL(3)
      REAL(DOUBLE)                    :: ZL(3)
      REAL(DOUBLE)                    :: XE(3)
      REAL(DOUBLE)                    :: CROSS_XLE(3)

! **********************************************************************************************************************************

! COORDINATE SYSTEMS
! ==================
!
! Basic
!  Used for the grid point DOFs of the strain-displacement and the element stiffness matrices.
!  No distinction is made between grid point and element node coordinates but they may be different if ZOFFS is enabled.
!  Orthogonal
!
! Cartesian local
!  e^_1, e^_2, e^_3 in Bathe but they are oriented differently there.
!  e^_3 is the midsurface normal which may be different from the director vector.
!  Used for strain in the strain-displacement matrix and the material elasticity matrix is transformed to this to integrate KE.
!  Defined the same way as the zero THETA material coordinate system in Siemens SimCenter. This definition is used because it
!  has uniform orientation on distorted flat elements and is only non-uniform as needed to accomodate out-of-plane curvature.
!  The uniformity allows stress to be interpolated and extrapolated to different locations conveniently.
!  Orthogonal
!
! Element
!  x_l, y_l, z_l
!  Used for element stress, strain, and force outputs
!  Defined the same way as MSC (element coordinate system) and SimCenter (local coordinate system).
!  Defined by x_l being the bisection of the R, S isoparametric basis vectors rotated about the normal by -45 degrees.
!  Orthogonal
!
! XEL
!  Used for extrapolating stress and strain from Gauss points to corners.
!  Grid point coordinates stored in XEL are in a coordinate system which is flat, with the normal being the cross product 
!  of vectors from grid points 1-3 and 2-4. The x axis is an arbitrary direction in this plane. The flat coordinate system
!  is used because the polynomial curve fit code to extrapolate stress/strain from Gauss points to corners is only 2D.
!  Orthogonal
!
! Material
!  Used for material elasticity read from the input file.
!  Currently, this is the same as the cartesian local coordinate system. To allow non-isotropic materials, it
!  should find the angle between the two systems's x axes at each integration point and rotate the material
!  elasticity matrix about that when building the stiffness matrix KE.
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
!  Parallel to the isometric coordinates but scaled by the element size. Eg. |g_t| = half thickness.
!  Not orthogonal
!
! Contravariant
!  g^r, g^s, g^t. g^1, g^2, g^3 in Bathe.
!  Contravariant to the covariant.
!  Not orthogonal
!
! NORMAL AND THICKNESS
! ====================
!
! There are two definitions of the shell  normal:
! 1) Normal to the midsurface. Defined by grid point positions.
! 2) Director vector. Can be defined arbitrarily.
! Both vary across the element if it's curved out of plane.
! Currently, the director vector is equal to the midsurface normal so the distinction doesn't matter. However, 
! they may be different if we include smoothing of shell normals by averaging with adjacent elements like MSC 
! does. In that case, each occurrence of normal or director vector needs to be checked to make sure it's the
! correct one. Also, element thickness might be defined as thickness in the direction of either of those
! directions so some uses of thickness may need to be adjusted.
!

! **********************************************************************************************************************************
  
! Initialize
      PHI_SQ  = ONE                                        ! Not used for this element
      
      
      IF (PCOMP_PROPS == 'Y') THEN
        WRITE(ERR,*) ' *ERROR: Code not written for composite material with QUAD8'
        WRITE(F06,*) ' *ERROR: Code not written for composite material with QUAD8'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
      ENDIF

        
      
! **********************************************************************************************************************************
! Generate the mass matrix for this element.
 
      IF (OPT(1) == 'Y') THEN
        !Not implememented yet but we can't make it a fatal error because this gets called even when it doesn't need it.

      ENDIF

 

! **********************************************************************************************************************************
! Calculate element thermal loads. 
  
      IF (OPT(2) == 'Y') THEN
      
        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 thermal loads'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 thermal loads'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
      
      ENDIF
  
! **********************************************************************************************************************************
! BE1 matrix (3 x 48) for membrane strain/stress/force data recovery. 
! BE2 matrix (3 x 48) for bending strain/stress/force data recovery.
! BE3 matrix (2 x 48) for transverse shear strain/stress/force data recovery.
! All calculated at Gauss points and not center.
! The displacements are in basic coordinates and the strains are in element coordinates.


! There's a possible bug where the numbering of the element grid points (eg. 1-2-3-4 vs 2-3-4-1) affects the stress/strain/elforce, 
! when the element is distorted. This includes von Mises which should be invariant to rotation.
!
! - It doesn't occur if we skip the extrapolation from Gauss points to corners so Gauss point strains are probably OK.
! - It makes no differences if the cartesian local coordiante system is changed to be G1G2 or the bisected diagonals projected 
!   onto the surface everywhere or like Siemens material coordinates with an intermediate reference plane.
! - It works OK if the cartesian local coordinate system is defined using the same vector for each element, eg. (1,0,0), instead 
!   of G1G2 with either the Siemens or direct projection. However, this won't generalize to elements in any orientation and might
!   just be hiding the problem.
! - The problem is probably the extrapolation from Gauss points to grid points. Maybe it should be done in covariant coordinates 
!   the way strains are interpolated by MITC. Somehow.

      IF (OPT(3) == 'Y') THEN

         STR_PT_NUM = 1

         CALL ORDER_GAUSS ( IORD_STRESS_Q8, SS_IJ, HH_IJ )

         DO I=1,IORD_STRESS_Q8
            DO J=1,IORD_STRESS_Q8

               STR_PT_NUM = STR_PT_NUM + 1

               R = SS_IJ(I)
               S = SS_IJ(J)
               
               CALL MITC8_B( R, S, -ONE, .TRUE., .TRUE., BI1)
               CALL MITC8_B( R, S, +ONE, .TRUE., .TRUE., BI2)

                                                  ! Membrane strain is the average of the strains at the two t points.
               BE1(1,:,STR_PT_NUM) = (BI2(1,:) + BI1(1,:)) / TWO           ! xx
               BE1(2,:,STR_PT_NUM) = (BI2(2,:) + BI1(2,:)) / TWO           ! yy
               BE1(3,:,STR_PT_NUM) = (BI2(4,:) + BI1(4,:)) / TWO           ! xy

                                                  ! Curvature is (strain_top - strain_bottom) / thickness
                                                  ! To allow grid point thicknesses, this should be the thickness
                                                  ! interpolated at the Gauss point.
               BE2(1,:,STR_PT_NUM) = (BI2(1,:) - BI1(1,:)) / EPROP(1)      ! xx
               BE2(2,:,STR_PT_NUM) = (BI2(2,:) - BI1(2,:)) / EPROP(1)      ! yy
               BE2(3,:,STR_PT_NUM) = (BI2(4,:) - BI1(4,:)) / EPROP(1)      ! xy

                                                  ! Transverse shear strain. Note reversed order of rows.
               BE3(1,:,STR_PT_NUM) = (BI2(6,:) + BI1(6,:)) / TWO           ! zx
               BE3(2,:,STR_PT_NUM) = (BI2(5,:) + BI1(5,:)) / TWO           ! yz
                              
            ENDDO
         ENDDO

                                                           ! Find angle of the element coordinate system's x axis from
                                                           ! the cartesian local coordinate system's x axis at each 
                                                           ! corner.
                                                           ! This will be used to transform stress and strain to the
                                                           ! element coordinate system after extrapolating to corners.
         GP_RS = MITC_GP_RS()
         DO STR_PT_NUM=2,5

            R = GP_RS(1, STR_PT_NUM - 1)
            S = GP_RS(2, STR_PT_NUM - 1)

            LOCAL_BASIS = MITC8_CARTESIAN_LOCAL_BASIS( R, S )
            XL = LOCAL_BASIS(:,1)                          ! X axis of cartesian local basis
            ZL = LOCAL_BASIS(:,3)                          ! Normal
            ELEMENT_BASIS = MITC8_ELEMENT_CS_BASIS( R, S )
            XE = ELEMENT_BASIS(:,1)                        ! X axis of element coordinate system

            CALL CROSS( XL, XE, CROSS_XLE )
            SHELL_STR_ANGLE( STR_PT_NUM ) = ATAN2(DOT_PRODUCT( ZL, CROSS_XLE ), DOT_PRODUCT( XL, XE ))
         
         ENDDO

      ENDIF

! **********************************************************************************************************************************
! Calculate element stiffness matrix KE.
 
      IF(OPT(4) == 'Y') THEN

! Based on
! MITC4 paper "A continuum mechanics based four-node shell element for general nonlinear analysis" 
!   by Dvorkin and Bathe
! MITC8 paper "A FORMULATION OF GENERAL SHELL ELEMENTS-THE USE OF MIXED INTERPOLATION OF TENSORIAL COMPONENTS" 
!   by Dvorkin and Bathe, 1986


         ! K = int( [B]^T [EE] [B] dV )
         !    dV = |det(J)|dr ds dt
         ! K = int( [B]^T [EE] [B] |det(J)| dr ds dt )
         !
         ! [EE] is material elasticity matrix in cartesian local coordinates
         ! stress = [EE] * strain
         ! strain = [B] * displacement
         ! K is in the basic coordinate system


                                                           ! Convert 2D material elasticity matrices to 3D.
         E(1,1) = EM(1,1)
         E(1,2) = EM(1,2)
         E(1,3) = ZERO
         E(1,4) = EM(1,3)
         E(1,5) = ZERO
         E(1,6) = ZERO

         E(2,2) = EM(2,2)
         E(2,3) = ZERO
         E(2,4) = EM(2,3)
         E(2,5) = ZERO
         E(2,6) = ZERO

         E(3,3) = ZERO
         E(3,4) = ZERO
         E(3,5) = ZERO
         E(3,6) = ZERO

         E(4,4) = EM(3,3)
         E(4,5) = ZERO
         E(4,6) = ZERO

         E(5,5) = ET(2,2) * EPROP(3)
         E(5,6) = ET(2,1) * EPROP(3)

         E(6,6) = ET(1,1) * EPROP(3)

         DO I=2,6                                           ! Copy UT to LT because it's symmetric.
            DO J=1,I-1
               E(I,J) = E(J,I)
            ENDDO 
         ENDDO   



         KE(:,:) = ZERO

         CALL ORDER_GAUSS ( IORD_IJ, SS_IJ, HH_IJ )
         CALL ORDER_GAUSS ( IORD_K, SS_K, HH_K )

         DO I=1,IORD_IJ
            DO J=1,IORD_IJ
               DO K=1,IORD_K
                  R = SS_IJ(I)
                  S = SS_IJ(J)
                  T = SS_K(K)
                  CALL MITC8_B( R, S, T, .TRUE., .TRUE., BI)

                  ! For non-isotropic materials, this should be rotated from the material coordinate system to the cartesian local
                  ! coordinate system here. The rotation angle may be different at each Gauss point.
                  EE(:,:) = E(:,:)

                  CALL MATMULT_FFF ( EE, BI, 6, 6, 6*ELGP, DUM1 )
                  CALL MATMULT_FFF_T ( BI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
                  DETJ = MITC_DETJ ( R, S, T )
                  INTFAC = DETJ*HH_IJ(I)*HH_IJ(J)*HH_K(K)
                  KE(:,:) = KE(:,:) + DUM2(:,:)*INTFAC
               ENDDO
            ENDDO 
         ENDDO   

 
  
      ENDIF
  

! **********************************************************************************************************************************
! Determine element pressure loads   
  
      IF (OPT(5) == 'Y') THEN

        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 pressure loads'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 pressure loads'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
          
      ENDIF
  
! **********************************************************************************************************************************
! Calculate linear differential stiffness matrix
  
      IF ((OPT(6) == 'Y') .AND. (LOAD_ISTEP > 1)) THEN

        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 differential stiffness matrix'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 differential stiffness matrix'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF


 
 


      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC8
