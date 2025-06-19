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
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  NUM_EMG_FATAL_ERRS, PCOMP_PROPS, ELGP, ES, KE, EM, ET, BE1, BE2, BE3
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO

      USE ORDER_GAUSS_Interface
      USE OUTA_HERE_Interface
      USE MATMULT_FFF_Interface
      USE MATMULT_FFF_T_Interface
      USE MITC_DETJ_Interface
      USE MITC8_B_Interface

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

! **********************************************************************************************************************************

! Coordinate systems
! ==================
!
! Basic
!  Used for the grid point DOFs of the strain-displacement and the element stiffness matrices.
!  Orthogonal
!
! Cartesian local
!  e^_1, e^_2, e^_3 in Bathe but they are oriented differently there.
!  Currently the same as the element coordinate system x_l, y_l, z_l.
!  Used for strain in the strain-displacement matrix and the material elasticity matrix is transformed to this to integrate KE.
!  e^_3 is the midsurface normal which may be different from the director vector.
!  Orthogonal
!
! Element
!  x_l, y_l, z_l
!  Defined the same way as MSC (element coordinate system) and SimCenter (local coordinate system).
!  Used for stress and strain output
!  Defined by x_l being the bisection of the R, S isoparametric basis vectors rotated about the normal by -45 degrees.
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
!  Parallel to the isometric coordinates but scaled by the element size. Eg. |g_t| = half thickness.
!  Not orthogonal
!
! Contravariant
!  g^r, g^s, g^t. g^1, g^2, g^3 in Bathe.
!  Contravariant to the covariant.
!  Not orthogonal
!


! **********************************************************************************************************************************
      
      
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
! BE1 matrix (3 x 48) for membrane strain/stress data recovery. 
! BE2 matrix (3 x 48) for bending strain/stress data recovery.
! BE3 matrix (2 x 48) for transverse shear strain/stress data recovery.
! All calculated at center of element/ply and at stress recovery points.
! The displacements are in basic coordinates and the strains are in element coordinates.
! Strain and stress are directly evaluated at the center and corner grid points, not extrapolated from Gauss points.
! If this is changed, then stress points 2-5 should be Gauss points instead of grid points.

      IF (OPT(3) == 'Y') THEN

! Generate BE1 at the stress recovery points. Put them into array BE1(i,j,k) at k indices 1-5.

         DO STR_PT_NUM=1,5

            SELECT CASE (STR_PT_NUM)
            CASE (1)                                       ! Center
               !victor todo Nastran's element center may not be at RS=0. Check the definition.
               R = 0; S = 0
            CASE (2)                                       ! Node 1
               R = -1; S = -1
            CASE (3)                                       ! Node 2
               R = +1; S = -1
            CASE (4)                                       ! Node 3
               R = +1; S = +1
            CASE (5)                                       ! Node 4
               R = -1; S = +1
            END SELECT

            CALL MITC8_B( R, S, -ONE, .TRUE., .TRUE., BI1)
            CALL MITC8_B( R, S, +ONE, .TRUE., .TRUE., BI2)

                                               ! Membrane strain is the average of the strains at the two t points.
            BE1(1,:,STR_PT_NUM) = (BI2(1,:) + BI1(1,:)) / TWO           ! xx
            BE1(2,:,STR_PT_NUM) = (BI2(2,:) + BI1(2,:)) / TWO           ! yy
            BE1(3,:,STR_PT_NUM) = (BI2(4,:) + BI1(4,:)) / TWO           ! xy

                                               ! Bending strain is half the difference of the strains at top and bottom.
            BE2(1,:,STR_PT_NUM) = (BI2(1,:) - BI1(1,:)) / TWO           ! xx
            BE2(2,:,STR_PT_NUM) = (BI2(2,:) - BI1(2,:)) / TWO           ! yy
            BE2(3,:,STR_PT_NUM) = (BI2(4,:) - BI1(4,:)) / TWO           ! xy

            !victor todo 
            !Min4 calculates BE3 at the center from the average at the for gauss points instead of 
            !directly at the center like membrane and bending. Might need that too but be careful of the different element coordinate system at each point.
                                               ! Transverse shear strain. Note reversed order of rows.
            BE3(1,:,STR_PT_NUM) = (BI2(6,:) + BI1(6,:)) / TWO           ! zx
            BE3(2,:,STR_PT_NUM) = (BI2(5,:) + BI1(5,:)) / TWO           ! yz

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

        E(5,5) = ET(2,2) !victor todo shear correction factor
        E(5,6) = ET(2,1) !victor todo shear correction factor ??

        E(6,6) = ET(1,1) !victor todo shear correction factor

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

! victor todo transform E from material to cartesian local coordiantes (EE) at this Gauss point. Not this simple copy.
              DO L=1,6
                DO M=1,6
                  EE(L,M) = E(L,M)
                ENDDO 
              ENDDO   

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
