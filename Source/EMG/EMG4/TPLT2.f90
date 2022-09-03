! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
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

      SUBROUTINE TPLT2(OPT, AREA, X2E, X3E, Y3E, CALC_EMATS, IERROR, KV, PTV, PPV, B2V, B3V, S2V, S3V, BIG_BB, MN4T_QD,TRIA_NUM,PSI)

! MIN3 triangular thick (Mindlin) plate bending element. This element is based on the following work:

! "A Three-Node Mindlin Plate Element With Improved Transverse Shear", by Alexander Tessler and Thomas J.R. Hughes,
! Computer Methods In Applied Mechanics And Engineering 50 (1985) pp 71-101

! Subroutine calculates:

!  1) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  2) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  3) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  4) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  5) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEMATC, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TPLT2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, THREE, FOUR, SIX, EIGHT, TWELVE, CONV_RAD_DEG
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, BE3, BENSUM, DT, FCONV_SHEAR_THICK, EB, EBM, EID, ET, ELDOF, FCONV, KE,      &
                                         MTRL_TYPE, PCOMP_LAM, PCOMP_PROPS, PHI_SQ, PPE, PRESS, PTE, SE2, SE3, SHELL_B, SHELL_DALP,&
                                         SHELL_D, SHELL_T, SHRSUM, STE2, TYPE
      USE PARAMS, ONLY                :  EPSIL, CBMIN3, CBMIN4T
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE TPLT2_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TPLT2'
      CHARACTER(1*BYTE), INTENT(IN)   :: CALC_EMATS        ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*) , INTENT(IN)   :: MN4T_QD           ! Arg used to say whether the triangular elem is part of a QUAD4

      INTEGER(LONG), INTENT(OUT)      :: IERROR            ! Local error indicator
      INTEGER(LONG), INTENT(IN)       :: TRIA_NUM          ! Tria number (1, 2, 3 or 4) for the subtriangles of a MIN4T QUAD4
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: ID(9) =   (/ 3, & ! ID(1) =  3 means virgin 9x9 elem DOF 1 is MYSTRAN 18x18 elem DOF  3
                                                      9, & ! ID(2) =  9 means virgin 9x9 elem DOF 2 is MYSTRAN 18x18 elem DOF  9
                                                     15, & ! ID(3) = 15 means virgin 9x9 elem DOF 3 is MYSTRAN 18x18 elem DOF 15
                                                      4, & ! ID(4) =  4 means virgin 9x9 elem DOF 4 is MYSTRAN 18x18 elem DOF  4
                                                     10, & ! ID(5) = 10 means virgin 9x9 elem DOF 5 is MYSTRAN 18x18 elem DOF 10
                                                     16, & ! ID(6) = 16 means virgin 9x9 elem DOF 6 is MYSTRAN 18x18 elem DOF 16
                                                      5, & ! ID(7) =  5 means virgin 9x9 elem DOF 7 is MYSTRAN 18x18 elem DOF  5
                                                     11, & ! ID(8) = 11 means virgin 9x9 elem DOF 8 is MYSTRAN 18x18 elem DOF 11
                                                     17 /) ! ID(9) = 17 means virgin 9x9 elem DOF 9 is MYSTRAN 18x18 elem DOF 17
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TPLT2_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: PSI               ! Angle to rotate orthotropic mat'l matrix of a sub-tria to align w QUAD
      REAL(DOUBLE) , INTENT(IN)       :: X2E               ! x coord of elem node 2
      REAL(DOUBLE) , INTENT(IN)       :: X3E               ! x coord of elem node 3
      REAL(DOUBLE) , INTENT(IN)       :: Y3E               ! y coord of elem node 3
      REAL(DOUBLE) , INTENT(OUT)      :: BIG_BB(3,18,1)    ! Strain-displ matrix for bending for all DOF's
      REAL(DOUBLE) , INTENT(OUT)      :: B2V(3,9)          ! Strain recovery matrix for virgin DOF's for bending
      REAL(DOUBLE) , INTENT(OUT)      :: B3V(3,9)          ! Strain recovery matrix for virgin DOF's for transverse shear
      REAL(DOUBLE) , INTENT(OUT)      :: KV(9,9)           ! KB + PHISQ*KS (the 9x9 virgin stiffness matrix for MIN3)
      REAL(DOUBLE) , INTENT(OUT)      :: PPV(9,NSUB)       ! The 9xNSUB  virgin thermal  load matrix for MIN3
      REAL(DOUBLE) , INTENT(OUT)      :: PTV(9,NTSUB)      ! The 9xNTSUB virgin pressure load matrix for MIN3
      REAL(DOUBLE) , INTENT(OUT)      :: S2V(3,9)          ! Stress recovery matrix for virgin DOF's for bending
      REAL(DOUBLE) , INTENT(OUT)      :: S3V(3,9)          ! Stress recovery matrix for virgin DOF's for transverse shear
      REAL(DOUBLE)                    :: ALP_TRIA(3)       ! Col of ALPVEC_TRIA

      REAL(DOUBLE)                    :: A(3)              ! 3 diffs in two x coords of the triangle (some x coords are 0)
      REAL(DOUBLE)                    :: A4                ! 4*AREA
      REAL(DOUBLE)                    :: A42               ! 4*AREA**2
      REAL(DOUBLE)                    :: A1(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Alpha*a)
      REAL(DOUBLE)                    :: A2(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Alpha*b)
      REAL(DOUBLE)                    :: B(3)              ! 3 diffs in two y coords of the triangle (some y coords are 0)
      REAL(DOUBLE)                    :: B1(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Beta*a)
      REAL(DOUBLE)                    :: B2(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Beta*b)
      REAL(DOUBLE)                    :: BB(3,9)           ! Bending strain-displ matrix for the MIN3 elem (from subr BBMIN3)
      REAL(DOUBLE)                    :: BS(2,9)           ! Shear strain-displ matrix for the MIN3 elem (from subr BSMIN3)
      REAL(DOUBLE)                    :: DUM0(9)           ! Intermediate variables used in calc PTE, PPE (thermal, pressure loads)
      REAL(DOUBLE)                    :: DUM31(3)          ! Intermadiate result in calc KE elem stiffness
      REAL(DOUBLE)                    :: DUM1(3,3)         ! Intermadiate result in calc KE elem stiffness
      REAL(DOUBLE)                    :: DUM2(3,3)         ! Intermadiate result in calc KE elem stiffness
      REAL(DOUBLE)                    :: DUM3(3,3)         ! Intermadiate result in calc KE elem stiffness
      REAL(DOUBLE)                    :: DUM4(3,3)         ! Intermadiate result in calc KE elem stiffness
      REAL(DOUBLE)                    :: DUM5(3,9)         ! Intermadiate result in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: DUM6(2,9)         ! Intermadiate result in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: DUM22(2,2)        ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM33(3,3)        ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM64(6,4)        ! Intermediate matrix
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare to real zero
      REAL(DOUBLE)                    :: FXX(3,3)          ! Intermadiate result in calc BB (Alex Tessler matrix fxx)
      REAL(DOUBLE)                    :: FXY(3,3)          ! Intermadiate result in calc BB (Alex Tessler matrix fxy)
      REAL(DOUBLE)                    :: FYY(3,3)          ! Intermadiate result in calc BB (Alex Tessler matrix fyy)
      REAL(DOUBLE)                    :: I00(3,3)          ! Intermadiate result in calc KS shear stiffness
      REAL(DOUBLE)                    :: IX0(3,3)          ! Intermadiate result in calc KS shear stiffness
      REAL(DOUBLE)                    :: IY0(3,3)          ! Intermadiate result in calc KS shear stiffness
      REAL(DOUBLE)                    :: KB(9,9)           ! Bending stiffness contribution to KE for the MIN3 elem
      REAL(DOUBLE)                    :: KS(9,9)           ! PHISQ*KS is the shear stiff contribution to KE for the MIN3 elem
      REAL(DOUBLE)                    :: QCONS             ! = AREA/24, used in calc PPE pressure loads
      REAL(DOUBLE)                    :: S1(3,3)           ! Intermadiate result in calc shear stiffness, KS
      REAL(DOUBLE)                    :: S2(3,3)           ! Intermadiate result in calc shear stiffness, KS
      REAL(DOUBLE)                    :: T1(3,3)           ! Intermadiate result in calc shear stiffness, KS
      REAL(DOUBLE)                    :: T2(3,3)           ! Intermadiate result in calc shear stiffness, KS
      REAL(DOUBLE)                    :: T3(3,3)           ! Intermadiate result in calc shear stiffness, KS
      REAL(DOUBLE)                    :: T4(3,3)           ! Intermadiate result in calc shear stiffness, KS
      REAL(DOUBLE)                    :: TF(6,6)           ! Transforms 6 stress and 6x6 material mats from material to element axes
!                                                            (TF' transforms strains)
      REAL(DOUBLE)                    :: TME(3,3)          ! Coord transf matrix which will rotate a vector in local element coord
!                                                            system to a vector in the MIN4T QUAD4 matl coord system (Um = TME*Ue)
      REAL(DOUBLE)                    :: TF_MB(3,3)        ! Portion of TF: transforms 3x3 EM, EB, EBM from material to elem axes
      REAL(DOUBLE)                    :: TF_TS(2,2)        ! Portion of TF: transforms 3x3 ET from material to elem axes
      REAL(DOUBLE)                    :: XI(3)
 
! Following  are matl matrices used when sub-trias of a MIN4T QUAD4 need to be transformed to align with orthotropic matl angles

      REAL(DOUBLE)                    :: ALPVEC_TRIA(6,MEMATC) 
      REAL(DOUBLE)                    :: EALP_TRIA(3)      ! Intermed var used in calc STEi therm stress coeffs
      REAL(DOUBLE)                    :: EB0(3,3)          ! Plane stress matl matrix for bending before coord transformation
      REAL(DOUBLE)                    :: EBM0(3,3)         ! Bend/membr coupling matl matrix before coord transformation
      REAL(DOUBLE)                    :: ET0(2,2)          ! 2D transverse shear matl matrix before coord transformation
      REAL(DOUBLE)                    :: EB_TRIA(3,3)      ! Plane stress matl matrix for bending after coord transformation
      REAL(DOUBLE)                    :: EBM_TRIA(3,3)     ! Bend/membr coupling matl matrix before after transformation
      REAL(DOUBLE)                    :: ET_TRIA(2,2)      ! 2D transverse shear matl matrix before after transformation

      REAL(DOUBLE)                    :: SHELL_B0_TRIA(3,3)! SHELL_B_TRIA before coord transformation
      REAL(DOUBLE)                    :: SHELL_D0_TRIA(3,3)! SHELL_D_TRIA before coord transformation
      REAL(DOUBLE)                    :: SHELL_T0_TRIA(2,2)! SHELL_T_TRIA before coord transformation
      REAL(DOUBLE)                    :: SHELL_B_TRIA(3,3) ! SHELL_B_TRIA after  coord transformation
      REAL(DOUBLE)                    :: SHELL_D_TRIA(3,3) ! SHELL_D_TRIA after  coord transformation
      REAL(DOUBLE)                    :: SHELL_T_TRIA(2,2) ! SHELL_T_TRIA after  coord transformation

                                                           ! SHELL_D_TRIA before coord transformation
      REAL(DOUBLE)                    :: SHELL_DALP0_TRIA(3)

                                                           ! SHELL_D_TRIA after  coord transformation
      REAL(DOUBLE)                    :: SHELL_DALP_TRIA(3)

      INTRINSIC DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      EPS1   = EPSIL(1)

      IERROR = 0

      DO I=1,9
         DO J=1,9
            KV(I,J) = ZERO
         ENDDO
         DO J=1,NSUB
            PPV(I,J) = ZERO
         ENDDO
         DO J=1,NTSUB
            PTV(I,J) = ZERO
         ENDDO
      ENDDO

! **********************************************************************************************************************************
! If this subr is being called because the triangular shell element is part of a MIN4T QUAD4 with orthotropic mat'l properties then
! we need to re-orient those ortho material properties to be aligned with the quad. 

! Set initial values before coord transform (NOTE: Only needed for DEBUG(53)

      DO I=1,3
         SHELL_DALP0_TRIA(I) = SHELL_DALP(I)
         DO J=1,3
            EB0(I,J)           = EB(I,J)
            SHELL_D0_TRIA(I,J) = SHELL_D(I,J) 
            EBM0(I,J)          = EBM(I,J)
            SHELL_B0_TRIA(I,J) = SHELL_B(I,J) 
         ENDDO
      ENDDO

      DO I=1,2
         DO J=1,2
            ET0(I,J)           = ET(I,J)
            SHELL_T0_TRIA(I,J) = SHELL_T(I,J) 
         ENDDO
      ENDDO

      DO I=1,3
         SHELL_DALP_TRIA(I) = SHELL_DALP(I)
         DO J=1,3
            EB_TRIA(I,J)      = EB(I,J)
            SHELL_D_TRIA(I,J) = SHELL_D(I,J) 
            EBM_TRIA(I,J)     = EBM(I,J)
            SHELL_B_TRIA(I,J) = SHELL_B(I,J) 
         ENDDO
      ENDDO

      DO I=1,2
         DO J=1,2
            ET_TRIA(I,J)      = ET(I,J)
            SHELL_T_TRIA(I,J) = SHELL_T(I,J) 
         ENDDO
      ENDDO

      DO I=1,6
         DO J=1,MEMATC
            ALPVEC_TRIA(I,J) = ALPVEC(I,J)
         ENDDO
      ENDDO

                                                           ! The following 3 conditions have to be met before we look at mat'l props
      IF ((MN4T_QD == 'Y') .AND. (TRIA_NUM >= 1) .AND. (TRIA_NUM <= 4)) THEN
                                                           ! If either bending or transverse shear props are ortho we need TF matrix
         CBMIN4T = CBMIN3
         IF ((MTRL_TYPE(2) == 8) .OR. (MTRL_TYPE(3) == 8)) THEN

            CALL PLANE_COORD_TRANS_21 ( PSI, TME, SUBR_NAME )
            CALL MATL_TRANSFORM_MATRIX ( TME, TF )
                                                        ! TF_MB is for Sxx, Syy, Sxy which are rows and cols 1,2,4 from TF
            TF_MB(1,1) = TF(1,1)     ;     TF_MB(1,2) = TF(1,2)     ;     TF_MB(1,3) = TF(1,4)
            TF_MB(2,1) = TF(2,1)     ;     TF_MB(2,2) = TF(2,2)     ;     TF_MB(2,3) = TF(2,4)
            TF_MB(3,1) = TF(4,1)     ;     TF_MB(3,2) = TF(4,2)     ;     TF_MB(3,3) = TF(4,4)

                                                        ! TF_ET is for Syz, Szx which are rows and cols 5,6 from TF
            TF_TS(1,1) = TF(5,5)     ;     TF_TS(1,2) = TF(5,6)
            TF_TS(2,1) = TF(6,5)     ;     TF_TS(2,2) = TF(6,6)

!           ------------------------------------------------------------------------------------------------------------------------
            IF ((MTRL_TYPE(2) == 2) .OR. (MTRL_TYPE(2) == 8)) THEN   ! Transform bending material matrix
               CALL MATMULT_FFF   ( EB_TRIA , TF_MB  , 3, 3, 3, DUM33)
               CALL MATMULT_FFF_T ( TF_MB   , DUM33  , 3, 3, 3, EB_TRIA)
            ENDIF

            IF ((MTRL_TYPE(2) == 2) .OR. (MTRL_TYPE(2) == 8)) THEN   ! Transform SHELL_D matrix
               CALL MATMULT_FFF   ( SHELL_D_TRIA , TF_MB  , 3, 3, 3, DUM33)
               CALL MATMULT_FFF_T ( TF_MB        , DUM33  , 3, 3, 3, SHELL_D_TRIA)
            ENDIF
!           ------------------------------------------------------------------------------------------------------------------------
            IF ((MTRL_TYPE(3) == 2) .OR. (MTRL_TYPE(3) == 8)) THEN   ! Transform transverse shear material matrix
               CALL MATMULT_FFF   ( ET_TRIA , TF_TS  , 2, 2, 2, DUM22)
               CALL MATMULT_FFF_T ( TF_TS   , DUM22  , 2, 2, 2, ET_TRIA)
            ENDIF

            IF ((MTRL_TYPE(3) == 2) .OR. (MTRL_TYPE(3) == 8)) THEN   ! Transform SHELL_T matrix
               CALL MATMULT_FFF   ( SHELL_T_TRIA , TF_TS  , 2, 2, 2, DUM22)
               CALL MATMULT_FFF_T ( TF_TS        , DUM22  , 2, 2, 2, SHELL_T_TRIA)
            ENDIF
!           ------------------------------------------------------------------------------------------------------------------------
            IF ((MTRL_TYPE(4) == 2) .OR. (MTRL_TYPE(4) == 8)) THEN   ! Transform coupled bending/membrane material matrix
               CALL MATMULT_FFF   ( EBM_TRIA, TF_MB  , 3, 3, 3, DUM33)
               CALL MATMULT_FFF_T ( TF_MB   , DUM33  , 3, 3, 3, EBM_TRIA)
            ENDIF

            IF ((MTRL_TYPE(4) == 2) .OR. (MTRL_TYPE(4) == 8)) THEN   ! Transform SHELL_B matrix
               CALL MATMULT_FFF   ( SHELL_B_TRIA , TF_MB  , 3, 3, 3, DUM33)
               CALL MATMULT_FFF_T ( TF_MB        , DUM33  , 3, 3, 3, SHELL_B_TRIA)
            ENDIF
!           ------------------------------------------------------------------------------------------------------------------------

            CALL MATMULT_FFF_T (TF, ALPVEC_TRIA, 6,6, MEMATC, DUM64) ! Transform CTE matrix
            DO I=1,6
               DO J=1,MEMATC
                  ALPVEC_TRIA(I,J) = DUM64(I,J)
               ENDDO
            ENDDO

            CALL MATMULT_FFF_T (TF_MB, SHELL_DALP_TRIA, 3, 3, 1, DUM31) ! Transform SHELL_DALP matrix
            DO I=1,3
               SHELL_DALP_TRIA(I) = DUM31(I)
            ENDDO
!           ------------------------------------------------------------------------------------------------------------------------

            IF (DEBUG(53) > 0) THEN
               CALL DEBUG_ROT_AXES_2
            ENDIF

         ENDIF

      ENDIF

! **********************************************************************************************************************************
      A(1) =  X3E - X2E
      A(2) = -X3E
      A(3) =  X2E
      B(1) = -Y3E
      B(2) =  Y3E
      B(3) =  ZERO
  
      A4  = FOUR*AREA
      A42 = A4*AREA
! BB is used in several places below:

      CALL BBMIN3 ( A, B, AREA, '(bending strains)', 'Y', BB )

! **********************************************************************************************************************************
! Determine element thermal loads. Code is only valid for materials with ALPHA-12 = 0.
 
      IF (OPT(2) == 'Y') THEN
 
         CALL MATMULT_FFF_T ( BB, SHELL_DALP, 3, 9, 1, DUM0 ) 
         DO J=1,NTSUB
            DO I=1,9
               PTV(I,J) = AREA*DUM0(I)*DT(4,J)
               IF (CALC_EMATS == 'Y') THEN
                  PTE(ID(I),J) = PTV(I,J)
               ENDIF
            ENDDO 
         ENDDO 
 
      ENDIF   
  
! **********************************************************************************************************************************
! Calculate element stiffness matrix KE. Note that we need to calc KE if the stress recovery matrices (for OPT(3)) are to be cal'd
! since the BE strain recovery matrices use PHI_SQ
 
      IF ((OPT(4) == 'Y') .OR. (OPT(3) == 'Y')) THEN
 
         DO I=1,9
            DO J=1,9
               KB(I,J) = ZERO
            ENDDO   
         ENDDO 
 
! Bending stiffness terms (KB)
  
         DO I=1,3
            DO J=1,3
               FXX(I,J) = B(I)*B(J)/A42
               FYY(I,J) = A(I)*A(J)/A42
               FXY(I,J) = B(I)*A(J)/A42
            ENDDO   
         ENDDO 
         DO I=1,3
            DO J=1,3
               KB(I+3,J+3)= AREA*(SHELL_D_TRIA(2,2)*FYY(I,J) + SHELL_D_TRIA(2,3)*(FXY(I,J) + FXY(J,I)) + SHELL_D_TRIA(3,3)*FXX(I,J)) 
            ENDDO   
         ENDDO 
  
         DO I=1,3
            DO J=1,3
               KB(I+3,J+6) = -AREA*(SHELL_D_TRIA(1,2)*FXY(J,I) + SHELL_D_TRIA(2,3)*FYY(I,J) + SHELL_D_TRIA(1,3)*FXX(I,J) +         &
                                    SHELL_D_TRIA(3,3)*FXY(I,J))
               KB(J+6,I+3) = KB(I+3,J+6)
            ENDDO   
         ENDDO 
  
         DO I=1,3
            DO J=1,3
               KB(I+6,J+6)= AREA*(SHELL_D_TRIA(1,1)*FXX(I,J) + SHELL_D_TRIA(1,3)*(FXY(I,J) + FXY(J,I)) + SHELL_D_TRIA(3,3)*FYY(I,J))
            ENDDO   
         ENDDO 
  
         BENSUM = ZERO
         DO I=4,9
            BENSUM = BENSUM + KB(I,I)
         ENDDO 
! Shear stress terms (KS)
  
         DO I=1,9
            DO J=1,9
               KS(I,J) = ZERO
            ENDDO   
         ENDDO 

         B2(1,1) =   ZERO
         B2(2,2) =   ZERO
         B2(3,3) =   ZERO
         B2(1,2) =  -B(2)*B(3)/A4
         B2(1,3) =  -B2(1,2)
         B2(2,1) =   B(1)*B(3)/A4
         B2(2,3) =  -B2(2,1)
         B2(3,1) =  -B(1)*B(2)/A4
         B2(3,2) =  -B2(3,1)

         A2(1,1) = ( A(2)*B(3) - A(3)*B(2))/A4
         A2(2,2) = (-A(1)*B(3) + A(3)*B(1))/A4
         A2(3,3) = (-A(2)*B(1) + A(1)*B(2))/A4
         A2(1,2) =  -A(2)*B(3)/A4
         A2(1,3) =   A(3)*B(2)/A4
         A2(2,1) =   A(1)*B(3)/A4
         A2(2,3) =  -A(3)*B(1)/A4
         A2(3,1) =  -A(1)*B(2)/A4
         A2(3,2) =   A(2)*B(1)/A4

         B1(1,1) = (-B(2)*A(3) + B(3)*A(2))/A4
         B1(2,2) = ( B(1)*A(3) - B(3)*A(1))/A4
         B1(3,3) = ( B(2)*A(1) - B(1)*A(2))/A4
         B1(1,2) =   B(2)*A(3)/A4
         B1(1,3) =  -B(3)*A(2)/A4
         B1(2,1) =  -B(1)*A(3)/A4
         B1(2,3) =   B(3)*A(1)/A4
         B1(3,1) =   B(1)*A(2)/A4
         B1(3,2) =  -B(2)*A(1)/A4

         A1(1,1) =   ZERO
         A1(2,2) =   ZERO
         A1(3,3) =   ZERO
         A1(1,2) =   A(2)*A(3)/A4
         A1(1,3) =  -A1(1,2)
         A1(2,1) =  -A(1)*A(3)/A4
         A1(2,3) =  -A1(2,1)
         A1(3,1) =   A(1)*A(2)/A4
         A1(3,2) =  -A1(3,1)

         I00(1,2) = AREA/TWELVE
         I00(1,3) = I00(1,2)
         I00(2,1) = I00(1,2)
         I00(2,3) = I00(1,2)
         I00(3,1) = I00(1,2)
         I00(3,2) = I00(1,2)
         I00(1,1) = TWO*I00(1,2)
         I00(2,2) = I00(1,1)
         I00(3,3) = I00(1,1)

         IX0(1,1) = B(1)/SIX
         IX0(1,2) = IX0(1,1)
         IX0(1,3) = IX0(1,1)
         IX0(2,1) = B(2)/SIX
         IX0(2,2) = IX0(2,1)
         IX0(2,3) = IX0(2,1)
         IX0(3,1) = B(3)/SIX
         IX0(3,2) = IX0(3,1)
         IX0(3,3) = IX0(3,1)

         IY0(1,1) = A(1)/SIX
         IY0(1,2) = IY0(1,1)
         IY0(1,3) = IY0(1,1)
         IY0(2,1) = A(2)/SIX
         IY0(2,2) = IY0(2,1)
         IY0(2,3) = IY0(2,1)
         IY0(3,1) = A(3)/SIX
         IY0(3,2) = IY0(3,1)
         IY0(3,3) = IY0(3,1)

         DO I=1,3
            DO J=1,3
               IF (I == J) THEN
                  S1(I,J) = A2(I,J) + ONE
                  S2(I,J) = B1(I,J) + ONE
               ELSE
                  S1(I,J) = A2(I,J)
                  S2(I,J) = B1(I,J)
               ENDIF
            ENDDO   
         ENDDO 

         DO I=1,3
            DO J=1,3
               T1(I,J) = (SHELL_T_TRIA(1,1)*B2(I,J) + SHELL_T_TRIA(1,2)*S1(I,J))
               T2(I,J) = (SHELL_T_TRIA(1,2)*B2(I,J) + SHELL_T_TRIA(2,2)*S1(I,J))
               T3(I,J) = (SHELL_T_TRIA(1,1)*S2(I,J) + SHELL_T_TRIA(1,2)*A1(I,J))
               T4(I,J) = (SHELL_T_TRIA(1,2)*S2(I,J) + SHELL_T_TRIA(2,2)*A1(I,J))
            ENDDO   
         ENDDO
                                                        ! 3x3 KS-11 Partition
         DO I=1,3
            DO J=1,3
               KS(I,J) = AREA*(SHELL_T_TRIA(1,1)*FXX(I,J) + SHELL_T_TRIA(1,2)*(FXY(I,J) + FXY(J,I)) + SHELL_T_TRIA(2,2)*FYY(I,J)) 
            ENDDO   
         ENDDO 
                                                        ! 3x3 KS-12, 21 Partition
         CALL MATMULT_FFF ( IX0, T1, 3, 3, 3, DUM1 )
         CALL MATMULT_FFF ( IY0, T2, 3, 3, 3, DUM2 )
         DO I=1,3
            DO J=1,3
               KS(I  ,J+3) = -DUM1(I,J) - DUM2(I,J)
               KS(J+3,I  ) = KS(I,J+3)
            ENDDO   
         ENDDO 
                                                        ! 3x3 KS-13, 31 Partition
         CALL MATMULT_FFF ( IX0, T3, 3, 3, 3, DUM1 )
         CALL MATMULT_FFF ( IY0, T4, 3, 3, 3, DUM2 )
         DO I=1,3
            DO J=1,3
               KS(I  ,J+6) = DUM1(I,J) + DUM2(I,J)
               KS(J+6,I  ) = KS(I,J+6)
            ENDDO   
         ENDDO 
                                                        ! 3x3 KS-22 Partition
         CALL MATMULT_FFF   ( I00, T1  , 3, 3, 3, DUM3 )
         CALL MATMULT_FFF_T ( B2 , DUM3, 3, 3, 3, DUM1 )
         CALL MATMULT_FFF   ( I00, T2  , 3, 3, 3, DUM4 )
         CALL MATMULT_FFF_T ( S1 , DUM4, 3, 3, 3, DUM2 )
         DO I=1,3
            DO J=1,3
               KS(I+3,J+3) = DUM1(I,J) + DUM2(I,J)
            ENDDO   
         ENDDO 
                                                        ! 3x3 KS-23, 32 Partition
         CALL MATMULT_FFF   ( I00, T3  , 3, 3, 3, DUM3 )
         CALL MATMULT_FFF_T ( B2 , DUM3, 3, 3, 3, DUM1 )
         CALL MATMULT_FFF   ( I00, T4  , 3, 3, 3, DUM4 )
         CALL MATMULT_FFF_T ( S1 , DUM4, 3, 3, 3, DUM2 )
         DO I=1,3
            DO J=1,3
               KS(I+3,J+6) = -DUM1(I,J) - DUM2(I,J)
               KS(J+6,I+3) = KS(I+3,J+6)
            ENDDO   
         ENDDO 
                                                        ! 3x3 KS-33 Partition
         CALL MATMULT_FFF   ( I00, T3  , 3, 3, 3, DUM3 )
         CALL MATMULT_FFF_T ( S2 , DUM3, 3, 3, 3, DUM1 )
         CALL MATMULT_FFF   ( I00, T4  , 3, 3, 3, DUM4 )
         CALL MATMULT_FFF_T ( A1 , DUM4, 3, 3, 3, DUM2 )
         DO I=1,3
            DO J=1,3
               KS(I+6,J+6) = DUM1(I,J) + DUM2(I,J)
            ENDDO   
         ENDDO 

         SHRSUM = ZERO
         DO I=4,9
            SHRSUM = SHRSUM + KS(I,I)
         ENDDO 
  ! Now calculate the finite elem shear factor, PHI_SQ  

         IF (SHRSUM > EPS1) THEN
            CALL CALC_PHI_SQ ( IERROR )
         ELSE
            PHI_SQ = ZERO
         ENDIF
! Return if IERROR > 0

         IF (IERROR > 0) RETURN
 
         DO I=1,9
            DO J=1,9
               KV(I,J) = KB(I,J) + PHI_SQ*KS(I,J)
               IF (CALC_EMATS == 'Y') THEN
                  KE(ID(I),ID(J)) = KE(ID(I),ID(J)) + KV(I,J)
               ENDIF
            ENDDO   
         ENDDO 
         IF (DEBUG(54) == 1) THEN
            IF (MN4T_QD == 'Y') THEN
               WRITE(F06,'(A,I2,A,A,I8))') ' KV, in TPLT2 for MIN4T QUAD4: tria ', TRIA_NUM, ' of 4 for ',TRIM(TYPE), EID
            ELSE
               WRITE(F06,'(A,I8)') ' KV in TPLT2 for TRIA3 ', EID
            ENDIF
            WRITE(F06,*) '    Column  3      Column  4      Column  5      Column  9      Column 10      Column 11      Column 15',&
                       '      Column 16      Column 17'
            DO I=1,9
               WRITE(F06,'(9ES15.6)') (KV(I,J),J=1,9)
            ENDDO
            WRITE(F06,*)
         ENDIF
  
! Set lower triangular partition equal to upper partition
         IF (CALC_EMATS == 'Y') THEN
            DO I=2,ELDOF
               DO J=1,I-1
                  KE(I,J) = KE(J,I)
               ENDDO   
            ENDDO
         ENDIF 
  
         FCONV(3) = FCONV_SHEAR_THICK                      ! NOTE: PHI_SQ removed as multiplier based on error found on 10/12/11
      ENDIF
  
! **********************************************************************************************************************************
! Calculate BE2, SE2 matrix (3 x 18) for strain/stress data recovery.
! Note: stress recovery matrices only make sense for individual plies (or whole elem if only 1 "ply")
 
! BS (transverse shear strain-displ matrices) is used in several places below:

      CALL BBMIN3 ( A, B, AREA, '(bending strains)', 'N', BB )
  
      XI(1) = ONE/THREE
      XI(2) = ONE/THREE
      XI(3) = ONE/THREE
      CALL BSMIN3 ( XI, A, B, AREA, '(transverse shear strains)', 'Y', BS )

! Calc BEi matrices regardless of OPT

      DO I=1,3
         DO J=1,9
            B2V(I,J) = ZERO
            B3V(I,J) = ZERO
         ENDDO
      ENDDO

      DO I=1,3
         DO J=1,9
            B2V(I,J)       = BB(I,J)
            BE2(I,ID(J),1) = BB(I,J)
         ENDDO   
      ENDDO

      DO I=1,2
         DO J=1,9
            B3V(I,J)       = BS(I,J)
            BE3(I,ID(J),1) = BS(I,J)
         ENDDO   
      ENDDO 
  
! SE2, STE2 generated in elem coords. Then, in LINK9 the stresses, calc'd in elem coords, will be transformed to ply coords

      IF (OPT(3) == 'Y') THEN

         DO I=1,3
            DO J=1,9
               S2V(I,J) = ZERO
               S3V(I,J) = ZERO
            ENDDO
         ENDDO

         CALL MATMULT_FFF ( EB_TRIA, BB, 3, 3, 9, DUM5 )   ! Generate SE2 in element coords (at this point EB is elem coords)
         DO I=1,3
            DO J=1,9
               S2V(I,J)       = DUM5(I,J)
               SE2(I,ID(J),1) = DUM5(I,J)
            ENDDO   
         ENDDO

         ALP_TRIA(1) = ALPVEC_TRIA(1,1)
         ALP_TRIA(2) = ALPVEC_TRIA(2,1)
         ALP_TRIA(3) = ALPVEC_TRIA(3,1)

         CALL MATMULT_FFF ( EB_TRIA, ALP_TRIA, 3, 3, 1, EALP_TRIA )  ! Mult EB (ply coords) times ALP (ply coords)

         DO J=1,NTSUB                                      ! Thermal stress terms
            DO I=1,3
               STE2(I,J,1) = EALP_TRIA(I)*DT(4,J)
            ENDDO   
         ENDDO 

         CALL MATMULT_FFF ( ET_TRIA, BS, 2, 2, 9, DUM6 )
         DO I=1,2
            DO J=1,9
               S3V(I,J)       = PHI_SQ*DUM6(I,J)
               SE3(I,ID(J),1) = PHI_SQ*DUM6(I,J)
            ENDDO   
         ENDDO 
  
      ENDIF
  
! **********************************************************************************************************************************
! If element is a composite and if it is a nonsym layup we need to calc BIG_BB for later use

      DO I=1,3
         DO J=1,18
            BIG_BB(I,J,1) = ZERO
         ENDDO
      ENDDO

      IF ((PCOMP_PROPS == 'Y') .AND. (PCOMP_LAM == 'NON')) THEN

         DO I=1,3
            DO J=1,9
               BIG_BB(I,ID(J),1) = BB(I,J)
            ENDDO
         ENDDO
  
      ENDIF

! **********************************************************************************************************************************
! Determine element pressure loads. 
 
      IF (OPT(5) == 'Y') THEN
      
         IF (DEBUG(16) == 0) THEN                          ! Generate PPE as work equilavent loads

            QCONS   = AREA/(TWO*TWELVE)
            DUM0(1) = QCONS*EIGHT
            DUM0(2) = QCONS*EIGHT
            DUM0(3) = QCONS*EIGHT
            DUM0(4) = QCONS*(-B(3) + B(2))
            DUM0(5) = QCONS*( B(3) - B(1))
            DUM0(6) = QCONS*( B(1) - B(2))
            DUM0(7) = QCONS*(-A(3) + A(2))
            DUM0(8) = QCONS*( A(3) - A(1))
            DUM0(9) = QCONS*( A(1) - A(2))
  
            DO J=1,NSUB
               DO I=1,9
                  PPV(I,J)     = PRESS(3,J)*DUM0(I)
                  IF (CALC_EMATS == 'Y') THEN
                     PPE(ID(I),J) = PPV(I,J)
                  ENDIF
               ENDDO 
            ENDDO   

         ELSE                                              ! Generate PPE as static equilavent loads

            DO J=1,NSUB
               PPV( 1,J) = AREA*PRESS(3,J)/THREE
               PPV( 2,J) = AREA*PRESS(3,J)/THREE
               PPV( 3,J) = AREA*PRESS(3,J)/THREE
               IF (CALC_EMATS == 'Y') THEN
                  PPE( 3,J) = AREA*PRESS(3,J)/THREE
                  PPE( 9,J) = AREA*PRESS(3,J)/THREE
                  PPE(15,J) = AREA*PRESS(3,J)/THREE
               ENDIF
            ENDDO 
  
         ENDIF

      ENDIF
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEBUG_ROT_AXES_2 

      USE IOUNT1, ONLY                :  F06
      USE MODEL_STUF, ONLY            :  EID

      IMPLICIT NONE

      CHARACTER(LEN=100)              :: MI(9)             ! Messages to be written out

! **********************************************************************************************************************************
      DO I=1,9
         MI(I)(1:) = ' '
      ENDDO

! Write outputs

      WRITE(F06,98720)

      WRITE(F06,'(A,I2,A,I8,A,1ES14.6,A)') ' Data for TRIA_NUM ', tria_num,' of MIN4T QUAD4 elem ', eid, ' which is to be rotated',&
                                          conv_rad_deg*psi,' deg'
      WRITE(F06,55566)

      MI(1) = ' Transforms 6 stress and 6x6 matl matrices from tria to quad axes (TF transpose transforms strains)'
      WRITE(F06,99664) 'TF', MI(1)
      DO I=1,3
         WRITE(F06,99667) (TF(I,J),J=1,6)
      ENDDO
      WRITE(F06,*)
      DO I=4,6
         WRITE(F06,99667) (TF(I,J),J=1,6)
      ENDDO
      WRITE(F06,*)

      MI(2) = ' Portion of TF: transforms 3x3 EB_TRIA, EBM_TRIA from tria to quad axes'
      WRITE(F06,99664) 'TF_MB', MI(2)
      DO I=1,3
         WRITE(F06,99668) (TF_MB(I,J),J=1,3)
      ENDDO
      WRITE(F06,*)

      MI(3) = '  Portion of TF: transforms 2x2 ET_TRIA from tria to quad axes'
      WRITE(F06,99664) 'TF_TS', MI(3)
      DO I=1,2
         WRITE(F06,99669) (TF_TS(I,J),J=1,2)
      ENDDO
      WRITE(F06,*)

      MI(4) = ' Coord transf matrix which will rotate a vector in local elem'
      MI(5) = ' coord system to a vector in the material coord system (Um = TME*Ue)'
      WRITE(F06,99664) 'TME', MI(4)   ;   WRITE(F06,99663) MI(5)
      DO I=1,3
         WRITE(F06,99668) (TME(I,J),J=1,3)
      ENDDO
      WRITE(F06,*)


bend: IF ((MTRL_TYPE(2) == 2) .OR. (MTRL_TYPE(2) == 8)) THEN

         WRITE(F06,99665) 'EB_TRIA '
         DO I=1,3
            WRITE(F06,99668) (EB0(I,J),J=1,3), (EB_TRIA(I,J),J=1,3)
         ENDDO
         WRITE(F06,*)   ;   WRITE(F06,*)

         WRITE(F06,99665) 'SHELL_D_TRIA'
         DO I=1,3
            WRITE(F06,99668) (SHELL_D0_TRIA(I,J),J=1,3), (SHELL_D_TRIA(I,J),J=1,3)
         ENDDO
         WRITE(F06,*)

      ENDIF bend

tshr: IF ((MTRL_TYPE(3) == 2) .OR. (MTRL_TYPE(3) == 8)) THEN

         write(f06,99665) 'ET_TRIA '
         do i=1,2
            write(f06,99669) (et0(i,j),j=1,2), (et_tria(i,j),j=1,2)
         enddo
         write(f06,*)   ;   write(f06,*)

         write(f06,99665) 'SHELL_T_TRIA'
         DO I=1,2
            WRITE(F06,99669) (SHELL_T0_TRIA(I,J),J=1,2), (SHELL_T_TRIA(I,J),J=1,2)
         ENDDO
         WRITE(F06,*)

      ENDIF tshr

mbend:IF ((MTRL_TYPE(4) == 2) .OR. (MTRL_TYPE(4) == 8)) THEN

         WRITE(F06,99665) 'EBM_TRIA'
         DO I=1,3
            WRITE(F06,99668) (EBM0(I,J),J=1,3), (EBM_TRIA(I,J),J=1,3)
         ENDDO
         WRITE(F06,*)   ;   WRITE(F06,*)

         WRITE(F06,99665) 'SHELL_B_TRIA'
         DO I=1,3
            WRITE(F06,99668) (SHELL_B0_TRIA(I,J),J=1,3), (SHELL_B_TRIA(I,J),J=1,3)
         ENDDO
         WRITE(F06,*)

      ENDIF mbend

      WRITE(F06,*)

      WRITE(F06,98799)
 
      WRITE(F06,*)

! **********************************************************************************************************************************
55566 FORMAT(1X,'------------------------------------------------------------------------------------------',/)

67549 FORMAT(6(1ES14.6))                                                                                                            

99663 FORMAT(1X,A)

99664 FORMAT('  Transformation matrix ',2a)

99665 FORMAT(16x,'Material matrix ',a12,' before/after TF transformation',/,                                                       &
             19x,'before',41X,'after',/,'  ----------------------------------------      ----------------------------------------')
 
99667 FORMAT(3(1ES14.6), 4X,3(1ES14.6))                                                                                            

99668 FORMAT(3(1ES14.6), 4X,3(1ES14.6))                                                                                                            

99669 FORMAT(7X,2(1ES14.6),18X,2(1ES14.6))                                                                                                            

98720 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::START DEBUG(53) OUTPUT FROM SUBROUTINE ROT_AXES_MATL_TO_LOC:::::::::::::::::::',&
             ':::::::::::::::::',/)

98799 FORMAT(' :::::::::::::::::::::::::::::::::::::END DEBUG(53) OUTPUT FROM SUBROUTINE ROT_AXES_MATL_TO_LOC::::::::::::::::::::',&
             ':::::::::::::::::'                                                                                                ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_ROT_AXES_2

      END SUBROUTINE TPLT2
