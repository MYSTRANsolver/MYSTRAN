! ###############################################################################################################################
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

   MODULE TPLT2_Interface

   INTERFACE

      SUBROUTINE TPLT2(OPT, AREA, X2E, X3E, Y3E, CALC_EMATS, IERROR, KV, PTV, PPV, B2V, B3V, S2V, S3V, BIG_BB, MN4T_QD,TRIA_NUM,PSI)


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEMATC, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TPLT2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, THREE, FOUR, SIX, EIGHT, TWELVE, CONV_RAD_DEG
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, BE3, BENSUM, DT, EB, EBM, EID, ET, ELDOF, FCONV, KE,                         &
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

      END SUBROUTINE TPLT2

   END INTERFACE

   END MODULE TPLT2_Interface

