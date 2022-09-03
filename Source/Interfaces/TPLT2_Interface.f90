! ###############################################################################################################################
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

   MODULE TPLT2_Interface

   INTERFACE

      SUBROUTINE TPLT2(OPT, AREA, X2E, X3E, Y3E, CALC_EMATS, IERROR, KV, PTV, PPV, B2V, B3V, S2V, S3V, BIG_BB, MN4T_QD,TRIA_NUM,PSI)


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEMATC, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TPLT2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, THREE, FOUR, SIX, EIGHT, TWELVE, CONV_RAD_DEG
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, BE3, BENSUM, DT, FCONV_SHEAR_THICK, EB, EBM, EID, ET, ELDOF, FCONV, KE,      &
                                         MTRL_TYPE, PCOMP_LAM, PCOMP_PROPS, PHI_SQ, PPE, PRESS, PTE, SE2, SE3, SHELL_B, SHELL_DALP,&
                                         SHELL_D, SHELL_T, SHRSUM, STE2, TYPE
      use model_stuf, only            :  psi_hat                                                                                  !?
      USE PARAMS, ONLY                :  EPSIL, CBMIN3, CBMIN4T
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE 
  
      CHARACTER(1*BYTE), INTENT(IN)   :: CALC_EMATS        ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*) , INTENT(IN)   :: MN4T_QD           ! Arg used to say whether the triangular elem is part of a QUAD4

      INTEGER(LONG), INTENT(OUT)      :: IERROR            ! Local error indicator
      INTEGER(LONG), INTENT(IN)       :: TRIA_NUM          ! Tria number (1, 2, 3 or 4) for the subtriangles of a MIN4T QUAD4
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

      REAL(DOUBLE)                    :: A1(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Alpha*a)
      REAL(DOUBLE)                    :: A2(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Alpha*b)
      REAL(DOUBLE)                    :: B1(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Beta*a)
      REAL(DOUBLE)                    :: B2(3,3)           ! Intermediate variables used in calc KS (Alex Tessler's matrix Beta*b)
      REAL(DOUBLE)                    :: DUM0(9)           ! Intermediate variables used in calc PTE, PPE (thermal, pressure loads)
      REAL(DOUBLE)                    :: QCONS             ! = AREA/24, used in calc PPE pressure loads
      REAL(DOUBLE)                    :: EALP_TRIA(3)      ! Intermed var used in calc STEi therm stress coeffs

      END SUBROUTINE TPLT2

   END INTERFACE

   END MODULE TPLT2_Interface

