! ###############################################################################################################################
! Begin MIT license text.
! _______________________________________________________________________________________________________

! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)
! Copyright 2026 Bruno Borber (pre-factored matrix support for adaptive Lanczos)

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

   MODULE DSBAND_PREFAC_Interface

   INTERFACE

      SUBROUTINE DSBAND_PREFAC ( RVEC, HOWMNY, SELECT, D, Z, LDZ,       &
     &           SIGMA, N, LDA, RFAC, KL, KU, WHICH, BMAT, NEV,         &
     &           TOL, RESID, NCV, V, LDV, IPARAM, WORKD, WORKL,         &
     &           LWORKL, IWORK, INFO, INFO_LAPACK, DTBSV_MSG, PITERS )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, SOL_NAME,        &
     &                                   NTERM_KLLDn, NTERM_MLLn,       &
     &                                   NTERM_KMSMn, NTERM_ALL
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  EIG_MSGLVL, EIG_LAP_MAT_TYPE
      USE SUBR_BEGEND_LEVELS, ONLY    :  ARPACK_BEGEND
      USE SuperLU_STUF, ONLY          :  SLU_FACTORS, SLU_INFO
      USE PARAMS, ONLY                :  SOLLIB
      USE SPARSE_MATRICES, ONLY       :  I_KLLDn, J_KLLDn, KLLDn,       &
     &                                   I_MLLn , J_MLLn , MLLn,        &
     &                                   I_KMSMn, J_KMSMn, KMSMn

      USE ARPACK_LANCZOS_EIG, ONLY    :  dsaupd, dseupd, cr13_a
      USE LAPACK_LIN_EQN_DGB
      USE LAPACK_LIN_EQN_DPB

      IMPLICIT NONE

      CHARACTER        WHICH*2, BMAT, HOWMNY
      CHARACTER*1      DTBSV_MSG
      INTEGER          N, LDA, KL, KU, NEV, NCV, LDV,                   &
     &                 LDZ, LWORKL, INFO
      INTEGER          INFO_LAPACK
      REAL(DOUBLE)     TOL, SIGMA
      LOGICAL          RVEC

      INTEGER          IPARAM(*), IWORK(*)
      LOGICAL          SELECT(*)
      REAL(DOUBLE)     D(*), RESID(*), V(LDV,*), Z(LDZ,*),              &
     &                 RFAC(LDA,*), WORKD(*), WORKL(*)

      INTEGER          PITERS

      END SUBROUTINE DSBAND_PREFAC

   END INTERFACE

   END MODULE DSBAND_PREFAC_Interface
