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

   MODULE EIG_LANCZOS_ARPACK_Interface

   INTERFACE

      SUBROUTINE EIG_LANCZOS_ARPACK

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KMSM_SDIA, LINKNO, NDOFL, NTERM_KLL, NTERM_KLLD, NTERM_KMSM,     &
                                         NTERM_KMSMn, NTERM_KMSMs, NTERM_MLL, NTERM_ULL, NUM_EIGENS, NUM_KLLD_DIAG_ZEROS,          &
                                         NUM_MLL_DIAG_ZEROS, NVEC, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, PI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  ARP_TOL, BAILOUT, DARPACK, EIGESTL, EPSIL, MXITERL, SOLLIB, SPARSTOR, SUPINFO,            &
                                         SUPWARN
      USE DOF_TABLES, ONLY            :  TDOFI
      USE SUBR_BEGEND_LEVELS, ONLY    :  EIG_LANCZOS_ARPACK_BEGEND
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, EIGEN_VEC, MODE_NUM
      USE MODEL_STUF, ONLY            :  EIG_FRQ1, EIG_FRQ2, EIG_LANCZOS_NEV_DELT, EIG_LAP_MAT_TYPE, EIG_MODE, EIG_N1, EIG_N2,     &
                                         EIG_NCVFACL, EIG_SIGMA
      USE ARPACK_MATRICES_1, ONLY     :  IWORK, RESID, RFAC, SELECT, VBAS, WORKD, WORKL
      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_KLLD, J_KLLD, KLLD, I_MLL, J_MLL, MLL, SYM_KLL, SYM_KLLD, SYM_MLL,   &
                                         I_KMSM, J_KMSM, KMSM, I_KMSMn, J_KMSMn, KMSMn, I_KMSMs, J_KMSMs, KMSMs
                                         
      USE ARPACK_LANCZOS_EIG
 
      IMPLICIT NONE
  
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      INTEGER(LONG)                   :: LWORKL            ! Used to dimension a work array
      INTEGER(LONG)                   :: NUM1              ! Number to use for max no. of eigens to find. Must be NUM1 <= NDOFL
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EIG_LANCZOS_ARPACK_BEGEND

      END SUBROUTINE EIG_LANCZOS_ARPACK

   END INTERFACE

   END MODULE EIG_LANCZOS_ARPACK_Interface

