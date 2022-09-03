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

   MODULE LINK4_Interface

   INTERFACE

      SUBROUTINE LINK4


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, ERR, ERRSTAT, F04, F06, L1M, L3A, SC1
      USE IOUNT1, ONLY                :  LINK1M,  LINK2I,  LINK3A, L1M_MSG, L3A_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, COMM, FATAL_ERR, LINKNO, MBUG, NDOFL,                                       &
                                         NTERM_KLL, NTERM_KLLD, NTERM_KLLDn,                                                       &
                                         NTERM_MLL, NTERM_MLLn,                                                                    &
                                         NVEC, NUM_EIGENS, NUM_KLLD_DIAG_ZEROS, NUM_MLL_DIAG_ZEROS, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  EPSIL, LANCMETH, SPARSTOR, SUPINFO
      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_METH, EIG_MSGLVL, EIG_LAP_MAT_TYPE, &
                                         EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM, EIG_SID, EIG_SIGMA, EIG_VECS, MAXMIJ,    &
                                         MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_KLLD, J_KLLD, KLLD, I_KLLDn, J_KLLDn, KLLDn,                         &
                                         I_MLL, J_MLL, MLL, I_MLLn, J_MLLn, MLLn
      USE OUTPUT4_MATRICES, ONLY      :  NUM_OU4_REQUESTS
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS, MODE_NUM, EIGEN_VAL, EIGEN_VEC
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND, BBAND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE 

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      INTEGER(LONG), PARAMETER        :: P_LINKNO = 2        ! Prior LINK no's that should have run before this LINK can execute.

      END SUBROUTINE LINK4

   END INTERFACE

   END MODULE LINK4_Interface

