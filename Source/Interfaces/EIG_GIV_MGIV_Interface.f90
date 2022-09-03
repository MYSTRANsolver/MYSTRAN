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

   MODULE EIG_GIV_MGIV_Interface

   INTERFACE

       SUBROUTINE EIG_GIV_MGIV

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KLL_SDIA, KLLD_SDIA, MLL_SDIA, NDOFL, NTERM_KLL, NTERM_KLLD,     &
                                         NTERM_MLL, NUM_EIGENS, NUM_KLLD_DIAG_ZEROS, NUM_MLL_DIAG_ZEROS, NVEC, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  BAILOUT, EPSIL, SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  EIG_GIV_MGIV_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, PI
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, EIGEN_VEC, MODE_NUM
      USE MODEL_STUF, ONLY            :  EIG_FRQ1, EIG_FRQ2, EIG_METH, EIG_N1, EIG_N2, EIG_VECS
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND, BBAND
      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_KLLD, J_KLLD, KLLD, I_MLL, J_MLL, MLL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LAPACK_GIV_MGIV_EIG
 
      IMPLICIT NONE
  
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      INTEGER(LONG)                   :: IWORK(8*NDOFL)      ! Integer workspace used by LAPACK.
      INTEGER(LONG)                   :: NUM1                ! Number to use for max no. of eigens to find. Must be NUM1 <= NDOFL
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EIG_GIV_MGIV_BEGEND

      REAL(DOUBLE)                    :: Q(NDOFL,NDOFL)      ! Matrix used in LAPACK reduction of eigen problem to standard form.

      END SUBROUTINE EIG_GIV_MGIV

   END INTERFACE

   END MODULE EIG_GIV_MGIV_Interface

