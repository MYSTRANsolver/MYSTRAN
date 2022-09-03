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

   MODULE REDUCE_MGG_TO_MNN_Interface

   INTERFACE

      SUBROUTINE REDUCE_MGG_TO_MNN ( PART_VEC_G_NM )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2R, LINK2R, L2R_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NDOFN, NDOFM, NTERM_MGG, NTERM_MNN, NTERM_MNM, NTERM_MMM, &
                                         NTERM_GMN, NTERM_LMN
      USE PARAMS, ONLY                :  EPSIL, MATSPARS, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_MGG_TO_MNN_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_LMN, J_LMN, LMN, I_MGG, J_MGG, MGG, I_MNN, J_MNN, MNN, I_MNM , J_MNM , MNM ,            &
                                         I_MMN, J_MMN, MMN, I_MMM, J_MMM, MMM, I_GMN, J_GMN, GMN, I_GMNt, J_GMNt, GMNt
      USE SPARSE_MATRICES, ONLY       :  SYM_GMN, SYM_LMN, SYM_MGG, SYM_MNN, SYM_MNM, SYM_MMN, SYM_MMM
      USE FULL_MATRICES, ONLY         :  MNN_FULL, MNM_FULL, MMM_FULL, GMN_FULL, DUM1, DUM2, DUM3
      USE SCRATCH_MATRICES

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_MGG_TO_MNN_BEGEND

      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix
 
      END SUBROUTINE REDUCE_MGG_TO_MNN

   END INTERFACE

   END MODULE REDUCE_MGG_TO_MNN_Interface

