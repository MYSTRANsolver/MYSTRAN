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

   MODULE REDUCE_KGG_TO_KNN_Interface

   INTERFACE

      SUBROUTINE REDUCE_KGG_TO_KNN ( PART_VEC_G_NM )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2J, LINK2J, L2J_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NDOFN, NDOFM, NTERM_HMN, NTERM_KGG, NTERM_KNN,            &
                                         NTERM_KNM, NTERM_KMM, NTERM_GMN
      USE PARAMS, ONLY                :  EPSIL, MATSPARS, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KGG_TO_KNN_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_HMN, J_HMN, HMN, I_KGG, J_KGG, KGG, I_KNN, J_KNN, KNN, I_KNM, J_KNM, KNM,               &
                                         I_KMM, J_KMM, KMM,I_KMN, J_KMN, KMN, I_GMN, J_GMN, GMN,  I_GMNt, J_GMNt, GMNt
      USE SPARSE_MATRICES, ONLY       :  SYM_GMN, SYM_HMN, SYM_KGG, SYM_KNN, SYM_KNM, SYM_KMM, SYM_KMN
      USE FULL_MATRICES, ONLY         :  KNN_FULL, KNM_FULL, KMM_FULL, GMN_FULL, DUM1, DUM2, DUM3
      USE SCRATCH_MATRICES

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KGG_TO_KNN_BEGEND

      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix
 
      END SUBROUTINE REDUCE_KGG_TO_KNN

   END INTERFACE

   END MODULE REDUCE_KGG_TO_KNN_Interface

