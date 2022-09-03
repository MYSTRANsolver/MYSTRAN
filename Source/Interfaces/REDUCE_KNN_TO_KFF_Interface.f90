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

   MODULE REDUCE_KNN_TO_KFF_Interface

   INTERFACE

      SUBROUTINE REDUCE_KNN_TO_KFF ( PART_VEC_N_FS, PART_VEC_S_SzSe, PART_VEC_F, PART_VEC_S )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2B, LINK2B, L2B_MSG
      USE SCONTR, ONLY                :  FATAL_ERR, NDOFN, NDOFF, NDOFS, NDOFSE, NTERM_KNN, NTERM_KFF, NTERM_KFS, NTERM_KSS,       &
                                         NTERM_KFSe, NTERM_KSSe, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KNN_TO_KFF_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KNN, J_KNN, KNN, I_KFF, J_KFF, KFF, I_KFS, J_KFS, KFS, I_KFSe, J_KFSe, KFSe,            &
                                         I_KSF, J_KSF, KSF, I_KSS, J_KSS, KSS, I_KSSe, J_KSSe, KSSe
      USE SPARSE_MATRICES, ONLY       :  SYM_KNN, SYM_KFF, SYM_KFS, SYM_KFSe, SYM_KSS, SYM_KSS, SYM_KSSe
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F(NDOFF)      ! Partitioning vector (1's for all of F set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)   ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S(NDOFS)      ! Partitioning vector (1's for all of S set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S_SzSe(NDOFS) ! Partitioning vector (S set into SZ and SE sets) 
      INTEGER(LONG), PARAMETER        :: NUM1        = 1        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KNN_TO_KFF_BEGEND

      END SUBROUTINE REDUCE_KNN_TO_KFF

   END INTERFACE

   END MODULE REDUCE_KNN_TO_KFF_Interface

