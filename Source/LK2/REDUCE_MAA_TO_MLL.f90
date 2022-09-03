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

      SUBROUTINE REDUCE_MAA_TO_MLL ( PART_VEC_A_LR )
 
! Call routines to reduce the MAA mass matrix from the A-set to the L, R-sets. See Appendix B to the MYSTRAN User's Reference Manual
! Reference Manual for the derivation of the reduction equations.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2M, L2N, LINK2M, LINK2N, L2M_MSG, L2N_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFL, NDOFR, NTERM_MAA, NTERM_MLL, NTERM_MRL, NTERM_MRR, &
                                         SOL_NAME
      USE PARAMS, ONLY                :  EPSIL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_MAA_TO_MLL_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_MAA, J_MAA, MAA, I_MLL, J_MLL, MLL, I_MRL, J_MRL, MRL, I_MRR, J_MRR, MRR,               &
                                         SYM_MAA, SYM_MLL, SYM_MRL, SYM_MRR
      USE SCRATCH_MATRICES

      USE REDUCE_MAA_TO_MLL_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_MAA_TO_MLL'
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_A_LR(NDOFA)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG)                   :: MLL_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MRL_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MRR_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_MAA_TO_MLL_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition MLL from MAA

      IF (NDOFL > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA,      PART_VEC_A_LR, PART_VEC_A_LR,       &
                                    NUM1, NUM1, MLL_ROW_MAX_TERMS, 'MLL', NTERM_MLL, SYM_MLL ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MLL', NDOFL, NTERM_MLL, SUBR_NAME )

         IF (NTERM_MLL > 0) THEN      
            CALL PARTITION_SS ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA, MAA, PART_VEC_A_LR, PART_VEC_A_LR,          &
                                 NUM1, NUM1, MLL_ROW_MAX_TERMS, 'MLL', NTERM_MLL, NDOFL, SYM_MLL, I_MLL, J_MLL, MLL )
         ENDIF

      ENDIF

! Partition MRL from MAA

      IF ((NDOFL > 0) .AND. (NDOFR > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA,      PART_VEC_A_LR, PART_VEC_A_LR,       &
                                    NUM2, NUM1, MRL_ROW_MAX_TERMS, 'MRL', NTERM_MRL, SYM_MRL ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MRL', NDOFR, NTERM_MRL, SUBR_NAME )

         IF (NTERM_MRL > 0) THEN
            CALL PARTITION_SS ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA, MAA, PART_VEC_A_LR, PART_VEC_A_LR,          &
                                 NUM2, NUM1, MRL_ROW_MAX_TERMS, 'MRL', NTERM_MRL, NDOFR, SYM_MRL, I_MRL, J_MRL, MRL )
         ENDIF

      ENDIF

! Partition MRR from MAA

      IF (NDOFR > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA,      PART_VEC_A_LR, PART_VEC_A_LR,       &
                                    NUM2, NUM2, MRR_ROW_MAX_TERMS, 'MRR', NTERM_MRR, SYM_MRR ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MRR', NDOFR, NTERM_MRR, SUBR_NAME )

         IF (NTERM_MRR > 0) THEN
            CALL PARTITION_SS ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA, MAA, PART_VEC_A_LR, PART_VEC_A_LR,          &
                                 NUM2, NUM2, MRR_ROW_MAX_TERMS, 'MRR', NTERM_MRR, NDOFR, SYM_MRR, I_MRR, J_MRR, MRR )
         ENDIF

      ENDIF

! Write matrices needed for Craig-Bampton, if this is a CB soln

      IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
         CALL WRITE_MATRIX_1 ( LINK2M, L2M, 'Y', 'KEEP', L2M_MSG, 'MRL', NTERM_MRL, NDOFR, I_MRL, J_MRL, MRL )
         CALL WRITE_MATRIX_1 ( LINK2N, L2N, 'Y', 'KEEP', L2N_MSG, 'MRR', NTERM_MRR, NDOFR, I_MRR, J_MRR, MRR )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_MAA_TO_MLL
