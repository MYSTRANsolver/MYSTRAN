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

      SUBROUTINE REDUCE_KAA_TO_KLL ( PART_VEC_A_LR )
 
! Call routines to reduce the KAA linear stiffness matrix from the A-set to the L, R-sets

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2K, L2L, LINK2K, LINK2L, L2K_MSG, L2L_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFL, NDOFR, NTERM_KAA, NTERM_KLL, NTERM_KRL, NTERM_KRR, &
                                         SOL_NAME
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KAA_TO_KLL_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KAA, J_KAA, KAA, I_KLL, J_KLL, KLL, I_KRL, J_KRL, KRL, I_KRR, J_KRR, KRR,               &
                                         SYM_KAA, SYM_KLL, SYM_KRL, SYM_KRR
      USE SCRATCH_MATRICES
 
      USE REDUCE_KAA_TO_KLL_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KAA_TO_KLL'
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_A_LR(NDOFA)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG)                   :: KLL_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KRL_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KRR_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KAA_TO_KLL_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition KLL from KAA (This is KLL before reduction, or KLL(bar) )

      IF (NDOFL > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA,      PART_VEC_A_LR, PART_VEC_A_LR,       &
                                    NUM1, NUM1, KLL_ROW_MAX_TERMS, 'KLL', NTERM_KLL, SYM_KLL ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KLL', NDOFL, NTERM_KLL, SUBR_NAME )

         IF (NTERM_KLL > 0) THEN      
            CALL PARTITION_SS ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA, KAA, PART_VEC_A_LR, PART_VEC_A_LR,          &
                                 NUM1, NUM1, KLL_ROW_MAX_TERMS, 'KLL', NTERM_KLL, NDOFL, SYM_KLL, I_KLL, J_KLL, KLL )
         ENDIF

      ENDIF

! Partition KRL from KAA

      IF ((NDOFL > 0) .AND. (NDOFR > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA,      PART_VEC_A_LR, PART_VEC_A_LR,       &
                                    NUM2, NUM1, KRL_ROW_MAX_TERMS, 'KRL', NTERM_KRL, SYM_KRL ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KRL', NDOFR, NTERM_KRL, SUBR_NAME )

         IF (NTERM_KRL > 0) THEN
            CALL PARTITION_SS ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA, KAA, PART_VEC_A_LR, PART_VEC_A_LR,          &
                                 NUM2, NUM1, KRL_ROW_MAX_TERMS, 'KRL', NTERM_KRL, NDOFR, SYM_KRL, I_KRL, J_KRL, KRL )
         ENDIF

      ENDIF

! Partition KRR from KAA

      IF (NDOFR > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA,      PART_VEC_A_LR, PART_VEC_A_LR,       &
                                    NUM2, NUM2, KRR_ROW_MAX_TERMS, 'KRR', NTERM_KRR, SYM_KRR ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KRR', NDOFR, NTERM_KRR, SUBR_NAME )

         IF (NTERM_KRR > 0) THEN
            CALL PARTITION_SS ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA, KAA, PART_VEC_A_LR, PART_VEC_A_LR,          &
                                 NUM2, NUM2, KRR_ROW_MAX_TERMS, 'KRR', NTERM_KRR, NDOFR, SYM_KRR, I_KRR, J_KRR, KRR )
         ENDIF

      ENDIF

! Write matrices needed for Craig-Bampton, if this is a CB soln

      IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
         CALL WRITE_MATRIX_1 ( LINK2K, L2K, 'Y', 'KEEP', L2K_MSG, 'KRL', NTERM_KRL, NDOFR, I_KRL, J_KRL, KRL )
         CALL WRITE_MATRIX_1 ( LINK2L, L2L, 'Y', 'KEEP', L2L_MSG, 'KRR', NTERM_KRR, NDOFR, I_KRR, J_KRR, KRR )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 2092 FORMAT(4X,A44,20X,I2,':',I2,':',I2,'.',I3)

 2400 FORMAT(' *ERROR  2400: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THERE IS AN O-SET BUT GUYAN REDUCTION MATRIX GOA HAS ',I12,' TERMS IN IT. MUST BE > 0')

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_KAA_TO_KLL
