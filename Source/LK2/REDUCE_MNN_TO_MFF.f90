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

      SUBROUTINE REDUCE_MNN_TO_MFF ( PART_VEC_N_FS )
 
! Call routines to reduce the MNN mass matrix from the N-set to the F, S-sets. See Appendix B to the MYSTRAN User's Reference Manual
! for the derivation of the reduction equations.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2S, LINK2S, L2S_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFN, NDOFF, NDOFS, NTERM_MNN, NTERM_MFF, NTERM_MFS, NTERM_MSS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_MNN_TO_MFF_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_MNN, J_MNN, MNN, I_MFF, J_MFF, MFF, I_MFS, J_MFS, MFS, I_MSF, J_MSF, MSF,               &
                                         I_MSS, J_MSS, MSS
      USE SPARSE_MATRICES, ONLY       :  SYM_MNN, SYM_MFF, SYM_MFS, SYM_MSS
      USE SCRATCH_MATRICES
 
      USE REDUCE_MNN_TO_MFF_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_MNN_TO_MFF'
      CHARACTER(  1*BYTE)             :: CLOSE_IT               ! Input to subr READ_MATRIX_1. 'Y'/'N' whether to close a file 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT             ! Char constant for the CLOSE status of a file
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)   ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG)                   :: MFF_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MFS_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
!xx   INTEGER(LONG)                   :: MSF_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MSS_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2        ! Used in subr's that partition matrices
      INTEGER(LONG)                   :: NTERM_MSF              ! Number of nonzeros in sparse matrix MSF (should = NTERM_MFS)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_MNN_TO_MFF_BEGEND

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition MFF from MNN. This is final MFF.

      IF (NDOFF > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
                                    NUM1, NUM1, MFF_ROW_MAX_TERMS, 'MFF', NTERM_MFF, SYM_MFF ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MFF', NDOFF, NTERM_MFF, SUBR_NAME )

         IF (NTERM_MFF > 0) THEN      
            CALL PARTITION_SS ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN, MNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
                                 NUM1, NUM1, MFF_ROW_MAX_TERMS, 'MFF', NTERM_MFF, NDOFF, SYM_MFF, I_MFF, J_MFF, MFF )
         ENDIF

      ENDIF

! Partition MFS from MNN

      IF ((NDOFF > 0) .AND. (NDOFS > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
                                    NUM1, NUM2, MFS_ROW_MAX_TERMS, 'MFS', NTERM_MFS, SYM_MFS ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MFS', NDOFF, NTERM_MFS, SUBR_NAME )

         IF (NTERM_MFS > 0) THEN
            CALL PARTITION_SS ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN, MNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
                                 NUM1, NUM2, MFS_ROW_MAX_TERMS, 'MFS', NTERM_MFS, NDOFF, SYM_MFS, I_MFS, J_MFS, MFS )
         ENDIF

      ENDIF

! Partition MSS from MNN.

      IF (NDOFS > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
                                    NUM2, NUM2, MSS_ROW_MAX_TERMS, 'MSS', NTERM_MSS, SYM_MSS ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MSS', NDOFS, NTERM_MSS, SUBR_NAME )

         IF (NTERM_MFF > 0) THEN      
            CALL PARTITION_SS ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN, MNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
                                 NUM2, NUM2, MSS_ROW_MAX_TERMS, 'MSS', NTERM_MSS, NDOFS, SYM_MSS, I_MSS, J_MSS, MSS )
         ENDIF

      ENDIF

! Partition MSF from MNN and write MSF to L2S for use in LINK9.

      IF ((NDOFF > 0) .AND. (NDOFS > 0)) THEN

!xx      CALL PARTITION_SS_NTERM ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
!xx                                 NUM2, NUM1, MSF_ROW_MAX_TERMS, 'MSF', NTERM_MSF, SYM_MFS ) 

!xx      IF (NTERM_MSF /= NTERM_MFS) THEN
!xx         FATAL_ERR = FATAL_ERR + 1
!xx         WRITE(ERR,936) SUBR_NAME, NTERM_MSF, NTERM_MFS
!xx         WRITE(F06,936) SUBR_NAME, NTERM_MSF, NTERM_MFS
!xx         CALL OUTA_HERE ( 'Y' )
!xx      ENDIF

!xx      CALL ALLOCATE_SPARSE_MAT ( 'MSF', NDOFS, NTERM_MSF, SUBR_NAME )

!xx      IF (NTERM_MSF > 0) THEN
!xx         CALL PARTITION_SS ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN, MNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
!xx                              NUM2, NUM1, MSF_ROW_MAX_TERMS, 'MSF', NTERM_MSF, NDOFS, SYM_MFS, I_MSF, J_MSF, MSF )

!xx         CLOSE_IT   = 'Y'
!xx         CLOSE_STAT = 'KEEP'
!xx         CALL WRITE_MATRIX_1 ( LINK2S, L2S, CLOSE_IT, CLOSE_STAT, L2S_MSG, 'MSF', NTERM_MSF, NDOFS, I_MSF, J_MSF, MSF )

!xx      ENDIF

         NTERM_MSF = NTERM_MFS
         CALL ALLOCATE_SPARSE_MAT ( 'MSF', NDOFS, NTERM_MSF, SUBR_NAME )

         IF (NTERM_MSF > 0) THEN
            CALL MATTRNSP_SS ( NDOFF, NDOFS, NTERM_MFS, 'MFS', I_MFS, J_MFS, MFS, 'MSF', I_MSF, J_MSF, MSF )

            CLOSE_IT   = 'Y'
            CLOSE_STAT = 'KEEP'
            CALL WRITE_MATRIX_1 ( LINK2S, L2S, CLOSE_IT, CLOSE_STAT, L2S_MSG, 'MSF', NTERM_MSF, NDOFS, I_MSF, J_MSF, MSF )

         ENDIF

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  936 FORMAT(' *ERROR   936: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF NONZERO TERMS IN SPARSE MATRIX MSF = ',I12,' IS NOT EQUAL TO THOSE IN MATRIX MFS = ',I12)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_MNN_TO_MFF
