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

      SUBROUTINE REDUCE_KNN_TO_KFF ( PART_VEC_N_FS, PART_VEC_S_SzSe, PART_VEC_F, PART_VEC_S )
 
! Call routines to reduce the KNN linear stiffness matrix from the N-set to the F, S-sets. See Appendix B to the MYSTRAN User's
! Reference Manual for the derivation of the reduction equations.

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
 
      USE REDUCE_KNN_TO_KFF_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KNN_TO_KFF'
      CHARACTER(  1*BYTE)             :: CLOSE_IT               ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close file or not 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT             ! Char constant for the CLOSE status of a file
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F(NDOFF)      ! Partitioning vector (1's for all of F set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)   ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S(NDOFS)      ! Partitioning vector (1's for all of S set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S_SzSe(NDOFS) ! Partitioning vector (S set into SZ and SE sets) 
      INTEGER(LONG)                   :: KFF_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KFS_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KSF_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KFSe_ROW_MAX_TERMS     ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KSS_ROW_MAX_TERMS      ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KSSe_ROW_MAX_TERMS     ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_KSF              ! Number of nonzeros in sparse matrix KSF (should = NTERM_KFS)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KNN_TO_KFF_BEGEND

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************

! Partition KFF from KNN. This is final KFF.

      IF (NDOFF > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
                                    NUM1, NUM1, KFF_ROW_MAX_TERMS, 'KFF', NTERM_KFF, SYM_KFF ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KFF', NDOFF, NTERM_KFF, SUBR_NAME )

         IF (NTERM_KFF > 0) THEN      
            CALL PARTITION_SS ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN, KNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
                                 NUM1, NUM1, KFF_ROW_MAX_TERMS, 'KFF', NTERM_KFF, NDOFF, SYM_KFF, I_KFF, J_KFF, KFF )
         ENDIF

      ENDIF

! Partition KFS from KNN. Then partition KFSe from KFS

      IF ((NDOFF > 0) .AND. (NDOFS > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
                                    NUM1, NUM2, KFS_ROW_MAX_TERMS, 'KFS', NTERM_KFS, SYM_KFS ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KFS', NDOFF, NTERM_KFS, SUBR_NAME )

         IF (NTERM_KFS > 0) THEN
            CALL PARTITION_SS ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN, KNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
                                 NUM1, NUM2, KFS_ROW_MAX_TERMS, 'KFS', NTERM_KFS, NDOFF, SYM_KFS, I_KFS, J_KFS, KFS )

            IF (NDOFSE > 0) THEN

               CALL PARTITION_SS_NTERM ( 'KFS', NTERM_KFS, NDOFF, NDOFS, SYM_KFS, I_KFS, J_KFS,      PART_VEC_F,    PART_VEC_S_SzSe&
                                         ,NUM1, NUM2, KFSe_ROW_MAX_TERMS, 'KFSe', NTERM_KFSe, SYM_KFSe ) 

               CALL ALLOCATE_SPARSE_MAT ( 'KFSe', NDOFF, NTERM_KFSe, SUBR_NAME )

               IF (NTERM_KFSe > 0) THEN
                  CALL PARTITION_SS ( 'KFS', NTERM_KFS, NDOFF, NDOFS, SYM_KFS, I_KFS, J_KFS, KFS, PART_VEC_F   , PART_VEC_S_SzSe,  &
                                       NUM1, NUM2, KFSe_ROW_MAX_TERMS, 'KFSe', NTERM_KFSe, NDOFF, SYM_KFSe, I_KFSe, J_KFSe, KFSe )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

! Partition KSF from KNN and write KSF to L2B for constraint force recovery in LINK9.

      IF ((NDOFF > 0) .AND. (NDOFS > 0)) THEN

!xx      CALL PARTITION_SS_NTERM ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
!xx                                 NUM2, NUM1, KSF_ROW_MAX_TERMS, 'KSF', NTERM_KSF, SYM_KFS ) 

!xx      IF (NTERM_KSF /= NTERM_KFS) THEN
!xx         FATAL_ERR = FATAL_ERR + 1
!xx         WRITE(ERR,936) SUBR_NAME, NTERM_KSF, NTERM_KFS
!xx         WRITE(F06,936) SUBR_NAME, NTERM_KSF, NTERM_KFS
!xx         CALL OUTA_HERE ( 'Y' )
!xx      ENDIF

!xx      CALL ALLOCATE_SPARSE_MAT ( 'KSF', NDOFS, NTERM_KSF, SUBR_NAME )

!xx      IF (NTERM_KFS > 0) THEN
!xx         CALL PARTITION_SS ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN, KNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
!xx                              NUM2, NUM1, KSF_ROW_MAX_TERMS, 'KSF', NTERM_KSF, NDOFS, SYM_KFS, I_KSF, J_KSF, KSF )

!xx         CLOSE_IT   = 'Y'
!xx         CLOSE_STAT = 'KEEP'
!xx         CALL WRITE_MATRIX_1 ( LINK2B, L2B, CLOSE_IT, CLOSE_STAT, L2B_MSG, 'KSF', NTERM_KSF, NDOFS, I_KSF, J_KSF, KSF )

!xx      ENDIF

         NTERM_KSF = NTERM_KFS
         CALL ALLOCATE_SPARSE_MAT ( 'KSF', NDOFS, NTERM_KSF, SUBR_NAME )

         IF (NTERM_KSF > 0) THEN
            CALL MATTRNSP_SS ( NDOFF, NDOFS, NTERM_KFS, 'KFS', I_KFS, J_KFS, KFS, 'KSF', I_KSF, J_KSF, KSF )

            CLOSE_IT   = 'Y'
            CLOSE_STAT = 'KEEP'
            CALL WRITE_MATRIX_1 ( LINK2B, L2B, CLOSE_IT, CLOSE_STAT, L2B_MSG, 'KSF', NTERM_KSF, NDOFS, I_KSF, J_KSF, KSF )

         ENDIF

      ENDIF

! Partition KSS from KNN. Then partition KSSe from KSS

      IF (NDOFS > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN,      PART_VEC_N_FS, PART_VEC_N_FS,       &
                                    NUM2, NUM2, KSS_ROW_MAX_TERMS, 'KSS', NTERM_KSS, SYM_KSS ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KSS', NDOFS, NTERM_KSS, SUBR_NAME )

         IF (NTERM_KSS > 0) THEN
      
            CALL PARTITION_SS ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN, KNN, PART_VEC_N_FS, PART_VEC_N_FS,          &
                                 NUM2, NUM2, KSS_ROW_MAX_TERMS, 'KSS', NTERM_KSS, NDOFS, SYM_KSS, I_KSS, J_KSS, KSS )

            IF (NDOFSE > 0) THEN

               CALL PARTITION_SS_NTERM ( 'KSS', NTERM_KSS, NDOFS, NDOFS, SYM_KSS, I_KSS, J_KSS,      PART_VEC_S,    PART_VEC_S_SzSe&
                                         ,NUM1, NUM2, KSSe_ROW_MAX_TERMS, 'KSSe', NTERM_KSSe, SYM_KSSe ) 

               CALL ALLOCATE_SPARSE_MAT ( 'KSSe', NDOFS, NTERM_KSSe, SUBR_NAME )

               IF (NTERM_KSSe > 0) THEN
                  CALL PARTITION_SS ( 'KSS', NTERM_KSS, NDOFS, NDOFS, SYM_KSS, I_KSS, J_KSS, KSS, PART_VEC_S   , PART_VEC_S_SzSe,  &
                                       NUM1, NUM2, KSSe_ROW_MAX_TERMS, 'KSSe', NTERM_KSSe, NDOFS, SYM_KSSe, I_KSSe, J_KSSe, KSSe )
               ENDIF

            ENDIF

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
                    ,/,14X,' THE NUMBER OF NONZERO TERMS IN SPARSE MATRIX KSF = ',I12,' IS NOT EQUAL TO THOSE IN MATRIX KFS = ',I12)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_KNN_TO_KFF
