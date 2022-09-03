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

      SUBROUTINE REDUCE_KNND_TO_KFFD ( PART_VEC_N_FS, PART_VEC_S_SzSe, PART_VEC_F, PART_VEC_S )
 
! Call routines to reduce the KNND differential stiffness matrix from the N-set to the F, S-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2B, LINK2B, L2B_MSG
      USE SCONTR, ONLY                :  FATAL_ERR, NDOFN, NDOFF, NDOFS, NDOFSE, NTERM_KNND, NTERM_KFFD, NTERM_KFSD, NTERM_KSSD,   &
                                         NTERM_KFSDe, NTERM_KSSDe, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KNND_TO_KFFD_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KNND, J_KNND, KNND, I_KFFD, J_KFFD, KFFD, I_KFSD, J_KFSD, KFSD, I_KFSDe, J_KFSDe, KFSDe,&
                                         I_KSFD, J_KSFD, KSFD, I_KSSD, J_KSSD, KSSD, I_KSSDe, J_KSSDe, KSSDe
      USE SPARSE_MATRICES, ONLY       :  SYM_KNND, SYM_KFFD, SYM_KFSD, SYM_KFSDe, SYM_KSSD, SYM_KSSD, SYM_KSSDe
      USE SCRATCH_MATRICES
 
      USE REDUCE_KNND_TO_KFFD_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KNND_TO_KFFD'
      CHARACTER(  1*BYTE)             :: CLOSE_IT               ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close file or not 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT             ! Char constant for the CLOSE status of a file
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F(NDOFF)      ! Partitioning vector (1's for all of F set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)   ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S(NDOFS)      ! Partitioning vector (1's for all of S set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S_SzSe(NDOFS) ! Partitioning vector (S set into SZ and SE sets) 
      INTEGER(LONG)                   :: KFFD_ROW_MAX_TERMS     ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KFSD_ROW_MAX_TERMS     ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
!xx   INTEGER(LONG)                   :: KSFD_ROW_MAX_TERMS     ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KFSDe_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KSSD_ROW_MAX_TERMS     ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KSSDe_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_KSFD             ! Number of nonzeros in sparse matrix KSFD (should = NTERM_KFSD)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KNND_TO_KFFD_BEGEND

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************

! Partition KFFD from KNND. This is final KFFD.

      IF (NDOFF > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND,      PART_VEC_N_FS, PART_VEC_N_FS,  &
                                    NUM1, NUM1, KFFD_ROW_MAX_TERMS, 'KFFD', NTERM_KFFD, SYM_KFFD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KFFD', NDOFF, NTERM_KFFD, SUBR_NAME )

         IF (NTERM_KFFD > 0) THEN      
            CALL PARTITION_SS ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND, KNND, PART_VEC_N_FS, PART_VEC_N_FS,    &
                                 NUM1, NUM1, KFFD_ROW_MAX_TERMS, 'KFFD', NTERM_KFFD, NDOFF, SYM_KFFD, I_KFFD, J_KFFD, KFFD )
         ENDIF

      ENDIF

! Partition KFSD from KNND. Then partition KFSDe from KFSD

      IF ((NDOFF > 0) .AND. (NDOFS > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND,      PART_VEC_N_FS, PART_VEC_N_FS,  &
                                    NUM1, NUM2, KFSD_ROW_MAX_TERMS, 'KFSD', NTERM_KFSD, SYM_KFSD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KFSD', NDOFF, NTERM_KFSD, SUBR_NAME )

         IF (NTERM_KFSD > 0) THEN
            CALL PARTITION_SS ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND, KNND, PART_VEC_N_FS, PART_VEC_N_FS,    &
                                 NUM1, NUM2, KFSD_ROW_MAX_TERMS, 'KFSD', NTERM_KFSD, NDOFF, SYM_KFSD, I_KFSD, J_KFSD, KFSD )

            IF (NDOFSE > 0) THEN

               CALL PARTITION_SS_NTERM ( 'KFSD', NTERM_KFSD, NDOFF, NDOFS, SYM_KFSD, I_KFSD, J_KFSD, PART_VEC_F, PART_VEC_S_SzSe   &
                                         ,NUM1, NUM2, KFSDe_ROW_MAX_TERMS, 'KFSDe', NTERM_KFSDe, SYM_KFSDe ) 

               CALL ALLOCATE_SPARSE_MAT ( 'KFSDe', NDOFF, NTERM_KFSDe, SUBR_NAME )

               IF (NTERM_KFSDe > 0) THEN
                  CALL PARTITION_SS ('KFSD', NTERM_KFSD, NDOFF, NDOFS, SYM_KFSD, I_KFSD, J_KFSD, KFSD, PART_VEC_F, PART_VEC_S_SzSe,&
                                  NUM1, NUM2, KFSDe_ROW_MAX_TERMS, 'KFSDe', NTERM_KFSDe, NDOFF, SYM_KFSDe, I_KFSDe, J_KFSDe, KFSDe )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

! Partition KSFD from KNND and write KSFD to L2B for constraint force recovery in LINK9.

      IF ((NDOFF > 0) .AND. (NDOFS > 0)) THEN

!xx      CALL PARTITION_SS_NTERM ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND,      PART_VEC_N_FS, PART_VEC_N_FS,  &
!xx                                 NUM2, NUM1, KSFD_ROW_MAX_TERMS, 'KSFD', NTERM_KSFD, SYM_KFSD ) 

!xx      IF (NTERM_KSFD /= NTERM_KFSD) THEN
!xx         FATAL_ERR = FATAL_ERR + 1
!xx         WRITE(ERR,936) SUBR_NAME, NTERM_KSFD, NTERM_KFSD
!xx         WRITE(F06,936) SUBR_NAME, NTERM_KSFD, NTERM_KFSD
!xx         CALL OUTA_HERE ( 'Y' )
!xx      ENDIF

!xx      CALL ALLOCATE_SPARSE_MAT ( 'KSFD', NDOFS, NTERM_KSFD, SUBR_NAME )

!xx      IF (NTERM_KFSD > 0) THEN
!xx         CALL PARTITION_SS ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND, KNND, PART_VEC_N_FS, PART_VEC_N_FS,    &
!xx                              NUM2, NUM1, KSFD_ROW_MAX_TERMS, 'KSFD', NTERM_KSFD, NDOFS, SYM_KFSD, I_KSFD, J_KSFD, KSFD )

!xx         CLOSE_IT   = 'Y'
!xx         CLOSE_STAT = 'KEEP'
!xx         CALL WRITE_MATRIX_1 ( LINK2B, L2B, CLOSE_IT, CLOSE_STAT, L2B_MSG, 'KSFD', NTERM_KSFD, NDOFS, I_KSFD, J_KSFD, KSFD )

!xx      ENDIF

         NTERM_KSFD = NTERM_KFSD
         CALL ALLOCATE_SPARSE_MAT ( 'KSFD', NDOFS, NTERM_KSFD, SUBR_NAME )

         IF (NTERM_KSFD > 0) THEN
            CALL MATTRNSP_SS ( NDOFF, NDOFS, NTERM_KFSD, 'KFSD', I_KFSD, J_KFSD, KFSD, 'KSFD', I_KSFD, J_KSFD, KSFD )

            CLOSE_IT   = 'Y'
            CLOSE_STAT = 'KEEP'
            CALL WRITE_MATRIX_1 ( LINK2B, L2B, CLOSE_IT, CLOSE_STAT, L2B_MSG, 'KSFD', NTERM_KSFD, NDOFS, I_KSFD, J_KSFD, KSFD )

         ENDIF

      ENDIF

! Partition KSSD from KNND. Then partition KSSDe from KSSD

      IF (NDOFS > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND,      PART_VEC_N_FS, PART_VEC_N_FS,  &
                                    NUM2, NUM2, KSSD_ROW_MAX_TERMS, 'KSSD', NTERM_KSSD, SYM_KSSD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KSSD', NDOFS, NTERM_KSSD, SUBR_NAME )

         IF (NTERM_KSSD > 0) THEN
      
            CALL PARTITION_SS ( 'KNND', NTERM_KNND, NDOFN, NDOFN, SYM_KNND, I_KNND, J_KNND, KNND, PART_VEC_N_FS, PART_VEC_N_FS,    &
                                 NUM2, NUM2, KSSD_ROW_MAX_TERMS, 'KSSD', NTERM_KSSD, NDOFS, SYM_KSSD, I_KSSD, J_KSSD, KSSD )

            IF (NDOFSE > 0) THEN

               CALL PARTITION_SS_NTERM ( 'KSSD', NTERM_KSSD, NDOFS, NDOFS, SYM_KSSD, I_KSSD, J_KSSD, PART_VEC_S, PART_VEC_S_SzSe   &
                                         ,NUM1, NUM2, KSSDe_ROW_MAX_TERMS, 'KSSDe', NTERM_KSSDe, SYM_KSSDe ) 

               CALL ALLOCATE_SPARSE_MAT ( 'KSSDe', NDOFS, NTERM_KSSDe, SUBR_NAME )

               IF (NTERM_KSSDe > 0) THEN
                  CALL PARTITION_SS ('KSSD', NTERM_KSSD, NDOFS, NDOFS, SYM_KSSD, I_KSSD, J_KSSD, KSSD, PART_VEC_S, PART_VEC_S_SzSe,&
                                  NUM1, NUM2, KSSDe_ROW_MAX_TERMS, 'KSSDe', NTERM_KSSDe, NDOFS, SYM_KSSDe, I_KSSDe, J_KSSDe, KSSDe )
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
                    ,/,14X,' THE NUMBER OF NONZERO TERMS IN SPARSE MATRIX KSFD = ',I12,' IS NOT EQUAL TO THOSE IN MATRIX KFSD = ', &
                       I12)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_KNND_TO_KFFD
