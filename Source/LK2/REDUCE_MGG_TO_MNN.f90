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

      SUBROUTINE REDUCE_MGG_TO_MNN ( PART_VEC_G_NM )
 
! Call routines to reduce the MGG mass matrix from the G-set to the N, M-sets. See Appendix B to the MYSTRAN User's Reference Manual
! for the derivation of the reduction equations.
 
! NOTE: This subr has code for sparse matrices as well as full matrices, (i.e. Bulk Data PARAM MATSPARS = 'Y' for sparse and 'N'
! for full). The code for full matrices was put in originally in order that the sparse code could be thoroughly checked. That task
! is complete and the remaining full matrix code has not been maintained since around 2005. In addition, new capability added to
! MYSTRAN since that approx time does not have full matrix code.
 
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

      USE REDUCE_MGG_TO_MNN_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_MGG_TO_MNN'
      CHARACTER(  1*BYTE)             :: SYM_CRS1            ! Storage format for matrix CRS1 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
      CHARACTER(  1*BYTE)             :: SYM_CRS3            ! Storage format for matrix CRS3 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: ITRNSPB             ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: MNN_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MNM_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
!xx   INTEGER(LONG)                   :: MMN_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MMM_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_CCS1          ! Number of terms in matrix CCS1  
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3          ! Number of terms in matrix CRS3  
      INTEGER(LONG)                   :: NTERM_MMN           ! Number of nonzeros in sparse matrix MMN (should = NTERM_MNM)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_MGG_TO_MNN_BEGEND

      REAL(DOUBLE)                    :: ALPHA = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: BETA  = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition MNN from MGG (This is MNN before reduction, or MNN(bar) )

      IF (NDOFN > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
                                    NUM1, NUM1, MNN_ROW_MAX_TERMS, 'MNN', NTERM_MNN, SYM_MNN ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MNN', NDOFN, NTERM_MNN, SUBR_NAME )

         IF (NTERM_MNN > 0) THEN      
            CALL PARTITION_SS ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG, MGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
                                 NUM1, NUM1, MNN_ROW_MAX_TERMS, 'MNN', NTERM_MNN, NDOFN, SYM_MNN, I_MNN, J_MNN, MNN )
         ENDIF

      ENDIF

! Partition MNM from MGG

      IF ((NDOFN > 0) .AND. (NDOFM > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
                                    NUM1, NUM2, MNM_ROW_MAX_TERMS, 'MNM', NTERM_MNM, SYM_MNM ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MNM', NDOFN, NTERM_MNM, SUBR_NAME )

         IF (NTERM_MNM > 0) THEN
            CALL PARTITION_SS ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG, MGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
                                 NUM1, NUM2, MNM_ROW_MAX_TERMS, 'MNM', NTERM_MNM, NDOFN, SYM_MNM, I_MNM, J_MNM, MNM )
         ENDIF

      ENDIF

! Partition MMN from MGG

      IF ((NDOFN > 0) .AND. (NDOFM > 0)) THEN

!xx      CALL PARTITION_SS_NTERM ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
!xx                                 NUM2, NUM1, MMN_ROW_MAX_TERMS, 'MMN', NTERM_MMN, SYM_MMN ) 

!xx      IF (NTERM_MMN /= NTERM_MNM) THEN
!xx         FATAL_ERR = FATAL_ERR + 1
!xx         WRITE(ERR,936) SUBR_NAME, NTERM_MMN, NTERM_MNM
!xx         WRITE(F06,936) SUBR_NAME, NTERM_MMN, NTERM_MNM
!xx         CALL OUTA_HERE ( 'Y' )
!xx      ENDIF

!xx      CALL ALLOCATE_SPARSE_MAT ( 'MMN', NDOFN, NTERM_MMN, SUBR_NAME )

!xx      IF (NTERM_MMN > 0) THEN
!xx         CALL PARTITION_SS ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG, MGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
!xx                              NUM2, NUM1, MMN_ROW_MAX_TERMS, 'MMN', NTERM_MMN, NDOFN, SYM_MMN, I_MMN, J_MMN, MMN )
!xx      ENDIF

         CALL ALLOCATE_SPARSE_MAT ( 'MMN', NDOFM, NTERM_MNM, SUBR_NAME )

         NTERM_MMN = NTERM_MNM
         IF (NTERM_MMN > 0) THEN
            CALL MATTRNSP_SS ( NDOFN, NDOFM, NTERM_MNM, 'MNM', I_MNM, J_MNM, MNM, 'MMN', I_MMN, J_MMN, MMN )

         ENDIF

      ENDIF

! Partition MMM from MGG

      IF (NDOFM > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
                                    NUM2, NUM2, MMM_ROW_MAX_TERMS, 'MMM', NTERM_MMM, SYM_MMM ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MMM', NDOFM, NTERM_MMM, SUBR_NAME )

         IF (NTERM_MMM > 0) THEN
            CALL PARTITION_SS ( 'MGG', NTERM_MGG, NDOFG, NDOFG, SYM_MGG, I_MGG, J_MGG, MGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
                                 NUM2, NUM2, MMM_ROW_MAX_TERMS, 'MMM', NTERM_MMM, NDOFM, SYM_MMM, I_MMM, J_MMM, MMM )
         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Reduce MGG to MNN = MNN(bar) + MNM*GMN + (MNM*GMN)' + GMN'*MMM*GMN.
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN

         CALL ALLOCATE_SPARSE_MAT ( 'GMNt', NDOFN, NTERM_GMN, SUBR_NAME )
         CALL MATTRNSP_SS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'GMNt', I_GMNt, J_GMNt, GMNt )

                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix GMN
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFN, NTERM_GMN, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

         IF (NTERM_MNM > 0) THEN                           ! Part I of reduced MNN: calc MNM*GMN & add it & transpose to orig MNN

                                                           ! I-1, sparse multiply to get CRS1 = MNM*GMN. Use CCS1 for GMN CCS
            CALL MATMULT_SSS_NTERM ( 'MNM' , NDOFN, NTERM_MNM , SYM_MNM, I_MNM , J_MNM ,                                           &
                                     'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1' ,       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'MNM' , NDOFN, NTERM_MNM , SYM_MNM, I_MNM , J_MNM , MNM ,                                           &
                               'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            NTERM_CRS2 = NTERM_CRS1                        ! I-2, allocate memory to array CRS2 which will hold transpose of CRS1
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )

                                                           ! I-3, transpose CRS1 to get CRS2 = (MNM*GMN)t
            CALL MATTRNSP_SS ( NDOFN, NDOFN, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CRS2', I_CRS2, J_CRS2, CRS2 )

                                                           ! I-4, sparse add to get CRS3 = CRS1 + CRS2 = (MNM*GMN) + (MNM*GMN)t
            CALL MATADD_SSS_NTERM (NDOFN,'MNM*GMN', NTERM_CRS1, I_CRS1, J_CRS1, 'N', '(MNM*GMN)t', NTERM_CRS2, I_CRS2, J_CRS2, 'N',&
                                         'CRS3', NTERM_CRS3)
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'MNM*GMN', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, '(MNM*GMN)t', NTERM_CRS2, I_CRS2, J_CRS2,   &
                              CRS2, ONE, 'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-5, deallocate CRS1, CRS2
            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! I-6, deallocate CRS2 which was (MNM*GMN)t

                                                           ! I-7, CRS3 = (MNM*GMN) + (MNM*GMN)t has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS3 as sym in CRS1     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFN, NTERM_CRS3, '(MNM*GMN) + (MNM*GMN)t', I_CRS3, J_CRS3, NTERM_CRS1 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS3 = (MNM*GMN) + (MNM*GMN)t all nonzeros', NDOFN, NTERM_CRS3, I_CRS3, J_CRS3, CRS3,&
                                            'CRS1 = (MNM*GMN) + (MNM*GMN)t stored sym'  ,        NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )
               SYM_CRS1 = 'Y'

            ELSE IF (SPARSTOR == 'NONSYM') THEN            !      If SPARSTOR == 'NONSYM', rewrite CRS3 in CRS1 with NTERM_CRS3

               NTERM_CRS1 = NTERM_CRS3
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )
               DO I=1,NDOFN+1
                  I_CRS1(I) = I_CRS3(I)
               ENDDO
               DO I=1,NTERM_CRS1
                  J_CRS1(I) = J_CRS3(I)
                    CRS1(I) =   CRS3(I)
               ENDDO
               SYM_CRS1 = 'N'

            ELSE                                           !      Error - incorrect SPARSTOR

               WRITE(ERR,932) SUBR_NAME, SPARSTOR
               WRITE(F06,932) SUBR_NAME, SPARSTOR
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )

            ENDIF

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-8, deallocate CRS3

                                                           ! I-9, sparse add to get CRS3 = MNN-bar + CRS1 = (MNM*GMN) + (MNM*GMN)t
            CALL MATADD_SSS_NTERM ( NDOFN, 'MNN-bar', NTERM_MNN, I_MNN, J_MNN, SYM_MNN, 'MNM*GMN + (MNM*GMN)t',                    &
                                    NTERM_CRS1, I_CRS1, J_CRS1, SYM_CRS1, 'CRS3', NTERM_CRS3 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'MNN-bar', NTERM_MNN, I_MNN, J_MNN, MNN, ONE, 'MNM*GMN + (MNM*GMN)t', NTERM_CRS1,             &
                              I_CRS1, J_CRS1, CRS1, ONE, 'CRS1', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-10, deallocate CRS1 and CRS3

            NTERM_MNN = NTERM_CRS3                         ! I-11, reallocate MNN to be size of CRS1
            WRITE(SC1, * ) '    Reallocate MNN'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MNN', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'MNN' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   MNN', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'MNN', NDOFN, NTERM_MNN, SUBR_NAME )
                                                           ! I-12, set MNN = CRS1
            DO I=1,NDOFN+1
               I_MNN(I) = I_CRS3(I)
            ENDDO
            DO J=1,NTERM_MNN
               J_MNN(J) = J_CRS3(J)
                 MNN(J) =   CRS3(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-13, deallocate CRS3
                                                           ! At this point, CRS1, CRS2, CRS3 are deallocated, CCS1 is being used
         ENDIF

         IF (NTERM_MMM > 0) THEN                           ! Part II of reduced MNN: calc GMN(t)*MMM*GMN and add to MNN

                                                           ! II-1 sparse multiply to get CRS1 = MMM*GMN using CCS1 for GMN CCS
            CALL MATMULT_SSS_NTERM ( 'MMM' , NDOFM, NTERM_MMM , SYM_MMM, I_MMM , J_MMM ,                                           &
                                     'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1' ,       NTERM_CRS1 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFM, NTERM_CRS1, SUBR_NAME )
            CALL MATMULT_SSS ( 'MMM' , NDOFM, NTERM_MMM , SYM_MMM, I_MMM , J_MMM , MMM ,                                           &
                               'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, CCS1,  AROW_MAX_TERMS,                          &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )


            IF (NTERM_MMN > 0) THEN
               CALL MATADD_SSS_NTERM (NDOFM, 'MMM*GMN', NTERM_CRS1, I_CRS1, J_CRS1, 'N', 'MMN', NTERM_MMN, I_MMN, J_MMN, SYM_MMN,  &
                                             'LMN', NTERM_LMN)

               CALL ALLOCATE_SPARSE_MAT ( 'LMN', NDOFM, NTERM_LMN, SUBR_NAME )

               CALL MATADD_SSS ( NDOFM, 'MMM*GM', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, 'MMN', NTERM_MMN, I_MMN, J_MMN, MMN,      &
                                 ONE, 'LMN', NTERM_LMN, I_LMN, J_LMN, LMN )
            ELSE
               NTERM_LMN = NTERM_CRS1
               CALL ALLOCATE_SPARSE_MAT ( 'LMN', NDOFM, NTERM_LMN, SUBR_NAME )
               DO I=1,NDOFM+1
                  I_LMN(I) = I_CRS1(I)
               ENDDO
               DO I=1,NTERM_LMN
                  J_LMN(I) = J_CRS1(I)
                  LMN(I) =   CRS1(I)
               ENDDO
            ENDIF


            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-2 , deallocate CCS1 which was CCS version of GMN

            NTERM_CCS1 = NTERM_CRS1                        ! II-3 , allocate CCS1 to be same as CRS1 but in CCS format
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFN, NTERM_CCS1, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NDOFN, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-4 , deallocate CRS1
                                                           ! II-5 , sparse multiply to get CRS1 = GMNt*CCS1 with CCS1 = MMM*GMN
!                                                            (note: use SYM_GMN for sym indicator for CCS1)
            CALL MATMULT_SSS_NTERM ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt,                                           &
                                     'CCS1', NDOFN, NTERM_CCS1, SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1 ',       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt, GMNt,                                           &
                               'CCS1', NDOFN, NTERM_CCS1, SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-6 , deallocate CCS1

                                                           ! II-7 , CRS1 = GMNt*MMM*GMN has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS3     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFN, NTERM_CRS1, 'GMNt*MMM*GMN all nonzeros', I_CRS1, J_CRS1, NTERM_CRS3 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = GMNt*MMM*GMN all nonzeros', NDOFN, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,           &
                                            'CRS3 = GMNt*MMM*GMN stored sym'  ,        NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )
               SYM_CRS3 = 'Y'

            ELSE IF (SPARSTOR == 'NONSYM') THEN            !      If SPARSTOR == 'NONSYM', rewrite CRS3 in CRS1 with NTERM_CRS3

               NTERM_CRS3 = NTERM_CRS1
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
               DO I=1,NDOFN+1
                  I_CRS3(I) = I_CRS1(I)
               ENDDO
               DO I=1,NTERM_CRS3
                  J_CRS3(I) = J_CRS1(I)
                    CRS3(I) =   CRS1(I)
               ENDDO
               SYM_CRS3 = 'N'

            ELSE                                           !      Error - incorrect SPARSTOR

               WRITE(ERR,932) SUBR_NAME, SPARSTOR
               WRITE(F06,932) SUBR_NAME, SPARSTOR
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )

            ENDIF
                                                           ! II-8 , sparse add to get CRS2 = MNN + CRS3 = MNN + GMNt*MMM*GMN
            CALL MATADD_SSS_NTERM ( NDOFN, 'MNN-bar + MNM*GMN + (MNM*GMN)t', NTERM_MNN, I_MNN, J_MNN, SYM_MNN, 'GMNt*MMM*GMN',     &
                                    NTERM_CRS3, I_CRS3, J_CRS3, SYM_CRS3, 'CRS2', NTERM_CRS2 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'MNN-bar + MNM*GMN + (MNM*GMN)t' , NTERM_MNN , I_MNN , J_MNN , MNN, ONE, 'GMNt*MMM*GMN',      &
                              NTERM_CRS3, I_CRS3, J_CRS3, CRS3, ONE, 'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-9 , deallocate CRS1 and CRS3
            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! II-10, deallocate CRS3

            NTERM_MNN = NTERM_CRS2                         ! II-11, reallocate MNN to be size of CRS2
            WRITE(SC1, * ) '    Reallocate MNN'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MNN', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'MNN' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   MNN', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'MNN', NDOFN, NTERM_MNN, SUBR_NAME )

            DO I=1,NDOFN+1                                 ! II-12, set reduced MNN = CRS2 until we see if MNM is null, below
               I_MNN(I) = I_CRS2(I)
            ENDDO
            DO I=1,NTERM_MNN
               J_MNN(I) = J_CRS2(I)
                 MNN(I) =   CRS2(I)
            ENDDO

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! II-13, Deallocate CRS2

         ELSE                                              ! MMM is null

            CALL DEALLOCATE_SCR_MAT ( 'CCS1')

            IF (NTERM_MMN > 0) THEN                        ! Set LMN - MMN if MMN nonzero, else LMN is null
               NTERM_LMN = NTERM_MMN
               CALL ALLOCATE_SPARSE_MAT ( 'LMN', NDOFM, NTERM_LMN, SUBR_NAME )
               DO I=1,NDOFM+1
                  I_LMN(I) = I_MMN(I)
               ENDDO
               DO I=1,NTERM_LMN
                  J_LMN(I) = J_MMN(I)
                  LMN(I) =   MMN(I)
               ENDDO
            ELSE
               NTERM_LMN = 0
            ENDIF

         ENDIF

         IF (NTERM_LMN > 0) THEN
            CALL WRITE_MATRIX_1 ( LINK2R, L2R, 'Y', 'KEEP', L2R_MSG, 'LMN', NTERM_LMN, NDOFM, I_LMN, J_LMN, LMN )
         ENDIF

         WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GMNt', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'GMNt' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate LMN ', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'LMN' )

! ----------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (MATSPARS == 'N') THEN

         CALL ALLOCATE_FULL_MAT ( 'MNN_FULL', NDOFN, NDOFN, SUBR_NAME )

         IF (NTERM_MNN > 0) THEN
            CALL SPARSE_CRS_TO_FULL ( 'MNN', NTERM_MNN, NDOFN, NDOFN, SYM_MNN, I_MNN, J_MNN,MNN, MNN_FULL )
         ENDIF
      
         IF (NTERM_MNM > 0) THEN                           ! Part 1: calc MNM*GMN and add it & it's transpose to MNN_FULL

            CALL ALLOCATE_FULL_MAT ( 'MNM_FULL', NDOFN, NDOFM, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'MNM', NTERM_MNM, NDOFN, NDOFM, SYM_MNM, I_MNM, J_MNM, MNM, MNM_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GMN_FULL', NDOFM, NDOFN, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GMN', NTERM_GMN, NDOFM, NDOFN, SYM_GMN, I_GMN, J_GMN, GMN, GMN_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFN, NDOFN, SUBR_NAME )

            CALL MATMULT_FFF (  MNM_FULL, GMN_FULL, NDOFN, NDOFM, NDOFN, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'MNM_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'GMN_FULL' )
            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFN, NDOFN, SUBR_NAME )

            ITRNSPB = 0                                    ! Calc MNN-bar + MNM*GMN = MNN + DUM1 and put into DUM1
            CALL MATADD_FFF ( MNN_FULL, DUM1, NDOFN, NDOFN, ALPHA, BETA, ITRNSPB, DUM2 )
            ITRNSPB = 1                                    ! Calc MNN-bar + MNM*GMN + (MNM*GMN)' = DUM2+DUM1' and put into MNN_FULL
            CALL MATADD_FFF ( DUM2    , DUM1, NDOFN, NDOFN, ALPHA, BETA, ITRNSPB, MNN_FULL )


            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         IF (NTERM_MMM > 0) THEN                           ! Part 2: calc GMN(t)*MMM*GMN and add to MNN_FULL

            CALL ALLOCATE_FULL_MAT ( 'MMM_FULL', NDOFM, NDOFM, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'MMM', NTERM_MMM, NDOFM, NDOFM, SYM_MMM, I_MMM, J_MMM, MMM, MMM_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GMN_FULL', NDOFM, NDOFN, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GMN', NTERM_GMN, NDOFM, NDOFN, SYM_GMN, I_GMN, J_GMN, GMN, GMN_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFM, NDOFN, SUBR_NAME )

            CALL MATMULT_FFF ( MMM_FULL, GMN_FULL, NDOFM, NDOFM, NDOFN, DUM2 )

            CALL DEALLOCATE_FULL_MAT ( 'MMM_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFN, NDOFN, SUBR_NAME )

            CALL MATMULT_FFF_T (GMN_FULL, DUM2, NDOFM, NDOFN, NDOFN, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

            CALL DEALLOCATE_FULL_MAT ( 'GMN_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFN, NDOFN, SUBR_NAME )

            ITRNSPB = 0
            CALL MATADD_FFF ( MNN_FULL, DUM1, NDOFN, NDOFN, ALPHA, BETA, ITRNSPB, DUM2 )
            DO I=1,NDOFN
               DO J=1,NDOFN
                  MNN_FULL(I,J) = DUM2(I,J)
               ENDDO
            ENDDO

            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         CALL CNT_NONZ_IN_FULL_MAT ( 'MNN_FULL  ', MNN_FULL, NDOFN, NDOFN, SYM_MNN, NTERM_MNN, SMALL )

         WRITE(SC1, * ) '    Reallocate MNN'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MNN', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'MNN' )
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   MNN', CR13
         CALL ALLOCATE_SPARSE_MAT ( 'MNN', NDOFN, NTERM_MNN, SUBR_NAME )

         IF (NTERM_MNN > 0) THEN                            ! Create new sparse arrays from MNN_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'MNN_FULL  ', NDOFN, NDOFN, MNN_FULL, NTERM_MNN, SYM_MNN, I_MNN, J_MNN, MNN )
            CALL DEALLOCATE_FULL_MAT ( 'MNN_FULL' )
         ENDIF

      ELSE

         WRITE(ERR,911) SUBR_NAME,MATSPARS
         WRITE(F06,911) SUBR_NAME,MATSPARS
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  911 FORMAT(' *ERROR   911: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER MATSPARS MUST BE EITHER ','Y',' OR ','N',' BUT VALUE IS ',A)

  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

  936 FORMAT(' *ERROR   936: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF NONZERO TERMS IN SPARSE MATRIX MMN = ',I12,' IS NOT EQUAL TO THOSE IN MATRIX MNM = ',I12)


12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_MGG_TO_MNN
