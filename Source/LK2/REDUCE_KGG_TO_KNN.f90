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

      SUBROUTINE REDUCE_KGG_TO_KNN ( PART_VEC_G_NM )
 
! Call routines to reduce the KGG linear stiffness matrix from the G-set to the N, M-sets. See Appendix B to the MYSTRAN User's
! Reference Manual for the derivation of the reduction equations.
 
! NOTE: This subr has code for sparse matrices as well as full matrices, (i.e. Bulk Data PARAM MATSPARS = 'Y' for sparse and 'N'
! for full). The code for full matrices was put in originally in order that the sparse code could be thoroughly checked. That task
! is complete and the remaining full matrix code has not been maintained since around 2005. In addition, new capability added to
! MYSTRAN since that approx time does not have full matrix code.
 
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

      USE REDUCE_KGG_TO_KNN_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KGG_TO_KNN'
      CHARACTER(  1*BYTE)             :: SYM_CRS1            ! Storage format for matrix CRS1 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
      CHARACTER(  1*BYTE)             :: SYM_CRS3            ! Storage format for matrix CRS3 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: ITRNSPB             ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: KNN_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KNM_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
!xx   INTEGER(LONG)                   :: KMN_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KMM_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_CCS1          ! Number of terms in matrix CCS1  
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3          ! Number of terms in matrix CRS3  
      INTEGER(LONG)                   :: NTERM_KMN           ! Number of nonzeros in sparse matrix KMN (should = NTERM_KNM)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KGG_TO_KNN_BEGEND

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
! Partition KNN from KGG (This is KNN before reduction, or KNN(bar) )

      IF (NDOFN > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
                                    NUM1, NUM1, KNN_ROW_MAX_TERMS, 'KNN', NTERM_KNN, SYM_KNN ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KNN', NDOFN, NTERM_KNN, SUBR_NAME )

         IF (NTERM_KNN > 0) THEN      
            CALL PARTITION_SS ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG, KGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
                                 NUM1, NUM1, KNN_ROW_MAX_TERMS, 'KNN', NTERM_KNN, NDOFN, SYM_KNN, I_KNN, J_KNN, KNN )
         ENDIF

      ENDIF

! Partition KNM from KGG

      IF ((NDOFN > 0) .AND. (NDOFM > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
                                    NUM1, NUM2, KNM_ROW_MAX_TERMS, 'KNM', NTERM_KNM, SYM_KNM ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KNM', NDOFN, NTERM_KNM, SUBR_NAME )

         IF (NTERM_KNM > 0) THEN
            CALL PARTITION_SS ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG, KGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
                                 NUM1, NUM2, KNM_ROW_MAX_TERMS, 'KNM', NTERM_KNM, NDOFN, SYM_KNM, I_KNM, J_KNM, KNM )
         ENDIF

      ENDIF

! Partition KMN from KGG

      IF ((NDOFN > 0) .AND. (NDOFM > 0)) THEN

!xx      CALL PARTITION_SS_NTERM ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
!xx                                 NUM2, NUM1, KMN_ROW_MAX_TERMS, 'KMN', NTERM_KMN, SYM_KMN ) 
!xx      IF (NTERM_KMN /= NTERM_KNM) THEN
!xx         FATAL_ERR = FATAL_ERR + 1
!xx         WRITE(ERR,936) SUBR_NAME, NTERM_KMN, NTERM_KNM
!xx         WRITE(F06,936) SUBR_NAME, NTERM_KMN, NTERM_KNM
!xx         CALL OUTA_HERE ( 'Y' )
!xx      ENDIF
!xx
!xx      CALL ALLOCATE_SPARSE_MAT ( 'KMN', NDOFM, NTERM_KMN, SUBR_NAME )
!xx
!xx      IF (NTERM_KMN > 0) THEN
!xx         CALL PARTITION_SS ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG, KGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
!xx                              NUM2, NUM1, KMN_ROW_MAX_TERMS, 'KMN', NTERM_KMN, NDOFM, SYM_KMN, I_KMN, J_KMN, KMN )
!xx      ENDIF
!xx
         NTERM_KMN = NTERM_KNM
         CALL ALLOCATE_SPARSE_MAT ( 'KMN', NDOFM, NTERM_KNM, SUBR_NAME )

         IF (NTERM_KMN > 0) THEN
            CALL MATTRNSP_SS ( NDOFN, NDOFM, NTERM_KNM, 'KNM', I_KNM, J_KNM, KNM, 'KMN', I_KMN, J_KMN, KMN )

         ENDIF

      ENDIF

! Partition KMM from KGG

      IF (NDOFM > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG,      PART_VEC_G_NM, PART_VEC_G_NM,       &
                                    NUM2, NUM2, KMM_ROW_MAX_TERMS, 'KMM', NTERM_KMM, SYM_KMM ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KMM', NDOFM, NTERM_KMM, SUBR_NAME )

         IF (NTERM_KMM > 0) THEN
            CALL PARTITION_SS ( 'KGG', NTERM_KGG, NDOFG, NDOFG, SYM_KGG, I_KGG, J_KGG, KGG, PART_VEC_G_NM, PART_VEC_G_NM,          &
                                 NUM2, NUM2, KMM_ROW_MAX_TERMS, 'KMM', NTERM_KMM, NDOFM, SYM_KMM, I_KMM, J_KMM, KMM )
         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Reduce KGG to KNN = KNN(bar) + KNM*GMN + (KNM*GMN)' + GMN'*KMM*GMN.
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN                              ! Reduce KGG to KNN using sparse matrix operations

         CALL ALLOCATE_SPARSE_MAT ( 'GMNt', NDOFN, NTERM_GMN, SUBR_NAME )
         CALL MATTRNSP_SS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'GMNt', I_GMNt, J_GMNt, GMNt )
                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix GMN
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFN, NTERM_GMN, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

         IF (NTERM_KNM > 0) THEN                           ! Part I of reduced KNN: calc KNM*GMN & add it & transpose to orig KNN

                                                           ! I-1, sparse multiply to get CRS1 = KNM*GMN. Use CCS1 for GMN CCS
            CALL MATMULT_SSS_NTERM ( 'KNM' , NDOFN, NTERM_KNM , SYM_KNM, I_KNM , J_KNM ,                                           &
                                     'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'KNM' , NDOFN, NTERM_KNM , SYM_KNM, I_KNM , J_KNM , KNM ,                                           &
                               'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )
            NTERM_CRS2 = NTERM_CRS1                        ! I-2, allocate memory to array CRS2 which will hold transpose of CRS1
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )

                                                           ! I-3, transpose CRS1 to get CRS2 = (KNM*GMN)t
            CALL MATTRNSP_SS ( NDOFN, NDOFN, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CRS2', I_CRS2, J_CRS2, CRS2 )
                                                           ! I-4, sparse add to get CRS3 = CRS1 + CRS2 = (KNM*GMN) + (KNM*GMN)t
            CALL MATADD_SSS_NTERM (NDOFN,'KNM*GMN', NTERM_CRS1, I_CRS1, J_CRS1, 'N', '(KNM*GMN)t', NTERM_CRS2, I_CRS2, J_CRS2, 'N',&
                                         'CRS3', NTERM_CRS3)
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'KNM*GMN', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, '(KNM*GMN)t', NTERM_CRS2,                   &
                              I_CRS2, J_CRS2, CRS2, ONE, 'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-5, deallocate CRS1 which was KNM*GMN
            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! I-6, deallocate CRS2 which was (KNM*GMN)t

                                                           ! I-7, CRS3 = (KNM*GMN) + (KNM*GMN)t has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS3 as sym in CRS1     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFN, NTERM_CRS3, '(KNM*GMN) + (KNM*GMN)t', I_CRS3, J_CRS3, NTERM_CRS1 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS3 = (KNM*GMN) + (KNM*GMN)t all nonzeros', NDOFN, NTERM_CRS3, I_CRS3, J_CRS3, CRS3, &
                                            'CRS1 = (KNM*GMN) + (KNM*GMN)t stored sym'  ,        NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )
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

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-8, deallocate CRS3 which was KNM*GMN + (KNM*GMN)t and now is in CRS1

                                                           ! I-9, add to get CRS3 = KNN-bar + CRS1 = KNN-bar + KNM*GMN + (KNM*GMN)t
            CALL MATADD_SSS_NTERM ( NDOFN, 'KNN-bar', NTERM_KNN , I_KNN , J_KNN , SYM_KNN , 'KNM*GMN + (KNM*GMN)t',                &
                                                      NTERM_CRS1, I_CRS1, J_CRS1, SYM_CRS1, 'CRS3', NTERM_CRS3 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'KNN-bar', NTERM_KNN, I_KNN, J_KNN, KNN, ONE, 'KNM*GMN + (KNM*GMN)t', NTERM_CRS1,             &
                                      I_CRS1, J_CRS1, CRS1, ONE, 'CRS1', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )
            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-10, deallocate CRS1 = KNM*GMN + (KNM*GMN)t 

            NTERM_KNN = NTERM_CRS3                         ! I-11, reallocate KNN to be size of CRS3
            WRITE(SC1, * ) '    Reallocate KNN'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNN', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'KNN' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KNN', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'KNN', NDOFN, NTERM_KNN, SUBR_NAME )

            DO I=1,NDOFN+1                                 ! I-12, set KNN = CRS3
               I_KNN(I) = I_CRS3(I)
            ENDDO
            DO J=1,NTERM_KNN
               J_KNN(J) = J_CRS3(J)
                 KNN(J) =   CRS3(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-13, deallocate CRS3
                                                           ! At this point, CRS1, CRS2, CRS3 are deallocated, CCS1 is being used
         ENDIF

         IF (NTERM_KMM > 0) THEN                           ! Part II of reduced KNN: calc GMN(t)*KMM*GMN and add to KNN

                                                           ! II-1, sparse multiply to get CRS1 = KMM*GMN using CCS1 for GMN CCS
            CALL MATMULT_SSS_NTERM ( 'KMM' , NDOFM, NTERM_KMM , SYM_KMM, I_KMM , J_KMM ,                                           &
                                     'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1',        NTERM_CRS1 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFM, NTERM_CRS1, SUBR_NAME )
            CALL MATMULT_SSS ( 'KMM' , NDOFM, NTERM_KMM , SYM_KMM, I_KMM , J_KMM , KMM ,                                           &
                               'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )
            IF (NTERM_KMN > 0) THEN
               CALL MATADD_SSS_NTERM (NDOFM, 'KMM*GMN', NTERM_CRS1, I_CRS1, J_CRS1, 'N', 'KMN', NTERM_KMN, I_KMN, J_KMN, SYM_KMN,  &
                                             'HMN', NTERM_HMN)

               CALL ALLOCATE_SPARSE_MAT ( 'HMN', NDOFM, NTERM_HMN, SUBR_NAME )

               CALL MATADD_SSS ( NDOFM, 'KMM*GM', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, 'KMN', NTERM_KMN, I_KMN, J_KMN, KMN,      &
                                 ONE, 'HMN', NTERM_HMN, I_HMN, J_HMN, HMN )
            ELSE
               NTERM_HMN = NTERM_CRS1
               CALL ALLOCATE_SPARSE_MAT ( 'HMN', NDOFM, NTERM_HMN, SUBR_NAME )
               DO I=1,NDOFM+1
                  I_HMN(I) = I_CRS1(I)
               ENDDO
               DO I=1,NTERM_HMN
                  J_HMN(I) = J_CRS1(I)
                  HMN(I) =   CRS1(I)
               ENDDO
            ENDIF

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-2, deallocate CCS1 which was CCS version of GMN

            NTERM_CCS1 = NTERM_CRS1                        ! II-3, allocate CCS1 to be same as CRS1 = KMM*GMN but in CCS format
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFN, NTERM_CCS1, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NDOFN, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-4, deallocate CRS1 which was KMM*GMN
                                                           ! II-5, sparse multiply to get CRS1 = GMNt*CCS1  with CCS1 = KMM*GMN
!                                                           (note: use SYM_GMN for sym indicator of KMM*GMN)
            CALL MATMULT_SSS_NTERM ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt,                                           &
                                     'CCS1', NDOFN, NTERM_CCS1, SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt, GMNt,                                           &
                               'CCS1', NDOFN, NTERM_CCS1, SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )
            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-6, deallocate CCS1

                                                           ! II-7, CRS1 = GMNt*KMM*GMN has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS3     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFN, NTERM_CRS1, 'GMNt*KMM*GMN all nonzeros', I_CRS1, J_CRS1, NTERM_CRS3 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = GMNt*KMM*GMN all nonzeros', NDOFN, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,           &
                                            'CRS3 = GMNt*KMM*GMN stored sym'  ,        NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )
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
                                                           ! II-8, sparse add to get CRS2 = KNN + CRS3 = KNN + GMNt*KMM*GMN
            CALL MATADD_SSS_NTERM ( NDOFN, 'KNN-bar + KNM*GMN + (KNM*GMN)t', NTERM_KNN, I_KNN, J_KNN, SYM_KNN, 'GMNt*KMM*GMN',     &
                                    NTERM_CRS3, I_CRS3, J_CRS3, SYM_CRS3, 'CRS2', NTERM_CRS2 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'KNN-bar + KNM*GMN + (KNM*GMN)t' , NTERM_KNN ,I_KNN, J_KNN, KNN, ONE, 'GMNt*KMM*GMN',         &
                              NTERM_CRS3, I_CRS3, J_CRS3, CRS3, ONE, 'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-9, deallocate CRS1
            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! II-10, deallocate CRS3

            NTERM_KNN = NTERM_CRS2                         ! II-11, reallocate KNN to be size of CRS2
            WRITE(SC1, * ) '    Reallocate KNN'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNN', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'KNN' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KNN', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'KNN', NDOFN, NTERM_KNN, SUBR_NAME )

            DO I=1,NDOFN+1                                 ! II-12, set reduced KNN = CRS2 until we see if KNM is null, below
               I_KNN(I) = I_CRS2(I)
            ENDDO
            DO I=1,NTERM_KNN
               J_KNN(I) = J_CRS2(I)
                 KNN(I) =   CRS2(I)
            ENDDO

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! II-13, Deallocate CRS2
            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

         ELSE

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

            IF (NTERM_KMN > 0) THEN                        ! Set HMN - KMN if KMN nonzero, else HMN is null
               NTERM_HMN = NTERM_KMN
               CALL ALLOCATE_SPARSE_MAT ( 'HMN', NDOFM, NTERM_HMN, SUBR_NAME )
               DO I=1,NDOFM+1
                  I_HMN(I) = I_KMN(I)
               ENDDO
               DO I=1,NTERM_HMN
                  J_HMN(I) = J_KMN(I)
                  HMN(I) =   KMN(I)
               ENDDO
            ELSE
               NTERM_HMN = 0
            ENDIF

         ENDIF

         IF (NTERM_HMN > 0) THEN
            CALL WRITE_MATRIX_1 ( LINK2J, L2J, 'Y', 'KEEP', L2J_MSG, 'HMN', NTERM_HMN, NDOFM, I_HMN, J_HMN, HMN )
         ENDIF

         WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GMNt', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'GMNt' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate HMN ', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'HMN' )
         CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (MATSPARS == 'N') THEN                       ! Reduce KGG to KNN using full matrix operations

         CALL ALLOCATE_FULL_MAT ( 'KNN_FULL', NDOFN, NDOFN, SUBR_NAME )

         IF (NTERM_KNN > 0) THEN                           ! Put KNN-bar into KNN_FULL
            CALL SPARSE_CRS_TO_FULL ( 'KNN', NTERM_KNN, NDOFN, NDOFN, SYM_KNN, I_KNN, J_KNN, KNN, KNN_FULL )
         ENDIF
      
         IF (NTERM_KNM > 0) THEN                           ! Part 1: calc KNM*GMN and add it & it's transpose to KNN_FULL

            CALL ALLOCATE_FULL_MAT ( 'KNM_FULL', NDOFN, NDOFM, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'KNM', NTERM_KNM, NDOFN, NDOFM, SYM_KNM, I_KNM, J_KNM, KNM, KNM_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GMN_FULL', NDOFM, NDOFN, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GMN', NTERM_GMN, NDOFM, NDOFN, SYM_GMN, I_GMN, J_GMN, GMN, GMN_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFN, NDOFN, SUBR_NAME )

            CALL MATMULT_FFF (  KNM_FULL, GMN_FULL, NDOFN, NDOFM, NDOFN, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'GMN_FULL' )
            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFN, NDOFN, SUBR_NAME )

            ITRNSPB = 0                                    ! Calc KNN-bar + KNM*GMN = KNN + DUM1 and put into DUM1
            CALL MATADD_FFF ( KNN_FULL, DUM1, NDOFN, NDOFN, ALPHA, BETA, ITRNSPB, DUM2 )
            ITRNSPB = 1                                    ! Calc KNN-bar + KNM*GMN + (KNM*GMN)' = DUM2+DUM1' and put into KNN_FULL
            CALL MATADD_FFF ( DUM2    , DUM1, NDOFN, NDOFN, ALPHA, BETA, ITRNSPB, KNN_FULL )


            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         IF (NTERM_KMM > 0) THEN                           ! Part 2: calc GMN(t)*KMM*GMN and add to KNN_FULL

            CALL ALLOCATE_FULL_MAT ( 'KMM_FULL', NDOFM, NDOFM, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'KMM', NTERM_KMM, NDOFM, NDOFM, SYM_KMM, I_KMM, J_KMM, KMM, KMM_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GMN_FULL', NDOFM, NDOFN, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GMN', NTERM_GMN, NDOFM, NDOFN, SYM_GMN, I_GMN, J_GMN, GMN, GMN_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFM, NDOFN, SUBR_NAME )

            CALL MATMULT_FFF ( KMM_FULL, GMN_FULL, NDOFM, NDOFM, NDOFN, DUM2 )

            CALL DEALLOCATE_FULL_MAT ( 'KMM_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFN, NDOFN, SUBR_NAME )

            CALL MATMULT_FFF_T (GMN_FULL, DUM2, NDOFM, NDOFN, NDOFN, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'GMN_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

            ITRNSPB = 0                                    ! Add GMN'*KMM*GMN to what is already in KNN_FULL (from above)
            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFN, NDOFN, SUBR_NAME )
            CALL MATADD_FFF ( KNN_FULL, DUM1, NDOFN, NDOFN, ALPHA, BETA, ITRNSPB, DUM2 )
            DO I=1,NDOFN
               DO J=1,NDOFN
                  KNN_FULL(I,J) = DUM2(I,J)
               ENDDO
            ENDDO

            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         CALL CNT_NONZ_IN_FULL_MAT ( 'KNN_FULL  ', KNN_FULL, NDOFN, NDOFN, SYM_KNN, NTERM_KNN, SMALL )

         WRITE(SC1, * ) '    Reallocate KNN'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNN', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'KNN' )
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KNN', CR13
         CALL ALLOCATE_SPARSE_MAT ( 'KNN', NDOFN, NTERM_KNN, SUBR_NAME )

         IF (NTERM_KNN > 0) THEN                           ! Create new sparse arrays from KNN_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'KNN_FULL  ', NDOFN, NDOFN, KNN_FULL, NTERM_KNN, SYM_KNN, I_KNN, J_KNN, KNN )
            CALL DEALLOCATE_FULL_MAT ( 'KNN_FULL' )
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
                    ,/,14X,' PARAMETER MATSPARS MUST BE EITHER "Y" OR "N" BUT VALUE IS ',A)

  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

  936 FORMAT(' *ERROR   936: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF NONZERO TERMS IN SPARSE MATRIX MMN = ',I12,' IS NOT EQUAL TO THOSE IN MATRIX MNM = ',I12)

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_KGG_TO_KNN
