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

      SUBROUTINE REDUCE_KGGD_TO_KNND ( PART_VEC_G_NM )
 
! Call routines to reduce the KGGD differential stiffness matrix from the G-set to the N, M-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2J, LINK2J, L2J_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NDOFN, NDOFM, NTERM_HMN, NTERM_KGGD, NTERM_KNND,          &
                                         NTERM_KNMD, NTERM_KMMD, NTERM_GMN
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KGGD_TO_KNND_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_HMN, J_HMN, HMN, I_KGGD, J_KGGD, KGGD, I_KNND, J_KNND, KNND, I_KNMD, J_KNMD, KNMD,      &
                                         I_KMMD, J_KMMD, KMMD, I_KMND, J_KMND, KMND, I_GMN, J_GMN, GMN,  I_GMNt, J_GMNt, GMNt
      USE SPARSE_MATRICES, ONLY       :  SYM_GMN, SYM_HMN, SYM_KGGD, SYM_KNND, SYM_KNMD, SYM_KMMD, SYM_KMND
      USE SCRATCH_MATRICES

      USE REDUCE_KGGD_TO_KNND_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KGGD_TO_KNND'
      CHARACTER(  1*BYTE)             :: SYM_CRS1            ! Storage format for matrix CRS1 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
      CHARACTER(  1*BYTE)             :: SYM_CRS3            ! Storage format for matrix CRS3 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
!                                                              the ones on and above the diagonal (controlled by param SPARSTOR)
      INTEGER(LONG)                   :: KNND_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KNMD_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
!xx   INTEGER(LONG)                   :: KMND_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KMMD_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_CCS1          ! Number of terms in matrix CCS1  
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3          ! Number of terms in matrix CRS3  
      INTEGER(LONG)                   :: NTERM_KMND           ! Number of nonzeros in sparse matrix KMND (should = NTERM_KNMD)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KGGD_TO_KNND_BEGEND

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition KNND from KGGD (This is KNND before reduction, or KNND(bar) )

      IF (NDOFN > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD,      PART_VEC_G_NM, PART_VEC_G_NM,  &
                                    NUM1, NUM1, KNND_ROW_MAX_TERMS, 'KNND', NTERM_KNND, SYM_KNND ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KNND', NDOFN, NTERM_KNND, SUBR_NAME )

         IF (NTERM_KNND > 0) THEN      
            CALL PARTITION_SS ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD, KGGD, PART_VEC_G_NM, PART_VEC_G_NM,    &
                                 NUM1, NUM1, KNND_ROW_MAX_TERMS, 'KNND', NTERM_KNND, NDOFN, SYM_KNND, I_KNND, J_KNND, KNND )
         ENDIF

      ENDIF

! Partition KNMD from KGGD

      IF ((NDOFN > 0) .AND. (NDOFM > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD,      PART_VEC_G_NM, PART_VEC_G_NM,  &
                                    NUM1, NUM2, KNMD_ROW_MAX_TERMS, 'KNMD', NTERM_KNMD, SYM_KNMD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KNMD', NDOFN, NTERM_KNMD, SUBR_NAME )

         IF (NTERM_KNMD > 0) THEN
            CALL PARTITION_SS ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD, KGGD, PART_VEC_G_NM, PART_VEC_G_NM,    &
                                 NUM1, NUM2, KNMD_ROW_MAX_TERMS, 'KNMD', NTERM_KNMD, NDOFN, SYM_KNMD, I_KNMD, J_KNMD, KNMD )
         ENDIF

      ENDIF

! Partition KMND from KGGD

      IF ((NDOFN > 0) .AND. (NDOFM > 0)) THEN

!xx      CALL PARTITION_SS_NTERM ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD,      PART_VEC_G_NM, PART_VEC_G_NM,  &
!xx                                 NUM2, NUM1, KMND_ROW_MAX_TERMS, 'KMND', NTERM_KMND, SYM_KMND ) 

!xx      IF (NTERM_KMND /= NTERM_KNMD) THEN
!xx         FATAL_ERR = FATAL_ERR + 1
!xx         WRITE(ERR,936) SUBR_NAME, NTERM_KMND, NTERM_KNMD
!xx         WRITE(F06,936) SUBR_NAME, NTERM_KMND, NTERM_KNMD
!xx         CALL OUTA_HERE ( 'Y' )
!xx      ENDIF

!xx      CALL ALLOCATE_SPARSE_MAT ( 'KMND', NDOFM, NTERM_KMND, SUBR_NAME )

!xx      IF (NTERM_KMND > 0) THEN
!xx         CALL PARTITION_SS ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD, KGGD, PART_VEC_G_NM, PART_VEC_G_NM,    &
!xx                              NUM2, NUM1, KMND_ROW_MAX_TERMS, 'KMND', NTERM_KMND, NDOFM, SYM_KMND, I_KMND, J_KMND, KMND )
!xx      ENDIF

         NTERM_KMND = NTERM_KNMD
         CALL ALLOCATE_SPARSE_MAT ( 'KMND', NDOFM, NTERM_KNMD, SUBR_NAME )

         IF (NTERM_KMND > 0) THEN
            CALL MATTRNSP_SS ( NDOFN, NDOFM, NTERM_KNMD, 'KNMD', I_KNMD, J_KNMD, KNMD, 'KMND', I_KMND, J_KMND, KMND )

         ENDIF

      ENDIF

! Partition KMMD from KGGD

      IF (NDOFM > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD,      PART_VEC_G_NM, PART_VEC_G_NM,  &
                                    NUM2, NUM2, KMMD_ROW_MAX_TERMS, 'KMMD', NTERM_KMMD, SYM_KMMD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KMMD', NDOFM, NTERM_KMMD, SUBR_NAME )

         IF (NTERM_KMMD > 0) THEN
            CALL PARTITION_SS ( 'KGGD', NTERM_KGGD, NDOFG, NDOFG, SYM_KGGD, I_KGGD, J_KGGD, KGGD, PART_VEC_G_NM, PART_VEC_G_NM,    &
                                 NUM2, NUM2, KMMD_ROW_MAX_TERMS, 'KMMD', NTERM_KMMD, NDOFM, SYM_KMMD, I_KMMD, J_KMMD, KMMD )
         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Reduce KGGD to KNND = KNND(bar) + KNMD*GMN + (KNMD*GMN)' + GMN'*KMMD*GMN.
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations


         IF (.NOT. ALLOCATED(GMN)) THEN
            CALL ALLOCATE_SPARSE_MAT ( 'GMN' , NDOFN, NTERM_GMN, SUBR_NAME )
         ENDIF
         CALL ALLOCATE_SPARSE_MAT ( 'GMNt', NDOFN, NTERM_GMN, SUBR_NAME )
         CALL MATTRNSP_SS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'GMNt', I_GMNt, J_GMNt, GMNt )

                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix GMN
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFN, NTERM_GMN, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

         IF (NTERM_KNMD > 0) THEN                           ! Part I of reduced KNND: calc KNMD*GMN & add & transpose to orig KNND

                                                           ! I-1, sparse multiply to get CRS1 = KNMD*GMN. Use CCS1 for GMN CCS
            CALL MATMULT_SSS_NTERM ( 'KNMD', NDOFN, NTERM_KNMD, SYM_KNMD, I_KNMD, J_KNMD,                                          &
                                     'GMN' , NDOFN, NTERM_GMN , SYM_GMN , J_CCS1, I_CCS1, AROW_MAX_TERMS,                          &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'KNMD' , NDOFN, NTERM_KNMD , SYM_KNMD, I_KNMD , J_KNMD , KNMD ,                                     &
                               'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            NTERM_CRS2 = NTERM_CRS1                        ! I-2, allocate memory to array CRS2 which will hold transpose of CRS1
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )

                                                           ! I-3, transpose CRS1 to get CRS2 = (KNMD*GMN)t
            CALL MATTRNSP_SS ( NDOFN, NDOFN, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CRS2', I_CRS2, J_CRS2, CRS2 )

                                                           ! I-4, sparse add to get CRS3 = CRS1 + CRS2 = (KNMD*GMN) + (KNMD*GMN)t
            CALL MATADD_SSS_NTERM (NDOFN,'KNMD*GMN', NTERM_CRS1, I_CRS1, J_CRS1,'N','(KNMD*GMN)t', NTERM_CRS2, I_CRS2, J_CRS2, 'N',&
                                         'CRS3', NTERM_CRS3)
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'KNMD*GMN', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, '(KNMD*GMN)t', NTERM_CRS2,                 &
                              I_CRS2, J_CRS2, CRS2, ONE, 'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-5, deallocate CRS1 which was KNMD*GMN
            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! I-6, deallocate CRS2 which was (KNMD*GMN)t

                                                           ! I-7, CRS3 = (KNMD*GMN) + (KNMD*GMN)t has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS3 as sym in CRS1     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFN, NTERM_CRS3, '(KNMD*GMN) + (KNMD*GMN)t', I_CRS3, J_CRS3, NTERM_CRS1 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ('CRS3 = (KNMD*GMN) + (KNMD*GMN)t all nonzeros', NDOFN, NTERM_CRS3, I_CRS3, J_CRS3, CRS3,&
                                           'CRS1 = (KNMD*GMN) + (KNMD*GMN)t stored sym'  ,        NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )
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

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-8, deallocate CRS3 which was KNMD*GMN + (KNMD*GMN)t. Now is in CRS1

                                                           ! I-9, add: CRS3 = KNND-bar + CRS1 = KNND-bar + KNMD*GMN + (KNMD*GMN)t
            CALL MATADD_SSS_NTERM ( NDOFN, 'KNND-bar', NTERM_KNND , I_KNND , J_KNND , SYM_KNND , 'KNMD*GMN + (KNMD*GMN)t',         &
                                                      NTERM_CRS1, I_CRS1, J_CRS1, SYM_CRS1, 'CRS3', NTERM_CRS3 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'KNND-bar', NTERM_KNND, I_KNND, J_KNND, KNND, ONE, 'KNMD*GMN + (KNMD*GMN)t', NTERM_CRS1,      &
                                      I_CRS1, J_CRS1, CRS1, ONE, 'CRS1', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-10, deallocate CRS1 = KNMD*GMN + (KNMD*GMN)t 

            NTERM_KNND = NTERM_CRS3                        ! I-11, reallocate KNND to be size of CRS3
            WRITE(SC1, * ) '    Reallocate KNND'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNND', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'KNND' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KNND', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'KNND', NDOFN, NTERM_KNND, SUBR_NAME )

            DO I=1,NDOFN+1                                 ! I-12, set KNND = CRS3
               I_KNND(I) = I_CRS3(I)
            ENDDO
            DO J=1,NTERM_KNND
               J_KNND(J) = J_CRS3(J)
                 KNND(J) =   CRS3(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-13, deallocate CRS3
                                                           ! At this point, CRS1, CRS2, CRS3 are deallocated, CCS1 is being used
         ENDIF

         IF (NTERM_KMMD > 0) THEN                           ! Part II of reduced KNND: calc GMN(t)*KMMD*GMN and add to KNND

                                                           ! II-1, sparse multiply to get CRS1 = KMMD*GMN using CCS1 for GMN CCS

            CALL MATMULT_SSS_NTERM ( 'KMMD', NDOFM, NTERM_KMMD, SYM_KMMD, I_KMMD, J_KMMD ,                                         &
                                     'GMN' , NDOFN, NTERM_GMN , SYM_GMN , J_CCS1, I_CCS1, AROW_MAX_TERMS,                          &
                                     'CRS1',        NTERM_CRS1 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFM, NTERM_CRS1, SUBR_NAME )
            CALL MATMULT_SSS ( 'KMMD' , NDOFM, NTERM_KMMD , SYM_KMMD, I_KMMD , J_KMMD , KMMD ,                                     &
                               'GMN' , NDOFN, NTERM_GMN , SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            IF (NTERM_KMND > 0) THEN
               CALL MATADD_SSS_NTERM (NDOFM, 'KMMD*GMN', NTERM_CRS1, I_CRS1, J_CRS1, 'N', 'KMND', NTERM_KMND, I_KMND, J_KMND,      &
                                              SYM_KMND, 'HMN', NTERM_HMN)

               CALL ALLOCATE_SPARSE_MAT ( 'HMN', NDOFM, NTERM_HMN, SUBR_NAME )

               CALL MATADD_SSS ( NDOFM, 'KMMD*GM', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, 'KMND', NTERM_KMND, I_KMND, J_KMND, KMND,&
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

            NTERM_CCS1 = NTERM_CRS1                        ! II-3, allocate CCS1 to be same as CRS1 = KMMD*GMN but in CCS format
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFN, NTERM_CCS1, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NDOFN, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-4, deallocate CRS1 which was KMMD*GMN
                                                           ! II-5, sparse multiply to get CRS1 = GMNt*CCS1  with CCS1 = KMMD*GMN
!                                                           (note: use SYM_GMN for sym indicator of KMMD*GMN)
            CALL MATMULT_SSS_NTERM ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt,                                           &
                                     'CCS1', NDOFN, NTERM_CCS1, SYM_GMN, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt, GMNt,                                           &
                               'CCS1', NDOFN, NTERM_CCS1, SYM_GMN, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-6, deallocate CCS1

                                                           ! II-7, CRS1 = GMNt*KMMD*GMN has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS3     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFN, NTERM_CRS1, 'GMNt*KMMD*GMN all nonzeros', I_CRS1, J_CRS1, NTERM_CRS3 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFN, NTERM_CRS3, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = GMNt*KMMD*GMN all nonzeros', NDOFN, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,          &
                                            'CRS3 = GMNt*KMMD*GMN stored sym'  ,        NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )
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
                                                           ! II-8, sparse add to get CRS2 = KNND + CRS3 = KNND + GMNt*KMMD*GMN
            CALL MATADD_SSS_NTERM ( NDOFN, 'KNND-bar + KNMD*GMN + (KNMD*GMN)t', NTERM_KNND, I_KNND, J_KNND, SYM_KNND,              &
                                   'GMNt*KMMD*GMN',NTERM_CRS3, I_CRS3, J_CRS3, SYM_CRS3, 'CRS2', NTERM_CRS2 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'KNND-bar + KNMD*GMN + (KNMD*GMN)t' , NTERM_KNND ,I_KNND, J_KNND, KNND, ONE, 'GMNt*KMMD*GMN', &
                              NTERM_CRS3, I_CRS3, J_CRS3, CRS3, ONE, 'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-9, deallocate CRS1
            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! II-10, deallocate CRS3

            NTERM_KNND = NTERM_CRS2                        ! II-11, reallocate KNND to be size of CRS2
            WRITE(SC1, * ) '    Reallocate KNND'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNND', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'KNND' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KNND', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'KNND', NDOFN, NTERM_KNND, SUBR_NAME )

            DO I=1,NDOFN+1                                 ! II-12, set reduced KNND = CRS2 until we see if KNMD is null, below
               I_KNND(I) = I_CRS2(I)
            ENDDO
            DO I=1,NTERM_KNND
               J_KNND(I) = J_CRS2(I)
                 KNND(I) =   CRS2(I)
            ENDDO

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! II-13, Deallocate CRS2
            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

         ELSE

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

            IF (NTERM_KMND > 0) THEN                        ! Set HMN - KMND if KMND nonzero, else HMN is null
               NTERM_HMN = NTERM_KMND
               CALL ALLOCATE_SPARSE_MAT ( 'HMN', NDOFM, NTERM_HMN, SUBR_NAME )
               DO I=1,NDOFM+1
                  I_HMN(I) = I_KMND(I)
               ENDDO
               DO I=1,NTERM_HMN
                  J_HMN(I) = J_KMND(I)
                  HMN(I) =   KMND(I)
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


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

  936 FORMAT(' *ERROR   936: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF NONZERO TERMS IN SPARSE MATRIX MMN = ',I12,' IS NOT EQUAL TO THOSE IN MATRIX MNM = ',I12)

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_KGGD_TO_KNND
