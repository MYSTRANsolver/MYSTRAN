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

      SUBROUTINE REDUCE_PG_TO_PN ( PART_VEC_G_NM, PART_VEC_SUB )
 
! Call routines to reduce the PG grid point load matrix from the G-set to the N, M-sets. See Appendix B to the MYSTRAN User's
! Reference Manual for the derivation of the reduction equations.
 
! NOTE: This subr has code for sparse matrices as well as full matrices, (i.e. Bulk Data PARAM MATSPARS = 'Y' for sparse and 'N'
! for full). The code for full matrices was put in originally in order that the sparse code could be thoroughly checked. That task
! is complete and the remaining full matrix code has not been maintained since around 2005. In addition, new capability added to
! MYSTRAN since that approx time does not have full matrix code.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NDOFN, NDOFM, NSUB, NTERM_GMN, NTERM_PG, NTERM_PN, NTERM_PM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_PG_TO_PN_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE PARAMS, ONLY                :  EPSIL, MATSPARS
      USE SPARSE_MATRICES, ONLY       :  I_PG, J_PG, PG, I_PN, J_PN, PN, I_PM, J_PM, PM, I_GMN, J_GMN, GMN, I_GMNt, J_GMNt, GMNt 
      USE SPARSE_MATRICES, ONLY       :  SYM_GMN, SYM_PG, SYM_PN, SYM_PM
      USE FULL_MATRICES, ONLY         :  PN_FULL, PM_FULL, GMN_FULL, DUM1, DUM2
      USE SCRATCH_MATRICES
 
      USE REDUCE_PG_TO_PN_USE_IFs

      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_PG_TO_PN'
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG)                   :: PN_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: PM_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_PG_TO_PN_BEGEND

      REAL(DOUBLE)                    :: ALPHA = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: BETA  = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: SMALL               ! A number used in filtering out small numbers from a full matrix
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF


! **********************************************************************************************************************************
! Partition PN from PG (This is PN before reduction, or PN(bar) )

      IF (NDOFN > 0) THEN

         CALL PARTITION_SS_NTERM ( 'PG' , NTERM_PG , NDOFG, NSUB , SYM_PG , I_PG , J_PG ,      PART_VEC_G_NM, PART_VEC_SUB,        &
                                    NUM1, NUM1, PN_ROW_MAX_TERMS, 'PN', NTERM_PN, SYM_PN ) 

         CALL ALLOCATE_SPARSE_MAT ( 'PN', NDOFN, NTERM_PN, SUBR_NAME )

         IF (NTERM_PN  > 0) THEN
            CALL PARTITION_SS ( 'PG' , NTERM_PG , NDOFG, NSUB , SYM_PG , I_PG , J_PG , PG , PART_VEC_G_NM, PART_VEC_SUB,           &
                                 NUM1, NUM1, PN_ROW_MAX_TERMS, 'PN', NTERM_PN , NDOFN, SYM_PN, I_PN , J_PN , PN  )

         ENDIF

      ENDIF

! Partition PM from PG

      IF (NDOFM > 0) THEN

         CALL PARTITION_SS_NTERM ( 'PG' , NTERM_PG , NDOFG, NSUB , SYM_PG , I_PG , J_PG ,      PART_VEC_G_NM, PART_VEC_SUB,        &
                                    NUM2, NUM1, PM_ROW_MAX_TERMS, 'PM', NTERM_PM, SYM_PM ) 

         CALL ALLOCATE_SPARSE_MAT ( 'PM', NDOFM, NTERM_PM, SUBR_NAME )

         IF (NTERM_PM  > 0) THEN
            CALL PARTITION_SS ( 'PG' , NTERM_PG , NDOFG, NSUB , SYM_PG , I_PG , J_PG , PG , PART_VEC_G_NM, PART_VEC_SUB,           &
                                 NUM2, NUM1, PM_ROW_MAX_TERMS, 'PM', NTERM_PM , NDOFM, SYM_PM, I_PM , J_PM , PM  )
         ENDIF

      ENDIF

! Reduce PG to PN = PN(bar) + GMN'*PM
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN


         IF (NTERM_PM > 0) THEN                            ! Calc GMNt*PM & add it orig PN

            CALL ALLOCATE_SPARSE_MAT ( 'GMNt', NDOFN, NTERM_GMN, SUBR_NAME )
            CALL MATTRNSP_SS ( NDOFM, NDOFN, NTERM_GMN, 'GMN', I_GMN, J_GMN, GMN, 'GMNt', I_GMNt, J_GMNt, GMNt )

                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix PM
! *********************************************************************************************************************************
! ERROR: NDOFM should be NSUB for col size of PM
!!          CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFM, NTERM_PM, SUBR_NAME )
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NSUB, NTERM_PM, SUBR_NAME )
! *********************************************************************************************************************************

            CALL SPARSE_CRS_SPARSE_CCS ( NDOFM, NSUB, NTERM_PM, 'PM', I_PM, J_PM, PM, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

                                                           ! Sparse multiply to get CRS1 = GMNt*PM = GMNt*CCS1.
! *********************************************************************************************************************************
! ERROR: NDOFM should be NSUB for col size of PM
!!          CALL MATMULT_SSS_NTERM ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt,                                           &
!!                                   'PM'  , NDOFM, NTERM_PM  , SYM_PM , J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
!!                                   'CRS1' ,       NTERM_CRS1 )

            CALL MATMULT_SSS_NTERM ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt,                                           &
                                     'PM'  , NSUB , NTERM_PM  , SYM_PM , J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1' ,       NTERM_CRS1 )
! *********************************************************************************************************************************

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFN, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'GMNt', NDOFN, NTERM_GMN , SYM_GMN, I_GMNt, J_GMNt, GMNt,                                           &
                               'PM'  , NSUB , NTERM_PM  , SYM_PM , J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

                                                           ! Sparse add to get CRS2 = PN-bar + CRS1
            CALL MATADD_SSS_NTERM ( NDOFN, 'PN-bar', NTERM_PN, I_PN, J_PN, SYM_PN, 'GMNt*PM', NTERM_CRS1, I_CRS1, J_CRS1, 'N',     &
                                         'CRS2', NTERM_CRS2 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFN, NTERM_CRS2, SUBR_NAME )
            CALL MATADD_SSS ( NDOFN, 'PN-bar', NTERM_PN, I_PN, J_PN, PN, ONE, 'GMNt*PM', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE,    &
                                     'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! Deallocate CRS1

            NTERM_PN = NTERM_CRS2                          ! Reallocate KAA to be size of CRS2
            WRITE(SC1, * ) '    Reallocate PN '
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PN ', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'PN' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PN ', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'PN', NDOFN, NTERM_PN, SUBR_NAME )

            DO I=1,NDOFN+1
               I_PN(I) = I_CRS2(I)
            ENDDO
            DO J=1,NTERM_PN
               J_PN(J) = J_CRS2(J)
                 PN(J) =   CRS2(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! Deallocate CRS2 and GMNt
            WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GMNt', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'GMNt' )

         ENDIF

      ELSE IF (MATSPARS == 'N') THEN

         CALL ALLOCATE_FULL_MAT ( 'PN_FULL', NDOFN, NSUB, SUBR_NAME )  ! Calc reduced PN

         IF (NTERM_PN > 0) THEN
            CALL SPARSE_CRS_TO_FULL ( 'PN', NTERM_PN, NDOFN, NSUB, SYM_PN, I_PN, J_PN, PN, PN_FULL )
         ENDIF

         IF (NTERM_PM > 0) THEN                            ! Calc GMN(t)*PM and add to partitioned PN to get reduced PN 
      
            CALL ALLOCATE_FULL_MAT ( 'PM_FULL', NDOFM, NSUB, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'PM', NTERM_PM, NDOFM, NSUB, SYM_PM, I_PM, J_PM, PM, PM_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GMN_FULL', NDOFM, NDOFN, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GMN', NTERM_GMN, NDOFM, NDOFN, SYM_GMN, I_GMN, J_GMN, GMN, GMN_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFN, NSUB, SUBR_NAME )

            CALL MATMULT_FFF_T (GMN_FULL, PM_FULL, NDOFM, NDOFN, NSUB, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'PM_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'GMN_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFN, NSUB, SUBR_NAME )

            CALL MATADD_FFF ( PN_FULL, DUM1, NDOFN, NSUB, ALPHA, BETA, ITRNSPB, DUM2 )
            DO I=1,NDOFN
               DO J=1,NSUB
                  PN_FULL(I,J) = DUM2(I,J)
               ENDDO
            ENDDO

            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF 


         CALL CNT_NONZ_IN_FULL_MAT ( 'PN_FULL  ', PN_FULL, NDOFN, NSUB, SYM_PN, NTERM_PN, SMALL )

         WRITE(SC1, * ) '    Reallocate PN'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PN', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'PN' )
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PN', CR13
         CALL ALLOCATE_SPARSE_MAT ( 'PN', NDOFN, NTERM_PN, SUBR_NAME )

         IF (NTERM_PN > 0) THEN                            ! Create new sparse arrays from PN_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'PN_FULL   ', NDOFN, NSUB , PN_FULL,  NTERM_PN,  SYM_PN,  I_PN,  J_PN,  PN  )
            CALL DEALLOCATE_FULL_MAT ( 'PN_FULL' )      
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


12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_PG_TO_PN

