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

      SUBROUTINE REDUCE_PF_TO_PA ( PART_VEC_F_AO, PART_VEC_SUB )
 
! Call routines to reduce the PF grid point load matrix from the F-set to the A, O-sets. See Appendix B to the MYSTRAN User's
! Reference Manual for the derivation of the reduction equations.
 
! NOTE: This subr has code for sparse matrices as well as full matrices, (i.e. Bulk Data PARAM MATSPARS = 'Y' for sparse and 'N'
! for full). The code for full matrices was put in originally in order that the sparse code could be thoroughly checked. That task
! is complete and the remaining full matrix code has not been maintained since around 2005. In addition, new capability added to
! MYSTRAN since that approx time does not have full matrix code.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KOO_SDIA, NDOFF, NDOFA, NDOFO, NSUB, NTERM_GOA, NTERM_PF,        &
                                         NTERM_PA, NTERM_PO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_PF_TO_PA_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE PARAMS, ONLY                :  EPSIL, MATSPARS
      USE SPARSE_MATRICES, ONLY       :  I_PF, J_PF, PF, I_PA, J_PA, PA, I_PO, J_PO, PO, I_GOA, J_GOA, GOA, I_GOAt, J_GOAt, GOAt 
      USE SPARSE_MATRICES, ONLY       :  SYM_GOA, SYM_PF, SYM_PA, SYM_PO
      USE FULL_MATRICES, ONLY         :  PA_FULL, PO_FULL, GOA_FULL, DUM1, DUM2
      USE SCRATCH_MATRICES
 
      USE REDUCE_PF_TO_PA_USE_IFs

      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_PF_TO_PA'
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F_AO(NDOFF)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG)                   :: PA_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: PO_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_PF_TO_PA_BEGEND

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
! Partition PA from PF (This is PA before reduction, or PA(bar) )

      IF (NDOFA > 0) THEN

         CALL PARTITION_SS_NTERM ( 'PF' , NTERM_PF , NDOFF, NSUB , SYM_PF , I_PF , J_PF      , PART_VEC_F_AO, PART_VEC_SUB,        &
                                    NUM1, NUM1, PA_ROW_MAX_TERMS, 'PA', NTERM_PA, SYM_PA ) 

         CALL ALLOCATE_SPARSE_MAT ( 'PA', NDOFA, NTERM_PA, SUBR_NAME )

         IF (NTERM_PA  > 0) THEN
            CALL PARTITION_SS ( 'PF' , NTERM_PF , NDOFF, NSUB , SYM_PF , I_PF , J_PF , PF , PART_VEC_F_AO, PART_VEC_SUB,           &
                                 NUM1, NUM1, PA_ROW_MAX_TERMS, 'PA', NTERM_PA , NDOFA, SYM_PA, I_PA , J_PA , PA  )
         ENDIF
                            
      ENDIF

! Partition PO from PF

      IF (NDOFO > 0) THEN

         CALL PARTITION_SS_NTERM ( 'PF' , NTERM_PF , NDOFF, NSUB , SYM_PF , I_PF , J_PF ,      PART_VEC_F_AO, PART_VEC_SUB,        &
                                    NUM2, NUM1, PO_ROW_MAX_TERMS, 'PO', NTERM_PO, SYM_PO ) 

         CALL ALLOCATE_SPARSE_MAT ( 'PO', NDOFO, NTERM_PO, SUBR_NAME )

         IF (NTERM_PO  > 0) THEN
            CALL PARTITION_SS ( 'PF' , NTERM_PF , NDOFF, NSUB , SYM_PF , I_PF , J_PF , PF , PART_VEC_F_AO, PART_VEC_SUB,           &
                                 NUM2, NUM1, PO_ROW_MAX_TERMS, 'PO', NTERM_PO , NDOFO, SYM_PO, I_PO , J_PO , PO  )
         ENDIF

      ENDIF

! Reduce PF to PA = PA(bar) + GOA'*PO
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN


         IF (NTERM_PO > 0) THEN                            ! Calc GOAt*PO & add it orig PA

            CALL ALLOCATE_SPARSE_MAT ( 'GOAt', NDOFA, NTERM_GOA, SUBR_NAME )
            CALL MATTRNSP_SS ( NDOFO, NDOFA, NTERM_GOA, 'GOA', I_GOA, J_GOA, GOA, 'GOAt', I_GOAt, J_GOAt, GOAt )
                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix PO
!! *********************************************************************************************************************************
! ERROR: NDOFO should be NSUB for col size of PO
!!          CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFO, NTERM_PO, SUBR_NAME )
!! *********************************************************************************************************************************
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NSUB, NTERM_PO, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFO, NSUB, NTERM_PO, 'PO', I_PO, J_PO, PO, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

                                                           ! Sparse multiply to get CRS1 = GOAt*PO = GOAt*CCS1.
! *********************************************************************************************************************************
! ERROR: NDOFO should be NSUB for col size of PO
!!          CALL MATMULT_SSS_NTERM ( 'GOAt', NDOFA, NTERM_GOA, SYM_GOA, I_GOAt, J_GOAt,                                            &
!!                                   'PO'  , NDOFO, NTERM_PO , SYM_PO , J_CCS1, I_CCS1, AROW_MAX_TERMS,                            &
!!                                   'CRS1',        NTERM_CRS1 )
! *********************************************************************************************************************************
            CALL MATMULT_SSS_NTERM ( 'GOAt', NDOFA, NTERM_GOA, SYM_GOA, I_GOAt, J_GOAt,                                            &
                                     'PO'  , NSUB , NTERM_PO , SYM_PO , J_CCS1, I_CCS1, AROW_MAX_TERMS,                            &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'GOAt', NDOFA, NTERM_GOA , SYM_GOA, I_GOAt, J_GOAt, GOAt,                                           &
                               'PO'  , NSUB , NTERM_PO  , SYM_PO , J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

                                                           ! Sparse add to get CRS2 = PA-bar + CRS1
            CALL MATADD_SSS_NTERM ( NDOFA, 'PA-bar', NTERM_PA, I_PA, J_PA, SYM_PA, 'GOAt*PO', NTERM_CRS1, I_CRS1, J_CRS1, 'N',     &
                                         'CRS2', NTERM_CRS2 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFA, NTERM_CRS2, SUBR_NAME )
            CALL MATADD_SSS ( NDOFA, 'PA-bar', NTERM_PA, I_PA, J_PA, PA, ONE, 'GOAt*PO', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE,    &
                                     'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! Deallocate CRS1

            NTERM_PA = NTERM_CRS2                          ! Reallocate KAA to be size of CRS2
            WRITE(SC1, * ) '    Reallocate PA '
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PA ', CR13   
            CALL DEALLOCATE_SPARSE_MAT ( 'PA' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PA ', CR13   
            CALL ALLOCATE_SPARSE_MAT ( 'PA', NDOFA, NTERM_PA, SUBR_NAME )
                                                           ! Set KAA = CRS1
            DO I=1,NDOFA+1
               I_PA(I) = I_CRS2(I)
            ENDDO
            DO J=1,NTERM_PA
               J_PA(J) = J_CRS2(J)
                 PA(J) =   CRS2(J)
            ENDDO 

            WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GOAt', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'GOAt' )
            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! Deallocate CRS2 and GOAt

         ENDIF

      ELSE IF (MATSPARS == 'N') THEN

         CALL ALLOCATE_FULL_MAT ( 'PA_FULL', NDOFA, NSUB, SUBR_NAME )  ! Calc reduced PA

         IF (NTERM_PA > 0) THEN
            CALL SPARSE_CRS_TO_FULL ( 'PA', NTERM_PA, NDOFA, NSUB, SYM_PA, I_PA, J_PA, PA, PA_FULL )
         ENDIF

         IF (NTERM_PO > 0) THEN                            ! Calc GOA(t)*PO and add to partitioned PA to get reduced PA 
      
            CALL ALLOCATE_FULL_MAT ( 'PO_FULL', NDOFO, NSUB, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'PO', NTERM_PO, NDOFO, NSUB, SYM_PO, I_PO, J_PO, PO, PO_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GOA_FULL', NDOFO, NDOFA, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GOA', NTERM_GOA, NDOFO, NDOFA, SYM_GOA, I_GOA, J_GOA, GOA, GOA_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFA, NSUB, SUBR_NAME )

            CALL MATMULT_FFF_T (GOA_FULL, PO_FULL, NDOFO, NDOFA, NSUB, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'PO_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'GOA_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFA, NSUB, SUBR_NAME )
                                                           ! Final reduced PA
            CALL MATADD_FFF ( PA_FULL, DUM1, NDOFA, NSUB, ALPHA, BETA, ITRNSPB, DUM2 )
            DO I=1,NDOFA
               DO J=1,NSUB
                  PA_FULL(I,J) = DUM2(I,J)
               ENDDO
            ENDDO

            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF 

         CALL CNT_NONZ_IN_FULL_MAT ( 'PA_FULL  ', PA_FULL, NDOFA, NSUB, SYM_PA, NTERM_PA, SMALL )

         WRITE(SC1, * ) '    Reallocate PA '
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PA ', CR13  ;  CALL DEALLOCATE_SPARSE_MAT ( 'PA' )
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PA ', CR13  ;  CALL ALLOCATE_SPARSE_MAT ('PA', NDOFA, NTERM_PA, SUBR_NAME)

         IF (NTERM_PA > 0) THEN                            ! Create new sparse arrays from PA_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'PA_FULL   ', NDOFA, NSUB , PA_FULL,  NTERM_PA,  SYM_PA,  I_PA,  J_PA,  PA  )
            CALL DEALLOCATE_FULL_MAT ( 'PA_FULL' )      
         ENDIF

      ELSE

         WRITE(ERR,911) SUBR_NAME,MATSPARS
         WRITE(F06,911) SUBR_NAME,MATSPARS
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

! If there are applied loads on the O-set, solve for UO0 = KOO(-1) x PO which is part of the final solution for UO 

      IF (NTERM_PO > 0) THEN
         CALL SOLVE_UO0
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
 
      END SUBROUTINE REDUCE_PF_TO_PA

