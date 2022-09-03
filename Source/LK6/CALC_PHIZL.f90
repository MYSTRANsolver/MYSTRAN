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

      SUBROUTINE CALC_PHIZL
 
! Merges matrices to get L-set displ transfer matrix:

!            PHIZL    = [ PHIZL1  |  PHIZL2  |  DLR ]
! where
!            PHIZL1   = -KLL(-1)*(MLR + MLL*DLR)
! and
!            PHIZL2   = -EIGEN_VEC*diag(EIGEN_VAL)

! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFL, NDOFR,                                                    &
                                         NTERM_DLR, NTERM_PHIZL, NTERM_PHIZL1, NTERM_PHIZL2 , NTERM_MLL, NTERM_MLR, NTERM_MRL,     &
                                         NUM_CB_DOFS, NVEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTPHIZL, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VEC, EIGEN_VAL
      USE SPARSE_MATRICES, ONLY       :  SYM_MLL, SYM_MLR, SYM_MRL, SYM_KLL, SYM_DLR, SYM_PHIZL  , SYM_PHIZL1  , SYM_PHIZL2  

      USE SPARSE_MATRICES, ONLY       :  I_MLL   , J_MLL   , MLL   , I_MLR   , J_MLR   , MLR   , I_MRL   , J_MRL   , MRL,          &
                                         I_KLL   , J_KLL   , KLL   , I_DLR   , J_DLR   , DLR   ,                                   &
                                         I_PHIZL , J_PHIZL , PHIZL , I_PHIZL1, J_PHIZL1, PHIZL1, I_PHIZL2, J_PHIZL2, PHIZL2

      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CRS3, J_CRS3, CRS3, I_CCS1, J_CCS1, CCS1 

      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_PHIZL_BEGEND

      USE CALC_PHIZL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CALC_PHIZL'

      INTEGER(LONG)                   :: AROW_MAX_TERMS    ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NTERM_CRS1        ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2        ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3        ! Number of terms in matrix CRS3  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_PHIZL_BEGEND

      REAL(DOUBLE)                    :: DUM1(NDOFL,NVEC)  ! Intermediate matrix
      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Part 1: Calculate PHIZL1 = -KLL(-1)*(MLR + MLL*DLR). Use CRS3 to hold (MLR + MLL*DLR)
! -------------------------------------------------------------------------------------

      NTERM_MLR = NTERM_MRL
      CALL ALLOCATE_SPARSE_MAT ( 'MLR', NDOFL, NTERM_MLR, SUBR_NAME )
      IF (NTERM_MLR > 0) THEN                              ! Transpose MRL to get MLR

         CALL MATTRNSP_SS ( NDOFR, NDOFL, NTERM_MRL, 'MRL', I_MRL, J_MRL, MRL, 'MLR', I_MLR, J_MLR, MLR )

      ENDIF

      IF (NTERM_MLL > 0) THEN                              ! There will be term MLL*DLR in CRS3
                                                           ! Put DLR into CCS format in array CCS1
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFR, NTERM_DLR, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS ( NDOFL, NDOFR, NTERM_DLR, 'DLR', I_DLR, J_DLR, DLR, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

                                                           ! Sparse multiply to get CRS1 = MLL*DLR = MLL*CCS1
!                                                            (note: use SYM_DLR for sym indicator for CCS1)
         CALL MATMULT_SSS_NTERM ( 'MLL' , NDOFL, NTERM_MLL , SYM_MLL, I_MLL, J_MLL,                                                &
                                  'DLR' , NDOFR, NTERM_DLR , SYM_DLR, J_CCS1, I_CCS1, AROW_MAX_TERMS,                              &
                                  'CRS1',        NTERM_CRS1 )

         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFL, NTERM_CRS1, SUBR_NAME )

         CALL MATMULT_SSS ( 'MLL' , NDOFL, NTERM_MLL , SYM_MLL, I_MLL , J_MLL , MLL,                                               &
                            'DLR' , NDOFR, NTERM_DLR , SYM_DLR, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                              &
                            'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

         CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

         IF (NTERM_MLR > 0) THEN                           ! Sparse add to get CRS3 = MLR + MLL*DLR = MLR + CRS1 

            CALL MATADD_SSS_NTERM ( NDOFL, 'MLR', NTERM_MLR, I_MLR, J_MLR, SYM_MLR, 'MLL*DLR', NTERM_CRS1, I_CRS1, J_CRS1, 'N',    &
                                   'CRS3', NTERM_CRS3 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFL, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFL, 'MLR', NTERM_MLR, I_MLR, J_MLR, MLR, ONE, 'MLL*DLR', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE,   &
                             'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

         ELSE                                              ! MLL > 0 but MLR = 0 so set CRS3 to CRS1 (which is MLL*DLR)

            NTERM_CRS3 = NTERM_CRS1
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFL, NTERM_CRS3, SUBR_NAME )
            DO I=1,NDOFL+1
               I_CRS3(I) = I_CRS1(I)
            ENDDO
            DO I=1,NTERM_CRS3
               J_CRS3(I) = J_CRS1(I)
                 CRS3(I) =   CRS1(I)
            ENDDO

         ENDIF

         CALL DEALLOCATE_SCR_MAT ( 'CRS1' )

      ELSE                                                 ! MLL = 0 so check MLR

         IF (NTERM_MLR > 0) THEN                           ! CRS3 = MLR

            NTERM_CRS3 = NTERM_MLR
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFL, NTERM_CRS3, SUBR_NAME )
            DO I=1,NDOFL+1
               I_CRS3(I) = I_MLR(I)
            ENDDO
            DO I=1,NTERM_CRS3
               J_CRS3(I) = J_MLR(I)
                 CRS3(I) =   MLR(I)
            ENDDO

         ELSE                                              ! CRS3 = 0 so PHIZL1 = 0

            NTERM_CRS3 = 0

         ENDIF

      ENDIF

! Now solve for PHIZL1

      IF (NTERM_CRS3 > 0) THEN                             ! CRS3 = MLR + MLL*DLR > 0 so solve KLL*PHIZL1 = CRS3 for PHIZL1  
         CALL SOLVE_PHIZL1 ( NTERM_CRS3 )
      ELSE
         NTERM_PHIZL1 = 0
      ENDIF
!xx   CALL ALLOCATE_SPARSE_MAT ( 'PHIZL1', NDOFL, NTERM_PHIZL1, SUBR_NAME )

      CALL DEALLOCATE_SCR_MAT ( 'CRS3' )

! Part 2: Calculate PHIZL2 = -EIGEN_VEC*diag(EIGEN_VAL)
! -----------------------------------------------------

      DO I=1,NDOFL                                         ! 1st calc full matrix with PHIZL2 terms
         DO J=1,NVEC
            DUM1(I,J) = -EIGEN_VEC(I,J)/EIGEN_VAL(J)
         ENDDO
      ENDDO
                                                           ! Now convert to sparse format
      CALL CNT_NONZ_IN_FULL_MAT ( 'PHIZL2', DUM1, NDOFL, NVEC, 'N', NTERM_PHIZL2, SMALL )
      CALL ALLOCATE_SPARSE_MAT ( 'PHIZL2', NDOFL, NTERM_PHIZL2, SUBR_NAME )
      CALL FULL_TO_SPARSE_CRS ( '-EIGEN_VEC*diag(EIGEN_VAL)', NDOFL, NVEC, DUM1, NTERM_PHIZL2, SMALL, SUBR_NAME, 'N',              &
                                 I_PHIZL2, J_PHIZL2, PHIZL2 )

! Part 3: Merge PHIZL1 and PHIZL2 into CRS2
! -------------------------------------

      NTERM_CRS2 = NTERM_PHIZL1 + NTERM_PHIZL2  

      CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFL, NTERM_CRS2, SUBR_NAME )

      CALL MERGE_MAT_COLS_SSS   ( 'PHIZL1', NTERM_PHIZL1, I_PHIZL1, J_PHIZL1, PHIZL1, SYM_PHIZL1, NDOFR,                           &
                                  'PHIZL2', NTERM_PHIZL2, I_PHIZL2, J_PHIZL2, PHIZL2, SYM_PHIZL2, NDOFL,                           &
                                  'CRS2',             I_CRS2, J_CRS2, CRS2, 'N'        )

! Part 4: Merge CRS2 (= PHIZL1, PHIZL2)  with DLR to get final PHIZL  
! ------------------------------------------------------------

      NTERM_PHIZL = NTERM_CRS2 + NTERM_DLR

      CALL ALLOCATE_SPARSE_MAT ( 'PHIZL', NDOFL, NTERM_PHIZL, SUBR_NAME )

      CALL MERGE_MAT_COLS_SSS  ( 'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2, 'N'     , NDOFR+NVEC,                                   &
                                 'DLR' , NTERM_DLR , I_DLR , J_DLR , DLR , SYM_DLR , NDOFL,                                        &
                                 'PHIZL' ,             I_PHIZL , J_PHIZL , PHIZL , 'N'        )

      CALL DEALLOCATE_SCR_MAT ( 'CRS2' )

      IF (PRTPHIZL > 0) THEN
         CALL WRITE_SPARSE_CRS ( 'PHIZL','  ','  ', NTERM_PHIZL, NDOFL, I_PHIZL, J_PHIZL, PHIZL )
      ENDIF

! Part 5: Deallocate PHIZL1 and PHIZL2  

      CALL DEALLOCATE_SPARSE_MAT ( 'PHIZL1' )
      CALL DEALLOCATE_SPARSE_MAT ( 'PHIZL2' )

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


! **********************************************************************************************************************************

      END SUBROUTINE CALC_PHIZL