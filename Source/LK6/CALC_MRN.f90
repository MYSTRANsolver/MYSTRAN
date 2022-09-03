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

      SUBROUTINE CALC_MRN  
 
! Calculates the R-set row by NVEC (number of eigenvectors) col matrix MRN   in the CB transformation matrix:

!                        MRN   = (MRL + DLR'*MLL)*EIGEN_VEC ....... (NOTE: EIGEN_VEC is L-set rows by N=NVEC cols)
 
! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR,                                                                  &
                                         NDOFL, NDOFR, NTERM_DLR, NTERM_MLL, NTERM_MLLn, NTERM_MPF0, NTERM_MRL, NTERM_MRN,         &
                                         NUM_MLL_DIAG_ZEROS, NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  SPARSTOR
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VEC
      USE SPARSE_MATRICES , ONLY      :  SYM_DLR, SYM_MLL, SYM_MLLn, SYM_MRL, SYM_MRL, SYM_MRN  

      USE SPARSE_MATRICES , ONLY      :  I_MLL , J_MLL , MLL , I_MLLn, J_MLLn, MLLn, I_MRL , J_MRL , MRL ,                         &
                                         I_DLR , J_DLR , DLR , I_DLRt, J_DLRt, DLRt, I_MRN, J_MRN, MRN,                            &
                                         I_MPF0, J_MPF0, MPF0  
                                         
      USE SCRATCH_MATRICES, ONLY      :  I_CCS1, J_CCS1, CCS1, I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CRS3, J_CRS3, CRS3
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_MRN_BEGEND

      USE CALC_MRN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CALC_MRN  '

      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3          ! Number of terms in matrix CRS3  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_MRN_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calc MRN = (MRL + DLR'*MLL)*EIGEN_VEC

      NTERM_MRN   = 0                                      ! First, allocate MRN until we get other terms later
      CALL ALLOCATE_SPARSE_MAT ( 'MRN', NDOFR, NTERM_MRN, SUBR_NAME )

      IF (NTERM_MLL > 0) THEN                              ! Part I of MRRcb: calc DLR'*MLL
                                            
         IF      (SPARSTOR == 'SYM   ') THEN               ! I-a: Convert MLL to nonsym MLLn, if required. Need for MATMULT_SSS

            CALL SPARSE_MAT_DIAG_ZEROS ( 'MLL', NDOFL, NTERM_MLL, I_MLL, J_MLL, NUM_MLL_DIAG_ZEROS )
            NTERM_MLLn = 2*NTERM_MLL - (NDOFL - NUM_MLL_DIAG_ZEROS)

            CALL ALLOCATE_SPARSE_MAT ( 'MLLn', NDOFL, NTERM_MLLn, SUBR_NAME )

            CALL CRS_SYM_TO_CRS_NONSYM ( 'MLL', NDOFL, NTERM_MLL, I_MLL, J_MLL, MLL, 'MLLn', NTERM_MLLn, I_MLLn, J_MLLn, MLLn, 'Y' )
                                                           ! I-a-1: CCS1 will be sparse CCS format version of sparse CRS matrix MLLn
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFL, NTERM_MLLn, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFL, NDOFL, NTERM_MLLn, 'MLLn', I_MLLn, J_MLLn, MLLn, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )
                                                           ! I-a-2: Mult DLRt*CCS1 where CCs1 is MLLn
            CALL MATMULT_SSS_NTERM ( 'DLRt', NDOFR, NTERM_DLR , SYM_DLR , I_DLRt, J_DLRt,                                          &
                                     'MLLn', NDOFL, NTERM_MLLn, SYM_MLLn, J_CCS1, I_CCS1, AROW_MAX_TERMS,                          &
                                     'CRS1' ,       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'DLRt', NDOFR, NTERM_DLR , SYM_DLR , I_DLRt, J_DLRt, DLRt,                                          &
                               'MLLn', NDOFL, NTERM_MLLn, SYM_MLLn, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                          &
                               'CRS1', ONE  , NTERM_CRS1,           I_CRS1, J_CRS1, CRS1 )

         ELSE IF (SPARSTOR == 'NONSYM') THEN               ! I-b: Use MLL as is since it is nonsym (needed for MATMULT_SSS)

            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFL, NTERM_MLL, SUBR_NAME )
                                                           ! I-b-1: CCS1 will be sparse CCS format version of sparse CRS matrix MLL
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFL, NDOFL, NTERM_MLL, 'MLL', I_MLL, J_MLL, MLL, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )
                                                           ! I-b-2: Mult DLRt*CCS1 where CCs1 is MLL
            CALL MATMULT_SSS_NTERM ( 'DLRt', NDOFR, NTERM_DLR , SYM_DLR, I_DLRt, J_DLRt,                                           &
                                     'MLL' , NDOFL, NTERM_MLL , SYM_MLL, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1' ,       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'DLRt', NDOFR, NTERM_DLR, SYM_DLR, I_DLRt, J_DLRt, DLRt,                                            &
                               'MLL' , NDOFL, NTERM_MLL, SYM_MLL, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                            &
                               'CRS1', ONE  , NTERM_CRS1        , I_CRS1, J_CRS1, CRS1 )

         ELSE

            WRITE(ERR,932) SUBR_NAME, SPARSTOR
            WRITE(F06,932) SUBR_NAME, SPARSTOR
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )

         ENDIF

         IF (DEBUG(103) > 0) THEN                          ! Algorithm for calculating MPF's will not use MRL (or MLR) 

            NTERM_CRS2 = NTERM_CRS1                        ! Store CRS1 = DLRt*MLL in CRS2 for use below in calculating MPF0
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFR, NTERM_CRS2, SUBR_NAME )

            DO I=1,NDOFR+1
               I_CRS2 (I) = I_CRS1(I)
            ENDDO
            DO J=1,NTERM_CRS2 
               J_CRS2 (J) = J_CRS1(J)
                 CRS2 (J) =   CRS1(J)
            ENDDO


         ENDIF

      ELSE

         NTERM_CRS1 = 0
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

      ENDIF

      CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

      IF (NTERM_MRL > 0) THEN                              ! Part II of MRN  : add MRL to CRS1 to get MRL + DLR'*MLL

         CALL MATADD_SSS_NTERM ( NDOFR, 'MRL', NTERM_MRL, I_MRL, J_MRL, SYM_MRL, 'DLRt*MLL', NTERM_CRS1, I_CRS1, J_CRS1, 'N',      &
                                       'CRS3', NTERM_CRS3 )
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFR, NTERM_CRS3, SUBR_NAME )
         CALL MATADD_SSS ( NDOFR, 'MRL', NTERM_MRL, I_MRL, J_MRL, MRL, ONE, 'DLRt*MLL', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE,     &
                                  'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

         CALL DEALLOCATE_SCR_MAT ( 'CRS1' )                ! II-3 , deallocate CRS1 and then reallocate it
         NTERM_CRS1 = NTERM_CRS3
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

         DO I=1,NDOFR+1                                    ! Now CRS1 = MRL + DLR'*MLL
            I_CRS1(I) = I_CRS3(I)
         ENDDO
         DO J=1,NTERM_CRS1
            J_CRS1(J) = J_CRS3(J)
              CRS1(J) =   CRS3(J)
         ENDDO

         CALL DEALLOCATE_SCR_MAT ( 'CRS3' )

      ENDIF

      IF (( NTERM_MLL > 0) .OR. (NTERM_MRL > 0)) THEN      ! Multiply CRS1 = MRL + DLR'*MLL  times EIGEN_VEC to get MRN

         CALL MATMULT_SFS_NTERM ('MRL + DLRt*MLL', NDOFR, NTERM_CRS1, 'N', I_CRS1, J_CRS1, 'EIGEN_VEC', NDOFL, NVEC, EIGEN_VEC     &
                                , AROW_MAX_TERMS, 'MRN  ', NTERM_MRN   )
         CALL DEALLOCATE_SPARSE_MAT ( 'MRN' )
         CALL ALLOCATE_SPARSE_MAT ( 'MRN', NDOFR, NTERM_MRN, SUBR_NAME )
         CALL MATMULT_SFS ('MRL + DLRt*MLL', NDOFR, NTERM_CRS1, 'N', I_CRS1, J_CRS1, CRS1, 'EIGEN_VEC', NDOFL, NVEC, EIGEN_VEC,    &
                            AROW_MAX_TERMS, 'MRN', ONE, NTERM_MRN, I_MRN, J_MRN, MRN  )
         NTERM_MPF0 = NTERM_MRN
 
         IF (DEBUG(103) == 0) THEN                         ! Include MRL (or MLR) in MPF (modal participation factor)  calculation

            NTERM_MPF0 = NTERM_MRN                         ! MPF0 = MRN = (MRL + DLR'*MLL)*EIGEN_VEC
            CALL ALLOCATE_SPARSE_MAT ( 'MPF0', NDOFR, NTERM_MPF0, SUBR_NAME )
            DO I=1,NDOFR+1
               I_MPF0(I) = I_MRN(I)
            ENDDO
            DO J=1,NTERM_MPF0
               J_MPF0(J) = J_MRN(J)
                 MPF0(J) =   MRN(J)
            ENDDO

         ELSE                                              ! Do not include MRL (or MLR) in MPF calculation

            NTERM_MPF0 = NTERM_CRS2                        ! MPF0 = DLR'*MLL*EIGEN_VEC if MRL not included
            CALL ALLOCATE_SPARSE_MAT ( 'MPF0', NDOFR, NTERM_MPF0, SUBR_NAME )
            CALL MATMULT_SFS ('DLRt*MLL', NDOFR, NTERM_CRS2, 'N', I_CRS2, J_CRS2, CRS2, 'EIGEN_VEC', NDOFL, NVEC, EIGEN_VEC,       &
                               AROW_MAX_TERMS, 'MPF0', ONE, NTERM_MPF0, I_MPF0, J_MPF0, MPF0  )
!xx         DO I=1,NDOFR+1
!xx            I_MPF0(I) = I_CRS2(I)
!xx         ENDDO
!xx         DO J=1,NTERM_CRS2
!xx            J_MPF0(J) = J_CRS2(J)
!xx              MPF0(J) =   CRS2(J)
!xx         ENDDO
            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )

         ENDIF

      ELSE                                                 ! MLL, MRL are null so MRN   is null also

         NTERM_MRN   = 0
         CALL ALLOCATE_SPARSE_MAT ( 'MRN  ', NDOFR, NTERM_MRN  , SUBR_NAME )

      ENDIF

      CALL DEALLOCATE_SCR_MAT ( 'CRS1' )


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

 97531 format(' J, J_CCS1(J)          = ',2i8)

 97532 format(' I, I_CCS1(I), CCS1(I) = ', 2i8,1es14.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE CALC_MRN  