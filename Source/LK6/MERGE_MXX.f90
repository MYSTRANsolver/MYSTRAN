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

      SUBROUTINE MERGE_MXX  
 
! Merges matrices to get CB stifness matrix:

!                    | MRRcb  MRN   |        MRRcb = MRR + MRL*DLR + (MRL*DLR)' + DLR'*MLL*DLR
!            MXX   = |              |        MRN   = (MRL + DLR'*MLL)*EIGEN_VEC
!                    |  sym    Mee  |        Mee   = generalized MASSES for the eigenvectors of the L-set

 
! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NVEC, NTERM_MRRcb, NTERM_MRRcbn, NTERM_MRN, NTERM_MXX,    &
                                         NTERM_MXXn
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  PRTMXX, SPARSTOR
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS
      USE SPARSE_MATRICES, ONLY       :  SYM_MRRcbn, SYM_MRN  , SYM_MXX  , SYM_MXXn
      USE SPARSE_MATRICES, ONLY       :  I_MRRcb, J_MRRcb, MRRcb, I_MRRcbn, J_MRRcbn, MRRcbn, I_MRN  , J_MRN  , MRN  ,             &
                                         I_MXX  , J_MXX  , MXX  , I_MXXn  , J_MXXn  , MXXn
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_MXX_BEGEND

      USE MERGE_MXX_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_MXX  '

      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: I_GEN_MASS2(NVEC+1)! 
      INTEGER(LONG)                   :: I_MNR(NVEC+1)      ! 
      INTEGER(LONG)                   :: I_MXXa(NDOFR+1)    ! Upper NDOFR rows of MXX
      INTEGER(LONG)                   :: I_MXXb(NVEC+1)     ! Lower NVEC  rows of MXX
      INTEGER(LONG)                   :: J_GEN_MASS2(NVEC)  ! 
      INTEGER(LONG)                   :: J_MNR(NTERM_MRN)   !
      INTEGER(LONG)                   :: J_MXXa(NTERM_MRRcbn+NTERM_MRN)
      INTEGER(LONG)                   :: J_MXXb(NTERM_MRN+NVEC)
!xx   INTEGER(LONG)                   :: K1,K2              ! Counters
      INTEGER(LONG)                   :: MXXn_MERGE_VEC(NDOFR+NVEC) 
      INTEGER(LONG)                   :: NTERM_MNR          ! 
      INTEGER(LONG)                   :: NTERM_MXXa         ! 
      INTEGER(LONG)                   :: NTERM_MXXb         ! 
!xx   INTEGER(LONG)                   :: NUM_MNR_IN_ROW_I   ! Number of terms in row i of MNR
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_MXX_BEGEND

      REAL(DOUBLE)                    :: GEN_MASS2(NVEC)
      REAL(DOUBLE)                    :: MNR(NTERM_MRN)
      REAL(DOUBLE)                    :: MXXa(NTERM_MRRcbn+NTERM_MRN)
      REAL(DOUBLE)                    :: MXXb(NTERM_MRN+NVEC)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NTERM_MNR  = NTERM_MRN            
      NTERM_MXXa = NTERM_MRRcbn + NTERM_MRN
      NTERM_MXXb = NTERM_MNR + NVEC       

! NTERM_MRRcbn was set in subr CALC_MRRcb

      CALL ALLOCATE_SPARSE_MAT ( 'MRRcbn', NDOFR, NTERM_MRRcbn, SUBR_NAME )

      IF      (SPARSTOR == 'SYM   ') THEN                  ! Convert MRRcb (stored symmetric) to MRRcbn (stored nonsymmetric)

         CALL CRS_SYM_TO_CRS_NONSYM ( 'MRRcb' , NDOFR, NTERM_MRRcb , I_MRRcb , J_MRRcb , MRRcb,                                    &
                                      'MRRcbn',        NTERM_MRRcbn, I_MRRcbn, J_MRRcbn, MRRcbn, 'Y' )
      ELSE IF (SPARSTOR == 'NONSYM') THEN

         DO I=1,NDOFR+1
            I_MRRcbn(I) = I_MRRcb(I)
         ENDDO
         DO J=1,NTERM_MRRcbn
            J_MRRcbn(J) = J_MRRcb(J)
              MRRcbn(J) =   MRRcb(J)
         ENDDO

      ELSE

         WRITE(ERR,932) SUBR_NAME, SPARSTOR
         WRITE(F06,932) SUBR_NAME, SPARSTOR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

! Allocate enough memory for complete MXXn matrix (nonsym format of MXX  )

      NTERM_MXXn = NTERM_MRRcbn + 2*NTERM_MRN + NVEC
      CALL ALLOCATE_SPARSE_MAT ( 'MXXn', NDOFR+NVEC, NTERM_MXXn, SUBR_NAME )

! Merge MXXa <-- MRRcbn and MRN (both in nonsym format) into temporary MXXa

      CALL MERGE_MAT_COLS_SSS ( 'MRRcbn' , NTERM_MRRcbn, I_MRRcbn, J_MRRcbn, MRRcbn, SYM_MRRcbn, NDOFR,                            &
                                'MRN  '  , NTERM_MRN   , I_MRN   , J_MRN   , MRN   , SYM_MRN   , NDOFR,                            &
                                'MXXa'                 , I_MXXa  , J_MXXa  , MXXa  , 'N' )

! Transpose MRN to MNR

      CALL MATTRNSP_SS ( NDOFR, NVEC, NTERM_MRN, 'MRN', I_MRN, J_MRN, MRN, 'MNR', I_MNR, J_MNR, MNR )

! Put gen mass data into sparse array GEN_MASS2:

      DO I=1,NVEC
         I_GEN_MASS2(I) = I
         J_GEN_MASS2(I) = I
           GEN_MASS2(I) = GEN_MASS(I)
      ENDDO
      I_GEN_MASS2(NVEC+1) = NVEC+1

! Merge MXXb <-- MNR and GEN_MASS

!xx    I_MXXb(1) = I_MNR(1)
!xx    DO I=1,NVEC
!xx       I_MXXb(I+1) = I_MNR(I+1) + I
!xx    ENDDO

!xx    K1 = 0
!xx    K2 = 0
!xx    DO I=1,NVEC
!xx       NUM_MNR_IN_ROW_I = I_MNR(I+1) - I_MNR(I)
!xx       DO J=1,NUM_MNR_IN_ROW_I
!xx          K1 = K1 + 1
!xx          K2 = K2 + 1
!xx          J_MXXb(K2) = J_MNR(K1)
!xx            MXXb(K2) =   MNR(K1)
!xx       ENDDO
!xx       K2 = K2 + 1
!xx       J_MXXb(K2) = J_MNR(K1) + I
!xx         MXXb(K2) = GEN_MASS(I)
!xx    ENDDO

      CALL MERGE_MAT_COLS_SSS ( 'MNR'     , NTERM_MNR, I_MNR      , J_MNR      , MNR      , 'N', NDOFR,                           &
                                'GEN_MASS', NVEC     , I_GEN_MASS2, J_GEN_MASS2, GEN_MASS2, 'N', NVEC,                            &
                                'MXXb'               , I_MXXb     , J_MXXb     , MXXb     , 'N' )

! Merge MXXa rows with rows of MXXb into MXXn

      DO I=1,NDOFR
         MXXn_MERGE_VEC(I) = 1
      ENDDO
      DO I=NDOFR+1,NDOFR+NVEC
         MXXn_MERGE_VEC(I) = 2
      ENDDO

      CALL MERGE_MAT_ROWS_SSS ( 'MXXa', NDOFR, NTERM_MXXa, I_MXXa, J_MXXa, MXXa, 1,                                                &
                                'MXXb', NVEC , NTERM_MXXb, I_MXXb, J_MXXb, MXXb, 2, MXXn_MERGE_VEC,                                &
                                'MXXn'                   , I_MXXn, J_MXXn, MXXn )

! Convert MXXn to symmetric format MXX  

      IF (SPARSTOR == 'SYM   ') THEN

         NTERM_MXX = NTERM_MRRcb + NTERM_MRN + NVEC
         CALL ALLOCATE_SPARSE_MAT   ( 'MXX' , NDOFR+NVEC, NTERM_MXX , SUBR_NAME )
         CALL CRS_NONSYM_TO_CRS_SYM ( 'MXXn', NDOFR+NVEC, NTERM_MXXn, I_MXXn, J_MXXn, MXXn,                                        &
                                      'MXX' ,             NTERM_MXX , I_MXX , J_MXX , MXX   )

      ELSE

         NTERM_MXX   = NTERM_MXXn                          ! If SPARSTOR is nonsym, then MXX is also stored nonsym
         CALL ALLOCATE_SPARSE_MAT ( 'MXX', NDOFR+NVEC, NTERM_MXX  , SUBR_NAME )
         DO I=1,NDOFR+NVEC+1
            I_MXX(I) = I_MXXn(I)
         ENDDO
         DO J=1,NTERM_MXX  
            J_MXX(J) = J_MXXn(J)
              MXX(J) =   MXXn(J)
         ENDDO

      ENDIF

      CALL DEALLOCATE_SPARSE_MAT ( 'MXXn' )

      IF (PRTMXX > 0) THEN
         CALL WRITE_SPARSE_CRS ( 'MXX  ','  ','  ', NTERM_MXX  , NDOFR+NVEC, I_MXX  , J_MXX  , MXX   )
      ENDIF

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
 
      END SUBROUTINE MERGE_MXX
