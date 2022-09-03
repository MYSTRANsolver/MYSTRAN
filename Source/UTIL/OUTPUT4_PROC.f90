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
 
      SUBROUTINE OUTPUT4_PROC ( CALLING_SUBR )
 
! Checks whether a matrix is requested for OUTPUT4 and, if so:

!   - calls OU4_PARTVEC_PROC to calculate the row/col partitioning vectors (OU4_PARTVEC_ROW, OU4_PARTVEC_COL)
!   - calls PARTITION_SS to do the actual partitioning
!   - calls WRITE_OU4_SPARSE_MAT or WRITE_OU4_FULL_MAT to write the matrix to an unformatted disk file

! This subr does not process the grid and/or element related Output Transformation Matrices (OTM's). That is done in LINK9  

      USE PENTIUM_II_KIND, ONLY        :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                 :  ERR, F04, F06, MOU4, OU4, OU4_MSG, OU4FIL, WRT_LOG

      USE SCONTR, ONLY                 :  BLNK_SUB_NAM, FATAL_ERR   ,                                                              &
                                          NTERM_CG_LTM, NTERM_DLR   , NTERM_IF_LTM, NTERM_KLL   ,                                  &
                                          NTERM_KRL   , NTERM_KRR   , NTERM_KRRcb , NTERM_KXX   ,                                  &
                                          NTERM_LTM   ,                                                                            &
                                          NTERM_MLL   , NTERM_MRL   , NTERM_MRN   , NTERM_MRR   ,                                  &
                                          NTERM_MRRcb , NTERM_MXX   , NTERM_PHIXG , NTERM_PHIZL ,                                  &
                                          NTERM_KAA, NTERM_MAA, NTERM_KGG, NTERM_MGG, NTERM_PA, NTERM_PG, NTERM_PL, SOL_NAME

      USE SCONTR, ONLY                 :  NTERM_KAA

      USE MODEL_STUF, ONLY             :  MCG
      USE PARAMS, ONLY                 :  MPFOUT

      USE EIGEN_MATRICES_1, ONLY       :  EIGEN_VAL, EIGEN_VEC, GEN_MASS,  MEFFMASS,  MPFACTOR_N6,  MPFACTOR_NR 

      USE FULL_MATRICES, ONLY          :  DUM1, PHIZG_FULL

      USE MODEL_STUF, ONLY             :  MCG

      USE RIGID_BODY_DISP_MATS, ONLY   :  TR6_0, TR6_CG

      USE SPARSE_MATRICES, ONLY        :  I_CG_LTM  ,I_DLR     ,I_IF_LTM  ,I_KAA     ,I_KGG     ,I_KLL     ,I_KRL     ,I_KRR     , &
                                          I_KRRcb   ,I_KXX     ,I_LTM     ,I_MAA     ,I_MGG     ,I_MLL     ,I_MRL     ,I_MRN     , &
                                          I_MRR     ,I_MRRcb   ,I_MXX     ,I_PA      ,I_PG      ,I_PL      ,I_PHIXG

      USE SPARSE_MATRICES, ONLY        :  J_CG_LTM  ,J_DLR     ,J_IF_LTM  ,J_KAA     ,J_KGG     ,J_KLL     ,J_KRL     ,J_KRR     , &
                                          J_KRRcb   ,J_KXX     ,J_LTM     ,J_MAA     ,J_MGG     ,J_MLL     ,J_MRL     ,J_MRN     , &
                                          J_MRR     ,J_MRRcb   ,J_MXX     ,J_PA      ,J_PG      ,J_PL      ,J_PHIXG

      USE SPARSE_MATRICES, ONLY        :  CG_LTM    ,DLR       ,IF_LTM    ,KAA       ,KGG       ,KLL       ,KRL       ,KRR       , &
                                          KRRcb     ,KXX       ,LTM       ,MAA       ,MGG       ,MLL       ,MRL       ,MRN       , &
                                          MRR       ,MRRcb     ,MXX       ,PA        ,PG        ,PL        ,PHIXG

      USE SPARSE_MATRICES, ONLY        :  SYM_CG_LTM,SYM_DLR   ,SYM_IF_LTM,SYM_KAA   ,SYM_KGG   ,SYM_KLL   ,SYM_KRL   ,SYM_KRR   , &
                                          SYM_KRRcb ,SYM_KXX   ,SYM_LTM   ,SYM_MAA   ,SYM_MGG   ,SYM_MLL   ,SYM_MRL   ,SYM_MRN   , &
                                          SYM_MRR   ,SYM_MRRcb ,SYM_MXX   ,SYM_PA    ,SYM_PG    ,SYM_PL    ,SYM_PHIXG

      USE SCRATCH_MATRICES, ONLY       :  I_CRS1, J_CRS1, CRS1

      USE OUTPUT4_MATRICES, ONLY       :  ACT_OU4_MYSTRAN_NAMES, HAS_OU4_MAT_BEEN_PROCESSED, NUM_OU4_REQUESTS, OU4_FILE_UNITS,     &
                                          OU4_PARTVEC_COL, OU4_PARTVEC_ROW,                                                        &
                                          OU4_PART_MAT_NAMES, OU4_PART_VEC_NAMES, RBM0, SUBR_WHEN_TO_WRITE_OU4_MATS

      USE TIMDAT, ONLY                 :  TSEC
      USE RIGID_BODY_DISP_MATS, ONLY   :  TR6_CG, TR6_0            
      USE EIGEN_MATRICES_1, ONLY       :  GEN_MASS, EIGEN_VAL, EIGEN_VEC, MEFFMASS, MPFACTOR_N6, MPFACTOR_NR

      USE SPARSE_MATRICES, ONLY        :  I_CG_LTM, J_CG_LTM, CG_LTM,      I_DLR   , J_DLR   , DLR   ,                             &
                                          I_IF_LTM, J_IF_LTM, IF_LTM,      I_KLL   , J_KLL   , KLL   ,                             &
                                          I_KRL   , J_KRL   , KRL   ,      I_KRR   , J_KRR   , KRR   ,                             &
                                          I_KRRcb , J_KRRcb , KRRcb ,      I_KXX   , J_KXX   , KXX   ,                             &
                                          I_LTM   , J_LTM   , LTM   ,                                                              &
                                          I_MLL   , J_MLL   , MLL   ,      I_MRL   , J_MRL   , MRL   ,                             &
                                          I_MRN   , J_MRN   , MRN   ,      I_MRR   , J_MRR   , MRR   ,                             &
                                          I_MRRcb , J_MRRcb , MRRcb ,      I_MXX   , J_MXX   , MXX   ,                             &
                                          I_PHIXG , J_PHIXG , PHIXG 

      USE SPARSE_MATRICES, ONLY        :  I_KAA, J_KAA, KAA, I_KGG, J_KGG, KGG, I_MAA, J_MAA, MAA, I_MGG, J_MGG, MGG,              &
                                          I_PA , J_PA , PA , I_PG , J_PG , PG , I_PL , J_PL , PL

      USE FULL_MATRICES, ONLY          :  PHIZG_FULL
      USE SUBR_BEGEND_LEVELS, ONLY     :  OUTPUT4_PROC_BEGEND
 
      USE OUTPUT4_PROC_USE_IFs

      IMPLICIT NONE
 

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'OUTPUT4_PROC'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this one
      CHARACTER( 1*BYTE)              :: CAN_PARTN         ! 'Y' if matrix can be partitioned (rows, cols or both)

      CHARACTER(LEN(ACT_OU4_MYSTRAN_NAMES))                                                                                        &
                                      :: MAT_NAME          ! Name of matrix requested for OUTPUT4 for this call to OUTPUT4_PROC

      CHARACTER(1*BYTE)               :: SM                ! 'Y' if OUTPUT4 matrix is stored symmetrically

      INTEGER(LONG)                   :: AROW_MAX_TERMS    ! Max number of terms in any row of partitioned matrix
      INTEGER(LONG)                   :: FORM              ! Format of OUTPUT4 matrix
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: NCOLS_F           ! Number of cols in the complete OUTPUT4 matrix 
      INTEGER(LONG)                   :: NROWS_F           ! Number of cols in the complete OUTPUT4 matrix 
      INTEGER(LONG)                   :: NCOLS_P           ! Number of cols in that will be in the OUTPUT4 matrix when partitioned
      INTEGER(LONG)                   :: NROWS_P           ! Number of cols in that will be in the OUTPUT4 matrix when partitioned
      INTEGER(LONG)                   :: NTERM_CRS1        ! Number of terms in partitioned sparse matrix CRS1
      INTEGER(LONG)                   :: VAL_C             ! Non-zero vals in OU4_PARTVEC_ROWS
      INTEGER(LONG)                   :: VAL_R             ! Non-zero vals in OU4_PARTVEC_COLS
      INTEGER(LONG)                   :: UNT               ! 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OUTPUT4_PROC_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,NUM_OU4_REQUESTS

         MAT_NAME = ACT_OU4_MYSTRAN_NAMES(I)
         UNT      = OU4_FILE_UNITS(I)

         IF      (MAT_NAME == 'CG_LTM          ') THEN     ! ( 1)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 1) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(CG_LTM)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'CG_LTM', NTERM_CG_LTM, NROWS_F, NCOLS_F, 'N', I_CG_LTM, J_CG_LTM,                &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'CG_LTM', NTERM_CG_LTM, NROWS_F, NCOLS_F, 'N', I_CG_LTM, J_CG_LTM, CG_LTM,        &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_CG_LTM, I_CG_LTM, J_CG_LTM, CG_LTM, UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'DLR             ') THEN     ! ( 2)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 2) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(DLR)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'L ', 'R ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'L ', 'R ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'DLR', NTERM_DLR, NROWS_F, NCOLS_F, 'N', I_DLR, J_DLR,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'DLR', NTERM_DLR, NROWS_F, NCOLS_F, 'N', I_DLR, J_DLR, DLR,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_DLR   , I_DLR   , J_DLR   , DLR   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'EIGEN_VAL       ') THEN     ! ( 3)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 3) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(EIGEN_VAL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'EIGEN_VAL', NROWS_F, NCOLS_F, EIGEN_VAL, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,             &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, EIGEN_VAL , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, EIGEN_VAL, UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'EIGEN_VEC       ') THEN     ! ( 4)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 4) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(EIGEN_VEC)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'G ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'G ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'EIGEN_VEC', NROWS_F, NCOLS_F, EIGEN_VEC, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,             &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, EIGEN_VEC , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, EIGEN_VEC, UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'GEN_MASS        ') THEN     ! ( 5)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 5) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(GEN_MASS)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'GEN_MASS', NROWS_F, NCOLS_F, GEN_MASS, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,                 &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, GEN_MASS , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, GEN_MASS , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'IF_LTM          ') THEN     ! ( 6)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 6) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(IF_LTM)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'IF_LTM', NTERM_IF_LTM, NROWS_F, NCOLS_F, 'N', I_IF_LTM, J_IF_LTM,                &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'IF_LTM', NTERM_IF_LTM, NROWS_F, NCOLS_F, 'N', I_IF_LTM, J_IF_LTM, IF_LTM,        &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_IF_LTM, I_IF_LTM, J_IF_LTM, IF_LTM, UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KAA             ') THEN     ! ( 7)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 7) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KAA)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'A ', 'A ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'A ', 'A ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KAA', NTERM_KAA, NROWS_F, NCOLS_F, 'N', I_KAA, J_KAA,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KAA', NTERM_KAA, NROWS_F, NCOLS_F, 'N', I_KAA, J_KAA, KAA,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KAA, I_KAA, J_KAA, KAA, UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KGG             ') THEN     ! ( 8)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 8) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KGG)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'G ', 'G ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'G ', 'G ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KGG', NTERM_KGG, NROWS_F, NCOLS_F, 'N', I_KGG, J_KGG,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KGG', NTERM_KGG, NROWS_F, NCOLS_F, 'N', I_KGG, J_KGG, KGG,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KGG   , I_KGG   , J_KGG   , KGG   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KLL             ') THEN     ! ( 9)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS( 9) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KLL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'L ', 'L ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'L ', 'L ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KLL', NTERM_KLL, NROWS_F, NCOLS_F, 'N', I_KLL, J_KLL,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KLL', NTERM_KLL, NROWS_F, NCOLS_F, 'N', I_KLL, J_KLL, KLL,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KLL   , I_KLL   , J_KLL   , KLL   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KRL             ') THEN     ! (10)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(10) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KRL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', 'L ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', 'L ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KRL', NTERM_KRL, NROWS_F, NCOLS_F, 'N', I_KRL, J_KRL,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KRL', NTERM_KRL, NROWS_F, NCOLS_F, 'N', I_KRL, J_KRL, KRL,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KRL   , I_KRL   , J_KRL   , KRL   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KRR             ') THEN     ! (11)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(11) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KRR)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', 'R ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', 'R ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KRR', NTERM_KRR, NROWS_F, NCOLS_F, 'N', I_KRR, J_KRR,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KRR', NTERM_KRR, NROWS_F, NCOLS_F, 'N', I_KRR, J_KRR, KRR,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KRR   , I_KRR   , J_KRR   , KRR   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KRRcb           ') THEN     ! (12)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(12) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KRRcb)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', 'R ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', 'R ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KRRcb', NTERM_KRRcb, NROWS_F, NCOLS_F, 'N', I_KRRcb, J_KRRcb,                    &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KRRcb', NTERM_KRRcb, NROWS_F, NCOLS_F, 'N', I_KRRcb, J_KRRcb, KRRcb,             &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KRRcb , I_KRRcb , J_KRRcb , KRRcb , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'KXX             ') THEN     ! (13)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(13) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(KXX)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'KXX', NTERM_KXX, NROWS_F, NCOLS_F, 'N', I_KXX, J_KXX,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'KXX', NTERM_KXX, NROWS_F, NCOLS_F, 'N', I_KXX, J_KXX, KXX,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_KXX   , I_KXX   , J_KXX   , KXX   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'LTM             ') THEN     ! (14)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(14) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(LTM)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'LTM', NTERM_LTM, NROWS_F, NCOLS_F, 'N', I_LTM, J_LTM,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'LTM', NTERM_LTM, NROWS_F, NCOLS_F, 'N', I_LTM, J_LTM, LTM,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_LTM   , I_LTM   , J_LTM   , LTM   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MCG             ') THEN     ! (15)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(15) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                  WRITE(ERR,101) MAT_NAME
                  WRITE(F06,101) MAT_NAME
               ENDIF
               NROWS_P = NROWS_F
               NCOLS_P = NCOLS_F
               CALL WRITE_OU4_FULL_MAT   ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, MCG      , UNT)
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
               HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
            ENDIF

         ELSE IF (MAT_NAME == 'MEFFMASS        ') THEN     ! (16)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(16) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MEFFMASS)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'MEFFMASS', NROWS_F, NCOLS_F, MEFFMASS, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,                 &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, MEFFMASS , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, MEFFMASS, UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MPFACTOR        ') THEN     ! (17)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(17) == CALLING_SUBR) THEN
               IF (MPFOUT == '6') THEN
                  CALL GET_OU4_MAT_STATS    ('MPFACTOR_N6', NROWS_F, NCOLS_F, FORM, SM )
                  IF (ALLOCATED(MPFACTOR_N6)) THEN
                     CAN_PARTN = 'N'
                     IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                        CALL OU4_PARTVEC_PROC (I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C)
                        CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                     ELSE
                        NROWS_P = NROWS_F
                        NCOLS_P = NCOLS_F
                     ENDIF
                     IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                        CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                        CALL PARTITION_FF ( 'MPFACTOR_N6', NROWS_F, NCOLS_F, MPFACTOR_N6, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,        &
                                             VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                        CALL WRITE_OU4_FULL_MAT ( 'MPFACTOR_N6', NROWS_P, NCOLS_P, FORM, SM, MPFACTOR_N6 , UNT)
                     ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                        CALL WRITE_OU4_FULL_MAT ( 'MPFACTOR_N6', NROWS_F, NCOLS_F, FORM, SM, MPFACTOR_N6, UNT)
                     ENDIF
                     HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
                  ELSE
                     IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                        WRITE(ERR,201) MAT_NAME, SOL_NAME
                        WRITE(F06,201) MAT_NAME, SOL_NAME
                     ENDIF
                     HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
                  ENDIF
               ELSE
                  CALL GET_OU4_MAT_STATS    ('MPFACTOR_NR', NROWS_F, NCOLS_F, FORM, SM )
                  IF (ALLOCATED(MPFACTOR_NR)) THEN
                     CAN_PARTN = 'N'
                     IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                        CALL OU4_PARTVEC_PROC (I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C)
                        CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                     ELSE
                        NROWS_P = NROWS_F
                        NCOLS_P = NCOLS_F
                     ENDIF
                     IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                        CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                        CALL PARTITION_FF ( 'MPFACTOR_NR', NROWS_F, NCOLS_F, MPFACTOR_NR, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,        &
                                             VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                        CALL WRITE_OU4_FULL_MAT ( 'MPFACTOR_NR', NROWS_P, NCOLS_P, FORM, SM, MPFACTOR_NR , UNT)
                     ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                        CALL WRITE_OU4_FULL_MAT ( 'MPFACTOR_NR', NROWS_F, NCOLS_F, FORM, SM, MPFACTOR_NR, UNT)
                     ENDIF
                     HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
                  ELSE
                     IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                        WRITE(ERR,201) MAT_NAME, SOL_NAME
                        WRITE(F06,201) MAT_NAME, SOL_NAME
                     ENDIF
                     HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
                  ENDIF
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MAA             ') THEN     ! (18)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(18) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MAA)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'A ', 'A ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'A ', 'A ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MAA', NTERM_MAA, NROWS_F, NCOLS_F, 'N', I_MAA, J_MAA,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MAA', NTERM_MAA, NROWS_F, NCOLS_F, 'N', I_MAA, J_MAA, MAA,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MAA   , I_MAA   , J_MAA   , MAA   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MGG             ') THEN     ! (19)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(19) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MGG)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'G ', 'G ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'G ', 'G ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MGG', NTERM_MGG, NROWS_F, NCOLS_F, 'N', I_MGG, J_MGG,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MGG', NTERM_MGG, NROWS_F, NCOLS_F, 'N', I_MGG, J_MGG, MGG,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MGG   , I_MGG   , J_MGG   , MGG   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MLL             ') THEN     ! (20)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(20) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MLL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'L ', 'L ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'L ', 'L ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MLL', NTERM_MLL, NROWS_F, NCOLS_F, 'N', I_MLL, J_MLL,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MLL', NTERM_MLL, NROWS_F, NCOLS_F, 'N', I_MLL, J_MLL, MLL,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MLL   , I_MLL   , J_MLL   , MLL   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MRL             ') THEN     ! (21)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(21) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MRL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', 'L ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', 'L ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MRL', NTERM_MRL, NROWS_F, NCOLS_F, 'N', I_MRL, J_MRL,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MRL', NTERM_MRL, NROWS_F, NCOLS_F, 'N', I_MRL, J_MRL, MRL,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MRL   , I_MRL   , J_MRL   , MRL   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MRN             ') THEN     ! (22)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(22) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MRN)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MRN', NTERM_MRN, NROWS_F, NCOLS_F, 'N', I_MRN, J_MRN,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MRN', NTERM_MRN, NROWS_F, NCOLS_F, 'N', I_MRN, J_MRN, MRN,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MRN   , I_MRN   , J_MRN   , MRN   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MRR             ') THEN     ! (23)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(23) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MRR)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', 'R ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', 'R ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MRR', NTERM_MRR, NROWS_F, NCOLS_F, 'N', I_MRR, J_MRR,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MRR', NTERM_MRR, NROWS_F, NCOLS_F, 'N', I_MRR, J_MRR, MRR,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MRR   , I_MRR   , J_MRR   , MRR   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MRRcb           ') THEN     ! (24)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(24) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MRRcb)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', 'R ', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', 'R ', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MRRcb', NTERM_MRRcb, NROWS_F, NCOLS_F, 'N', I_MRRcb, J_MRRcb,                    &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MRRcb', NTERM_MRRcb, NROWS_F, NCOLS_F, 'N', I_MRRcb, J_MRRcb, MRRcb,             &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MRRcb , I_MRRcb , J_MRRcb , MRRcb , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'MXX             ') THEN     ! (25)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(25) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(MXX)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, '--', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, '--', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'MXX', NTERM_MXX, NROWS_F, NCOLS_F, 'N', I_MXX, J_MXX,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'MXX', NTERM_MXX, NROWS_F, NCOLS_F, 'N', I_MXX, J_MXX, MXX,                       &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_MXX   , I_MXX   , J_MXX   , MXX   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'PA              ') THEN     ! (26)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(26) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(PA)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'A ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'A ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'PA', NTERM_PA, NROWS_F, NCOLS_F, 'N', I_PA, J_PA,                                &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'PA', NTERM_PA, NROWS_F, NCOLS_F, 'N', I_PA, J_PA, PA,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_PA    , I_PA    , J_PA    , PA    , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'PG              ') THEN     ! (27)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(27) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(PG)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'G ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'G ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'PG', NTERM_PG, NROWS_F, NCOLS_F, 'N', I_PG, J_PG,                                &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'PG', NTERM_PG, NROWS_F, NCOLS_F, 'N', I_PG, J_PG, PG,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_PG    , I_PG    , J_PG    , PG    , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'PL              ') THEN     ! (28)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(28) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(PL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'L ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'L ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'PL', NTERM_PL, NROWS_F, NCOLS_F, 'N', I_PL, J_PL,                                &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'PL', NTERM_PL, NROWS_F, NCOLS_F, 'N', I_PL, J_PL, PL,                            &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_PL    , I_PL    , J_PL    , PL    , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'PHIXG           ') THEN     ! (29)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(29) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(PHIXG)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'G ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'G ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL PARTITION_SS_NTERM   ( 'PHIXG', NTERM_PHIXG, NROWS_F, NCOLS_F, 'N', I_PHIXG, J_PHIXG,                    &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, 'N' )

                     CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS_P, NTERM_CRS1, SUBR_NAME )

                     CALL PARTITION_SS         ( 'PHIXG', NTERM_PHIXG, NROWS_F, NCOLS_F, 'N', I_PHIXG, J_PHIXG, PHIXG,             &
                                                  OU4_PARTVEC_ROW, OU4_PARTVEC_COL, VAL_R, VAL_C, AROW_MAX_TERMS,                  &
                                                 'CRS1', NTERM_CRS1, NROWS_P, 'N', &
                                                  I_CRS1, J_CRS1, CRS1 )

                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, NTERM_CRS1, I_CRS1, J_CRS1, CRS1, UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_SPARSE_MAT (MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, NTERM_PHIXG , I_PHIXG , J_PHIXG , PHIXG , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'PHIZG           ') THEN     ! (30)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(30) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(PHIZG_FULL)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'G ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'G ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'PHIZG_FULL', NROWS_F, NCOLS_F, PHIZG_FULL, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,             &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, PHIZG_FULL , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, PHIZG_FULL , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'RBM0            ') THEN     ! (31)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(31) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                  WRITE(ERR,101) MAT_NAME
                  WRITE(F06,101) MAT_NAME
               ENDIF
               NROWS_P = NROWS_F
               NCOLS_P = NCOLS_F
               CALL WRITE_OU4_FULL_MAT   ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, RBM0     , UNT)
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
               HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
            ENDIF

         ELSE IF (MAT_NAME == 'TR6_0           ') THEN     ! (32)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(32) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(TR6_0)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'TR6_0', NROWS_F, NCOLS_F, TR6_0, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,                       &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, TR6_0 , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, TR6_0    , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE IF (MAT_NAME == 'TR6_CG          ') THEN     ! (33)
            IF (SUBR_WHEN_TO_WRITE_OU4_MATS(33) == CALLING_SUBR) THEN
               CALL GET_OU4_MAT_STATS    ( MAT_NAME    , NROWS_F, NCOLS_F, FORM, SM )
               IF (ALLOCATED(TR6_CG)) THEN
                  CAN_PARTN = 'N'
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     CALL OU4_PARTVEC_PROC ( I, MAT_NAME, NROWS_F, NCOLS_F, 'R ', '--', CAN_PARTN, NROWS_P, NCOLS_P, VAL_R, VAL_C )
                     CALL WRITE_PARTNd_MAT_HDRS ( MAT_NAME, 'R ', '--', NROWS_P, NCOLS_P )
                  ELSE
                     NROWS_P = NROWS_F
                     NCOLS_P = NCOLS_F
                  ENDIF
                  IF (CAN_PARTN == 'Y') THEN               ! Partition the matrix and write it out to the OU4 file
                     CALL ALLOCATE_FULL_MAT ( 'DUM1', NROWS_P, NCOLS_P, SUBR_NAME )
                     CALL PARTITION_FF ( 'TR6_CG', NROWS_F, NCOLS_F, TR6_CG, OU4_PARTVEC_ROW, OU4_PARTVEC_COL,                     &
                                          VAL_R, VAL_C, 'DUM1', NROWS_P, NCOLS_P, DUM1 )
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_P, NCOLS_P, FORM, SM, TR6_CG , UNT)
                  ELSE                                     ! Matrix cannot be partitioned so just write it out to the OU4 file
                     CALL WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS_F, NCOLS_F, FORM, SM, TR6_CG   , UNT)
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'Y'
               ELSE
                  IF (OU4_PART_MAT_NAMES(I,1)(1:) /= ' ') THEN
                     WRITE(ERR,201) MAT_NAME, SOL_NAME
                     WRITE(F06,201) MAT_NAME, SOL_NAME
                  ENDIF
                  HAS_OU4_MAT_BEEN_PROCESSED( I,1) = 'N'
               ENDIF
            ENDIF

         ELSE

            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,946) SUBR_NAME, MAT_NAME, CALLING_SUBR
            WRITE(F06,946) SUBR_NAME, MAT_NAME, CALLING_SUBR
            CALL OUTA_HERE ( 'Y' )

         ENDIF

         CALL DEALLOCATE_SCR_MAT  ( 'CRS1' )
         CALL DEALLOCATE_FULL_MAT ( 'DUM1' )

      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *WARNING    : OUTPUT4 MATRIX "',A,'" IS NOT AVAILABLE FOR PARTITIONING. IT DOES NOT HAVE EITHER COLS OR ROWS THAT', &
                           ' ARE GRID/COMP SET ORIENTED. IT WILL NOT BE PARTITIONED')

  201 FORMAT(' *WARNING    : OUTPUT4 MATRIX "',A,'" IS NOT CURRENTLY ALLOCATED AND IS UNAVAILABLE FOR PARTITIONING IN SOL "',A,'"')

  945 FORMAT(' *ERROR   945: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' FILE UNIT NUMBER ',I8,' IS NOT ONE OF THE NUMBERS ASSIGNED TO OUTPUT4 FILES')

  946 FORMAT(' *ERROR   946: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INVALID INPUT: MAT_NAME = ',A,' CALLING SUBR = ',A)


! **********************************************************************************************************************************

      END SUBROUTINE OUTPUT4_PROC
