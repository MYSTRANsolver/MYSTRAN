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
 
      SUBROUTINE WRITE_OU4_SPARSE_MAT ( MAT_NAME, NROWS, NCOLS, FORM, SYM, NTERM_MAT, I_MAT, J_MAT, MAT, UNT )
 
! Writes a matrix that is in sparse CRS format to unformatted file attached to unit UNT in NASTRAN OUTPUT4 format.
! Used for OUTPUT4 matrices

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, LEN_INPUT_FNAME, OU4, OU4FIL, mou4, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  PRTOU4, SPARSTOR
      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1, I_CCS1, J_CCS1, CCS1
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_OU4_SPARSE_MAT_BEGEND
 
      USE WRITE_OU4_SPARSE_MAT_USE_IFs

      IMPLICIT NONE
 

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_OU4_SPARSE_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name (only 1st 8 characters will be written)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM               ! 'Y' if input matrix is symmetric
      CHARACTER(LEN(MAT_NAME))        :: CCS_MAT_NAME      ! Name for CCS form of MAT
      CHARACTER(LEN(MAT_NAME))        :: CRS_MAT_NAME      ! Name for CRS form of MAT
      CHARACTER(LEN_INPUT_FNAME+3)    :: FILNAM
      CHARACTER(16*BYTE)              :: MAT_OUT_NAME      ! 16 chars of MAT_NAME (or padded w/ blanks)

      INTEGER(LONG), INTENT(IN)       :: FORM              ! NASTRAN matrix FORM (not really used in MYSTRAN but needed for OUTPUT4)
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in MAT
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: NTERM_MAT         ! Number of nonzero terms in MAT
      INTEGER(LONG), INTENT(IN)       :: I_MAT(NROWS+1)    ! Row indicators for MAT: I_MAT(I+1)-I_MAT(i) = no. terms in row I
      INTEGER(LONG), INTENT(IN)       :: J_MAT(NTERM_MAT)  ! Col numbers in MAT
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number where to write matrix
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters
      INTEGER(LONG), PARAMETER        :: IROW        = 1   ! 
!xx   INTEGER(LONG)                   :: NTERM_ROW_CCS1    ! Number of nonzero terms in row I of CRS1
      INTEGER(LONG)                   :: NUM_MAT_DIAG_0    ! 
      INTEGER(LONG)                   :: NTERM_CCS1        ! 
      INTEGER(LONG)                   :: NTERM_CRS1        ! 
      INTEGER(LONG)                   :: NTERM_COL_J       ! 
      INTEGER(LONG), PARAMETER        :: PREC        = 2   ! Matrix precision (2 indicates double precision)
      INTEGER(LONG), PARAMETER        :: ROW_BEG     = 1   ! 1st row of matrix output to UNT is row 1
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_OU4_SPARSE_MAT_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MAT(NTERM_MAT)    ! Array of terms in matrix MAT
      REAL(DOUBLE)                    :: CCS1_COL(NROWS)   ! One column of CCS1 in full format
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Get file name for unit UNT

      FILNAM(1:) = ' '
      DO I=1,MOU4
         IF (OU4(I) == UNT) THEN
            FILNAM = OU4FIL(I)
         ENDIF
      ENDDO

      IF (LEN(MAT_NAME) > 16) THEN
         MAT_OUT_NAME(1:) = MAT_NAME(1:16)
      ELSE
         MAT_OUT_NAME(1:) = ' '
         MAT_OUT_NAME(1:) = MAT_NAME(1:)
      ENDIF

      CCS_MAT_NAME(1:)   = MAT_NAME(1:)                    ! // ' in CCS format'
      CRS_MAT_NAME(1:)   = MAT_NAME(1:)                    ! // ' in nonsym format'


! Convert to CCS format

      NTERM_CRS1 = NTERM_MAT
                                                           ! If matrix stored as sym (therefore is square), first convert to nonsym
      IF ((SYM == 'Y') .AND. (SPARSTOR == 'SYM')) THEN

         CALL SPARSE_MAT_DIAG_ZEROS ( MAT_NAME, NROWS, NTERM_MAT, I_MAT, J_MAT, NUM_MAT_DIAG_0 )
         NTERM_CRS1 = 2*NTERM_MAT  - (NROWS - NUM_MAT_DIAG_0)
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NROWS, NTERM_CRS1, SUBR_NAME )      
         CALL CRS_SYM_TO_CRS_NONSYM ( MAT_NAME, NROWS, NTERM_MAT, I_MAT, J_MAT, MAT, 'CRS1', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, 'N' )

         NTERM_CCS1 = NTERM_CRS1
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NCOLS, NTERM_CCS1, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS (NROWS,NCOLS,NTERM_CRS1, CRS_MAT_NAME, I_CRS1, J_CRS1, CRS1, CCS_MAT_NAME, J_CCS1, I_CCS1,     &
                                     CCS1, 'N' )

      ELSE                                                 ! Matrix was stored nonsym, so convert to CCS directly

         NTERM_CCS1 = NTERM_MAT
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NCOLS, NTERM_CCS1, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS (NROWS,NCOLS,NTERM_MAT , MAT_NAME    , I_MAT , J_MAT , MAT , CCS_MAT_NAME, J_CCS1, I_CCS1,     &
                                     CCS1, 'N' )

      ENDIF

! Write matrix header

!xx   WRITE(UNT) NCOLS, NROWS, FORM, PREC, MAT_NAME(1:16)
      WRITE(UNT) NCOLS, NROWS, FORM, PREC, MAT_OUT_NAME(1:4), MAT_OUT_NAME(5:8)

! Write matrix data


      IF (PRTOU4 > 0) THEN                                 ! Write matrix to f06 file, if requested
         WRITE(F06,101) MAT_NAME, NCOLS, NROWS, FORM, PREC
         WRITE(F06,104) (J,J=1,NCOLS)
      ENDIF

      K = 0
      DO J=1,NCOLS
         NTERM_COL_J = J_CCS1(J+1) - J_CCS1(J)
         DO I=1,NROWS
            CCS1_COL(I) = ZERO
         ENDDO
         IF (NTERM_COL_J > 0) THEN
            DO I=1,NTERM_COL_J
               K = K + 1
               CCS1_COL(I_CCS1(K)) = CCS1(K)
            ENDDO
         ENDIF                                             ! Always write whole matrix 
         WRITE(UNT) J, ROW_BEG, 2*NROWS, (CCS1_COL(I),I=1,NROWS)
         IF (PRTOU4 > 0) THEN
            WRITE(F06,105) J,(CCS1_COL(I),I=1,NROWS)
         ENDIF
      ENDDO

      IF (PRTOU4 > 0) THEN
         WRITE(F06,*)
      ENDIF

      WRITE(UNT) NCOLS+1, IROW, PREC, (ZERO, I=1,PREC)     ! Write matrix trailer

      CALL DEALLOCATE_SCR_MAT ( 'CCS1' )
      CALL DEALLOCATE_SCR_MAT ( 'CRS1' )

      WRITE(F06,1001) MAT_NAME, NROWS, NCOLS, FILNAM

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(14X,A,8X,'NCOLS = ',I8,8X,'NROWS = ',I8,8X,'FORM  = ',I8,8X,'PREC  = ',I8)

  104 FORMAT(32767(21X,I3))

  105 FORMAT(I10,32767(2X,1ES22.14))

 1001 FORMAT(' *INFORMATION: MATRIX ',A,' WITH ',I8,' ROWS AND ',I8,' COLS HAS BEEN WRITTEN IN OUTPUT4 FORMAT TO FILE ',A)




 
! **********************************************************************************************************************************

      END SUBROUTINE WRITE_OU4_SPARSE_MAT
