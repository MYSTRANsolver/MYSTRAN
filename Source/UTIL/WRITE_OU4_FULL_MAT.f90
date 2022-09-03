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
 
      SUBROUTINE WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS, NCOLS, FORM, SYM, MAT, UNT )
 
! Writes a matrix that is in full format to unformatted file attached to unit UNT in NASTRAN OUTPUT4 format.
! Used for OUTPUT4 matrices

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, LEN_INPUT_FNAME, OU4, OU4FIL, MOU4, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  PRTOU4
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_OU4_FULL_MAT_BEGEND
 
      USE WRITE_OU4_FULL_MAT_USE_IFs

      IMPLICIT NONE
 

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_OU4_FULL_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name (only 1st 8 characters will be written)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM               ! 'Y' if input matrix is symmetric
      CHARACTER(LEN_INPUT_FNAME+3)    :: FILNAM            ! The filename for an OUTPUT4 file corresponding to input unit number UNT
      CHARACTER(16*BYTE)              :: MAT_OUT_NAME      ! 16 chars of MAT_NAME (or padded w/ blanks)

      INTEGER(LONG), INTENT(IN)       :: FORM              ! NASTRAN matrix FORM (not really used in MYSTRAN but needed for OUTPUT4)
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in MAT
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number where to write matrix
      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG), PARAMETER        :: IROW        = 1   ! A term written to UNT for the trailer record (just to be like NASTRAN)
      INTEGER(LONG), PARAMETER        :: PREC        = 2   ! Matrix precision (2 indicates double precision)
      INTEGER(LONG), PARAMETER        :: ROW_BEG     = 1   ! 1st row of matrix output to UNT is row 1
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_OU4_FULL_MAT_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MAT(NROWS,NCOLS)  ! Array of terms in matrix MAT
 
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

! Write matrix header

      WRITE(UNT) NCOLS, NROWS, FORM, PREC, MAT_OUT_NAME(1:4), MAT_OUT_NAME(5:8)

! Write matrix data

      DO J=1,NCOLS
         WRITE(UNT) J, ROW_BEG, 2*NROWS, (MAT(I,J),I=1,NROWS)
      ENDDO

! Write matrix trailer

      WRITE(UNT) NCOLS+1, IROW, PREC, (ZERO, I=1,PREC)

! Write matrix to f06 file, if requested

      IF (PRTOU4 > 0) THEN

         WRITE(F06,101) MAT_NAME, NCOLS, NROWS, FORM, PREC

         WRITE(F06,104) (J,J=1,NCOLS)
         DO I=1,NROWS
            WRITE(F06,105) I,(MAT(I,J),J=1,NCOLS)
         ENDDO
         WRITE(F06,*)

      ENDIF

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
 
      END SUBROUTINE WRITE_OU4_FULL_MAT
