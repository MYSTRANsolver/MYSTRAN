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
 
      SUBROUTINE READ_MATRIX_2 ( FILNAM, UNT, OPND, CLOSE_IT, CLOSE_STAT, MESSAG, NAME, NROWS, NTERMS, READ_NTERM  &
                               , I2_MATOUT, J_MATOUT, MATOUT )
 
! Reads matrix data from an unformatted file into a sparse format described below The format of the data in the file must be:

! If READ_NTERM = 'Y':

!    Record 1  : NTERMS = number nonzero terms in matrix (also number records following this one)
!    Record 1+i: row number, col number, nonzero value for matrix MATOUT

! If READ_NTERM = 'N':

!    Record i:   row number, col number, nonzero value for matrix MATOUT

! The matrix must be read in one row at a time and the rows MUST be in numerical order

! The output from this subroutine is the matrix described in row, col, nonzero value format:

!            I2_MATOUT(1 - NTERMS) : k-th value is the matrix row number of the k-th term in array MATOUT
!             J_MATOUT(1 - NTERMS) : k-th value is the matrix col number of the k-th term in array MATOUT
!               MATOUT(1 - NTERMS) : k-th value is the k-th nonzero value in the matrix 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_MATRIX_2_BEGEND
 
      USE READ_MATRIX_2_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READ_MATRIX_2'
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_IT          ! ='Y'/'N' whether to close UNT or note
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! What to do with file when it is closed
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! File description. Input to subr UNFORMATTED_OPEN 
      CHARACTER(LEN=*), INTENT(IN)    :: READ_NTERM        ! If 'Y', read NTERM from file before reading matrix
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Matrix name
      CHARACTER(LEN=*), INTENT(IN)    :: OPND              ! If 'Y', then do not open UNT, If 'N', open it
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in the matrix
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of matrix terms that should be in FILNAM
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number of FILNAM
      INTEGER(LONG), INTENT(OUT)      :: I2_MATOUT(NTERMS) ! Row numbers for terms in matrix MATOUT
      INTEGER(LONG), INTENT(OUT)      :: J_MATOUT(NTERMS)  ! Col numbers for terms in matrix MATOUT
      INTEGER(LONG)                   :: IERROR    = 0     ! Error count
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: K                 ! DO loop index                
      INTEGER(LONG)                   :: NUM_TERMS         ! Head rec read from files that denotes how many records in FILNAM
      INTEGER(LONG)                   :: OLD_ROW_NUM       ! A variable used to tell when a new row of MATOUT is being read
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading FILNAM
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_MATRIX_2_BEGEND

      REAL(DOUBLE) , INTENT(OUT)      :: MATOUT(NTERMS)    ! Real values for matrix MATOUT

      INTRINSIC DABS
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO K=1,NTERMS
         I2_MATOUT(K) = 0
          J_MATOUT(K) = 0
            MATOUT(K) = ZERO
      ENDDO

! Calc outputs

      OUNT(1) = ERR
      OUNT(2) = F06

      IF (OPND == 'N') THEN
         CALL FILE_OPEN ( UNT, FILNAM, OUNT, 'OLD', MESSAG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

! Should we read NTERMS from file before reading matrix?

      IF (READ_NTERM == 'Y') THEN
         READ(UNT,IOSTAT=IOCHK) NUM_TERMS
         IF (IOCHK /= 0) THEN
            REC_NO = 1
            CALL READERR ( IOCHK, FILNAM, MESSAG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )                                 ! Can't read NUM_TERMS fro file, so quit
         ENDIF
         IF (NUM_TERMS /= NTERMS) THEN
            WRITE(ERR, 924) SUBR_NAME, NAME,NUM_TERMS, NTERMS, FILNAM
            WRITE(F06, 924) SUBR_NAME, NAME,NUM_TERMS, NTERMS, FILNAM
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                                 ! Coding error (wrong number terms in matrix), so quit
         ENDIF
      ENDIF

! If we got here, there was no problem with NTERMS, so read matrix

!xx   WRITE(SC1, * )
      OLD_ROW_NUM = 0
      IERROR  = 0
      WRITE(SC1,12345,ADVANCE='NO') NAME, OLD_ROW_NUM+1, NROWS, NTERMS, CR13
      DO K = 1,NTERMS
         READ(UNT,IOSTAT=IOCHK) I2_MATOUT(K), J_MATOUT(K), MATOUT(K)
         IF (IOCHK /= 0) THEN
            IF (READ_NTERM == 'Y') THEN
               REC_NO = K + 1
            ELSE
               REC_NO = K
            ENDIF
            CALL READERR ( IOCHK, FILNAM, MESSAG, REC_NO, OUNT, 'Y' )
            IERROR = IERROR + 1
         ENDIF
         IF (I2_MATOUT(K) > OLD_ROW_NUM) THEN
            WRITE(SC1,12345,ADVANCE='NO') NAME, I2_MATOUT(K), NROWS, NTERMS, CR13
         ENDIF
      ENDDO
      WRITE(SC1,*) CR13      

      IF (IERROR /= 0) THEN
         WRITE(ERR,9996) SUBR_NAME,IERROR
         WRITE(F06,9996) SUBR_NAME,IERROR
         CALL OUTA_HERE ( 'Y' )                                    ! Quit due to above errors reading matrix
      ENDIF

      IF (CLOSE_IT == 'Y') THEN
         CALL FILE_CLOSE ( UNT, FILNAM, CLOSE_STAT, 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  924 FORMAT(' *ERROR   924: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF TERMS IN MATRIX ',A,' = ',I12,' BUT THE NUMBER OF TERMS SHOULD BE = ',I12,' IN FILE:'   &
                    ,/,15X,A)

 9996 FORMAT(/,' PROCESSING ABORTED IN SUBROUTINE ',A,' DUE TO ABOVE ',I8,' ERRORS')

12345 FORMAT(7X,A8,': read row ',i8,' of ',i8,' (matrix has ',i12,' terms)',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE READ_MATRIX_2
