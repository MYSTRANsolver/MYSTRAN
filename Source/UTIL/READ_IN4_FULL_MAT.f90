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

      SUBROUTINE READ_IN4_FULL_MAT ( ELEM_TYP, ELEM_ID, MAT_NAME_IN, NRI, NCI, UNT, FILNAM, MAT_FULL, IERRT, CALLING_SUBR )

! Reads a matrix that is in full NASTRAN OUTPUT4 format from file FILNAM attached to unit UNT

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  NUM_EMG_FATAL_ERRS
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_IN4_FULL_MAT_BEGEND

      USE READ_IN4_FULL_MAT_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READ_IN4_FULL_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Name of subr that called this one
      CHARACTER(LEN=*), INTENT(IN)    :: ELEM_TYP          ! Elem type for which this subr was called
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! Name of file data is to be read from
      CHARACTER( 1*BYTE)              :: MAT_FND           ! 'Y' if matrix was found on FILNAM
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME_IN       ! Name of matrix to read from UNT
      CHARACTER( 8*BYTE)              :: MAT_NAME          ! Name of matrix in file that is read

      INTEGER(LONG), INTENT(IN)       :: UNT               ! I/O unit number from which to read MAT 
      INTEGER(LONG), INTENT(IN)       :: ELEM_ID           ! ID of element for which this subr was called
      INTEGER(LONG), INTENT(IN)       :: NRI               ! Number of rows expected in MAT_FULL
      INTEGER(LONG), INTENT(IN)       :: NCI               ! Number of cols expected in MAT
      INTEGER(LONG), INTENT(OUT)      :: IERRT             ! IERR1+IERR2
      INTEGER(LONG)                   :: FORM              ! 
      INTEGER(LONG)                   :: ICOL              ! The column number being read in a record from FILNAM
      INTEGER(LONG)                   :: IERR1             ! Local error count
      INTEGER(LONG)                   :: IERR2             ! Local error count
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: IROW              ! Starting row number for terms read in a record from FILNAM
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters
      INTEGER(LONG)                   :: NCOLS             ! Number of cols for matrix read from IN4
      INTEGER(LONG)                   :: NROWS             ! Number of rows for matrix read from IN4
      INTEGER(LONG)                   :: NWRDS             ! Number of 4 byte words in a column read from FILNAM
      INTEGER(LONG)                   :: MAT_NUM           ! Number of matrix read from file
      INTEGER(LONG)                   :: NC                ! From matrix trailer. Should be NCOLS+1
      INTEGER(LONG)                   :: PREC              ! Matrix precision (2 indicates double precision)
      INTEGER(LONG)                   :: REC_NUM           ! 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_IN4_FULL_MAT_BEGEND

      REAL(DOUBLE), ALLOCATABLE       :: CCS1_COL(:)       ! One column of MAT
      REAL(DOUBLE), INTENT(OUT)       :: MAT_FULL(NRI,NCI) ! Array of terms in matrix MAT
      REAL(DOUBLE)                    :: RJUNK             ! Values read from file for matrix other than the one we want
!xx   REAL(DOUBLE)                    :: Z0                ! Zero values read from matrix trailer

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      MAT_NUM = 0
      MAT_FND = 'N'

      DO I=1,NRI
         DO J=1,NCI
            MAT_FULL(I,J) = ZERO
         ENDDO
      ENDDO

do_1: DO                                                   ! Loop over unknown number of matrices in this IN4 file

         IERR1 = 0
         IERR2 = 0
         IERRT = 0
         REC_NUM = 0
         MAT_NUM = MAT_NUM + 1                             ! Read header for a matrix
         READ(UNT,IOSTAT=IOCHK) NCOLS, NROWS, FORM, PREC, MAT_NAME        ;  REC_NUM = REC_NUM + 1
         IF (PREC /= 2) THEN
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR          = FATAL_ERR + 1
            WRITE(ERR,956) PREC, FILNAM            
            WRITE(F06,956) PREC, FILNAM
            RETURN            
         ENDIF

         IF      (IOCHK == 0) THEN                         ! No problems reading header, so proceed to read matrix values


            IF (MAT_NAME == MAT_NAME_IN) THEN

               IF ((NROWS /= NRI) .OR. (NCOLS /= NCI)) THEN
                  IERR1 = IERR1 + 1
               ENDIF

               IF (IERR1 == 0) THEN

                  MAT_FND = 'Y'

                  ALLOCATE ( CCS1_COL(NROWS) )

                  DO J=1,NCOLS+1                           ! Read cols, 1 at a time. Extra one is not used
                     READ(UNT,IOSTAT=IOCHK) ICOL, IROW, NWRDS, (CCS1_COL(K),K=IROW,IROW+(NWRDS/PREC)-1)     ;  REC_NUM = REC_NUM + 1
                      IF      (IOCHK == 0) THEN            ! No problem reading matrix row, so enter data into MAT_FULL
                        IF (J <= NCOLS) THEN
                           DO K=IROW,IROW+(NWRDS/2)-1
                              MAT_FULL(K,ICOL) = CCS1_COL(K)
                           ENDDO
                        ENDIF
                     ELSE IF (IOCHK > 0) THEN              ! Error reading matrix row so set error condition
                        IERR2 = IERR2 + 1
                        IERRT = IERRT + IERR2
                        CALL IN4_READ_ERR ( 21 )
                     ELSE IF (IOCHK < 0) THEN              ! EOR or EOF before finished reading all rows, so set error condition
                        IERR2 = IERR2 + 1
                        IERRT = IERRT + IERR2
                        CALL IN4_READ_ERR ( 22 )
                     ENDIF
                  ENDDO
                  IF (IERR2 > 0) THEN
                     EXIT do_1
                  ENDIF
                                                           ! Read trailer
!xx               READ(UNT,IOSTAT=IOCHK) NC, IROW, PREC, (Z0,J=1,PREC)                     ;  REC_NUM = REC_NUM + 1
!xx               IF      (IOCHK > 0) THEN                 ! Error reading trailer so set error condition
!xx                  IERR2 = IERR2 + 1
!xx                  IERRT = IERRT + IERR2
!xx                  CALL IN4_READ_ERR ( 31 )
!xx                  EXIT do_1
!xx               ELSE IF (IOCHK < 0) THEN                 ! EOR or EOF  reading trailer so set error condition
!xx                  IERR2 = IERR2 + 1
!xx                  IERRT = IERRT + IERR2
!xx                  CALL IN4_READ_ERR ( 32 )
!xx                  EXIT do_1
!xx               ENDIF


                  DEALLOCATE ( CCS1_COL )

                  EXIT do_1                                ! Matrix was found and read, so exit loop and return to calling subr

               ELSE

                  WRITE(ERR,953) ELEM_TYP, ELEM_ID, MAT_NAME, FILNAM, NRI, NCI, NROWS, NCOLS
                  WRITE(F06,953) ELEM_TYP, ELEM_ID, MAT_NAME, FILNAM, NRI, NCI, NROWS, NCOLS
                  NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
                  FATAL_ERR          = FATAL_ERR + 1
                  IERRT              = IERRT + IERR1
                  RETURN

               ENDIF

            ELSE

               DO J=1,NCOLS
                  READ(UNT,IOSTAT=IOCHK) ICOL, IROW, NROWS, (RJUNK,K=1,NROWS)
               ENDDO
               READ(UNT,IOSTAT=IOCHK) NC, IROW, PREC, (RJUNK,J=1,PREC)  ! Read trailer

            ENDIF

         ELSE IF (IOCHK > 0) THEN                          ! Error reading header so set error condition

            IERR2 = IERR2 + 1
            IERRT = IERRT + IERR2
            CALL IN4_READ_ERR ( 11 )
            EXIT do_1

         ELSE IF (IOCHK < 0) THEN                          ! EOR or EOF, so assume no more matrices and exit

            EXIT do_1

         ENDIF

      ENDDO do_1

      IF (ALLOCATED(CCS1_COL)) THEN
         DEALLOCATE ( CCS1_COL )
      ENDIF

      IF (IERR2 > 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9999) IERR2, SUBR_NAME, CALLING_SUBR
         WRITE(F06,9999) IERR2, SUBR_NAME, CALLING_SUBR
         CALL OUTA_HERE ( 'Y' )
      ENDIF

       IF (MAT_FND == 'N') THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,950) MAT_NAME_IN, FILNAM, SUBR_NAME, CALLING_SUBR
         WRITE(F06,950) MAT_NAME_IN, FILNAM, SUBR_NAME, CALLING_SUBR
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



  950 FORMAT(' *ERROR   950: INPUT MATRIX "',A,'" WAS NOT FOUND ON FILE:'                                                          &
                    ,/,15X,  A                                                                                                     &
                    ,/,14X,' IN SUBR: ',A,' CALLED BY SUBR: ',A)

  953 FORMAT(' *ERROR   953: FOR ',A,I8,' THE NUMBER OF ROWS AND COLS FOR INPUT MATRIX ',A                                         &
                    ,/,14X,' FROM FILE: ',A                                                                                        &
                    ,/,14X,' SHOULD BE ROWS           = ',I8,', COLS = ',I8,' (BASED ON THE ELEMENT''s B.D CUSERIN ENTRY)'&
                    ,/,14X,' BUT WAS FOUND TO BE ROWS = ',I8,', COLS = ',I8,' (BASED ON THE HEADER RECORD OF THE ABOVE FILE)')

  956 FORMAT(' *ERROR   956: THE PRECISION OF THE MATRIX READ FROM THE FILE BELOW WAS ',I8,'. ONLY PREC -= 2 IS ALLOWED',/,15X,A)

 9999 FORMAT(' PROCESSING STOPPED DUE TO ',I8,' ERROR(S) READING IN4 MATRICES IN SUBR ',A,/,' THIS SUBR WAS CALLED BY ',A)

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE IN4_READ_ERR ( II )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F06

      IMPLICIT NONE

      CHARACTER(30*BYTE)              :: MESSAGE           ! Error message to write

      INTEGER(LONG), INTENT(IN)       :: II                ! Error message indicator

! **********************************************************************************************************************************
      IF      (II == 11) THEN
         MESSAGE = 'ERROR READING MATRIX HEADER'
      ELSE IF (II == 21) THEN
         MESSAGE = 'ERROR READING MATRIX '
      ELSE IF (II == 22) THEN
         MESSAGE = 'EOR/EOF READING MATRIX '
      ELSE IF (II == 31) THEN
         MESSAGE = 'ERROR READING MATRIX TRAILER'
      ELSE IF (II == 32) THEN      
         MESSAGE = 'EOR/EOF READING MATRIX TRAILER'
      ENDIF

      WRITE(ERR,947) MESSAGE, REC_NUM, MAT_NUM, FILNAM, SUBR_NAME
      WRITE(F06,947) MESSAGE, REC_NUM, MAT_NUM, FILNAM, SUBR_NAME

! **********************************************************************************************************************************
  947 FORMAT(' *ERROR   947: ',A,' RECORD NUMBER ',I8,' IN MATRIX NUMBER ',I8,'. ERROR OCCURRED WHILE READING FROM FILE:'          &
               ,/,15X,A,/,14X,' IN SUBR ',A)

! **********************************************************************************************************************************

      END SUBROUTINE IN4_READ_ERR

      END SUBROUTINE READ_IN4_FULL_MAT
