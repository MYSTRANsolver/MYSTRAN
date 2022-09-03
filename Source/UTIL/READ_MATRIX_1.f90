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

      SUBROUTINE READ_MATRIX_1 ( FILNAM, UNT, OPND, CLOSE_IT, CLOSE_STAT, MESSAG, NAME, NTERM, READ_NTERM, NROWS  &
                               , I_MATOUT, J_MATOUT, MATOUT )
 
! Reads matrix data from an unformatted file into a sparse format described below. The format of the data in the file must be:

! If READ_NTERM = 'Y':

!    Record 1  : NTERM       = No. nonzero terms in matrix (also noumber of records following this one)
!    Record 1+i: i, j, value = row no., col no., nonzero value for matrix MATOUT

! If READ_NTERM = 'N':

!    Record i:   i, j, value = row no., col no., nonzero value for matrix MATOUT

! The matrix must be read in one row at a time and the rows MUST be in numerical order

! The sparse matrix format output from this subroutine is the matrix described in compressed row storage format:

!             I_MATOUT(1 to NROWS+1) : i-th value is index in MATOUT where matrix row i begins
!             J_MATOUT(1 to NTERM)   : k-th value is the matrix col no. of the k-th term in array MATOUT
!               MATOUT(1 to NTERM)   : k-th value is the k-th nonzero value in the matrix 
 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_MATRIX_1_BEGEND

      USE READ_MATRIX_1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READ_MATRIX_1'
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_IT          ! ='Y'/'N' whether to close UNT or note
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! What to do with file when it is closed
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! File description. Input to subr UNFORMATTED_OPEN 
      CHARACTER(LEN=*), INTENT(IN)    :: READ_NTERM        ! If 'Y', read NTERM from file before reading matrix
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Matrix name
      CHARACTER(LEN=*), INTENT(IN)    :: OPND              ! If 'Y', then do not open UNT, If 'N', open it
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MATOUT
      INTEGER(LONG), INTENT(IN)       :: NTERM             ! Number of matrix terms that should be in FILNAM
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number of FILNAM
      INTEGER(LONG), INTENT(OUT)      :: I_MATOUT(NROWS+1) ! Row numbers for terms in matrix MATOUT
      INTEGER(LONG), INTENT(OUT)      :: J_MATOUT(NTERM)   ! Col numbers for terms in matrix MATOUT
      INTEGER(LONG)                   :: READ_ERR    = 0   ! Error count
      INTEGER(LONG)                   :: I,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: IROW              ! Integer row value read from FILNAM
      INTEGER(LONG)                   :: IROW_OLD          ! Previous value of IROW
      INTEGER(LONG)                   :: JCOL              ! Col number for MATOUT
      INTEGER(LONG)                   :: KTERM             ! Count of number of nonzero terms read from FILNAM
      INTEGER(LONG)                   :: NUM_TERMS         ! Head rec read from files that denotes how many records in FILNAM
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading FILNAM
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_MATRIX_1_BEGEND
 
      REAL(DOUBLE) , INTENT(OUT)      :: MATOUT(NTERM)     ! Real values for matrix MATOUT
      REAL(DOUBLE)                    :: RVAL              ! Real values read from FILNAM
 
      INTRINSIC                       :: DABS
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Quick return if there are no terms in the matrix

      IF (NTERM == 0) RETURN

! Initialize outputs

      DO I=1,NROWS+1
         I_MATOUT(I) = 0
      ENDDO

      DO I=1,NTERM
         J_MATOUT(I) = 0
           MATOUT(I) = ZERO
      ENDDO

      OUNT(1) = ERR
      OUNT(2) = F06

      IF (OPND == 'N') THEN
         CALL FILE_OPEN ( UNT, FILNAM, OUNT, 'OLD', MESSAG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

! Should we read NTERM from file before reading matrix?

      IF (READ_NTERM == 'Y') THEN
         READ(UNT,IOSTAT=IOCHK) NUM_TERMS
         IF (IOCHK /= 0) THEN
            REC_NO = 1
            CALL READERR ( IOCHK, FILNAM, MESSAG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )                                 ! Can't read NUM_TERMS from file, so quit
         ENDIF
         IF (NUM_TERMS /= NTERM) THEN
            WRITE(ERR, 924) SUBR_NAME, NAME, NUM_TERMS, NTERM, FILNAM
            WRITE(F06, 924) SUBR_NAME, NAME, NUM_TERMS, NTERM, FILNAM
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                                 ! Coding error (wrong no. terms in matrix), so quit
         ENDIF
      ENDIF

! If we got here, there was no problem with NTERM, so read matrix

      KTERM       = 0
      IROW_OLD    = 0
      I_MATOUT(1) = 1

!xx   WRITE(SC1, * )
      WRITE(SC1,12345,ADVANCE='NO') NAME, IROW_OLD+1, NROWS, NTERM, CR13
k_do1:DO K = 1,NTERM
         READ(UNT,IOSTAT=IOCHK) IROW,JCOL,RVAL
         IF (IOCHK /= 0) THEN
            IF (READ_NTERM == 'Y') THEN
               REC_NO = K + 1
            ELSE
               REC_NO = K
            ENDIF
            CALL READERR ( IOCHK, FILNAM, MESSAG, REC_NO, OUNT, 'Y' )
            READ_ERR = READ_ERR + 1                        ! Error reading IROW, JCOL, RVAL record from unit UNT
            CYCLE k_do1
         ELSE
            IF (IROW > IROW_OLD) THEN
               WRITE(SC1,12345,ADVANCE='NO') NAME, IROW, NROWS, NTERM, CR13
               DO I=IROW_OLD+1,IROW
                  I_MATOUT(I+1) = I_MATOUT(I)
               ENDDO
               IROW_OLD = IROW
            ELSE IF (IROW < IROW_OLD) THEN
               WRITE(ERR,926) SUBR_NAME, NAME, IROW_OLD, IROW, FILNAM
               WRITE(F06,926) SUBR_NAME, NAME, IROW_OLD, IROW, FILNAM
               READ_ERR = READ_ERR + 1                     ! Coding error (matrix stored incorrectly), so quit
               FATAL_ERR = FATAL_ERR + 1
               CYCLE k_do1
            ENDIF
         ENDIF
         I_MATOUT(IROW+1) = I_MATOUT(IROW+1) + 1
         KTERM            = KTERM + 1
         J_MATOUT(KTERM)  = JCOL          
           MATOUT(KTERM)  = RVAL
      ENDDO k_do1
      WRITE(SC1,*) CR13

      IF (READ_ERR /= 0) THEN
         WRITE(ERR,9996) SUBR_NAME,READ_ERR
         WRITE(F06,9996) SUBR_NAME,READ_ERR
         CALL OUTA_HERE ( 'Y' )                            ! Quit due to above errors in k_do1 loop
      ENDIF

      IF (IROW < NROWS) THEN                               ! Fill out remainder of I_MATOUT, if needed
         DO I=IROW+1,NROWS
            I_MATOUT(I+1) = I_MATOUT(I)
         ENDDO
      ENDIF

      IF (CLOSE_IT == 'Y') THEN
         CALL FILE_CLOSE ( UNT, FILNAM, CLOSE_STAT, 'Y' )
      ENDIF

! Check sensibility of I_MATOUT

      IF (DEBUG(101) >= 2) THEN
         CALL CHECK_SPARSE_CRS_I ( NAME, SUBR_NAME, NROWS, NTERM, I_MATOUT, 101 )
      ENDIF

! Write I_A if requested

      IF (DEBUG(101) >= 1) THEN
         WRITE(F06,101) NAME, NAME
         WRITE(F06,102) (I_MATOUT(I),I=1,NROWS+1)
         WRITE(F06,*)
         WRITE(F06,103) NAME, NAME, NAME, NROWS
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(38X,'SPARSE ROW INDICATOR ARRAY I_',A,'(I) FOR MATRIX ',A,':',/)

  102 FORMAT(10I12)

  103 FORMAT(' NOTE: THE DIFFERENCE   I_',A,'(I+1) - I_',A,'(I)   SHOULD EQUAL THE NUMBER OF NONZERO TERMS IN ROW I OF ',A         &
          ,/,'       THE MATRIX HAS ',I8,' ROWS AND IS LISTED ABOVE WITH ELEMENTS 1-10 ON THE FIRST LINE, ELEMENTS 11-20 ON THE',  &
                   ' 2ND LINE, ETC.'/)

  924 FORMAT(' *ERROR   924: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF TERMS IN MATRIX ',A,' = ',I12,' BUT THE NUMBER OF TERMS SHOULD BE = ',I12,' IN FILE:'   &
                    ,/,15X,A)

  926 FORMAT(' *ERROR   926: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX ',A,' IS STORED INCORRECTLY. THE MATRIX MUST BE STORED BY ROWS IN NUMERICAL ORDER '            &
                    ,/,14X,' HOWEVER, ROW ',I12,' IS STORED BEFORE ROW ',I12,' IN FILE:'                                           &
                    ,/,15X,A)

 9996 FORMAT(/,' PROCESSING ABORTED IN SUBROUTINE ',A,' DUE TO ABOVE ',I8,' ERRORS')

12345 FORMAT(7X,A8,': read row ',i8,' of ',i8,' (matrix has ',i12,' terms)',A)

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE CHECK_SPARSE_CRS_I ( MAT_A_NAME, CALLING_SUBR, NROWS_A, NTERM_A, I_A, DEBUG_NUM )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE

      CHARACTER(LEN=*)                :: CALLING_SUBR
      CHARACTER(LEN=*)                :: MAT_A_NAME
      CHARACTER( 1*BYTE)              :: QUIT = 'N'

      INTEGER(LONG) ,INTENT(IN)       :: DEBUG_NUM
      INTEGER(LONG) ,INTENT(IN)       :: I_A(NROWS+1)
      INTEGER(LONG) ,INTENT(IN)       :: NROWS_A
      INTEGER(LONG) ,INTENT(IN)       :: NTERM_A
      INTEGER(LONG)                   :: KTERM_A
      INTEGER(LONG)                   :: NTERMS_A_ROW_I    !
      INTEGER(LONG)                   :: NUM_ROW_ERRS

! **********************************************************************************************************************************
      NUM_ROW_ERRS = 0
      KTERM_A      = 0
      DO I=1,NROWS_A
         NTERMS_A_ROW_I = I_A(I+1) - I_A(I)
         IF (NTERMS_A_ROW_I < 0) THEN                      ! Error. This indicates row I has < 0 number of terms in it.
            NUM_ROW_ERRS = NUM_ROW_ERRS + 1 
         ELSE
            KTERM_A = KTERM_A + NTERMS_A_ROW_I
         ENDIF
      ENDDO

      IF (NUM_ROW_ERRS > 0) THEN
         WRITE(ERR,913) SUBR_NAME, MAT_A_NAME, MAT_A_NAME, NUM_ROW_ERRS, DEBUG_NUM, MAT_A_NAME, CALLING_SUBR
         WRITE(F06,913) SUBR_NAME, MAT_A_NAME, MAT_A_NAME, NUM_ROW_ERRS, DEBUG_NUM, MAT_A_NAME, CALLING_SUBR
         IF (DEBUG(DEBUG_NUM) >= 3) THEN
            QUIT = 'Y'
         ENDIF
      ENDIF 

      IF (KTERM_A /= NTERM_A) THEN
         WRITE(ERR,928) SUBR_NAME, MAT_A_NAME, MAT_A_NAME, NROWS_A, MAT_A_NAME, MAT_A_NAME, KTERM_A, NTERM_A, DEBUG_NUM,           &
                                   MAT_A_NAME, CALLING_SUBR
         WRITE(F06,928) SUBR_NAME, MAT_A_NAME, MAT_A_NAME, NROWS_A, MAT_A_NAME, MAT_A_NAME, KTERM_A, NTERM_A, DEBUG_NUM,           &
                                   MAT_A_NAME, CALLING_SUBR
         IF (DEBUG(DEBUG_NUM) >= 3) THEN
            QUIT = 'Y'
         ENDIF
      ENDIF 

      IF (QUIT == 'Y') THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      RETURN

! **********************************************************************************************************************************
  913 FORMAT(' *ERROR   913: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX ',A,' HAS A SPARSE I_',A,' THAT IS INCORRECT.'                                                 &
                    ,/,14X,' THERE ARE AT LEAST ',I8,' ROWS WHERE THERE ARE < 0 NUMBER OF TERMS INDICATED.'                        &
                    ,/,14X,' IN ADDITION, THERE SHOULD BE NO ZERO TERMS IN THE MATRIX'              &
                    ,/,14X,' USE BULK DATA ENTRY DEBUG ',I3,' WITH VALUE >=2 TO GET A LISTING OF I_',A                             &
                    ,/,14X,' THIS SUBR WAS CALLED BY SUBR ',A,/)

  928 FORMAT(' *ERROR   928: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX ',A,' HAS A SPARSE I_',A,' THAT IS INCORRECT. THE SUM OVER I FROM 1 TO THE NUMBER OF ROWS = ', &
                             I8,' OF THE MATRIX TERMS:'                                                                            &
                    ,/,14X,' I_',A,'(I+1) - I_',A,'(I) = ',I8,' SHOULD EQUAL THE NUMBER OF NONZERO TERMS IN THE MATRIX WHICH = ',I8&
                    ,/,14X,' USE BULK DATA ENTRY DEBUG ',I3,' WITH VALUE >=3 TO GET A LISTING OF I_',A                             &
                    ,/,14X,' THIS SUBR WAS CALLED BY SUBR ',A,/)

! **********************************************************************************************************************************

      END SUBROUTINE CHECK_SPARSE_CRS_I

      END SUBROUTINE READ_MATRIX_1
