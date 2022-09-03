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
 
      SUBROUTINE LOADE0
 
! LOADE0 does a preliminary read of the EXEC CONTROL DECK to find if there is a RESTART entry
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, FILE_NAM_MAXLEN, IN0, IN1, INC, LEN_INPUT_FNAME, INFILE,           &
                                         LEN_RESTART_FNAME, LNUM_IN4_FILES, RESTART_FILNAM, SCR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN, FATAL_ERR, RESTART
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADE0_BEGEND
 
      USE LOADE0_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LOADE0'
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD              ! Exec Control deck card
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD1             ! CARD shifted to begin in col 1
      CHARACTER(12*BYTE)              :: DECK_NAME   = 'EXEC CONTROL'
      CHARACTER( 4*BYTE), PARAMETER   :: END_CARD  = 'CEND'
      CHARACTER(LEN(INFILE))          :: FILNAM
 
      INTEGER(LONG)                   :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IBEG              ! Col where FILNAM begins after leading blanks
      INTEGER(LONG)                   :: IEND              ! Col where FILNAM ends after trailing blanks
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator.
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when reading a Case Control card from unit IN1
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADE0_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      LNUM_IN4_FILES = 0
		RESTART        = 'N'

!xx   REWIND (IN1)
main: DO

         READ(IN1,101,IOSTAT=IOCHK) CARD

         IF (IOCHK < 0) THEN                               ! Quit if EOF/EOR occurs during read
            WRITE(ERR,1011) END_CARD
            WRITE(F06,1011) END_CARD
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

         IF (IOCHK > 0) THEN                               ! Check if error occurs during read.
				WRITE(ERR,1010) DECK_NAME
				WRITE(F06,1010) DECK_NAME
            WRITE(F06,'(A)') CARD
            FATAL_ERR = FATAL_ERR + 1
            CYCLE
         ENDIF

         CALL REPLACE_TABS_W_BLANKS ( CARD )               ! Replace all tab characters with a white space

         CALL CSHIFT ( CARD, ' ', CARD1, CHAR_COL, IERR )  ! Shift CARD so it begins in col 1

         IF (CARD1(1:7) == 'RESTART'   ) THEN              ! No errors, so look for RESTART
            RESTART = 'Y'
            FILNAM(1:) = CARD1(8:)

            IBEG = 1
i_do2:      DO I=1,FILE_NAM_MAXLEN
               IF (FILNAM(I:I) == ' ') THEN
                  CYCLE
               ELSE
                  IBEG = I
                  EXIT i_do2
               ENDIF
            ENDDO i_do2

            IEND = 1
i_do3:      DO I=FILE_NAM_MAXLEN,1,-1
               IF (FILNAM(I:I) == ' ') THEN
                  CYCLE
               ELSE
                  IEND = I
                  EXIT i_do3
               ENDIF
            ENDDO i_do3

            IF (IEND == IBEG) THEN
               LEN_RESTART_FNAME = LEN_INPUT_FNAME
               RESTART_FILNAM = INFILE(1:LEN_INPUT_FNAME)
            ELSE
               LEN_RESTART_FNAME = IEND - IBEG + 2
               RESTART_FILNAM(1:) = FILNAM(IBEG:IEND) // '.'
            ENDIF

         ELSE IF (CARD1(1:3) == 'IN4') THEN
            LNUM_IN4_FILES = LNUM_IN4_FILES + 1

         ELSE IF (CARD1(1:4) == 'CEND' ) THEN              ! Check for CEND card
            EXIT main

         ENDIF     

      ENDDO main

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1010 FORMAT(' *ERROR  1010: ERROR READING FOLLOWING ',A,' ENTRY. ENTRY IGNORED')

 1011 FORMAT(' *ERROR  1011: NO ',A10,' ENTRY FOUND BEFORE END OF FILE OR END OF RECORD IN INPUT FILE')

! **********************************************************************************************************************************
 
      END SUBROUTINE LOADE0
