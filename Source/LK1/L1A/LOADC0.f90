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
 
      SUBROUTINE LOADC0
 
! Preliminary reading of the Case Control to count several data sizes so that arrays may be allocated prior to the final reading
! of the Case Control.

! LOADC0 reads in the CASE CONTROL DECK and:
!   1) Counts the number of subcases and increments LSUB
!   2) Counts the number of SET cards and calls CC_SET0 to count the number of characters in SET's to determine LSETLN
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_ENTRY_LEN, FATAL_ERR, LSETS, LSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADC0_BEGEND
 
      USE LOADC0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LOADC0'
      CHARACTER(LEN=CC_ENTRY_LEN)     :: CARD              ! Case Control card
      CHARACTER(LEN=CC_ENTRY_LEN)     :: CARD1             ! CARD shifted to begin in col 1
      CHARACTER(12*BYTE)              :: DECK_NAME   = 'CASE CONTROL'
      CHARACTER(10*BYTE)              :: END_CARD

      INTEGER(LONG)                   :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG)                   :: IERR              ! Error indicator. If CHAR not found, IERR set to 1
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when reading a Case Control card from unit IN1
      INTEGER(LONG)                   :: JERR              ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADC0_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      JERR     = 0
      END_CARD = 'CEND      '

! Process CASE CONTROL DECK
 
      JERR = 0
      END_CARD = 'BEGIN BULK'

      DO

         READ(IN1,101,IOSTAT=IOCHK) CARD                   ! Read an input deck card
 
         IF (IOCHK < 0) THEN                               ! Quit if EOF/EOR occurs during read
            WRITE(ERR,1011) END_CARD  
            WRITE(F06,1011) END_CARD  
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
 
         IF (IOCHK > 0) THEN                               ! If error occurs during read, write message & CYCLE back to read again
            WRITE(ERR,1010) DECK_NAME
            WRITE(F06,1010) DECK_NAME
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            CYCLE
         ENDIF
 
         CALL REPLACE_TABS_W_BLANKS ( CARD )               ! Replace all tab characters with a white space

         CALL CSHIFT ( CARD, ' ', CARD1, CHAR_COL, IERR )  ! Shift CARD so it begins in col 1

! Check for Case Control cards. Exit loop on 'BEGIN BULK'
 
         IF      (CARD1(1: 4) == 'SUBC'      ) THEN
            LSUB = LSUB + 1

         ELSE IF (CARD1(1: 3) == 'SET'       ) THEN
            LSETS = LSETS + 1
            CALL CC_SET0 ( CARD1 )

         ELSE IF (CARD1(1:10) == 'BEGIN BULK') THEN
            EXIT
         ENDIF
 
      ENDDO
 
! If there were no explicit subcases, then default LSUB to 1
 
      IF (LSUB == 0) THEN
         LSUB = 1
      ENDIF
 
      IF (JERR > 0) THEN
         WRITE(ERR,10141)
         WRITE(F06,10141)
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
  101 FORMAT(A)

 1010 FORMAT(' *ERROR  1010: ERROR READING FOLLOWING ',A,' ENTRY. ENTRY IGNORED')

 1011 FORMAT(' *ERROR  1011: NO ',A10,' ENTRY FOUND BEFORE END OF FILE OR END OF RECORD IN INPUT FILE')

10141 FORMAT(/,' PROCESSING TERMINATED DUE TO ABOVE ERRORS')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE LOADC0
