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
 
      SUBROUTINE READ_INCLUDE_FILNAM ( CARD, IERR )
 
! If there is an INCLUDE entry this subr reads the file name from that entry. The entry will be of the form:
!     INCLUDE 'filename' with or without the ' marks

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, FILE_NAM_MAXLEN, INC, INCFIL
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_INCLUDE_FILNAM_BEGEND

      USE READ_INCLUDE_FILNAM_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: LEXIST            ! 'T' if INCFIL exists

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READ_INCLUDE_FILNAM'
      CHARACTER(LEN=EC_ENTRY_LEN), INTENT(IN)  :: CARD     ! An entry from an input data (DAT) file
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD1             ! CARD shifted to begin in col 1
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD2             ! CARD1 with the "INCLUDE" removed
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD3             ! CARD1 with the "INCLUDE" removed
      CHARACTER( 1*BYTE)              :: DONE              ! Indicator of having found start and end of file name

      INTEGER(LONG), INTENT(OUT)      :: IERR              ! Local error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_INCLUDE_FILNAM_BEGEND
      INTEGER(LONG)                   :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG)                   :: DELTA_END_COL     ! Delta from START_ COL to END_COL
      INTEGER(LONG)                   :: END_COL           ! Col from CARD1 where the 2nd ' exists, if it does exist
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when reading a Case Control card from unit IN1
      INTEGER(LONG)                   :: START_COL         ! Col from CARD1 where the 1st ' exists, if it does exist
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to 


! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      INCFIL(1:) = ' '
      CALL CSHIFT ( CARD , ' ', CARD1, CHAR_COL, IERR )    ! This will remove leading blanks
      CARD2(1:)  = ' '
      CARD2(1:)  = CARD1(8:)                               ! CARD2 has only the file name plus, perhaps, leading/trailing blanks
      CALL CSHIFT ( CARD2, ' ', CARD3, CHAR_COL, IERR)     ! CARD3 is CARD2 without leading blanks
      DONE       = 'N'
      IERR       = 0

! Default units for writing errors the ERR, F06 files

      OUNT(1) = ERR
      OUNT(2) = F06

! Case 1: there are ' marks at the beginning and end of the file name

      START_COL = INDEX(CARD3,'''',.FALSE.) + 1
      IF (START_COL > 1) THEN                              ! 0 doesn't work, for some reason
         DELTA_END_COL = INDEX(CARD3(START_COL+1:),'''')
         IF (DELTA_END_COL > 0) THEN
            END_COL = START_COL + DELTA_END_COL - 1
            INCFIL(1:) = CARD3(START_COL:END_COL)
            DONE = 'Y'
         ELSE                                              ! There was a ' mark at the beginning but not at the end
            WRITE(ERR,1044) 'a', CARD
            WRITE(F06,1044) 'a', CARD
            IERR = 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDIF

! Case 2: there are no quote marks

      IF (DONE == 'N') THEN

         DO I=EC_ENTRY_LEN,1,-1                            ! Find START_COL when there is no ' mark
            IF ((CARD3(I:I) == ' ') .OR. (CARD3(I:I) == ACHAR(9))) THEN
               CYCLE
            ELSE
               END_COL = I
               EXIT
            ENDIF
         ENDDO

         DO I=1,EC_ENTRY_LEN,1
            IF ((CARD3(I:I) == ' ') .OR. (CARD3(I:I) == ACHAR(9))) THEN
               CYCLE
            ELSE
               START_COL = I
               EXIT
            ENDIF
         ENDDO

         IF (END_COL > START_COL) THEN
            INCFIL(1:) = CARD3(START_COL:END_COL)
            DONE = 'Y'
         ELSE
            WRITE(ERR,1044) 'b', CARD
            WRITE(F06,1044) 'b', CARD
            IERR = 2
            FATAL_ERR = FATAL_ERR + 1
         ENDIF

      ENDIF

      IF (DONE == 'Y') THEN

         INCFIL(1:) = CARD3(START_COL:END_COL)

         INQUIRE(FILE=INCFIL,EXIST=LEXIST)
         IF (LEXIST) THEN
            OPEN(INC,FILE=INCFIL,STATUS='OLD',ACTION='READ',IOSTAT=IOCHK)
            IF (IOCHK /= 0) CALL OPNERR ( IOCHK, INCFIL, OUNT, 'Y' )
         ELSE
            WRITE(ERR,1042) INCFIL
            WRITE(F06,1042) INCFIL
            FATAL_ERR = FATAL_ERR + 1
            IERR = 3
         ENDIF

      ELSE

         WRITE(ERR,1044) 'c', CARD
         WRITE(F06,1044) 'c', CARD
         IERR = 4
         FATAL_ERR = FATAL_ERR + 1

      ENDIF

      IF (DEBUG(115) > 0) THEN
         CALL DEB_READ_INCL_FILNAM
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1042 FORMAT(' *ERROR  1042: THE FOLLOWING "INCLUDE" FILE  DOES NOT EXIST OR THE INCLUDE ENTRY WAS IN ERROR: "',A,'"')

 1044 FORMAT(' *ERROR  1044',A,':INCORRECT FORMAT FOR "INCLUDE" ENTRY: ',A)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEB_READ_INCL_FILNAM

      IMPLICIT NONE

! **********************************************************************************************************************************
      WRITE(F06,'(A,A,A)') ' INCLUDE statement   = "', CARD, '"'
      WRITE(F06,'(A,A,A)') ' INCLUDE 2nd word    = "', CARD3, '"'
      WRITE(F06,'(A,I4 )') ' START_COL           = ' , START_COL
      WRITE(F06,'(A,I4 )') ' END_COL             = ' , END_COL
      WRITE(F06,'(A,A,A)') ' INCLUDE filename is = "', INCFIL(1:END_COL-START_COL+1), '"'

! **********************************************************************************************************************************

      END SUBROUTINE DEB_READ_INCL_FILNAM

      END SUBROUTINE READ_INCLUDE_FILNAM

