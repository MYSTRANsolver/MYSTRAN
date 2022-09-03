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

      SUBROUTINE STOKEN ( CALLING_SUBR, TOKSTR, TOKEN_BEG, STRNG_END, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )

! Routine to tokenize character string data. The most common use is in extracting tokens from  the data contained in
! a SET Case Control command that can be of the form:

!         I1, I2, I3 THRU I4 EXCEPT I6, I7, ....

! On repeated calls to STOKEN the routine will find, either:

!  1) a single character token of max length MAX_TOKEN_LEN, or
!  2) a triad of char tokens of the form I1 THRU I2 where I1, I2 are integers

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  MAX_TOKEN_LEN, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  STOKEN_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE STOKEN_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)) :: SUBR_NAME = 'STOKEN'
      CHARACTER(LEN=*)          , INTENT(IN)   :: CALLING_SUBR! Character string to tokenize 
      CHARACTER(LEN=*)          , INTENT(IN)   :: TOKSTR      ! Character string to tokenize 
      CHARACTER( 3*BYTE)        , INTENT(INOUT):: EXCEPT      ! Flag indicating whether EXCEPT is "ON " or "OFF"
      CHARACTER( 3*BYTE)        , INTENT(INOUT):: THRU        ! Flag indicating whether THRU   is "ON " or "OFF"
      CHARACTER(LEN=LEN(TOKSTR)), INTENT(OUT)  :: ERRTOK      ! Char string with data for an error to be printed by calling subr
      CHARACTER( 8*BYTE), INTENT(OUT)          :: TOKEN(3)    ! Array of 3 char tokens (e.g. could contain I1, THRU, I2)
      CHARACTER( 8*BYTE), INTENT(OUT)          :: TOKTYP(3)   ! Array of 3 char indicators of what type of tokens are in TOKEN(1-3)

      INTEGER(LONG), INTENT(IN)                :: STRNG_END   ! Column of last character in TOKSTR
      INTEGER(LONG), INTENT(INOUT)             :: TOKEN_BEG   ! On entry, where to start to look for a token in TOKSTR
!                                                               During processing, it is where the current token starts in TOKSTR
!                                                               On return, it is the start of the next token in TOKSTR.

      INTEGER(LONG), INTENT(OUT)               :: IERROR      ! Integer error no. when an error occurs when processing tokens
      INTEGER(LONG), INTENT(OUT)               :: NTOKEN      ! The number of tokens found in this execution
      INTEGER(LONG)                            :: I           ! DO loop index
      INTEGER(LONG)                            :: TOKEN_END   ! Where, in TOKSTR, the end of the current token is located
      INTEGER(LONG)                            :: NUM_TOK_EXP ! No. of tokens we expect (if we find "THRU", we should find 3 tokens)
      INTEGER(LONG)                            :: PRINT_ITEM  ! An item number to print when DEBUG(19) is turned on
      INTEGER(LONG), PARAMETER                 :: SUBR_BEGEND = STOKEN_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      NTOKEN = 0
      IERROR = 0
      ERRTOK(1:) = ' '
      DO I=1,3
         TOKEN(I)(1:) = ' '
         TOKTYP(I)    = 'BLANK   '
      ENDDO
      IF (DEBUG(19) == 1) THEN                             ! Print debug data
         PRINT_ITEM = 0
         CALL DEB_STOKEN ( PRINT_ITEM )
      ENDIF

      NUM_TOK_EXP = 1                                      ! Initialize variables

      IF (DEBUG(19) == 1) THEN                             ! Print debug data
         PRINT_ITEM = 1
         CALL DEB_STOKEN ( PRINT_ITEM )
         WRITE(F06,*)
         WRITE(F06,*) '    Begin outer:DO'
         WRITE(F06,*) '    --------------'
      ENDIF

      DO I=TOKEN_BEG,STRNG_END                             ! Make sure we are positioned at beginning of a token
         IF ((TOKSTR(I:I) == ' ') .OR. (TOKSTR(I:I) == ',')) THEN
            TOKEN_BEG = TOKEN_BEG + 1
         ELSE
            EXIT
         ENDIF
      ENDDO

! Top of loop for processing tokens. If there is only 1 token, we will execute the loop once.
! If there is a triad ("I1 THRU I2") we will process the loop 3 times

outer:DO

         TOKEN_END = STRNG_END                             ! Find end of current token
i_loop1: DO I = TOKEN_BEG,STRNG_END
            IF ((TOKSTR(I:I) == ' ') .OR. (TOKSTR(I:I) == ',')) THEN
               TOKEN_END = I-1
               EXIT i_loop1
            ENDIF
         ENDDO i_loop1

         NTOKEN = NTOKEN + 1
                                                           ! Check for token too long and set error flag if so
         IF ((TOKEN_END - TOKEN_BEG + 1) > MAX_TOKEN_LEN) THEN
            IERROR                         = 1
            ERRTOK(1: )                    = TOKSTR(TOKEN_BEG:TOKEN_END)
            TOKEN(NTOKEN)(1:MAX_TOKEN_LEN) = TOKSTR(TOKEN_BEG:TOKEN_BEG+MAX_TOKEN_LEN-1)
         ELSE
            TOKEN(NTOKEN)(1:) = TOKSTR(TOKEN_BEG:TOKEN_END)
         ENDIF

         CALL TOKCHK ( TOKEN(NTOKEN), TOKTYP(NTOKEN) )     ! Process TOKEN

         IF (DEBUG(19) == 1) THEN                          ! Print debug data
            PRINT_ITEM = 2
            CALL DEB_STOKEN ( PRINT_ITEM )
         ENDIF

         IF (TOKEN(NTOKEN) == 'EXCEPT  ') THEN             ! Set EXCEPT flag if TOKEN = 'EXCEPT  ', but only if 
!                                                            THRU is 'ON ' & EXCEPT is not already 'ON '.
            IF ((THRU == 'ON ') .AND. (EXCEPT == 'OFF')) THEN
               EXCEPT = 'ON '

            ELSE                                           ! Otherwise, set error number and exit outer

               IF      ((THRU == 'ON ') .AND. (EXCEPT == 'ON ')) THEN
                  IERROR = 3
               ELSE IF ((THRU == 'OFF') .AND. (EXCEPT == 'ON ')) THEN
                  IERROR = 4
               ELSE IF ((THRU == 'OFF') .AND. (EXCEPT == 'OFF')) THEN
                  IERROR = 4
               ENDIF

               EXIT outer                                  ! Exit outer due to errors

            ENDIF

         ENDIF

         IF (DEBUG(19) == 1) THEN                          ! Print debug data
            PRINT_ITEM = 3
            CALL DEB_STOKEN ( PRINT_ITEM )
         ENDIF

         IF (TOKEN_END <= STRNG_END) THEN

            TOKEN_BEG = STRNG_END + 1                      ! Find start of next token. Default it to STRNG_END+1 to start with
i_loop2:    DO I = TOKEN_END+1,STRNG_END                   ! just in case we are already at the end of TOKSTR.
               IF ((TOKSTR(I:I) == ' ') .OR. (TOKSTR(I:I) == ',')) THEN
                  CYCLE i_loop2
               ELSE
                  TOKEN_BEG = I                            ! This is where the next token starts
                  EXIT i_loop2
               ENDIF
            ENDDO i_loop2

            IF (TOKEN_BEG <= STRNG_END) THEN

               IF ((NTOKEN == 1) .AND. ((TOKEN_BEG+3) <= STRNG_END)) THEN
                                                           ! Peek ahead to see if 2nd token is "THRU"
                  IF (TOKSTR(TOKEN_BEG:TOKEN_BEG+3)=='THRU') THEN! We found "THRU"
                     NUM_TOK_EXP = 3
                     THRU   = 'ON '
                     EXCEPT = 'OFF'
                     TOKEN_END = TOKEN_BEG + 3

                     IF (DEBUG(19) == 1) THEN
                        PRINT_ITEM = 4
                        CALL DEB_STOKEN ( PRINT_ITEM )
                        WRITE(F06,*)
                        WRITE(F06,*) '    Cycling to top of outer:DO'
                        WRITE(F06,*) '    --------------------------'
                     ENDIF

                     CYCLE outer                           ! CYCLE to read/process 2nd token

                  ELSE

                     EXIT outer                            ! NTOKEN was 1 but we didn't find "THRU" next, so exit outer

                  ENDIF

               ELSE IF ((NTOKEN == 2) .AND. (NUM_TOK_EXP == 3)) THEN
                                                           ! Need to go back for 3rd token since we know we expect 3
                  IF (DEBUG(19) == 1) THEN
                     PRINT_ITEM = 5
                     CALL DEB_STOKEN ( PRINT_ITEM )
                     WRITE(F06,*)
                     WRITE(F06,*) '    Cycling to top of outer:DO'
                     WRITE(F06,*) '    --------------------------'
                  ENDIF

                  CYCLE outer                              ! CYCLE to read/process 3nd token

               ELSE

                  EXIT outer                               ! Need to exit outer (NTOKEN >= 3, etc)

               ENDIF

            ELSE

               EXIT outer                                  ! Need to exit outer (TOKEN_BEG > STRNG_END)

            ENDIF

         ELSE                                              ! TOKEN_END >= STRNG_END, so set TOKEN_BEG = STRNG_END + 1 and quit

            TOKEN_BEG = STRNG_END + 1

            IF ((NUM_TOK_EXP == 3) .AND. (NTOKEN < 3)) THEN
               IERROR = 2                                  ! Error: we found 'THRU' but didnt get 3 tokens
            ENDIF

            EXIT outer                                     ! Need to exit outer (TOKEN_END > STRNG_END)

         ENDIF

      ENDDO outer 
 
      IF (DEBUG(19) == 1) THEN                             ! Print debug data following loop
         PRINT_ITEM = 6
         CALL DEB_STOKEN ( PRINT_ITEM )
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################
 
      CONTAINS

! ##################################################################################################################################

      SUBROUTINE DEB_STOKEN ( PRINT_ITEM )

      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  STOKEN_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEB_STOKEN'

      INTEGER(LONG), INTENT(IN)       :: PRINT_ITEM        ! What item to print
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = STOKEN_BEGEND + 1

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************

      IF      (PRINT_ITEM == 0) THEN
         WRITE(F06,*)
         WRITE(F06,99000) CALLING_SUBR, '"', TOKSTR(1:STRNG_END), '"' 
      ELSE IF (PRINT_ITEM == 1) THEN
         WRITE(F06,99001) TOKEN_BEG     ,THRU,EXCEPT,NUM_TOK_EXP,NTOKEN,(TOKTYP(I),I=1,3),(TOKEN(I),I=1,3),IERROR
      ELSE IF (PRINT_ITEM == 2) THEN
         WRITE(F06,99002) TOKEN_BEG,TOKEN_END,THRU,EXCEPT,NUM_TOK_EXP,NTOKEN,(TOKTYP(I),I=1,3),(TOKEN(I),I=1,3),IERROR
      ELSE IF (PRINT_ITEM == 3) THEN
         WRITE(F06,99003) TOKEN_BEG,TOKEN_END,THRU,EXCEPT,NUM_TOK_EXP,NTOKEN,(TOKTYP(I),I=1,3),(TOKEN(I),I=1,3),IERROR
      ELSE IF (PRINT_ITEM == 4) THEN
         WRITE(F06,99004) TOKEN_BEG,TOKEN_END,THRU,EXCEPT,NUM_TOK_EXP,NTOKEN,(TOKTYP(I),I=1,3),(TOKEN(I),I=1,3),IERROR
      ELSE IF (PRINT_ITEM == 5) THEN
         WRITE(F06,99005) TOKEN_BEG,TOKEN_END,THRU,EXCEPT,NUM_TOK_EXP,NTOKEN,(TOKTYP(I),I=1,3),(TOKEN(I),I=1,3),IERROR
      ELSE IF (PRINT_ITEM == 6) THEN
         WRITE(F06,99006) TOKEN_BEG     ,THRU,EXCEPT,NUM_TOK_EXP,NTOKEN,(TOKTYP(I),I=1,3),(TOKEN(I),I=1,3),IERROR
         WRITE(F06,99999)
      ENDIF

99000 FORMAT(' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////',&
              '/////////////////',/,' Subr STOKEN (called by subr ',A,')  has array TOKSTR at the beg. of subr STOKEN =',/,1X,3A,//&
              ,45x,'Progress of subr STOKEN in parsing part of TOKSTR:',/                                                          &
              ,45x,'--------------------------------------------------',/                                                          &
              ,35x,'   TOKEN   THRU EXCEPT   Num Tokens  TOKTYP1  TOKTYP2  TOKTYP3  TOKEN1   TOKEN2   TOKEN3   IERROR',/           &
              ,35X,' ---------              ------------',/                                                                        &
              ,35X,' Beg   End              Expect Found')

99001 FORMAT(/,' (1) Beg STOKEN-before outer:DO:',I6,6X,2X,A3,2X,A3,7X,I2,3X,I2,3X,6(1X,A8),3X,I2)

99002 FORMAT(  ' (2) After calling TOKCHK      :',I6,I6,2X,A3,2X,A3,7X,I2,3X,I2,3X,6(1X,A8),3X,I2)

99003 FORMAT(  ' (3) After EXCEPT flag check   :',I6,I6,2X,A3,2X,A3,7X,I2,3X,I2,3X,6(1X,A8),3X,I2,/)

99004 FORMAT(  ' (4) CYCLE back to outer DO    :',I6,I6,2X,A3,2X,A3,7X,I2,3X,I2,3X,6(1X,A8),3X,I2,/)

99005 FORMAT(  ' (5) CYCLE back to outer DO    :',I6,I6,2X,A3,2X,A3,7X,I2,3X,I2,3X,6(1X,A8),3X,I2,/)

99006 FORMAT(  ' (6) End STOKEN-after outer DO :',I6,6X,2X,A3,2X,A3,7X,I2,3X,I2,3X,6(1X,A8),3X,I2)

99999 FORMAT(' \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\' &
            ,'\\\\\\\\\\\\\\\\\\\\\\\\', //)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN
! **********************************************************************************************************************************

      END SUBROUTINE DEB_STOKEN

      END SUBROUTINE STOKEN
