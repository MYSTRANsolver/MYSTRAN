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

      SUBROUTINE TOKCHK ( TOKEN, TOKTYPE )

! Determines the type of an 8 character input string, TOKEN. Output TOKTYPE is:

!   TOKTYPE = 'UNKNOWN ' if input TOKEN is none of the below
!   TOKTYPE = 'COMMENT ' if input TOKEN is $    
!   TOKTYPE = 'BLANK   ' if input TOKEN is blank
!   TOKTYPE = 'INTEGER ' if input TOKEN is an integer
!   TOKTYPE = 'FL PT   ' if input TOKEN is a floating point number
!   TOKTYPE = 'ALL     ' if input TOKEN is "ALL"
!   TOKTYPE = 'THRU    ' if input TOKEN is "THRU"
!   TOKTYPE = 'NONE    ' if input TOKEN is "NONE"
!   TOKTYPE = 'PRINT   ' if input TOKEN is "PRINT"
!   TOKTYPE = 'PUNCH   ' if input TOKEN is "PUNCH"
!   TOKTYPE = 'BOTH    ' if input TOKEN is "BOTH"
!   TOKTYPE = 'EXCEPT  ' if input TOKEN is "EXCEPT"
!   TOKTYPE = 'FIJFIL  ' if input TOKEN is "FIJFIL"

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06

      USE TOKCHK_USE_IFs

      IMPLICIT NONE

      CHARACTER(8*BYTE), INTENT(IN)   :: TOKEN             ! The input character string
      CHARACTER(8*BYTE), INTENT(OUT)  :: TOKTYPE           ! The type of TOKEN (see above)

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IEND              ! Col number (1-8) in TOKEN where TOKEN ends
      INTEGER(LONG)                   :: ISTART            ! Col number (1-8) in TOKEN where TOKEN begins

! **********************************************************************************************************************************
      TOKTYPE = 'UNKNOWN '

! Check for blank token

      IF (TOKEN == '        ') THEN
         TOKTYPE = 'BLANK   '
         RETURN
      ENDIF

! Check for comment token

      IF ((TOKEN(1:1) == '$') .OR. (TOKEN(2:2) == '$') .OR. (TOKEN(3:3) == '$') .OR. (TOKEN(4:4) == '$') .OR.                      &
          (TOKEN(5:5) == '$') .OR. (TOKEN(6:6) == '$') .OR. (TOKEN(7:7) == '$') .OR. (TOKEN(8:8) == '$')) THEN
         TOKTYPE = 'COMMENT '
         RETURN
      ENDIF

! Check for an integer. First, skip leading and trailing blanks then read digits until another blank (or field end)
! is encountered.

      IF (TOKTYPE == 'UNKNOWN') THEN

         ISTART = 1
         DO I=1,8
            IF(TOKEN(I:I) == ' ') THEN
               ISTART = ISTART + 1
               CYCLE
            ELSE
               EXIT
            ENDIF
         ENDDO

         IEND = 8
         DO I=8,1,-1
            IF(TOKEN(I:I) == ' ') THEN
               IEND = IEND - 1
               CYCLE
            ELSE
               EXIT
            ENDIF
         ENDDO

         IF ((TOKEN(ISTART:ISTART) == '+') .OR. (TOKEN(ISTART:ISTART) == '-')) THEN 
            ISTART = ISTART+1
         ENDIF
         DO I=ISTART,IEND
            IF ((TOKEN(I:I)=='1') .OR. (TOKEN(I:I)=='2') .OR. (TOKEN(I:I)=='3') .OR. (TOKEN(I:I)=='4') .OR.  &  
                (TOKEN(I:I)=='5') .OR. (TOKEN(I:I)=='6') .OR. (TOKEN(I:I)=='7') .OR. (TOKEN(I:I)=='8') .OR.  &
                (TOKEN(I:I)=='9') .OR. (TOKEN(I:I)=='0')) THEN 
               TOKTYPE = 'INTEGER '
            ELSE
               TOKTYPE = 'UNKNOWN '
               EXIT
            ENDIF
         ENDDO
         IF (TOKTYPE == 'INTEGER ') THEN
            RETURN
         ENDIF

      ENDIF 

! Check for floating point number

      IF (TOKTYPE == 'UNKNOWN') THEN                       ! First find a '.'
         DO I = 1,8
            IF (TOKEN(I:I) == '.') THEN
               TOKTYPE = '?FL PT? '
               EXIT
            ENDIF
         ENDDO   

         IF (TOKTYPE == '?FL PT? ') THEN                   ! Check characters for floating point number
            CONTINUE
            DO I = 1,8
               IF ((TOKEN(I:I)=='1') .OR. (TOKEN(I:I)=='2') .OR. (TOKEN(I:I)=='3') .OR. (TOKEN(I:I)=='4') .OR.  &
                   (TOKEN(I:I)=='5') .OR. (TOKEN(I:I)=='6') .OR. (TOKEN(I:I)=='7') .OR. (TOKEN(I:I)=='8') .OR.  &
                   (TOKEN(I:I)=='9') .OR. (TOKEN(I:I)=='0') .OR. (TOKEN(I:I)=='+') .OR. (TOKEN(I:I)=='-') .OR.  &
                   (TOKEN(I:I)=='D') .OR. (TOKEN(I:I)=='E') .OR. (TOKEN(I:I)=='.') .OR. (TOKEN(I:I)==' ')) THEN
                   TOKTYPE = 'FL PT   '
                   CYCLE
               ELSE
                  TOKTYPE = 'UNKNOWN '
                  EXIT
               ENDIF
            ENDDO   
            IF (TOKTYPE == 'FL PT   ') THEN
               RETURN
            ENDIF
         ENDIF

      ENDIF

! Check for 'ALL'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='A') .OR. (TOKEN(1:1)=='a')) THEN
            IF            ((TOKEN(2:2)=='L') .OR. (TOKEN(2:2)=='l')) THEN
               IF         ((TOKEN(3:3)=='L') .OR. (TOKEN(3:3)=='l')) THEN
                  TOKTYPE = 'ALL     '
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'THRU'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='T') .OR. (TOKEN(1:1)=='t')) THEN
            IF            ((TOKEN(2:2)=='H') .OR. (TOKEN(2:2)=='h')) THEN
               IF         ((TOKEN(3:3)=='R') .OR. (TOKEN(3:3)=='r')) THEN
                  IF      ((TOKEN(4:4)=='U') .OR. (TOKEN(4:4)=='u')) THEN
                     TOKTYPE = 'THRU    '
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'NONE'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='N') .OR. (TOKEN(1:1)=='n')) THEN
            IF            ((TOKEN(2:2)=='O') .OR. (TOKEN(2:2)=='o')) THEN
               IF         ((TOKEN(3:3)=='N') .OR. (TOKEN(3:3)=='n')) THEN
                  IF      ((TOKEN(4:4)=='E') .OR. (TOKEN(4:4)=='e')) THEN
                     TOKTYPE = 'NONE    '
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'PRINT'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='P') .OR. (TOKEN(1:1)=='p')) THEN
            IF            ((TOKEN(2:2)=='R') .OR. (TOKEN(2:2)=='r')) THEN
               IF         ((TOKEN(3:3)=='I') .OR. (TOKEN(3:3)=='i')) THEN
                  IF      ((TOKEN(4:4)=='N') .OR. (TOKEN(4:4)=='n')) THEN
                     IF   ((TOKEN(5:5)=='T') .OR. (TOKEN(5:5)=='t')) THEN
                        TOKTYPE = 'PRINT   '
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'PUNCH'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='P') .OR. (TOKEN(1:1)=='p')) THEN
            IF            ((TOKEN(2:2)=='U') .OR. (TOKEN(2:2)=='u')) THEN
               IF         ((TOKEN(3:3)=='N') .OR. (TOKEN(3:3)=='n')) THEN
                  IF      ((TOKEN(4:4)=='C') .OR. (TOKEN(4:4)=='c')) THEN
                     IF   ((TOKEN(5:5)=='H') .OR. (TOKEN(5:5)=='h')) THEN
                        TOKTYPE = 'PUNCH   '
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'BOTH'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='B') .OR. (TOKEN(1:1)=='b')) THEN
            IF            ((TOKEN(2:2)=='O') .OR. (TOKEN(2:2)=='o')) THEN
               IF         ((TOKEN(3:3)=='T') .OR. (TOKEN(3:3)=='t')) THEN
                  IF      ((TOKEN(4:4)=='H') .OR. (TOKEN(4:4)=='h')) THEN
                     TOKTYPE = 'BOTH    '
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'EXCEPT'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='E') .OR. (TOKEN(1:1)=='e')) THEN
            IF            ((TOKEN(2:2)=='X') .OR. (TOKEN(2:2)=='x')) THEN
               IF         ((TOKEN(3:3)=='C') .OR. (TOKEN(3:3)=='c')) THEN
                  IF      ((TOKEN(4:4)=='E') .OR. (TOKEN(4:4)=='e')) THEN
                     IF   ((TOKEN(5:5)=='P') .OR. (TOKEN(5:5)=='p')) THEN
                        IF((TOKEN(6:6)=='T') .OR. (TOKEN(6:6)=='t')) THEN
                           TOKTYPE = 'EXCEPT  '
                           RETURN
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! Check for 'FIJFIL'

      IF (TOKTYPE == 'UNKNOWN') THEN
         IF               ((TOKEN(1:1)=='F') .OR. (TOKEN(1:1)=='f')) THEN
            IF            ((TOKEN(2:2)=='I') .OR. (TOKEN(2:2)=='i')) THEN
               IF         ((TOKEN(3:3)=='J') .OR. (TOKEN(3:3)=='j')) THEN
                  IF      ((TOKEN(4:4)=='F') .OR. (TOKEN(4:4)=='f')) THEN
                     IF   ((TOKEN(5:5)=='I') .OR. (TOKEN(5:5)=='i')) THEN
                        IF((TOKEN(6:6)=='L') .OR. (TOKEN(6:6)=='l')) THEN
                           TOKTYPE = 'FIJFIL  '
                           RETURN
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

! **********************************************************************************************************************************

      END SUBROUTINE TOKCHK
