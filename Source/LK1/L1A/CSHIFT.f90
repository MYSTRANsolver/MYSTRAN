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
 
      SUBROUTINE CSHIFT ( CARD_IN, CHAR, CARD_SHIFTED, CHAR_COL, IERR )
 
! Shifts card string data on CARD_IN so that the data after character CHAR is shifted to start in col 1 (with blanks
! between CHAR and data on CARD_IN deleted). An error is indicated if CHAR is not found. The special case of CHAR = ' '
! input to this subr indicates we want to shift the card to begin in column 1
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CSHIFT_BEGEND
 
      USE CSHIFT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM))         :: SUBR_NAME = 'CSHIFT'
      CHARACTER(LEN=*) , INTENT(IN)            :: CARD_IN           ! Input Case Control card
      CHARACTER(LEN=LEN(CARD_IN)) , INTENT(OUT):: CARD_SHIFTED      ! C.C. card shifted to begin in 1st nonblank col after CHAR_COL
      CHARACTER(1*BYTE), INTENT(IN)            :: CHAR              ! Character to find in CARD
 
      INTEGER(LONG), INTENT(OUT)               :: IERR              ! Error indicator. If CHAR not found, IERR set to 1
      INTEGER(LONG), INTENT(OUT)               :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG)                            :: CARD_IN_LEN       ! Length of CARD 
      INTEGER(LONG)                            :: I                 ! DO loop index
      INTEGER(LONG)                            :: ISTART            ! The col on CARD where nonblank data begins after CHAR_COL
      INTEGER(LONG), PARAMETER                 :: SUBR_BEGEND = CSHIFT_BEGEND

      INTRINSIC INDEX
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CARD_IN_LEN = LEN(CARD_IN)

! Initialize CARD_SHIFTED

      DO I=1,CARD_IN_LEN
         CARD_SHIFTED(I:I) = ' '
      ENDDO
 
      IERR = 0
      IF (CHAR == ' ') THEN                                ! Special case: shift card to begin in 1st nonblank col after col 1
         CHAR_COL = 0
      ELSE                                                 ! Regular case: shift card to begin in 1st nonblank col after CHAR_COL+1
         CHAR_COL = INDEX(CARD_IN(1:),CHAR)
         IF (CHAR_COL == 0) THEN
            IERR = 1
            RETURN
         ENDIF
      ENDIF

      ISTART = 1
      DO I=CHAR_COL+1,CARD_IN_LEN                          ! Skip over blanks to calc ISTART, the col in CARD_IN to shift to
         IF ((CARD_IN(I:I) == ' ') .OR. (CARD_IN(I:I) == ACHAR(9))) THEN
            CYCLE
         ELSE
            ISTART = I
            EXIT
         ENDIF
      ENDDO
      CARD_SHIFTED(1:) = CARD_IN(ISTART:CARD_IN_LEN)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE CSHIFT
