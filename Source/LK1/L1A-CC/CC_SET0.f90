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
 
      SUBROUTINE CC_SET0 ( CARD )
 
! Processes Case Control SET cards to determine LSETLN, the length of all SET characters that go into array
! ALL_STES_ARRAY. No error messages are written if, in trying to determine LSETLN, errors occur. This subr returns
! and, when CC_SET runs (when called by LOADC), it will discover the same errors and report them before attempting
! to write characters to ALL_SETS_ARRAY
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_ENTRY_LEN, LSETLN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_SET0_BEGEND
 
      USE CC_SET0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CC_SET0'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=LEN(CARD))        :: CARD1             ! SET card read from C.C deck
      CHARACTER( 8*BYTE)              :: TOKEN             ! An 8 char string from the SET card where the set ID should be located
      CHARACTER( 8*BYTE)              :: TOKTYP            ! The type of the char string TOKEN
 
      INTEGER(LONG)                   :: ECOL      = 0     ! Col, on CARD, where "=" sign is located
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator if there is a cont card (yes if last entry is ",")
      INTEGER(LONG)                   :: MORE      = 0     ! Count of additional bytes to add to LSETLN as this card is read
      INTEGER(LONG)                   :: IOCHK     = 0     ! IOSTAT error number when reading a Case Control card from unit IN1
      INTEGER(LONG)                   :: K         = 0     ! Counter
      INTEGER(LONG)                   :: SETERR    = 0     ! Error indicator as set ID is read
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_SET0_BEGEND
 
      INTRINSIC INDEX
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CARD1 = CARD

! Process SET cards only to count LSETLN. Later processing in CC_SET will put data into ALL_SETS_ARRAY. 
 
! Check for SET cards. These can have continuation if last entry is a comma. Need to make sure that equal sign is
! included for use in subr SETPRO which processes the character string ALL_SETS_ARRAY. A logical SET card consists
! of all physical cards in a set.
 
! Make sure equal sign is in. We use it later in subcase processor
 
      ECOL = INDEX(CARD1(1:),'=')
      IF (ECOL == 0) THEN
         RETURN
      ENDIF
 
! Now find SET ID and check for proper type
 
      TOKEN = '        '
      SETERR = 0
      K = 0
      DO I=5,ECOL-1
         IF (CARD1(I:I) == ' ') CYCLE
         K = K + 1
         IF (K > 8) THEN
            SETERR = 1
            EXIT
         ENDIF
         TOKEN(K:K) = CARD1(I:I)
      ENDDO 
      IF (SETERR == 0) THEN   
         CALL TOKCHK ( TOKEN, TOKTYP )
         IF (TOKTYP /= 'INTEGER ') THEN
            RETURN
         ENDIF
      ENDIF
 
! Find out if there are continuation cards (last non-blank entry on  this card will be a comma if there is more)
 
      DO
 
! Get rid of trailing blanks
 
         MORE = CC_ENTRY_LEN
         DO I=CC_ENTRY_LEN,1,-1
            IF (CARD1(I:I) == ' ') THEN
               MORE = MORE - 1
            ELSE
               EXIT
            ENDIF
         ENDDO
 
         LSETLN = LSETLN + MORE
         
! Find out if last entry is a ','
 
         ICONT = 0
         DO I=CC_ENTRY_LEN,1,-1
            IF (CARD1(I:I) == ' ') THEN
               CYCLE
            ELSE IF (CARD1(I:I) == ',') THEN
               ICONT = 1
               EXIT
            ELSE 
               ICONT = 0
               EXIT
            ENDIF
         ENDDO 
 
         IF (ICONT == 1) THEN
            READ(IN1,101,IOSTAT=IOCHK) CARD1
            IF (IOCHK > 0) THEN
               RETURN
            ENDIF
            CYCLE
         ELSE
            EXIT
         ENDIF
      ENDDO 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)
 
! **********************************************************************************************************************************
 
      END SUBROUTINE CC_SET0 
