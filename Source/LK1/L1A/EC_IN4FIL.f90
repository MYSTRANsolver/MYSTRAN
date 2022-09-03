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
 
      SUBROUTINE EC_IN4FIL ( CARD )
 
! Processes Executive Control IN4 entries that define IN4 files to be read
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, FILE_NAM_MAXLEN, IN4FIL, IN4FIL_NUM, NUM_IN4_FILES
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_TOKEN_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  EC_IN4FIL_BEGEND
 
      USE EC_IN4FIL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EC_IN4FIL'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=LEN(CARD))        :: CARD1             ! Part of CARD
      CHARACTER(LEN=LEN(CARD))        :: CARD2             ! Part of CARD
      CHARACTER( 8*BYTE)              :: TOKEN(3)          ! Char string output from subr STOKEN, called herein
      CHARACTER( 8*BYTE)              :: TOKTYP(3)         ! Type of the char TOKEN's output from subr STOKEN, called herein
 
      INTEGER(LONG)                   :: DOLLAR_COL        ! Col, on CARD, where "$" sign is located
      INTEGER(LONG)                   :: ECOL      = 0     ! Col, on CARD, where "=" sign is located
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IEND              ! 
      INTEGER(LONG)                   :: ISTART            ! 
      INTEGER(LONG)                   :: JEND      = 0     ! 
      INTEGER(LONG)                   :: K         = 0     ! Counter
      INTEGER(LONG)                   :: SETERR    = 0     ! Error indicator as set ID is read
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EC_IN4FIL_BEGEND
 
      INTRINSIC INDEX
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process SET cards 

      CARD1 = CARD 

! Find equal sign
 
      ECOL = INDEX(CARD1(1:),'=')
      IF (ECOL == 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1267)
         WRITE(F06,1267)
         RETURN
      ENDIF
 
! Now find IN4FIL_NUM and check for proper type ('INTEGER ') and value within limits
 
      NUM_IN4_FILES = NUM_IN4_FILES + 1
      TOKEN(1)  = '        '
      SETERR    = 0
      K = 0
      DO I=5,ECOL-1
         IF (CARD1(I:I) == ' ') CYCLE
         K = K + 1
         IF (K > MAX_TOKEN_LEN) THEN
            SETERR = 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1272) MAX_TOKEN_LEN
            WRITE(F06,1272) MAX_TOKEN_LEN
            EXIT
         ENDIF
         TOKEN(1)(K:K) = CARD1(I:I)
      ENDDO 
      IF (SETERR == 0) THEN   
         CALL TOKCHK ( TOKEN(1), TOKTYP(1) )
         IF (TOKTYP(1) == 'INTEGER ') THEN
            READ(TOKEN(1),'(I8)') IN4FIL_NUM(NUM_IN4_FILES)
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1273)
            WRITE(F06,1273)
            RETURN
         ENDIF
      ENDIF
  
! Remainder of card has IN4 file name (and also possibly leading/trailing blanks and trailing comment)

      CARD2 = CARD1(ECOL+1:)
      DOLLAR_COL = INDEX(CARD2(1:),'$')

      DO I=1,FILE_NAM_MAXLEN
         IF (CARD2(I:I) /= ' ') THEN
            ISTART = I
            EXIT
         ENDIF
      ENDDO

      JEND = FILE_NAM_MAXLEN
      IF (DOLLAR_COL > 0) THEN
         JEND = DOLLAR_COL-1
      ENDIF

      DO I=JEND,ISTART,-1
         IF (CARD2(I:I) == ' ') THEN
            CYCLE
         ELSE
            IEND = I
            EXIT
         ENDIF
      ENDDO

      IN4FIL(NUM_IN4_FILES)(1:) = CARD2(ISTART:IEND)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1267 FORMAT(' *ERROR  1267: FORMAT ERROR ON CASE CONTROL SET ENTRY. COULD NOT FIND EQUAL (=) SIGN')

 1272 FORMAT(' *ERROR  1272: SET ID ON CASE CONTROL SET ENTRY CANNOT BE HAVE MORE THAN ',I4,' DIGITS')

 1273 FORMAT(' *ERROR  1273: THE SET ID ON ABOVE CASE CONTROL SET ENTRY IS NOT AN INTEGER')

97865 format(' I, DOLLAR_COL, JEND, ISTART, IEND, IN4FIL(I) = ',5i8,'"',a,'"')

! **********************************************************************************************************************************
 
      END SUBROUTINE EC_IN4FIL 
