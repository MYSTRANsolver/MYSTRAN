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
 
      SUBROUTINE CC_OUTPUTS ( CARD, WHAT, SETID )
 
! Process the character string in parens for the following Case Control output request entries (e.g. SORT1, PRINT, etc)

!       ACCE()
!       DISP()
!       ELFO()
!       GPFO()
!       MPCF()
!       OLOA()
!       SPCF()
!       STRE()
!       STRN()

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_CMD_DESCRIBERS, LSUB, NCCCD, NSUB 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_OUTPUTS_BEGEND
 
      USE CC_OUTPUTS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CC_OUTPUTS'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: WHAT              ! Which CC type output to process (e.g., DISP, SPCF, etc)
      CHARACTER(LEN=LEN(CARD))        :: CHAR_STRING       ! Character string between parens () if it exists
 
      INTEGER(LONG), INTENT(OUT)      :: SETID             ! Set ID on this Case Control card
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: ICOL1       = 0   ! Location, in CARD, where "(" begins
      INTEGER(LONG)                   :: ICOL2       = 0   ! Location, in CARD, where ")" begins
      INTEGER(LONG)                   :: IERR        = 0   ! Error designator from subr PARSE_CSV_STRING
      INTEGER(LONG)                   :: NUM_WORDS   = 0   ! Number of words in the string between parens (), if present
      INTEGER(LONG)                   :: STRING_LEN  = 0   ! Length of character string between "()" in the ELDATA card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_OUTPUTS_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      IERR = 0

      CHAR_STRING(1:) = ' '

      DO I=1,NCCCD
         CC_CMD_DESCRIBERS(I)(1:) = ' '
      ENDDO
 
! Find out if "NONE", "ALL" or SETID
 
      CALL GET_ANSID ( CARD, SETID )   
 
! Find out if there is data enclosed between parens (). If so, STRING_LEN will be > 0.
 
      ICOL1 = INDEX(CARD(1:),'(')
      ICOL2 = INDEX(CARD(1:),')')
      STRING_LEN = ICOL2 - ICOL1 - 1

! If there is a string of data in between the parens then parse it to get all of the words and put them into array CC_CMD_DESCRIBERS

      IF (STRING_LEN > 0) THEN                             ! There is a pair () of parens, so we will have to parse data between ()
         CHAR_STRING(1:STRING_LEN) = CARD(ICOL1+1:ICOL2-1)
         CALL PARSE_CHAR_STRING ( CHAR_STRING, STRING_LEN, NCCCD, LEN(CC_CMD_DESCRIBERS), NUM_WORDS, CC_CMD_DESCRIBERS, IERR )
      ELSE
         RETURN
      ENDIF

! Check that command descriptors (e.g. "SORT1") are allowable

      IF (IERR == 0) THEN
         IF (NUM_WORDS <= NCCCD) THEN
            CALL CHK_CC_CMD_DESCRIBERS ( WHAT, NUM_WORDS )
         ELSE
            CALL CHK_CC_CMD_DESCRIBERS ( WHAT, NCCCD     )
         ENDIF
      ELSE
         RETURN
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE CC_OUTPUTS 
