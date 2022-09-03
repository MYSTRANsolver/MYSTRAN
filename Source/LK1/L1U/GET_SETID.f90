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
 
      SUBROUTINE GET_SETID ( CARD, SETID )
 
! Gets SET ID from CASE CONTROL cards:  LOAD, METHOD, MPC, NLPARM, SPC, TEMP 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  CC_ENTRY_LEN, FATAL_ERR, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_SETID_BEGEND

      USE GET_SETID_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_SETID'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Case Control card (can be modified by subr CSHIFT, called herein)
      CHARACTER(LEN=LEN(CARD)+1)      :: CARD1             ! CARD shifted to begin in col after "=" sign
      CHARACTER(LEN=LEN(CARD)+1)      :: ERRTOK            ! An output from subr STOKEN, called herein
      CHARACTER( 3*BYTE)              :: EXCEPT            ! An input/output to/from subr STOKEN, called herein
      CHARACTER( 3*BYTE)              :: THRU              ! An inputoutput to/from subr STOKEN, called herein
      CHARACTER( 8*BYTE)              :: TOKEN(3)          ! An output from subr STOKEN, called herein
      CHARACTER( 8*BYTE)              :: TOKTYP(3)         ! An output from subr STOKEN, called herein
 
      INTEGER(LONG), INTENT(OUT)      :: SETID             ! Set ID read from CARD after '=', if CARD contains an integer here.
!                                                            SETID is set to -1 if 'ALL' is found after '=' & to 0 if 'NONE' found
      INTEGER(LONG)                   :: ECOL              ! Column on CARD where '=' is located
      INTEGER(LONG)                   :: IERR              ! An output from subr CSHIFT, called herein
      INTEGER(LONG)                   :: IERROR            ! An output from subr STOKEN, called herein
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error value from READ
      INTEGER(LONG)                   :: ISTART            ! An input to subr STOKEN, called herein
      INTEGER(LONG)                   :: NTOKEN            ! An output from subr STOKEN, called herein
      INTEGER(LONG)                   :: TOKLEN            ! An input to subr STOKEN, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_SETID_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Get SETID
 
      SETID = 0
      CALL CSHIFT ( CARD, '=', CARD1, ECOL, IERR )
      IF (IERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1703)
         WRITE(F06,1703)
      ELSE
         ISTART = 1
         THRU   = 'OFF'
         EXCEPT = 'OFF'
         TOKLEN = CC_ENTRY_LEN
         CALL STOKEN ( SUBR_NAME, CARD1, ISTART, TOKLEN, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )
         IF (NTOKEN > 1) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1704) NTOKEN
            WRITE(F06,1704) NTOKEN
         ELSE
            IF ((ISTART <= TOKLEN) .AND. (CARD1(ISTART:ISTART) /= '$')) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1704) NTOKEN
               WRITE(F06,1704) NTOKEN
            ELSE
               IF (TOKTYP(1) == 'INTEGER ') THEN
                  READ(TOKEN(1),'(I8)',IOSTAT=IOCHK) SETID
                  IF ((IOCHK < 0) .OR. (IOCHK > 0)) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1705)
                     WRITE(F06,1705)
                  ENDIF
                  IF (SETID <= 0) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1706)
                     WRITE(F06,1706)
                  ENDIF
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1707)
                  WRITE(F06,1707)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1703 FORMAT(' *ERROR  1703: CANNOT FIND EQUAL SIGN (=) ON ABOVE CASE CONTROL ENTRY ')

 1704 FORMAT(' *ERROR  1704: ABOVE CASE CONTROL ENTRY MUST HAVE ONLY ONE INTEGER SET NUMBER FOLLOWING THE "=" SIGN.'               &
                    ,/,14X,' ABOVE ENTRY DOES NOT MEET THIS REQUIREMENT (NTOKEN = ', I8, ')')

 1705 FORMAT(' *ERROR  1705: ERROR READING SET ID ON PREVIOUS CASE CONTROL ENTRY')

 1706 FORMAT(' *ERROR  1706: ZERO OR NEGATIVE SET ID NOT ALLOWED ON PREVIOUS CASE CONTROL ENTRY')

 1707 FORMAT(' *ERROR  1707: SET ID MUST BE AN INTEGER OF <= 8 DIGITS')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE GET_SETID
