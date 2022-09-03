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
 
      SUBROUTINE CC_ELDA ( CARD )
 
! Processes Case Control ELDATA cards

! NOTE: The coding assumes that ELDATA(i,BOTH) is only valid for i >= IOUTMIN_FIJ (which is 1) and i <= IOUTMAX_FIJ (which is 5)
! ----
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, WARN_ERR, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_ELDA_BEGEND
      USE MODEL_STUF, ONLY            :  CCELDT
 
      USE CC_ELDA_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'CC_ELDA'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=LEN(CARD))        :: ERRTOK            ! Character string that holds part of an error message from subr STOKEN
      CHARACTER( 3*BYTE)              :: EXCEPT            ! An input/output for subr STOKEN
      CHARACTER( 1*BYTE)              :: FIJFIL_WARN = 'N' ! Set to 'Y' if warning message written for FIJFIL option
      CHARACTER( 3*BYTE)              :: THRU              ! An input/output for subr STOKEN
      CHARACTER( 8*BYTE)              :: TOKEN(3)          ! Char string output from subr STOKEN, called herein
      CHARACTER(LEN=LEN(CARD))        :: TOKSTR            ! Character string to tokenize 
      CHARACTER( 8*BYTE)              :: TOKTYP(3)         ! Type of the char TOKEN's output from subr STOKEN, called herein
 
      INTEGER(LONG)                   :: ICOL1       = 0   ! Location, in CARD, where "(" begins
      INTEGER(LONG)                   :: ICOL2       = 0   ! Location, in CARD, where ")" begins
      INTEGER(LONG)                   :: IERROR            ! An output from subr STOKEN, called herein
      INTEGER(LONG)                   :: IOCHK       = 0   ! IOSTAT error number when reading data from a file
      INTEGER(LONG)                   :: IOFF        = 8   ! An index offset into array CCELDT
      INTEGER(LONG)                   :: IOUT              ! Integer number, in ELDATA request, indicating type of output request
      INTEGER(LONG), PARAMETER        :: IOUTMIN_BUG = 0   ! Min val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA print options)
      INTEGER(LONG), PARAMETER        :: IOUTMAX_BUG = 9   ! Max val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA print options)
      INTEGER(LONG), PARAMETER        :: IOUTMIN_FIJ = 2   ! Min val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA file  options)
      INTEGER(LONG), PARAMETER        :: IOUTMAX_FIJ = 6   ! Max val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA file  options)
      INTEGER(LONG)                   :: NTOKEN            ! An output from subr STOKEN, called herein
      INTEGER(LONG)                   :: SETID       = 0   ! Set ID on this Case Control card
      INTEGER(LONG)                   :: STRNG_LEN   = 0   ! Length of character string between "()" in the ELDATA card
      INTEGER(LONG)                   :: TOKEN_BEG   = 0   ! An input to subr STOKEN, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_ELDA_BEGEND
 
      INTRINSIC INDEX
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process ELDATA cards.
 
! Processes element debug output or disk file output. Card format is:

!           ELDATA(i,PRINT or FIJFIL or BOTH) = NONE or ALL or SETID

! where i is 0,1,2,3,4 or 5 and either PRINT or FIJFIL or BOTH must be selected. i determines kind of elem output.
 
! First get i,PRINT, FIJFIL between (). CCELDT(j) array will be set equal to:

!       1)  -1   if "ALL" is requested,
!       2)   0   if "NONE" is requested, or
!       3) SETID if a set ID is requested

! CCELDT( 0) is CC requests for print to BUGFIL of elem geometric data
! CCELDT( 1) is CC requests for print to BUGFIL of elem property and material info
! CCELDT( 2) is CC requests for print to BUGFIL of elem thermal and pressure matrices    : PTE, PPE
! CCELDT( 3) is CC requests for print to BUGFIL of elem mass matrix                      : ME
! CCELDT( 4) is CC requests for print to BUGFIL of elem stiffness matrix                 : KE
! CCELDT( 5) is CC requests for print to BUGFIL of elem stress & strain recovery matrices: SEi, STEi, BEi
! CCELDT( 6) is CC requests for print to BUGFIL of elem displacement and load matrices   : UEL, PEL (all subcases)
! CCELDT( 7) is CC requests for print to BUGFIL of elem shape fcns and Jacobian matrices
! CCELDT( 8) is CC requests for print to BUGFIL of elem strain-displacement matrices
! CCELDT( 9) is CC requests for print to BUGFIL of elem checks on strain-displ matrices for RB motion & constant strain
! CCELDT(10) is CC requests for write to F21FIL unformatted file of element              : PTE, PPE
! CCELDT(11) is CC requests for write to F22FIL unformatted file of element              : ME
! CCELDT(12) is CC requests for write to F23FIL unformatted file of element              : KE
! CCELDT(13) is CC requests for write to F24FIL unformatted file of element              : SEi, STEi, BEi
! CCELDT(14) is CC requests for write to F25FIL unformatted file of element              : UEL, PEL (all subcases)
! CCELDT(15) is CC requests currently not used

! CCELDT will be processed in subroutine SCPRO to fill in output array ELDAT(k) with elements whose output was
! requested by setting bit j-1 in array ELDAT to 1 if CCELDT(j) is nonzero

! Find out if "NONE", "ALL" or SETID
 
      CALL GET_ANSID ( CARD, SETID )   
 
! Get data in between ()
 
      ICOL1  = INDEX(CARD(1:),'(')
      ICOL2  = INDEX(CARD(1:),')')
      STRNG_LEN = ICOL2 - ICOL1 - 1
      IF ((ICOL1 == 0) .OR. (ICOL2 == 0) .OR. (ICOL2 < ICOL1)) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1201)
         WRITE(F06,1201)
         RETURN
      ELSE IF (STRNG_LEN < 1) THEN 
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1202)
         WRITE(F06,1202)
         RETURN
      ELSE
 
! Read 1st token after opening paren: "(". Need to make sure that the token is:

!  1) An integer                               : TOKTYP(1) /= 'INTEGER ',
!  2) Integer has <= 8 digits                  : IERROR /= 0, and
!  3) Integer starts before closing paren, ")" : 
 
         TOKSTR(1:STRNG_LEN) = CARD(ICOL1+1:ICOL2-1)
         TOKEN_BEG = 1
         THRU   = 'OFF'
         EXCEPT = 'OFF'
         CALL STOKEN ( SUBR_NAME, TOKSTR, TOKEN_BEG, STRNG_LEN, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )
         IF ((TOKTYP(1) /= 'INTEGER ') .OR. (IERROR /= 0)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1281)
            WRITE(F06,1281)
            RETURN
         ELSE
            READ(TOKEN(1),'(I8)',IOSTAT=IOCHK) IOUT
            IF (IOCHK > 0) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1264)
               WRITE(F06,1264)
            ENDIF
            IF (TOKEN_BEG <= STRNG_LEN) THEN 
               THRU   = 'OFF'
               EXCEPT = 'OFF'

               CALL STOKEN ( SUBR_NAME, TOKSTR, TOKEN_BEG, STRNG_LEN, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )
               IF (IERROR == 1) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1283) 'ELDATA', ERRTOK
                  WRITE(F06,1283) 'ELDATA', ERRTOK
               ELSE
                  IF      (TOKTYP(1) == 'PRINT   ') THEN
                     IF ((IOUT < IOUTMIN_BUG) .OR. (IOUT > IOUTMAX_BUG)) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1282) IOUTMIN_BUG, IOUTMAX_BUG, 'PRINT'
                        WRITE(F06,1282) IOUTMIN_BUG, IOUTMAX_BUG, 'PRINT'
                        RETURN
                     ENDIF
                     CCELDT(IOUT) = SETID
                  ELSE IF (TOKTYP(1) == 'FIJFIL  ') THEN
                     IF ((IOUT < IOUTMIN_FIJ) .OR. (IOUT > IOUTMAX_FIJ)) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1282) IOUTMIN_FIJ, IOUTMAX_FIJ, 'FIJFIL'
                        WRITE(F06,1282) IOUTMIN_FIJ, IOUTMAX_FIJ, 'FIJFIL'
                        RETURN
                     ENDIF
                     CCELDT(IOUT+IOFF) = SETID
                  ELSE IF (TOKTYP(1) == 'BOTH    ') THEN
                     IF ((IOUT < IOUTMIN_FIJ) .OR. (IOUT > IOUTMAX_FIJ)) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1282) IOUTMIN_FIJ, IOUTMAX_FIJ, 'BOTH'
                        WRITE(F06,1282) IOUTMIN_FIJ, IOUTMAX_FIJ, 'BOTH'
                        RETURN
                     ENDIF
                     CCELDT(IOUT)      = SETID
                     CCELDT(IOUT+IOFF) = SETID
                  ELSE
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1284) TOKEN(1)
                     WRITE(F06,1284) TOKEN(1)
                  ENDIF
               ENDIF
            ELSE                                        ! TOKEN_BEG > STRNG_LEN, so quit
               CCELDT(IOUT) = SETID                     ! Default for ELDATA output is PRINT
            ENDIF
            IF (TOKEN_BEG <= STRNG_LEN) THEN            ! Error: there shouldn't be more than the integer, PRINT, FIJFIL or BOTH
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,1285) CARD,TOKEN(1)
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,1285) CARD,TOKEN(1)
               ENDIF
               IF (FIJFIL_WARN == 'N') THEN
                  WARN_ERR = WARN_ERR + 1
               ENDIF
            ENDIF 
         ENDIF
      ENDIF
 
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! As of 03/07/2020 there is some error in the calculation of the check on strain displ matrices for RB and constant strain modes
! of displacement. Therefore, this check is temporarily suspended

      if (cceldt(9) /= 0) then
         write(f06,12344)
         write(f06,12345)
         write(f06,12344)
         write(f06,*)
      endif

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1264 FORMAT(' *ERROR  1264: ERROR READING ELDATA CASE CONTROL ENTRY FOR THE INTEGER DESCRIBING THE TYPE OF OUTPUT. ENTRY IGNORED')

 1201 FORMAT(' *ERROR  1201: COULD NOT FIND MATCHING PARENTHESES () FOLLOWING ELDATA CASE CONTROL COMMAND.'                        &
                    ,/,14X,' THE PARENTHESES ARE FOR THE PURPOSE OF CONTAINING THE SPECIFIC ELDATA OUTPUT REQUEST')

 1202 FORMAT(' *ERROR  1202: THERE IS NO DATA BETWEEN THE MATCHING PARENTHESES FOLLOWING THE ELDATA COMMAND')

 1281 FORMAT(' *ERROR  1281: THE FIRST ENTRY FOLLOWING THE OPENING PARENTHESIS OF THE ELDATA CASE CONTROL ENTRY:'                  &
                    ,/,14X,' (1) MUST BE AN INTEGER,'                                                                              &
                    ,/,14X,' (2) MUST HAVE LESS THAN 8 DIGITS,'                                                                    &
                    ,/,14X,' (3) AND MUST BEGIN BEFORE CLOSING PARENTHESIS')

 1282 FORMAT(' *ERROR  1282: THE INTEGER DESCRIBING THE TYPE OF OUTPUT FOR CASE CONTROL ELDATA ENTRY MUST:'                        &
                    ,/,14X,' HAVE A VALUE >= ',I2,' AND <= ',I2,' FOR OPTION ',A)

 1283 FORMAT(' *ERROR  1283: ENTRIES ON ',A,' CASE CONTROL ENTRY MUST BE <= 8 CHARACTERS. THE FOLLOWING ENTRY EXCEEDS THIS:'       &
                    ,/,15X,A,/)

 1284 FORMAT(' *ERROR  1284: DATA BETWEEN PARENS FOLLOWING INTEGER ON ELFORCE/FORCE CASE CONTROL ENTRY MUST BE "PRINT", "FIJFIL",',&
                           ' OR "BOTH".'                                                                                           &
                    ,/,14X,' ENTRY = ',A8,' NOT RECOGNIZED')

 1285 FORMAT(' *WARNING    : ERROR READING ELDATA CASE CONTROL ENTRY:',/,15X,A,/,15X,'DATA BETWEEN PARENS FOLOWING INTEGER SHOULD',&
                           ' ONLY BE ONE OF: "PRINT", "FIJFIL" OR "BOTH". FIRST ENTRY, ',A,', WILL BE USED')

12344 format('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')

12345 format('NOTE: as of 03/07/2020 the check on strain-displacement matrices using Case Control ELDATA(9) is suspended',/,6x,    &
                   'until an error in the calculation is fixed. This can be overridden with DEBUG(202) > 0')

! **********************************************************************************************************************************
 
      END SUBROUTINE CC_ELDA
