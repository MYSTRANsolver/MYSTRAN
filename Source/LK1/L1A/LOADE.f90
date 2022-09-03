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
 
      SUBROUTINE LOADE
 
! LOADE reads in the EXEC CONTROL DECK
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN, CHKPNT, FATAL_ERR, WARN_ERR, JCARD_LEN, JF,     &
                                         PROG_NAME, SOL_NAME, RESTART
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      use debug_parameters, only      :  debug

      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_MYSTRAN_NAMES, ACT_OU4_OUTPUT_NAMES, ALLOW_OU4_MYSTRAN_NAMES,                     &
                                         ALLOW_OU4_OUTPUT_NAMES, OU4_PART_MAT_NAMES, OU4_PART_VEC_NAMES, NUM_OU4_VALID_NAMES

      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADE_BEGEND
 
      USE LOADE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LOADE'
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD              ! Exec Control deck card
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD1             ! CARD shifted to begin in col 1
      CHARACTER(LEN=JCARD_LEN)        :: CHARFLD           ! Character field used when suvr I4FLD is called
      CHARACTER(12*BYTE)              :: DECK_NAME = 'EXEC CONTROL'
      CHARACTER( 1*BYTE)              :: DOLLAR_WARN       ! Indicator of whether there was a $ sign in col 1
      CHARACTER( 4*BYTE), PARAMETER   :: END_CARD  = 'CEND'
      CHARACTER(LEN=EC_ENTRY_LEN)     :: ERRTOK            ! An error message that may be returned from subr STOKEN
      CHARACTER( 3*BYTE)              :: EXCEPT    = 'OFF' ! An input/output variable for subr STOKEN, called herein

                                                           
      CHARACTER( 1*BYTE)              :: PRT_OU4_VALID_NAMES! If 'Y', print valid OUTPUT4 matrix names

      CHARACTER( 3*BYTE)              :: THRU      = 'OFF' ! An input/output variable for subr STOKEN, called herein
      CHARACTER( 8*BYTE)              :: TOKEN(3)          ! Character tokens returned from subr STOKEN
      CHARACTER( 8*BYTE)              :: TOKTYP(3)         ! Description of the TOKEN's returned from subr STOKEN

                                                           ! Proper SOL number
      CHARACTER( 10*BYTE)             :: SOL_NUM_SHOULD_BE = '1, 3 or 31'
      CHARACTER( 1*BYTE)              :: ANY_OU4_NAME_BAD  ! 'Y'/'N' if requested OUTPUT4 matrix name is valid
 
      INTEGER(LONG)                   :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG)                   :: EC_OUTPUT4_ERR = 0! Count of errors when readig OUTPUT4 entries
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERR           = 0! Error indicator.
      INTEGER(LONG)                   :: JERR           = 0! Error indicator.
      INTEGER(LONG)                   :: IERROR            ! An error number returned from subr STOKEN
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when reading a Case Control card from unit IN1
      INTEGER(LONG)                   :: ISTART            ! An input/output for subr STOKEN (where a token begins in CARD) 
      INTEGER(LONG)                   :: NTOKEN            ! An output from subr STOKEN (how many tokens were read)
      INTEGER(LONG)                   :: SOL_INT           ! Integer value read from an Exec Control SOL entry
      INTEGER(LONG)                   :: TOKLEN            ! Length of character string sent to subr STOKEN (= LEN(CARD))
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADE_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DOLLAR_WARN = 'N'

      EC_OUTPUT4_ERR      = 0
      ANY_OU4_NAME_BAD    = 'Y'
      PRT_OU4_VALID_NAMES = 'N'

! Initialize arrays used in OUTPUT4 processing

      DO I=1, NUM_OU4_VALID_NAMES
         ACT_OU4_MYSTRAN_NAMES(I)(1:) = ' '
         ACT_OU4_OUTPUT_NAMES(I)(1:)  = ' '
         OU4_PART_MAT_NAMES(I,1)(1:)    = ' '
         OU4_PART_VEC_NAMES(I,1)(1:)    = ' '
         OU4_PART_VEC_NAMES(I,2)(1:)    = ' '
      ENDDO

! Process EXECUTIVE CONTROL DECK
 
      DO
 
         READ(IN1,101,IOSTAT=IOCHK) CARD
 
         IF (IOCHK < 0) THEN                               ! Quit if EOF/EOR occurs during read
            WRITE(ERR,1011) END_CARD
            WRITE(F06,1011) END_CARD
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

         IF (IOCHK > 0) THEN                               ! Check if error occurs during read.
            WRITE(ERR,1010) DECK_NAME
            WRITE(F06,1010) DECK_NAME
            WRITE(F06,'(A)') CARD
            FATAL_ERR = FATAL_ERR + 1
            CYCLE
         ENDIF

         WRITE(F06,101) CARD

         CALL REPLACE_TABS_W_BLANKS ( CARD )               ! Replace all tab characters with a white space

         CALL CSHIFT ( CARD, ' ', CARD1, CHAR_COL, IERR )  ! Shift CARD so it begins in col 1
 
         IF (CARD1(1:1) == '$') THEN
            DO I=IACHAR('A'),IACHAR('Z')
               IF (CARD1(2:2) == ACHAR(I)) THEN
                  DOLLAR_WARN = 'Y'
               ENDIF
            ENDDO

         ELSE IF (CARD1(1:EC_ENTRY_LEN) == ' ') THEN
            CYCLE

         ELSE IF (CARD1(1: 3) == 'APP'       ) THEN
            CONTINUE
 
         ELSE IF (CARD1(1: 4) == 'CEND'      ) THEN
            EXIT     
 
         ELSE IF (CARD1(1: 6) == 'CHKPNT'    ) THEN
            CHKPNT = 'Y'

         ELSE IF (CARD1(1: 5) == 'DEBUG'     ) THEN
            CALL EC_DEBUG ( CARD1 )

         ELSE IF (CARD1(1: 2) == 'ID'        ) THEN
            CONTINUE
 
         ELSE IF (CARD1(1: 3) == 'IN4'       ) THEN
            CALL EC_IN4FIL ( CARD1 )
 
         ELSE IF (CARD1(1: 7) == 'OUTPUT4'   ) THEN
            CALL EC_OUTPUT4 ( CARD1, JERR, ANY_OU4_NAME_BAD )
            EC_OUTPUT4_ERR = EC_OUTPUT4_ERR + JERR
            IF (ANY_OU4_NAME_BAD == 'Y') THEN
               PRT_OU4_VALID_NAMES = 'Y'
            ENDIF

         ELSE IF (CARD1(1: 5) == 'PARTN'     ) THEN
            CALL EC_PARTN ( CARD1, JERR )
            EC_OUTPUT4_ERR = EC_OUTPUT4_ERR + JERR
 
         ELSE IF (CARD1(1: 7) == 'RESTART'   ) THEN
            RESTART = 'Y'

         ELSE IF (CARD1(1: 3) == 'SOL'       ) THEN

            SOL_NAME(1:LEN(SOL_NAME)) = ' '
            ISTART = 4
            TOKLEN = LEN(CARD1)
            CALL STOKEN ( SUBR_NAME, CARD1, ISTART, TOKLEN, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )

            IF (NTOKEN > 1) THEN

               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1027)
               WRITE(F06,1027)
               CYCLE

            ELSE

               IF (TOKTYP(1) == 'UNKNOWN ') THEN           ! Maybe TOKEN(1) is a name (i.e. SOL_NAME)
                                                           ! Code removed that was here to check IERROR = 1. See explanation above
                  IF      ((TOKEN(1) == 'STATICS ') .OR. (TOKEN(1) == 'SESTATIC')) THEN
                     SOL_NAME(1:7)  =  'STATICS'
                     SOL_INT        = 1

                  ELSE IF ((TOKEN(1) == 'MODAL   ') .OR. (TOKEN(1) == 'MODES   ') .OR. (TOKEN(1) == 'NORMAL M').OR.                &
                           (TOKEN(1) == 'SEMODES ')) THEN
                     SOL_NAME(1:5)  =  'MODES'
                     SOL_INT        = 3

                  ELSE IF ((TOKEN(1) == 'DIFFEREN') .OR. (TOKEN(1) == 'DIFF STI')) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:8)  =  'DIFFEREN'
                     SOL_INT        = 4

                  ELSE IF ((TOKEN(1) == 'BUCKLING') .OR. (TOKEN(1) == 'SEBUCKL')) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:8)  =  'BUCKLING'
                     SOL_INT        = 5

                  ELSE IF ((TOKEN(1) == 'NLSTATIC') .OR. (TOKEN(1) == 'GNOLIN  ') .OR. (TOKEN(1) == 'DIFFEREN')) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:8)  =  'NLSTATIC'
                     SOL_INT        = 106

                  ELSE IF (TOKEN(1)(1:3) == 'GEN') THEN
                     CALL STOKEN ( SUBR_NAME, CARD1, ISTART, TOKLEN, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )
                     IF   (TOKEN(1) == 'CB      ') THEN ! This is TOKEN(1) instead of (2) since ISTART is after 'NORMAL'
                        CALL STOKEN ( SUBR_NAME, CARD1, ISTART, TOKLEN, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )
                        IF   (TOKEN(1) == 'MODEL   ') THEN ! This is TOKEN(1) instead of (2) since ISTART is after 'NORMAL'
                           SOL_NAME(1:LEN(SOL_NAME)) = ' '
                           SOL_NAME(1:12)= 'GEN CB MODEL'
                        ELSE
                           FATAL_ERR = FATAL_ERR + 1
                           WRITE(ERR,1030)
                           WRITE(F06,1030)
                           CYCLE
                        ENDIF
                     ENDIF
                  ELSE
                     WRITE(ERR,1017) TOKEN(1)
                     WRITE(F06,1017) TOKEN(1)
                     FATAL_ERR = FATAL_ERR + 1
                     CYCLE               
                  ENDIF

               ELSE IF (TOKTYP(1) == 'INTEGER ') THEN      ! TOKEN(1) is the integer SOL_INT value

                  CHARFLD(1:) = ' '
                  CHARFLD(1:) = TOKEN(1)(1:)
                  CALL I4FLD ( CHARFLD, JF(2), SOL_INT )
                  CALL CRDERR ( CARD1 )
                  IF      ((SOL_INT == 1 ) .OR. (SOL_INT == 101)) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:7) = 'STATICS'
                  ELSE IF ((SOL_INT == 3 ) .OR. (SOL_INT == 103))  THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:5) = 'MODES'
                  ELSE IF ((SOL_INT == 4 ) .OR. (SOL_INT == 104)) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:8) = 'DIFFEREN'
                  ELSE IF ((SOL_INT == 5 ) .OR. (SOL_INT == 105)) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME(1:8) = 'BUCKLING'
                  ELSE IF (SOL_INT == 31) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME      = 'GEN CB MODEL'
                  ELSE IF ((SOL_INT == 66) .OR. (SOL_INT == 106)) THEN
                     SOL_NAME(1:LEN(SOL_NAME)) = ' '
                     SOL_NAME      = 'NLSTATIC'
                  ELSE
                     WRITE(ERR,999) SOL_NUM_SHOULD_BE, SOL_INT
                     WRITE(F06,999) SOL_NUM_SHOULD_BE, SOL_INT
                     FATAL_ERR = FATAL_ERR + 1
                     CYCLE
                  ENDIF

               ELSE

                  WRITE(ERR,1017) TOKEN(1)
                  WRITE(F06,1017) TOKEN(1)
                  FATAL_ERR = FATAL_ERR + 1
                  CYCLE

               ENDIF

            ENDIF
           
         ELSE IF (CARD1(1: 4) == 'TIME'      ) THEN
            CONTINUE

         ELSE                                              ! Card is not recognized.
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,101) CARD
            WRITE(ERR,9993) PROG_NAME
            IF (SUPWARN == 'N') THEN
               WRITE(F06,9993) PROG_NAME
            ENDIF

         ENDIF

      ENDDO   
  
      IF (DOLLAR_WARN == 'Y') THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1101)
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1101)
         ENDIF
      ENDIF

! Check to make sure that a SOL card was read in EXEC CONTROL.
 
      IF (SOL_NAME(1:) == ' ') THEN
        WRITE(ERR,1016)
        WRITE(F06,1016)
        FATAL_ERR = FATAL_ERR + 1
      ENDIF

! Make sure user has not requested RESTART in nonlinear analyses

      IF ((RESTART == 'Y') .AND. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1102) SOL_NAME
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1102) SOL_NAME
         ENDIF
         RESTART = 'N'
      ENDIF

! List valid OUTPUT4 names if needed

      IF (PRT_OU4_VALID_NAMES == 'Y') THEN
         WRITE(F06,*)
         WRITE(F06,1028)
         DO I=1,NUM_OU4_VALID_NAMES
            IF (ALLOW_OU4_MYSTRAN_NAMES(I) == ALLOW_OU4_OUTPUT_NAMES(I)) THEN
               WRITE(F06,996) I, ALLOW_OU4_MYSTRAN_NAMES(I)
            ELSE
               WRITE(F06,997) I, ALLOW_OU4_MYSTRAN_NAMES(I), ALLOW_OU4_OUTPUT_NAMES(I)
            ENDIF
         ENDDO
         WRITE(F06,*)
      ENDIF

! NLSTATIC not completed yet so give error and quit

      if (SOL_NAME == 'NLSTATIC') then
         if (debug(202) == 0) then                         ! Use  non-advertised debug(202) /= 0 to continue testing NLSTATIC
            Write(err,1103) 
            Write(f06,1103) 
            ec_output4_err = ec_output4_err + 1
         endif
      endif

! If EC_OUTPUT4_ERR > 0 quit

      IF (EC_OUTPUT4_ERR > 0) THEN
         WRITE(ERR,998) EC_OUTPUT4_ERR
         WRITE(F06,998) EC_OUTPUT4_ERR
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

  996 FORMAT(' (',I3,')',5X,A)

  997 FORMAT(' (',I3,')',5X,A,' or its NASTRAN alias NAME: ',A)

  998 FORMAT(' PROCESSING TERMINATED DUE TO ABOVE ',I8,' ERRORS')

  999 FORMAT(' *ERROR   999: INCORRECT SOLUTION IN EXEC CONTROL. SHOULD BE ',A,' BUT IS SOL = ',I8)

 1010 FORMAT(' *ERROR  1010: ERROR READING FOLLOWING ',A,' ENTRY. ENTRY IGNORED')

 1011 FORMAT(' *ERROR  1011: NO ',A10,' ENTRY FOUND BEFORE END OF FILE OR END OF RECORD IN INPUT FILE')

 1016 FORMAT(' *ERROR  1016: A CORRECT SOL ENTRY WAS NOT FOUND IN THE EXEC CONTROL DECK')

 1017 FORMAT(' *ERROR  1017: INCORRECT ENTRY : ',A8,' ON SOL EXECUTIVE CONTROL ENTRY')

 1027 FORMAT(' *ERROR  1027: SOL EXEC CONTROL ENTRY MUST HAVE ONLY ONE INTEGER NUMBER FOLLOWING SOL.',                             &
                           ' ABOVE ENTRY DOES NOT MEET THIS REQUIREMENT')

 1028 FORMAT(' The list of valid names of matrices for OUTPUT4 are:',/)

 1030 FORMAT(' *ERROR  1030: THE ENTRY "GEN CB MODEL" ON SOL ENTRY FOR CRAIG-BAMPTON MODEL GENERATION MAY HAVE BEEN MISSPELLED')

 1101 FORMAT(' *WARNING    : BE CAREFUL WITH LINES THAT BEGIN WITH A $ SIGN IN COL 1 FOLLOWED BY AN UPPER CASE LETTER IN EXEC OR', &
                           ' CASE CONTROL.'                                                                                        &
                    ,/,14X,' THE LINE CAN BE MISINTERPRETED AS A DIRECTIVE FOR THE BANDIT GRID RESEQUENCING ALGORITHM.'            &
                    ,/,14X,' SEE THE BANDIT.PDF FILE INSTALLED WHEN YOU RAN SETUP.EXE TO INSTALL MYSTRAN')

 1102 FORMAT(' *WARNING    : REQUEST FOR RESTART IS NOT ALLOWED IN SOLUTION ',A)

 1103 format(' *INFORMATION: NLSTATIC code not completed so quiting')

 9901 FORMAT(' *WARNING:   : EXEC CONTROL ENTRY "SOL" CANNOT BE "',A,'". SOL ',A,' WILL BE USED')

 9993 FORMAT(' *WARNING    : PRIOR ENTRY NOT PROCESSED BY ',A)


! **********************************************************************************************************************************
 
      END SUBROUTINE LOADE
