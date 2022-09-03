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
 
      SUBROUTINE LOADB_RESTART
 
! LOADB_RESTART reads in some entries in the Bulk Data deck (e.g. DEBUG, PARAM) for a RESTART run
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, ECHO, FATAL_ERR, JCARD_LEN, JF, PROG_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADB_RESTART_BEGEND 
 
      USE LOADB_RESTART_USE_IFs

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: NUM_PARMS = 25      ! Number of PARAM entries allowed in RESTART
      INTEGER(LONG), PARAMETER        :: NUM_DEB   = 28      ! Number of DEBUG entries allowed in RESTART

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'LOADB_RESTART'

      CHARACTER(LEN=BD_ENTRY_LEN)     :: CARD                ! 16 col field card (either CARD1 if small field or CARD1 + CARD2 if
!                                                              a large field card. This is output from subr FFIELD

      CHARACTER(LEN=BD_ENTRY_LEN)     :: CARD1               ! BD card (a small field card or the 1st half of a large field card)
      CHARACTER(LEN=BD_ENTRY_LEN)     :: CARD2               ! 2nd half of a large field card
      CHARACTER( 9*BYTE)              :: DECK_NAME   = 'BULK DATA'
      CHARACTER( 7*BYTE), PARAMETER   :: END_CARD    = 'ENDDATA'
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of 8 characters making up CARD
      CHARACTER( 1*BYTE)              :: LARGE_FLD_INP       ! If 'Y', card is in large field format
      CHARACTER( 8*BYTE)              :: PARM_NAME(NUM_PARMS)! Names of PARAM entries allowed in RESTART
 
      INTEGER(LONG)                   :: COMMENT_COL         ! Col on CARD where a comment begins (if one exists)
      INTEGER(LONG)                   :: DEB_NUM(NUM_DEB)    ! Allowable DEBUG indices in restart
      INTEGER(LONG)                   :: I                   ! DO loop index
      INTEGER(LONG)                   :: INDEX               ! Index
      INTEGER(LONG)                   :: INT_VAL             ! Integer value read fron a card field
      INTEGER(LONG)                   :: IERR                ! Error indicator from subr FFIELD
      INTEGER(LONG)                   :: IOCHK               ! IOSTAT error number when reading Bulk Data cards from unit IN1 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADB_RESTART_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,NUM_PARMS
         PARM_NAME(I) = '        '
      ENDDO

      PARM_NAME( 1) = 'AUTOSPC '        
      PARM_NAME( 2) = 'ELFORCEN'        
      PARM_NAME( 3) = 'EQCHECK '        
      PARM_NAME( 4) = 'GRDPNT  '        
      PARM_NAME( 5) = 'POST    '        
      PARM_NAME( 6) = 'PRTBASIC'        
      PARM_NAME( 7) = 'PRTCORD '        
      PARM_NAME( 8) = 'PRTDISP'        
      PARM_NAME( 9) = 'PRTDOF  '        
      PARM_NAME(10) = 'PRTFOR  '        
      PARM_NAME(11) = 'PRTGMN  '
      PARM_NAME(12) = 'PRTGOA  '
      PARM_NAME(13) = 'PRTjMN  '
      PARM_NAME(14) = 'PRTMASS '
      PARM_NAME(15) = 'PRTQSYS '
      PARM_NAME(16) = 'PRTRMG  '
      PARM_NAME(17) = 'PRTSCP  '
      PARM_NAME(18) = 'PRTTSET '
      PARM_NAME(19) = 'PRTSTIFF'
      PARM_NAME(20) = 'PRTSTIFD'
      PARM_NAME(21) = 'PRTUO0  '
      PARM_NAME(22) = 'PRTYS   '
      PARM_NAME(23) = 'RELINK3 '
      PARM_NAME(24) = 'SORT_MAX'        
      PARM_NAME(25) = 'TINY    '        

      DEB_NUM( 1) =   1
      DEB_NUM( 2) =   2
      DEB_NUM( 3) =   3
      DEB_NUM( 4) =   5
      DEB_NUM( 5) =   6
      DEB_NUM( 6) =   9
      DEB_NUM( 7) =  11
      DEB_NUM( 8) =  13
      DEB_NUM( 9) =  15
      DEB_NUM(10) =  18
      DEB_NUM(11) =  19
      DEB_NUM(12) =  21
      DEB_NUM(13) =  22
      DEB_NUM(14) =  81
      DEB_NUM(15) =  82
      DEB_NUM(16) =  83
      DEB_NUM(17) =  84
      DEB_NUM(18) =  85
      DEB_NUM(19) =  86
      DEB_NUM(20) =  91
      DEB_NUM(21) =  92
      DEB_NUM(22) = 101
      DEB_NUM(23) = 195
      DEB_NUM(24) = 196
      DEB_NUM(25) = 197
      DEB_NUM(26) = 198
      DEB_NUM(27) = 199
      DEB_NUM(28) = 200

      WRITE(F06,100)

! Process Bulk Data cards in a large loop that runs until either an ENDDATA card is found or when an error or EOF/EOR occurs
    
      DO
         READ(IN1,101,IOSTAT=IOCHK) CARD1
         CARD(1:) = CARD1(1:)
 
! Quit if EOF/EOR occurs.
 
         IF (IOCHK < 0) THEN
            WRITE(ERR,1011) END_CARD
            WRITE(F06,1011) END_CARD
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
 
! Check if error occurs.
 
         IF (IOCHK > 0) THEN
            WRITE(ERR,1010) DECK_NAME
            WRITE(F06,1010) DECK_NAME
            WRITE(F06,'(A)') CARD1
            FATAL_ERR = FATAL_ERR + 1
            CYCLE
         ENDIF
 
! Remove any comments within the CARD1 by deleting everything from $ on (after col 1)

         COMMENT_COL = 1
         DO I=2,BD_ENTRY_LEN
            IF (CARD1(I:I) == '$') THEN
               COMMENT_COL = I
               EXIT
            ENDIF
         ENDDO

         IF (COMMENT_COL > 1) THEN
            CARD1(COMMENT_COL:) = ' '
         ENDIF

! Determine if the card is large or small format

         LARGE_FLD_INP = 'N'
         DO I=1,8
            IF (CARD1(I:I) == '*') THEN
               LARGE_FLD_INP = 'Y'
            ENDIF
         ENDDO

! FFIELD converts free-field card to fixed field and left justifies data in fields 2-9 and outputs a 10 field, 16 col/field CARD1
 
         IF ((CARD1(1:1) /= '$')  .AND. (CARD1(1:) /= ' ')) THEN

            IF (LARGE_FLD_INP == 'N') THEN

               CALL FFIELD ( CARD1, IERR )
               CARD(1:) = CARD1(1:)

            ELSE

               READ(IN1,101,IOSTAT=IOCHK) CARD2            ! Read 2nd physical entry for a large field parent B.D. entry
 
               IF (IOCHK < 0) THEN
                  WRITE(ERR,1011) END_CARD
                  WRITE(F06,1011) END_CARD
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
 
               IF (IOCHK > 0) THEN
                  WRITE(ERR,1010) DECK_NAME
                  WRITE(F06,1010) DECK_NAME
                  WRITE(F06,'(A)') CARD2
                  FATAL_ERR = FATAL_ERR + 1
                  CYCLE
               ENDIF
 
               IF (ECHO /= 'NONE  ') THEN
                  WRITE(F06,101) CARD2
               ENDIF
 
               COMMENT_COL = 1
               DO I=2,BD_ENTRY_LEN
                  IF (CARD2(I:I) == '$') THEN
                     COMMENT_COL = I
                     EXIT
                  ENDIF
               ENDDO

               IF (COMMENT_COL > 1) THEN
                  CARD2(COMMENT_COL:) = ' '
               ENDIF

               CALL FFIELD2 ( CARD1, CARD2, CARD, IERR )

            ENDIF

            IF (IERR /= 0) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,101) CARD
               WRITE(ERR,1003)
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,101) CARD
               ENDIF
               WRITE(F06,1003)
               CYCLE
            ENDIF 

         ENDIF
 
! Process Bulk Data card

         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
         IF      ((JCARD(1)(1:6) == 'DEBUG ') .OR. (JCARD(1)(1:6) == 'DEBUG*'))  THEN
            READ(JCARD(2),'(I8)') INDEX
            DO I=1,NUM_DEB
               CALL I4FLD ( JCARD(2), JF(2), INDEX )
               IF (INDEX == DEB_NUM(I)) THEN
                  CALL I4FLD ( JCARD(3), JF(3), INT_VAL )
                  DEBUG(INDEX) = INT_VAL
                  WRITE(F06,101) CARD
                  EXIT
               ENDIF
            ENDDO

         ELSE IF ((JCARD(1)(1:6) == 'PARAM ') .OR. (JCARD(1)(1:6) == 'PARAM*'))  THEN
            IF (JCARD(2) == 'AUTOSPC ') THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,103)
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,103)
               ENDIF
               JCARD(4)(1:) = ' '
               JCARD(5)(1:) = ' '
               JCARD(6)(1:) = ' '
               CALL MKCARD ( JCARD, CARD )
            ENDIF
            DO I=1,NUM_PARMS
               IF (JCARD(2) == PARM_NAME(I)) THEN
                  WRITE(F06,101) CARD
                  CALL BD_PARAM  ( CARD )
                  EXIT
               ENDIF
            ENDDO

         ELSE IF (CARD(1:8) == 'ENDDATA ')  THEN 
            WRITE(F06,101) CARD
            EXIT
 
         ELSE IF ((CARD(1:1) == '$') .OR. (CARD(1:BD_ENTRY_LEN) == ' ')) THEN
            CYCLE

         ELSE                                              ! CARD not processed by MYSTRAN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,101) CARD
            WRITE(ERR,9993) PROG_NAME
            IF (SUPWARN == 'N') THEN
               WRITE(F06,101) CARD
               WRITE(F06,9993) PROG_NAME
            ENDIF

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
  100 FORMAT('$'                                                                                                                ,/,&
             '$ Following are the only Bulk Data entries that were processed for this restart. All other model data will be the'  ,&
              ' same as from the'                                                                                               ,/,&
             '$ original run except that any PARAM or DEBUG Bulk Data entries not in this restart deck will revert to their'      ,&
              ' default values.'                                                                                                ,/,&
             '$ Some PARAM and DEBUG entries may not be processed in this restart. See the MYSTRAN documentaton discussion on'    ,&
              ' restarts.'                                                                                                      ,/,&
             '$')


  101 FORMAT(A)

  103 FORMAT(' *WARNING    : THE ONLY FIELD FROM THE PARAM AUTOSPC THAT WILL BE USED IN THIS RESTART IS FIELD 7 FOR SPC FORCE INFO')

 1003 FORMAT(' *ERROR  1003: ALL FIELDS ON THE ABOVE ENTRY MUST BE NO LONGER THAN 8 CHARACTERS')

 1010 FORMAT(' *ERROR  1010: ERROR READING FOLLOWING ',A,' ENTRY. ENTRY IGNORED')

 1011 FORMAT(' *ERROR  1011: NO ',A10,' ENTRY FOUND BEFORE END OF FILE OR END OF RECORD IN INPUT FILE')

 9993 FORMAT(' *WARNING    : PRIOR ENTRY NOT PROCESSED BY ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE LOADB_RESTART
