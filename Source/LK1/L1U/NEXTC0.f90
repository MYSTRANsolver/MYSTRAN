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

      SUBROUTINE NEXTC0 ( CARD, ICONT, IERR )

! This version of NEXTC is used in BD_xxxx0 routines called by LOADB0 and is the same as NEXTC except that it does
! not write CARD to F06 under any circumstances

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, IN1, INFILE
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, FATAL_ERR, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  NEXTC0_BEGEND

      USE NEXTC0_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'NEXTC0'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A MYSTRAN data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! 10 fields of characters of CARD
      CHARACTER(24*BYTE)              :: MESSAG            ! Message for output error purposes
      CHARACTER(LEN(JCARD))           :: NEWTAG            ! Field 1  of cont   card
      CHARACTER(LEN(JCARD))           :: OLDTAG            ! Field 10 of parent card
      CHARACTER(LEN=LEN(CARD))        :: TCARD             ! Temporary version of CARD

      INTEGER(LONG), INTENT(OUT)      :: ICONT             ! =1 if next card is current card's continuation or =0 if not
      INTEGER(LONG), INTENT(OUT)      :: IERR              ! Error indicator from subr FFIELD, called herein
      INTEGER(LONG)                   :: COMMENT_COL       ! Col on CARD where a comment begins (if one exists)
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error value from READ
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr READERR  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file. Input to subr READERR
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = NEXTC0_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize error indicator and ICONT

      IERR  = 0
      ICONT = 0

! Make units for writing errors the error file and output file

      OUNT(1) = ERR
      OUNT(2) = F06

! Make JCARD for parent CARD

      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

! Read next card. If it is the cont card for this parent, keep it. Otherwise backspace the input file.

      OLDTAG = JCARD(10)
      MESSAG = 'BULK DATA CARD          '
      READ(IN1,101,IOSTAT=IOCHK) TCARD
      IF (IOCHK /= 0) THEN
         REC_NO = -99
         CALL READERR ( IOCHK, INFILE, MESSAG, REC_NO, OUNT, 'Y' )
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

! Remove any comments within the CARD by deleting everything fro $ on (after col 1)

      COMMENT_COL = 1
      DO I=2,BD_ENTRY_LEN
         IF (TCARD(I:I) == '$') THEN
            COMMENT_COL = I
            EXIT
         ENDIF
      ENDDO

      IF (COMMENT_COL > 1) THEN
         TCARD(COMMENT_COL:) = ' '
      ENDIF

! Make JCARD for TCARD above and get FFIELD to left adjust and fix-field it (if necessary).
!xx CODE COMMENTED OUT IS REPLACED WITH CODE BELOW IT
!xx   IF ((TCARD(1:1) /= '$')  .AND. (TCARD(1:) /= ' ')) THEN
!xx      CALL FFIELD ( TCARD, IERR )
!xx      CALL MKJCARD ( SUBR_NAME, TCARD, JCARD )
!xx      IF (OLDTAG == JCARD(1)) THEN
!xx         ICONT = 1
!xx         CARD = TCARD
!xx      ELSE
!xx         BACKSPACE(IN1)
!xx      ENDIF
!xx   ELSE
!xx      BACKSPACE(IN1)
!xx   ENDIF

      IF (TCARD(1:1) /= '$') THEN
         CALL FFIELD ( TCARD, IERR )
         CALL MKJCARD ( SUBR_NAME, TCARD, JCARD )
         NEWTAG = JCARD(1)
         IF (NEWTAG == OLDTAG) THEN
            ICONT = 1
         ELSE IF ((OLDTAG(1:1) == '+') .AND. (NEWTAG(1:1) == ' ') .AND. (OLDTAG(2:8) == NEWTAG(2:8))) THEN
            ICONT = 1
         ELSE IF ((OLDTAG(1:1) == ' ') .AND. (NEWTAG(1:1) == '+') .AND. (OLDTAG(2:8) == NEWTAG(2:8))) THEN
            ICONT = 1
         ELSE
            BACKSPACE(IN1)
            RETURN
         ENDIF 
         CARD = TCARD
      ELSE
         BACKSPACE(IN1)
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

! **********************************************************************************************************************************

      END SUBROUTINE NEXTC0
