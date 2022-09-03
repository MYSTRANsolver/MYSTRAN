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
 
      SUBROUTINE BD_ASET1 ( CARD, LARGE_FLD_INP )
 
! Processes ASET1, OMIT1 Bulk Data Cards
! Data is written to file LINK1N for later processing after checks on format of data.
! Each record contains:   COMPJ, GRIDJ1, GRIDJ2, SET

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1N
      USE SCONTR, ONLY                :  FATAL_ERR, IERRFL, JCARD_LEN, JF, NAOCARD, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_ASET1_BEGEND
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN
 
      USE BD_ASET1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_ASET1'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of 8 characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER(LEN(TSET_CHR_LEN))    :: SET               ! 'A' or 'O' depending on whether the B.D card is ASET or OMIT
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! An output from subr TOKCHK called herein
 
      INTEGER(LONG)                   :: ICONT             ! Indicator of whether a continuation card exists for this parent card
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: IERR              ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0     ! Error indicator for several types of error in format #2 of input
      INTEGER(LONG)                   :: COMPJ     = 0     ! Displ component(s)  read from a B.D. ASET/OMIT card
      INTEGER(LONG)                   :: GRIDJ     = 0     ! A grid point number read from a B.D. ASET/OMIT card in format #2
      INTEGER(LONG)                   :: GRIDJ1    = 0     ! 1st grid in format #1 of ASET/OMIT input
      INTEGER(LONG)                   :: GRIDJ2    = 0     ! 2nd grid in format #1 of ASET/OMIT input
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_ASET1_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! ASET1, OMIT1 Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
! Format #1:
!    2      COMPJ, Displ component(s)
!   3-9     GRIDJ's, Grid ID's
! on optional continuation cards:
!   2-9     Grid ID's
 
! Format #2:
!    2      COMPJ , Displ component(s)
!    3      GRIDJ1, Grid ID number 1
!    4      "THRU"
!    5      GRIDJ2, Grid ID number 2
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Find out if card is ASET1 or OMIT1:
 
      IF      (JCARD(1)(1:5) == 'ASET1') THEN
         SET = 'A '
      ELSE IF (JCARD(1)(1:5) == 'OMIT1') THEN
         SET = 'O '
      ENDIF
 
! Field 4 of ASET1 or OMIT1 must have "THRU" or a grid pt number or blank.

      TOKEN = JCARD(4)(1:8)                                ! Only send the 1st 8 chars of this JCARD. It has been left justified
      CALL TOKCHK ( TOKEN, TOKTYP )                        ! TOKTYP must be THRU', 'INTEGR', or 'BLANK'
      
! **********************************************************************************************************************************
! Format # 2
 
      IF (TOKTYP == 'THRU    ') THEN                      
 
         JERR = 0

         CALL IP6CHK ( JCARD(2), JCARDO, IP6TYP, IDUM )    ! Get components (and put into integer COMPJ)
         IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'ZERO    ') .OR. (IP6TYP == 'BLANK   ')) THEN
            CALL I4FLD ( JCARDO, JF(2), COMPJ )
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1123) JF(2),JCARD(1),JF(2),JCARD(2)
            WRITE(F06,1123) JF(2),JCARD(1),JF(2),JCARD(2)
         ENDIF
 
         IF (JCARD(3)(1:) /= ' ') THEN                     ! Get 1st Grid ID, GRIDJ1
            CALL I4FLD ( JCARD(3), JF(3), GRIDJ1 )
         ELSE            
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'GRID POINT', JF(3), JCARD(1)
            WRITE(F06,1125) 'GRID POINT', JF(3), JCARD(1)
         ENDIF
 
         IF (JCARD(5)(1:) /= ' ') THEN                     ! Get 2nd Grid ID, GRIDJ2
            CALL I4FLD ( JCARD(5), JF(5), GRIDJ2 )
         ELSE            
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'GRID POINT', JF(5), JCARD(1)
            WRITE(F06,1125) 'GRID POINT', JF(5), JCARD(1)
         ENDIF
 
         IF ((IERRFL(3)=='N') .AND. (IERRFL(5)=='N')) THEN ! Check GRIDJ2 > GRIDJ1 if there were no errors reading them
            IF (GRIDJ2 <= GRIDJ1) THEN
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1128) JCARD(1)
               WRITE(F06,1128) JCARD(1)
            ENDIF
         ENDIF            
 
         CALL BD_IMBEDDED_BLANK ( JCARD,0,3,0,5,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 3, 5
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )! Issue warning if fields 6, 7, 8, 9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

         IF ((JERR == 0) .AND. (IERRFL(2) == 'N') .AND. (IERRFL(3) == 'N') .AND. (IERRFL(5) == 'N')) THEN
            WRITE(L1N) COMPJ,GRIDJ1,GRIDJ2,SET             ! Write data to file LINK1N if no errors
            NAOCARD = NAOCARD + 1
         ENDIF
 
 
! **********************************************************************************************************************************
! Format #1 
 
      ELSE IF ((TOKTYP == 'INTEGER ') .OR. (TOKTYP == 'BLANK   ')) THEN 
 
         JERR = 0

         CALL IP6CHK ( JCARD(2), JCARDO, IP6TYP, IDUM )    ! Get components (and put into integer COMPJ)
         IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'ZERO    ') .OR. (IP6TYP == 'BLANK   ')) THEN
            CALL I4FLD ( JCARDO, JF(2), COMPJ )
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1123) JF(2),JCARD(1),JF(2),JCARD(2)
            WRITE(F06,1123) JF(2),JCARD(1),JF(2),JCARD(2)
         ENDIF
 
         DO J=3,9                                          ! Get Grid ID's in fields 3 - 9 and write data to LINK1N
            IF (JCARD(J)(1:) == ' ') THEN
               CYCLE
            ELSE            
               CALL I4FLD ( JCARD(J), JF(J), GRIDJ )
               IF ((JERR == 0) .AND. (IERRFL(J) == 'N')) THEN
                  WRITE(L1N) COMPJ,GRIDJ,GRIDJ,SET         ! Note, GRIDJ is written twice to be compatible w/ Format #2
                  NAOCARD = NAOCARD + 1
               ENDIF
            ENDIF
         ENDDO
  
         CALL BD_IMBEDDED_BLANK ( JCARD,0,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 3-9
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields         
 
         DO                                                ! Optional continuation cards w/ grid ID's, or blank, in fields 2-9 
            IF (LARGE_FLD_INP == 'N') THEN
               CALL NEXTC  ( CARD, ICONT, IERR )
            ELSE
               CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
               CARD = CHILD
            ENDIF
            CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
            IF (ICONT == 1) THEN
               DO J=2,9
                  IF (JCARD(J)(1:) == ' ') THEN
                     CYCLE                                 ! CYCLE to next field when a field is blank
                  ELSE            
                     CALL I4FLD ( JCARD(J), JF(J), GRIDJ )
                     IF ((JERR == 0) .AND. (IERRFL(J) == 'N')) THEN
                        WRITE(L1N) COMPJ,GRIDJ,GRIDJ,SET   ! Note we don't write if error in either COMPJ or GRIDJ
                        NAOCARD = NAOCARD + 1
                     ENDIF
                  ENDIF
               ENDDO  
               CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 ) ! Make sure that there are no imbedded blanks in fields 2-9
               CALL CRDERR ( CARD )                        ! CRDERR prints errors found when reading fields
               CYCLE
            ELSE
               EXIT
            ENDIF
         ENDDO 
 
      ELSE                                                 ! Error - Field 4 did not have "THRU", or an integer, or was blank 
 
         FATAL_ERR = FATAL_ERR+1
         WRITE(ERR,1127) JF(4), JCARD(1)
         WRITE(F06,1127) JF(4), JCARD(1)
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1123 FORMAT(' *ERROR  1123: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' CARD. MUST BE A COMBINATION OF DIGITS 1-6'                &
                    ,/,14X,' HOWEVER, FIELD ',I3, ' HAS: "',A,'"')
 
 1125 FORMAT(' *ERROR  1125: NO ',A,' SPECIFIED IN FIELD',I4,' ON ',A,' CARD')

 1127 FORMAT(' *ERROR  1127: INVALID DATA IN FIELD ',I2,' OF ',A,' CARD. FIELD MUST HAVE THRU OR A GRID NUMBER OR BE BLANK')


 1128 FORMAT(' *ERROR  1128: ON ',A,' THE IDs MUST BE IN INCREASING ORDER FOR THRU OPTION')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE BD_ASET1
