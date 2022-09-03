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
 
      SUBROUTINE BD_ASET ( CARD )
 
! Processes ASET, OMIT Bulk Data Cards
! Data is written to file LINK1N for later processing after checks on format of data.
! Each record contains:

!         COMPJ, GRIDJ, GRIDJ, SET

! GRIDJ is written twice to be compatible with the data written to file LINK1N for ASET1 data
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1N
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, NAOCARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_ASET_BEGEND
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN
 
      USE BD_ASET_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_ASET'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: SET               ! 'A' or 'O' depending on whether the B.D card is ASET or OMIT
 
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0     ! Count of no. of errors when data fields are read from ASET/OMIT cards
      INTEGER(LONG)                   :: COMPJ     = 0     ! Displ component(s)  read from a B.D. ASET/OMIT card
      INTEGER(LONG)                   :: GRIDJ     = 0     ! A grid point number read from a B.D. ASET/OMIT card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_ASET_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! ASET, OMIT Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
!   2,4,6,8 Grid ID's      
!   3,5,7,9 Displ Components
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Find out if card is ASET or OMIT (must be one, otherwise this
! subroutine would not have been invoked):
 
      IF (JCARD(1)(1:4) == 'ASET') THEN
         SET = 'A'
      ELSE IF (JCARD(1)(1:4) == 'OMIT') THEN
         SET = 'O'
      ENDIF
 
! Process 8 fields of CARD (pairs of Grid ID's and DOF's)
 
      DO J=1,4
         IF ((JCARD(2*J)(1:) == ' ') .AND. (JCARD(2*J+1)(1:) == ' ')) THEN 
            CYCLE
         ENDIF
 
! Get Grid ID. If Grid ID field blank, error
 
         IF (JCARD(2*J)(1:) == ' ') THEN
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'GRID POINT', JF(2*J), JCARD(1)
            WRITE(F06,1125) 'GRID POINT', JF(2*J), JCARD(1)
         ELSE            
            CALL I4FLD ( JCARD(2*J), JF(2*J), GRIDJ )
         ENDIF
 
! Get DOF components (and put into integer COMPJ).
 
         CALL IP6CHK ( JCARD(2*J+1), JCARDO, IP6TYP, IDUM )
         IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'ZERO    ') .OR. (IP6TYP == 'BLANK   ')) THEN
            CALL I4FLD ( JCARDO, JF(2*J+1), COMPJ )
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1123) JF(2*J+1),JCARD(1),JF(2*J+1),JCARD(2*J+1)
            WRITE(F06,1123) JF(2*J+1),JCARD(1),JF(2*J+1),JCARD(2*J+1)
         ENDIF
 
! Write data to file LINK1N if there were no errors. Note, GRIDJ is written twice. This is done since
! ASET1 entries (processed in another subr), can have a format of GRID1 THRU GRID2.
 
         IF ((JERR == 0) .AND. (IERRFL(2*J) == 'N') .AND. (IERRFL(2*J+1) == 'N')) THEN
            WRITE(L1N) COMPJ,GRIDJ,GRIDJ,SET
            NAOCARD = NAOCARD + 1
         ENDIF
 
      ENDDO
   
      CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,0,6,0,8,0 )     ! Make sure that there are no imbedded blanks in fields 2, 4, 6, 8
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
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

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_ASET
