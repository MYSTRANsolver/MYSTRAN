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
 
      SUBROUTINE BD_SUPORT ( CARD )
 
! Processes SUPORT Bulk Data Cards
! Data is written to file LINK1T for later processing after checks on format of data.
! Each record contains:   GRIDJ1, COMPJ

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1T
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, NUM_SUPT_CARDS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SUPORT_BEGEND
 
      USE BD_SUPORT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SUPORT'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
 
      INTEGER(LONG)                   :: COMP(4)   = 0     ! Displ component(s)  read from a B.D. SUPORT card
      INTEGER(LONG)                   :: GRID(4)   = 0     ! A grid point number read from a B.D. SUPORT card
      INTEGER(LONG)                   :: IERR              ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: JERR      = 0     ! Error indicator for several types of error
      INTEGER(LONG)                   :: NUM_PAIRS         ! Number of pairs of grid/comp found on this SUPORT card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SUPORT_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! SUPORT Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
!    2      GRID, Grid ID number 1
!    3      COMP, Displ component(s) for GRID1
!    4      GRID, Grid ID number 2
!    5      COMP, Displ component(s) for GRID2
!    6      GRID, Grid ID number 3
!    7      COMP, Displ component(s) for GRID3
!    8      GRID, Grid ID number 4
!    9      COMP, Displ component(s) for GRID4
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! **********************************************************************************************************************************
      JERR = 0

      NUM_PAIRS = 0
      DO I=2,8,2

         IERR = 0
                                                           ! First make sure that if grid or comp is specified, other one is also
         IF ((JCARD(I)(1:) == ' ') .AND. (JCARD(I+1)(1:) /= ' ')) THEN
            WRITE(ERR,1125) 'GRID POINT', JF(I), JCARD(1)
            WRITE(F06,1125) 'GRID POINT', JF(I), JCARD(1)
            IERR = IERR + 1
         ENDIF
         IF ((JCARD(I)(1:) /= ' ') .AND. (JCARD(I+1)(1:) == ' ')) THEN
            WRITE(ERR,1125) 'DISPL COMPONENT', JF(I+1), JCARD(1)
            WRITE(F06,1125) 'DISPL COMPONENT', JF(I+1), JCARD(1)
            IERR = IERR + 1
         ENDIF

         IF (IERR == 0) THEN
            IF (JCARD(I)(1:) /= ' ') THEN                  ! Grid field not blank so read GRID, COMP
               NUM_PAIRS = NUM_PAIRS + 1
               CALL I4FLD ( JCARD(I), JF(I), GRID(NUM_PAIRS) )
               CALL IP6CHK ( JCARD(I+1), JCARDO, IP6TYP, IDUM )
               IF (IP6TYP == 'COMP NOS') THEN
                  CALL I4FLD ( JCARDO, JF(I+1), COMP(NUM_PAIRS) )
               ELSE
                  JERR      = JERR + 1
                  FATAL_ERR = FATAL_ERR + 1 
                  WRITE(ERR,1123) JF(I+1),JCARD(1),JF(I+1),JCARD(I+1)
                  WRITE(F06,1123) JF(I+1),JCARD(1),JF(I+1),JCARD(I+1)
               ENDIF
            ENDIF
         ENDIF

      ENDDO

      CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,0,6,0,8,0 )  ! Make sure that there are no imbedded blanks in fields 2,4,6,8
      CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      IF (JERR == 0) THEN
         DO I=1,NUM_PAIRS
            WRITE(L1T) GRID(I), COMP(I)                 ! Write data to file LINK1T if no errors
            NUM_SUPT_CARDS = NUM_SUPT_CARDS + 1
         ENDDO
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

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SUPORT
