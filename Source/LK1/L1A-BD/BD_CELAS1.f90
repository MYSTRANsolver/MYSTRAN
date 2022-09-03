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
 
      SUBROUTINE BD_CELAS1 ( CARD )
 
! Processes CELAS1 Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, MEDAT_CELAS1, NCELAS1, NELE, NEDAT
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CELAS_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE
 
      USE BD_CELAS1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CELAS1'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CELAS_ELID        ! Field 2 of CELAS1 card (this CELAS1's elem ID)
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD but with fields 5 and 6 switched to get G.P.'s together in EDAT
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IDOF              ! Displ component (1,2,3,4,5 or 6) that one end of CELSA conn. to
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CELAS_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CELAS1 scalar spring element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) =E1 for CELAS1
!    2      Element ID     EDAT(nedat+1)
!    3      Property ID    EDAT(nedat+2)
!    4      Grid-A         EDAT(nedat+3)
!    5      Comp-A         EDAT(nedat+5)
!    6      Grid-B         EDAT(nedat+4)
!    7      Comp-B         EDAT(nedat+6)
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      CELAS_ELID = JCARD(2)
 
! Make JCARD_EDAT, which is the version that will have JCARD fields 5, 6 switched when subr ELEPRO called

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Check property ID field. Set to element ID if blank
  
      IF (JCARD(3)(1:) == ' ') THEN
         JCARD_EDAT(3) = JCARD(2)
      ENDIF

! Flip Comp-A and Grid-B in JCARD_EDAT so when ELEPRO runs it will have Grid-A and Grid-B back-to-back
 
      JCARD_EDAT(5) = JCARD(6)
      JCARD_EDAT(6) = JCARD(5)

      CALL ELEPRO ( 'Y', JCARD_EDAT, 6, MEDAT_CELAS1, 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'N', 'N' )
      NCELAS1 = NCELAS1+1
      ETYPE(NELE) = 'ELAS1   '
 
! Check to make sure that numbers in fields 5 and 7 are valid component numbers
 
      IF (IERRFL(JF(5)) == 'N') THEN
         CALL I4FLD ( JCARD(5), JF(5), IDOF )
         IF ((IDOF <= 0) .OR. (IDOF > 6)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1133) IDOF, JF(5), CELAS_ELID
            WRITE(F06,1133) IDOF, JF(5), CELAS_ELID
         ENDIF
      ENDIF
 
      IF (IERRFL(JF(7)) == 'N') THEN
         CALL I4FLD ( JCARD(7), JF(7), IDOF )
         IF ((IDOF <= 0) .OR. (IDOF > 6)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1133) IDOF, JF(7), CELAS_ELID
            WRITE(F06,1133) IDOF, JF(7), CELAS_ELID
         ENDIF
      ENDIF
 
! Issue warning if fields 8, 9 are not blank

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,7,0,0 )   ! Make sure that there are no imbedded blanks in fields 2-7
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )   ! Issue warning if fields 8, 9 are not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1133 FORMAT(' *ERROR  1133: INVALID COMPONEMT NUMBER = ',I8,' IN FIELD ',I2,' ON CELAS1 ID = ',A8,' .MUST BE SINGLE DIGIT 1-6')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CELAS1
