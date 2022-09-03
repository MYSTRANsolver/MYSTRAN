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
 
      SUBROUTINE BD_CMASS3 ( CARD )
 
! Processes CMASS3 Bulk Data Cards. NOTE: MYSTRAN scalar masses must be attached to only 1 scalar point
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, NCMASS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CMASS_BEGEND
      USE MODEL_STUF, ONLY            :  CMASS
 
      USE BD_CMASS3_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CMASS3'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: CMASS_ELID        ! Element ID
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: SPOINT1,SPOINT2   ! 2 scalar points (1 must be blank or zero)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CMASS_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CMASS3 scalar spring element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Element ID     CMASS(ncmass,1)
!  none     Type (1,2,3,4) CMASS(ncmass,2)
!    3      Prop ID, PID   CMASS(ncmass,3)
!    4      Grid-A         CMASS(ncmass,4)
!    5      Grid-B         CMASS(ncmass,6)
 
 
      NCMASS = NCMASS + 1

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Get element ID and check for duplicate

      CALL I4FLD ( JCARD(2), JF(2), CMASS_ELID )
      IF (IERRFL(2) == 'N') THEN
         DO I=1,NCMASS-1
            IF (CMASS_ELID == CMASS(I,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),CMASS_ELID
               WRITE(F06,1145) JCARD(1),CMASS_ELID
               EXIT
            ENDIF
         ENDDO 
         CMASS(NCMASS,1) = CMASS_ELID
      ENDIF

! Get type and put into CNASS col 2

      IF      (JCARD(1)(1:6) == 'CMASS1') THEN
         CMASS(NCMASS,2) = 1
      ELSE IF (JCARD(1)(1:6) == 'CMASS2') THEN
         CMASS(NCMASS,2) = 2
      ELSE IF (JCARD(1)(1:6) == 'CMASS3') THEN
         CMASS(NCMASS,2) = 3
      ELSE IF (JCARD(1)(1:6) == 'CMASS4') THEN
         CMASS(NCMASS,2) = 4
      ENDIF

! Get prop ID and the 2 grids. Set components to 1

      CALL I4FLD ( JCARD(3), JF(3), CMASS(NCMASS,3) )

      SPOINT1 = 0
      CALL I4FLD ( JCARD(4), JF(4), CMASS(NCMASS,4) )
      IF (IERRFL(4) == 'N') THEN
         SPOINT1 = CMASS(NCMASS,4)
      ENDIF

      SPOINT2 = 0
      CALL I4FLD ( JCARD(5), JF(5), CMASS(NCMASS,6) )
      IF (IERRFL(5) == 'N') THEN
         SPOINT2 = CMASS(NCMASS,6)
      ENDIF

      CMASS(NCMASS,5) = 1
      CMASS(NCMASS,7) = 1

! Special case for MYSTRAN: 1 of the scalar points must be 0 (so that this scalar mass defines only a 1x1 matrix, NOT 2x2)

      IF ((SPOINT1 /= 0) .AND. (SPOINT2 /= 0)) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1138) JCARD(1), JCARD(2), SPOINT1, SPOINT2
         WRITE(F06,1138) JCARD(1), JCARD(2), SPOINT1, SPOINT2
      ENDIF

! Make sure that at least 1 SPOINT was defined

      IF ((SPOINT1 <= 0) .AND. (SPOINT2 <= 0)) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1138) JCARD(1), JCARD(2), SPOINT1, SPOINT2
         WRITE(F06,1138) JCARD(1), JCARD(2), SPOINT1, SPOINT2
      ENDIF

! Issue warning if fields 6-9 are not blank

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,0,0,0,0 )   ! Make sure that there are no imbedded blanks in fields 2-5
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )   ! Issue warning if fields 6-9 are not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1138 FORMAT(' *ERROR  1138: ',2A,' MUST HAVE 1 AND ONLY 1 SCALAR POINT DEFINED BUT ENTRY HAS: ',I8,' AND ',I8,                    &
                                  ' (SPECIAL CASE FOR MYSTRAN)')

 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CMASS3
