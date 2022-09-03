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
 
      SUBROUTINE BD_CELAS2 ( CARD )
 
! Processes CELAS2 Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, MEDAT_CELAS2, NCELAS2, NELE, NEDAT, NPELAS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CELAS_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE, PELAS, RPELAS
 
      USE BD_CELAS2_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CELAS2'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CELAS_ELID        ! Field 2 of CELAS2 card (this CELAS2's elem ID)
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD but with fields 5 and 6 switched to get G.P.'s together in EDAT
 
      INTEGER(LONG)                   :: ELEM_ID           ! Elem ID from field 2
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I4INP             ! An integer read
      INTEGER(LONG)                   :: IDOF              ! Displ component (1,2,3,4,5 or 6) that one end of CELSA conn. to
      INTEGER(LONG)                   :: IERR              ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CELAS_BEGEND
 
      REAL(DOUBLE)                    :: R8INP             ! A real value read

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CELAS2 scalar spring element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) =E1 for CELAS2
!    2      Element ID     EDAT(nedat+1)
!    3      Stiffness, K   RPELAS(npelas,1)
!    4      Grid-A         EDAT(nedat+3)
!    5      Comp-A         EDAT(nedat+5)
!    6      Grid-B         EDAT(nedat+4)
!    7      Comp-B         EDAT(nedat+6)
!    8      Damping, GE    RPELAS(npelas,2)
!    9      Str rec, S     RPELAS(npelas,3)
!  none     Prop ID, PID   EDAT(nedat,2) (created: PID = -EID)

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! First, check that fields 2-9 have the proper data type (we are going to have to rearrange the fields prior to calling ELEPRO).
! If any erors, return

      IERR = 0

      CALL I4FLD ( JCARD(2), JF(2), I4INP )
      CALL R8FLD ( JCARD(3), JF(3), R8INP )
      CALL I4FLD ( JCARD(4), JF(4), I4INP )
      CALL I4FLD ( JCARD(5), JF(5), I4INP )
      CALL I4FLD ( JCARD(6), JF(6), I4INP )
      CALL I4FLD ( JCARD(7), JF(7), I4INP )
      CALL R8FLD ( JCARD(8), JF(8), R8INP )
      CALL R8FLD ( JCARD(9), JF(9), R8INP )

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )
      IF (IERR > 0) THEN
         RETURN
      ENDIF

! Get elem ID from field 2 so we can use negative of it as property ID

      CELAS_ELID = JCARD(2)
      NPELAS = NPELAS + 1
      CALL I4FLD ( JCARD(2), JF(2), ELEM_ID )
      PELAS(NPELAS,1) = -ELEM_ID

! Write real data to array RPELAS (so we can rewrite JCARD(3) to be a prop ID)

      CALL R8FLD ( JCARD(3), JF(3), RPELAS(NPELAS,1) )
      CALL R8FLD ( JCARD(8), JF(8), RPELAS(NPELAS,2) )
      CALL R8FLD ( JCARD(9), JF(9), RPELAS(NPELAS,3) )

! Make JCARD_EDAT, which is the version that will have JCARD sent to subr ELEPRO

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Now change JCARD(3) to be a property ID so that subr ELEPRO will handle EDAT data correctly. We want PID = -EID but we send
! JCARD(3) = JCARD(2) (which has PID = EID) to ELEPRO. When ELEPRO returns change term in EDAT for PID to be -PID (i.e. PID = -EID)

      JCARD_EDAT(3) = JCARD_EDAT(2)

! Flip Comp-A and Grid-B in JCARD_EDAT so when ELEPRO runs it will have Grid-A and Grid-B back-to-back
 
      JCARD_EDAT(5) = JCARD(6)
      JCARD_EDAT(6) = JCARD(5)
                                                           ! Do not check fields. That was already done above
      CALL ELEPRO ( 'Y', JCARD_EDAT, 6, MEDAT_CELAS2, 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N' )
      NCELAS2 = NCELAS2+1
      ETYPE(NELE) = 'ELAS2   '
 
! Now change PID in EDAT to -PID

      EDAT(NEDAT-4) = -EDAT(NEDAT-4)      

! Check to make sure that numbers in fields 5 and 7 are valid component numbers
 
      IF (IERRFL(JF(5)) == 'N') THEN
         CALL I4FLD ( JCARD(5), JF(5), IDOF )
         IF ((IDOF <= 0) .OR. (IDOF > 6)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1133) IDOF,JF(5),CELAS_ELID
            WRITE(F06,1133) IDOF,JF(5),CELAS_ELID
         ENDIF
      ENDIF
 
      IF (IERRFL(JF(7)) == 'N') THEN
         CALL I4FLD ( JCARD(7), JF(7), IDOF )
         IF ((IDOF <= 0) .OR. (IDOF > 6)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1133) IDOF,JF(7),CELAS_ELID
            WRITE(F06,1133) IDOF,JF(7),CELAS_ELID
         ENDIF
      ENDIF
 
      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,7,8,9 )   ! Make sure that there are no imbedded blanks in fields 2-7
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1133 FORMAT(' *ERROR  1133: INVALID COMPONEMT NUMBER = ',I8,' IN FIELD ',I2,' ON CELAS2 ID = ',A8,' .MUST BE SINGLE DIGIT 1-6')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CELAS2
