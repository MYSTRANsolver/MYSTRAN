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
 
      SUBROUTINE BD_CELAS4 ( CARD )
 
! Processes CELAS4 Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, MEDAT_CELAS4, NCELAS4, NELE, NEDAT, NPELAS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CELAS_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE, PELAS, RPELAS
 
      USE BD_CELAS4_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CELAS4'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD but with fields 5 and 6 switched to get G.P.'s together in EDAT
 
      INTEGER(LONG)                   :: ELEM_ID           ! Elem ID from field 2
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I4INP             ! Integer value read from a field of the CELAS4 entry
      INTEGER(LONG)                   :: IERR              ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CELAS_BEGEND
 
      REAL(DOUBLE)                    :: R8INP             ! Real value read from a field on the PSHEAR entry

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CELAS4 scalar spring element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) =E1 for CELAS4
!    2      Element ID     EDAT(nedat+1)
!           Property ID    EDAT(nedat+2) (fictitous property PID = -EID)
!    3      Stiffness      PELAS(npelas,1)
!    4      Scalar point A EDAT(nedat+3)
!    5      Scalar point B EDAT(nedat+5)
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! First, check that fields 2-5 have the proper data type (we are going to have to rearrange the fields prior to calling ELEPRO).
! If any erors, return

      IERR = 0

      CALL I4FLD ( JCARD(2), JF(2), I4INP )
      CALL R8FLD ( JCARD(3), JF(3), R8INP )
      CALL I4FLD ( JCARD(4), JF(4), I4INP )
      CALL I4FLD ( JCARD(5), JF(5), I4INP )

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-5
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )! Issue warning if fields 6, 7, 8, 9 not blank
      CALL CRDERR ( CARD )
      IF (IERR > 0) THEN
         RETURN
      ENDIF

! Get elem ID from field 2 so we can use negative of it as property ID

      NPELAS = NPELAS + 1
      CALL I4FLD ( JCARD(2), JF(2), ELEM_ID )
      IF (IERRFL(2) == 'N') THEN
         PELAS(NPELAS,1) = -ELEM_ID
      ENDIF

! Write real data to array RPELAS (so we can rewrite JCARD(3) to be a prop ID)

      CALL R8FLD ( JCARD(3), JF(3), RPELAS(NPELAS,1) )

! Make JCARD_EDAT, which is the version that will have JCARD fields 5, 6 switched when subr ELEPRO called

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Now change JCARD(3) to be a property ID so that subr ELEPRO will handle EDAT data correctly. We want PID = -EID but we send
! JCARD(3) = JCARD(2) (which has PID = EID) to ELEPRO. When ELEPRO returns we change term in EDAT for PID to be -PID (i.e. -EID)

      JCARD_EDAT(3) = JCARD_EDAT(2)
                                                           ! Do not check fields. That was already done above
      CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_CELAS4, 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N' )
      NCELAS4 = NCELAS4+1
      ETYPE(NELE) = 'ELAS4   '
 
! Now change PID in EDAT to -PID

      EDAT(NEDAT-2) = -EDAT(NEDAT-2)      

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CELAS4
