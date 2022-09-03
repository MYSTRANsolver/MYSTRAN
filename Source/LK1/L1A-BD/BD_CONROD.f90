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
 
      SUBROUTINE BD_CONROD ( CARD )
 
! Processes CONROD Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, IERRFL, JCARD_LEN, JF, MEDAT_CROD, NCROD, NELE, NEDAT, NPROD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CONROD_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE, PROD, RPROD
 
      USE BD_CONROD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CONROD'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD but with fields 5 and 6 switched to get G.P.'s together in EDAT
 
      INTEGER(LONG)                   :: ELEM_ID           ! Elem ID from field 2
      INTEGER(LONG)                   :: MATL_ID           ! Matl ID from field 5
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I4INP             ! An integer read
      INTEGER(LONG)                   :: IERR              ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CONROD_BEGEND
 
      REAL(DOUBLE)                    :: R8INP             ! A real value read

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CONROD element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) = 'ROD     '
!    2      Element ID     EDAT(nedat+1)
!           Property ID    EDAT(nedat+2) (fictitous property PID = -EID)
!    3      Grid A         EDAT(nedat+3)
!    4      Grid B         EDAT(nedat+4)
!           PID            PROD(nprod,1)
!    5      MID            PROD(nprod,2)
!    6      Area          RPROD(nprod,1)
!    7      J             RPROD(nprod,2)
!    8      Tors stress C RPROD(nprod,3)
!    9      NSM           RPROD(nprod,4)
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! First, check that fields 2-9 have the proper data type (we are going to have to rearrange the fields prior to calling ELEPRO).
! If any erors, return

      IERR = 0

      DO I=2,5
         CALL I4FLD ( JCARD(I), JF(I), I4INP )
         IF (IERRFL(I) == 'Y') IERR = IERR + 1
      ENDDO

      DO I=6,9
         CALL R8FLD ( JCARD(I), JF(I), R8INP )
         IF (IERRFL(I) == 'Y') IERR = IERR + 1
      ENDDO

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )
      IF (IERR > 0) THEN
         RETURN
      ENDIF

! Set JCARD_EDAT to have fields 1-5 like a CROD

      DO I=1,10
         JCARD_EDAT(I)(1:) = ' '
      ENDDO
      JCARD_EDAT(1) = JCARD(1)                             ! Elem type
      JCARD_EDAT(2) = JCARD(2)                             ! Elem ID
      JCARD_EDAT(3) = JCARD(2)                             ! Prop ID = Elem ID for the time being
      JCARD_EDAT(4) = JCARD(3)                             ! Grid 1
      JCARD_EDAT(5) = JCARD(4)                             ! Grid 2

! Now call EDAT to check data as if this were a normal CROD. Here, ELEPRO needs prop ID = elem ID to work properly

      CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_CROD, 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'N', 'N' )
      NCROD = NCROD+1
      ETYPE(NELE) = 'ROD     '
 
! Now change PID in EDAT to -PID so EMG can find the properties

      EDAT(NEDAT-2) = -EDAT(NEDAT-2)      

! Put property ID (neg of elem ID) and material ID's into array PROD

      NPROD = NPROD + 1

      ELEM_ID = 0
      CALL I4FLD ( JCARD(2), JF(2), ELEM_ID )
      IF (IERRFL(2) == 'N') THEN
         PROD(NPROD,1) = -ELEM_ID
      ENDIF

      MATL_ID = 0
      CALL I4FLD ( JCARD(5), JF(5), MATL_ID )
      IF (IERRFL(5) == 'N') THEN
         PROD(NPROD,2) = MATL_ID
      ENDIF

! Put real data from CONROD into array RPROD. We already checked that the data in theses fields can be read by R8FLD
 
      CALL R8FLD ( JCARD(6), JF(6), RPROD(NPROD,1) )
      CALL R8FLD ( JCARD(7), JF(7), RPROD(NPROD,2) )
      CALL R8FLD ( JCARD(8), JF(8), RPROD(NPROD,3) )
      CALL R8FLD ( JCARD(9), JF(9), RPROD(NPROD,4) )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CONROD
