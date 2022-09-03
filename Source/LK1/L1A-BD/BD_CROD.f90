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
  
      SUBROUTINE BD_CROD ( CARD )
  
! Processes CROD Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, MEDAT_CROD, NCROD, NEDAT, NELE
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CROD_BEGEND
      USE MODEL_STUF, ONLY            :  ETYPE
 
      USE BD_CROD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CROD'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD values sent to subr ELEPRO
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CROD_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CROD element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) =R1 for CROD
!    2      Element ID     EDAT(nedat+1)
!    3      Property ID    EDAT(nedat+2)
!    4      Grid A         EDAT(nedat+3)
!    5      Grid B         EDAT(nedat+4)
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Set JCARD_EDAT to JCARD

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Check property ID field. Set to element ID if blank
  
      IF (JCARD(3)(1:) == ' ') THEN
         JCARD_EDAT(3) = JCARD(2)
      ENDIF

! Read and check data

      CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_CROD, 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'N', 'N' )
      NCROD = NCROD+1
      ETYPE(NELE) = 'ROD     '

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,0,0,0,0 )   ! Make sure that there are no imbedded blanks in fields 2-5
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )   ! Issue warning if fields 6, 7, 8, 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CROD
