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
 
      SUBROUTINE BD_PLOTEL ( CARD )
 
! Processes PLOTEL Bulk Data Cards

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, IERRFL, JCARD_LEN, JF, MEDAT_PLOTEL, NELE, NPLOTEL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PLOTEL_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE
 
      USE BD_PLOTEL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PLOTEL'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD values sent to subr ELEPRO

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I4INP             ! An integer read
      INTEGER(LONG)                   :: IERR              ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PLOTEL_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PLOTEL element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) =R1 for CROD
!    2      Element ID     EDAT(nedat+1)
!    3      Grid A         EDAT(nedat+3)
!    4      Grid B         EDAT(nedat+4)
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! First, check that fields 2-4 have the proper data type (we are going to have to rearrange the fields prior to calling ELEPRO).
! If any erors, return

      IERR = 0

      DO I=2,4
         CALL I4FLD ( JCARD(I), JF(I), I4INP )
         IF (IERRFL(I) == 'Y') IERR = IERR + 1
      ENDDO

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )     ! Make sure that there are no imbedded blanks in fields 2-4
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )   ! Issue warning if fields 5-9 not blank
      CALL CRDERR ( CARD )
      IF (IERR > 0) THEN
         RETURN
      ENDIF

! Set JCARD_EDAT to have fields 2-5 like CROD (but with prop ID = elem ID, where prop ID is not used anyway)

      DO I=1,10
         JCARD_EDAT(I)(1:) = ' '
      ENDDO
      JCARD_EDAT(1) = JCARD(1)                             ! Elem type
      JCARD_EDAT(2) = JCARD(2)                             ! Elem ID 
      JCARD_EDAT(3) = JCARD(2)                             ! Prop ID = elem ID (won't be used) 
      JCARD_EDAT(4) = JCARD(3)                             ! G1
      JCARD_EDAT(5) = JCARD(4)                             ! G2

! Read and check data

      CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_PLOTEL, 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N' )
      NPLOTEL = NPLOTEL + 1
      ETYPE(NELE) = 'PLOTEL  '

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,0,0,0,0,0 )   ! Make sure that there are no imbedded blanks in fields 2-4
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )   ! Issue warning if fields 5-9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PLOTEL
