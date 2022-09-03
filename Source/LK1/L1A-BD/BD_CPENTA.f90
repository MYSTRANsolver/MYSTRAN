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
  
      SUBROUTINE BD_CPENTA ( CARD, LARGE_FLD_INP, NUM_GRD )
  
! Processes CPENTA Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, JCARD_LEN, NCPENTA6, NCPENTA15, NEDAT, NELE, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CPENTA_BEGEND
      USE MODEL_STUF, ONLY            :  ETYPE
 
      USE BD_CPENTA_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CPENTA'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: ID                ! Character value of element ID (field 2 of parent card)
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD values sent to subr ELEPRO
      CHARACTER(LEN(JCARD))           :: NAME              ! JCARD(1) from parent entry
 
      INTEGER(LONG), INTENT(OUT)      :: NUM_GRD           ! Number of GRID's + SPOINT's for the elem
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CPENTA_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CPENTA element Bulk Data Card routine
 
!   FIELD   ITEM                   ARRAY ELEMENT
!   -----   ------------   ---------------------------------

! Mandatory parent card

!    1      Element type   ETYPE(nele) = 'PENTA6' or 'PENTA15'
!    2      Element ID     EDAT(nedat+1)
!    3      Property ID    EDAT(nedat+1)
!    4-9    Grids 1-6      EDAT(nedat+3) thru EDAT(nedat+8)

! Possible 2nd card if this is a 15 node PENTA:

!    2-9    Grids 7-14     EDAT(nedat+9) thru EDAT(nedat+16) if this is a PENTA 15 node element

! Possible 3rd card if this is a 15 node PENTA:

!    2      Grid  15       EDAT(nedat+17) if this is a PENTA 15 node element


! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      NAME = JCARD(1)
      ID   = JCARD(2)
 
! Set JCARD_EDAT to JCARD

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Read and check data

      CALL ELEPRO ( 'Y', JCARD_EDAT, 8, 8, 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y' )

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,7,8,9 )   ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
! Check for continuation (if so must be a PENTA15 which then requires 3rd card also)

      ETYPE(NELE)(1:) = ' '
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 0) THEN                                 ! Assume PENTA6 since there was no 2nd card

         NCPENTA6    = NCPENTA6 + 1                        ! Default values unless there is a non-blank continuation
         ETYPE(NELE) = 'PENTA6   '
         NUM_GRD     = 6

      ELSE

         IF (CARD(1:) /= ' ') THEN                         ! Only process continuation entries if 1st one is not totally blank
                                                           ! Assume PENTA15 since there was a 2nd card
            NCPENTA15   = NCPENTA15 + 1
            ETYPE(NELE) = 'PENTA15  '
            NUM_GRD     = 15

            DO I=1,10
               JCARD_EDAT(I) = JCARD(I)
            ENDDO

            CALL ELEPRO ( 'N', JCARD_EDAT, 8, 8, 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y' )
            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )
            CALL CRDERR ( CARD )

            IF (LARGE_FLD_INP == 'N') THEN
               CALL NEXTC  ( CARD, ICONT, IERR )           ! Required 3rd card for PENTA15
            ELSE
               CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
               CARD = CHILD
            ENDIF
            CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
            IF (ICONT == 1) THEN

               DO I=1,10
                  JCARD_EDAT(I) = JCARD(I)
               ENDDO
 
               CALL ELEPRO ( 'N', JCARD_EDAT, 1, 1, 'Y', 'N', 'N', 'N', 'N', 'N', 'N', 'N' )

               CALL BD_IMBEDDED_BLANK( JCARD,2,0,0,0,0,0,0,0 )
               CALL CARD_FLDS_NOT_BLANK(JCARD,0,3,4,5,6,7,8,9)
               CALL CRDERR ( CARD )

            ELSE

               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1136) NAME, ID
               WRITE(F06,1136) NAME, ID

            ENDIF

         ELSE                                              ! Since no cont data was read, default to PENTA6

            NCPENTA6    = NCPENTA6 + 1
            ETYPE(NELE) = 'PENTA6   '
            NUM_GRD     = 6
            ENDIF

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1136 FORMAT(' *ERROR  1136: REQUIRED CONTINUATION FOR ',A,' ID = ',A,' MISSING')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CPENTA
