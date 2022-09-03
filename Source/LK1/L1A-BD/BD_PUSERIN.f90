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
  
      SUBROUTINE BD_PUSERIN ( CARD )
  
!  Processes PUSERIN Bulk Data Cards
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, NUM_IN4_FILES
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPUSERIN, NPUSERIN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PUSERIN_BEGEND
      USE MODEL_STUF, ONLY            :  PUSERIN, USERIN_MAT_NAMES
 
      USE BD_PUSERIN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME     = 'BD_PUSERIN'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
!xx   CHARACTER(LEN=*), INTENT(IN)    :: CARD_AS_INPUT     ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: IN4_NUM           ! IN4 file number read from field 3 of RPUSERIN entry
      INTEGER(LONG)                   :: J                 ! DO loop indices
!xx   INTEGER(LONG)                   :: ISTART            ! Start col in CARD for matrix names
      INTEGER(LONG)                   :: PROPERTY_ID   = 0 ! Property ID (field 2 of this property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PUSERIN_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  PUSERIN Bulk Data Card routine
 
!    FIELD   ITEM                         ARRAY ELEMENT
!    -----   ------------                 -------------
!     2      Prop ID                      PUSERIN(npuserin, 1)
!     3      IN4_NUM                      PUSERIN(npuserin, 2)          File num for IN4FIL - will be compared later w/ IN4FIL_NUM
!     4      Stiff mat name               USERIN_MAT_NAMES(npuserin,1)
!     5      Mass  mat name               USERIN_MAT_NAMES(npuserin,2)
!     6      Load matrix name             USERIN_MAT_NAMES(npuserin,3)
!     7      6x6 rigid body mass matrix   USERIN_MAT_NAMES(npuserin,4)
 

!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPUSERIN = NPUSERIN+1
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), PROPERTY_ID )          ! Prop ID
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPUSERIN-1
            IF (PROPERTY_ID == PUSERIN(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
            ENDIF
         ENDDO   
         PUSERIN(NPUSERIN,1) = PROPERTY_ID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), IN4_NUM )              ! IN4 index number
      IF (IERRFL(3) == 'N') THEN
         PUSERIN(NPUSERIN,2) = IN4_NUM
      ENDIF
 
!xx   ISTART = 25
                                                           ! Read stiff matrix name in field 4
!xx   USERIN_MAT_NAMES(NPUSERIN,1) = CARD_AS_INPUT(ISTART  :ISTART+  7)
      USERIN_MAT_NAMES(NPUSERIN,1) = JCARD(4)
                                                           ! Read mass  matrix name in field 5
!xx   USERIN_MAT_NAMES(NPUSERIN,2) = CARD_AS_INPUT(ISTART+ 8:ISTART+15)
      USERIN_MAT_NAMES(NPUSERIN,2) = JCARD(5)
                                                           ! Read load  matrix name in field 6
!xx   USERIN_MAT_NAMES(NPUSERIN,3) = CARD_AS_INPUT(ISTART+16:ISTART+23)
      USERIN_MAT_NAMES(NPUSERIN,3) = JCARD(6)
                                                           ! Read RBM0  matrix name in field 7
!xx   USERIN_MAT_NAMES(NPUSERIN,4) = CARD_AS_INPUT(ISTART+24:ISTART+31)
      USERIN_MAT_NAMES(NPUSERIN,4) = JCARD(7)


      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,0,0,0,0,0 )     ! Make sure that there are no imbedded blanks in fields 2-3
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )   ! Issue warning if fields 8, 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)
 
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PUSERIN
