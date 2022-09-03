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
  
      SUBROUTINE BD_PUSER1 ( CARD, LARGE_FLD_INP )
  
!  Processes PUSER1 Bulk Data Cards
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPUSER1, NPUSER1
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PUSER1_BEGEND
      USE MODEL_STUF, ONLY            :  PUSER1, RPUSER1
 
      USE BD_PUSER1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME     = 'BD_PUSER1'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: ICONT         = 0 ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR          = 0 ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATERIAL_ID   = 0 ! Material ID (field 3 of this property card)
      INTEGER(LONG)                   :: PROPERTY_ID   = 0 ! Property ID (field 2 of this property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PUSER1_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  PUSER1 Bulk Data Card routine
 
!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
!     2      Prop ID         PUSER1(npUSER1, 1)
!     3      Mat ID          PUSER1(npUSER1, 2)
!     4      Param 1        RPUSER1(npUSER1, 1)
!     5      Param 2        RPUSER1(npUSER1, 2)
!     6      Param 3        RPUSER1(npUSER1, 3)
!     7      Param 4        RPUSER1(npUSER1, 4)
!     8      Param 5        RPUSER1(npUSER1, 5)
!     9      Param 6        RPUSER1(npUSER1, 6)
!  on optional second card:
!     2      Param 7        RPUSER1(npUSER1, 7)
!     3      Param 8        RPUSER1(npUSER1, 8)
!     4      Param 9        RPUSER1(npUSER1, 9)
!     5      Param 10       RPUSER1(npUSER1,10)
 
 
!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPUSER1 = NPUSER1+1
!xx   IF (NPUSER1 > LPUSER1) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LPUSER1
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LPUSER1
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), PROPERTY_ID )          ! Read property ID and enter into array PUSER1
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPUSER1-1
            IF (PROPERTY_ID == PUSER1(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
            ENDIF
         ENDDO   
         PUSER1(NPUSER1,1) = PROPERTY_ID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), MATERIAL_ID )          ! Read material ID and enter into array PUSER1
      IF (IERRFL(3) == 'N') THEN
         IF (MATERIAL_ID <= 0) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', MATERIAL_ID 
            WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', MATERIAL_ID 
         ELSE
            PUSER1(NPUSER1,2) = MATERIAL_ID
         ENDIF
      ENDIF

      DO J = 1,6                                           ! Read real property values in fields 4-9
         CALL R8FLD ( JCARD(J+3), JF(J+3), RPUSER1(NPUSER1,J) )
      ENDDO   

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )
 
! Read and check data on optional Second Card:
 
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN
         DO J = 7,10                                       ! Read real property values in fields 2-5
            CALL R8FLD ( JCARD(J-5), JF(J-5), RPUSER1(NPUSER1,J) )
         ENDDO   

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-5
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )! Issue warning if fields 6-9 not blank
         CALL CRDERR ( CARD )

      ENDIF
 
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

 1192 FORMAT(' *ERROR  1192: ID IN FIELD ',I3,' OF ',A,A,' MUST BE ',A,' BUT IS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PUSER1
