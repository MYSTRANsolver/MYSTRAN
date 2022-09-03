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

      SUBROUTINE BD_MAT9 ( CARD, LARGE_FLD_INP )

! Processes MAT9 Bulk Data Cards.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LMATL, MRMATLC, NMATL, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_MATL_BEGEND
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE MODEL_STUF, ONLY            :  MATL, RMATL

      USE BD_MAT9_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_MAT9'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of 8 characters making up CARD
      CHARACTER(LEN(JCARD))           :: ID                ! Character value of element ID (field 2 of parent card)
      CHARACTER(LEN(JCARD))           :: NAME              ! JCARD(1) from parent entry
 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATL_ID   = 0     ! The ID for this MAT9 (field 2)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_MATL_BEGEND

      REAL(DOUBLE)                    :: R8INP             ! A real input value read

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! MAT9 Bulk Data Card routine

!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Material ID     MATL(nmatl, 1)
!           Material type   MATL(nmatl, 2) (e.g. 2 indicates MAT9 entry)
!    3      G11            RMATL(nmatl, 1)
!    4      G12            RMATL(nmatl, 2)
!    5      G13            RMATL(nmatl, 3)
!    6      G14            RMATL(nmatl, 4)
!    7      G15            RMATL(nmatl, 5)
!    8      G16            RMATL(nmatl, 6)
!    9      G22            RMATL(nmatl, 7)
! on first continuation card:
!    2      G23            RMATL(nmatl, 8)
!    3      G24            RMATL(nmatl, 9)
!    4      G25            RMATL(nmatl,10)
!    5      G26            RMATL(nmatl,11)
!    6      G33            RMATL(nmatl,12)
!    7      G34            RMATL(nmatl,13)
!    8      G35            RMATL(nmatl,14)
!    9      G36            RMATL(nmatl,15)
! on second continuation card:
!    2      G44            RMATL(nmatl,16)
!    3      G45            RMATL(nmatl,17)
!    4      G46            RMATL(nmatl,18)
!    5      G55            RMATL(nmatl,19)
!    6      G56            RMATL(nmatl,20)
!    7      G66            RMATL(nmatl,21)
!    8      RHO            RMATL(nmatl,22)
!    9      A1             RMATL(nmatl,23)
! on optional third continuation card:
!    2      A2             RMATL(nmatl,24)
!    3      A3             RMATL(nmatl,25)
!    4      A4             RMATL(nmatl,26)
!    5      A6             RMATL(nmatl,27)
!    6      A6             RMATL(nmatl,28)
!    7      TREF           RMATL(nmatl,29)
!    8      GE             RMATL(nmatl,30)

! Make JCARD from CARD

      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      NAME = JCARD(1)
      ID   = JCARD(2)

! Check for too many MATL entries

      NMATL = NMATL+1
      IF (NMATL > LMATL) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1163) SUBR_NAME,JCARD(1),LMATL
         WRITE(F06,1163) SUBR_NAME,JCARD(1),LMATL
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
      ENDIF

      CALL I4FLD ( JCARD(2), JF(2), MATL_ID )              ! Check for duplicate ID number
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NMATL-1
            IF (MATL_ID == MATL(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),MATL_ID
               WRITE(F06,1145) JCARD(1),MATL_ID
               EXIT
            ENDIF
         ENDDO 
         MATL(NMATL,1) = MATL_ID
         MATL(NMATL,2) = 9                                 ! Type is 9 for MAT9 card
      ENDIF

      DO J = 1,7                                           ! Read 7 fields of real data on the parent card
         R8INP = ZERO
         CALL R8FLD ( JCARD(J+2), JF(J+2), R8INP )
         IF (IERRFL(J+2) == 'N') THEN
            RMATL(NMATL,J) = R8INP
         ENDIF
      ENDDO

! Mandatory 1st continuation card card:

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         DO J=8,15
            CALL R8FLD ( JCARD(J-6), JF(J-6), RMATL(NMATL,J) )
         ENDDO

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-4
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1136) NAME, ID
         WRITE(F06,1136) NAME, ID

      ENDIF

! Mandatory 2nd continuation card card:

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         DO J=16,23
            CALL R8FLD ( JCARD(J-14), JF(J-14), RMATL(NMATL,J) )
         ENDDO

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-4
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1136) NAME, ID
         WRITE(F06,1136) NAME, ID

      ENDIF

! Null optional data:

      DO J=24,MRMATLC
         RMATL(NMATL,J) = ZERO
      ENDDO   

! Optional third continuation card:

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         DO J=24,30
            CALL R8FLD ( JCARD(J-22), JF(J-22), RMATL(NMATL,J) )
         ENDDO

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,0 )  ! Make sure that there are no imbedded blanks in fields 2-4
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )! Issue warning if field 9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

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
 
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)


! **********************************************************************************************************************************

      END SUBROUTINE BD_MAT9
