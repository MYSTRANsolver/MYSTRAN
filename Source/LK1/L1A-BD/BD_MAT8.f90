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

      SUBROUTINE BD_MAT8 ( CARD, LARGE_FLD_INP )

! Processes MAT8 Bulk Data Cards.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LMATL, MRMATLC, NMATL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_MATL_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  MATL, RMATL

      USE BD_MAT8_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_MAT8'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: SETID             ! The set ID in field 2
 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATL_ID   = 0     ! The ID for this MAT8 (field 2)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_MATL_BEGEND

      REAL(DOUBLE)                    :: E1                 ! Modulus in longitudinal direction
      REAL(DOUBLE)                    :: E2                 ! Modulus in lateral direction
      REAL(DOUBLE)                    :: NU12               ! Poissons ratio

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! MAT8 Bulk Data Card routine

!   FIELD   ITEM                                                              ARRAY ELEMENT
!   -----   ------------                                                      -------------
!    2      Material ID                                                         MATL(nmatl,1)
!           Material type                                                       MATL(nmatl,2) (e.g. 8 indicates MAT8 entry)
!    3      E1 elastic modulus                                                 RMATL(nmatl,1)
!    4      E2 elastic modulus                                                 RMATL(nmatl,2)
!    5      NU12 Poisson's ratio                                               RMATL(nmatl,3)
!    6      G12 in plane shear modulus                                         RMATL(nmatl,4)
!    7      G1Z Transverse shear modulus (1-Z plane)                           RMATL(nmatl,5)
!    8      G2Z Transverse shear modulus (2-Z plane)                           RMATL(nmatl,6)
!    9      RHO mass density                                                   RMATL(nmatl,7)
! on optional second card:
!    2      A1 thermal expansion coeff (1 direction)                           RMATL(nmatl,8)
!    3      A2 thermal expansion coeff (2 direction)                           RMATL(nmatl,9)
!    4      TREF temperaure reference                                          RMATL(nmatl,10)
!    5      Xt Longitudinal dir tension allow stress or strain                 RMATL(nmatl,11)
!    6      Xc Longitudinal dir compr allow stress or strain                   RMATL(nmatl,12)
!    7      Yt Lateral dir tension allow stress or strain                      RMATL(nmatl,13)
!    8      Yc Lateral dir compr allow stress or strain                        RMATL(nmatl,14)
!    9      S in-plane shear allowable stress or strain                        RMATL(nmatl,15)
! on optional third card:
!    2      GE structural damping coeff                                        RMATL(nmatl,16)
!    3      F12 Interaction term for failure theory of Tsai-Wu                 RMATL(nmatl,17)
!    4      STRN Indicates whether Xt, Xc, Yt, Yc are stress or strain allows  RMATL(nmatl,18)


! Make JCARD from CARD

      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

! Check for too many MATL entries

      NMATL = NMATL+1
      IF (NMATL > LMATL) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1163) SUBR_NAME,JCARD(1),LMATL
         WRITE(F06,1163) SUBR_NAME,JCARD(1),LMATL
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
      ENDIF

      SETID = JCARD(2)
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
         MATL(NMATL,2) = 8                                 ! Type is 8 for MAT8 card
      ENDIF

      DO J = 1,7
         CALL R8FLD ( JCARD(J+2), JF(J+2), RMATL(NMATL,J) )
      ENDDO

      E1 = ZERO
      IF (IERRFL(3) == 'N') THEN
         E1 = RMATL(NMATL,1)
      ENDIF

      E2 = ZERO
      IF (IERRFL(4) == 'N') THEN
         E2 = RMATL(NMATL,2)
      ENDIF

      NU12 = ZERO
      IF (IERRFL(5) == 'N') THEN
         NU12 = RMATL(NMATL,3)
      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! Null optional data:

      DO J = 8,18
         RMATL(NMATL,J) = ZERO
      ENDDO   

! Optional second card:

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

      ENDIF

! Optional third card:

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         DO J=16,18
            CALL R8FLD ( JCARD(J-14), JF(J-14), RMATL(NMATL,J) )
         ENDDO

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-4
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )! Issue warning if fields 5, 6, 7, 8, 9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ENDIF

! Check MAT8 values

      CALL MAT8_VALUE_CHECK

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

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE MAT8_VALUE_CHECK

      USE CONSTANTS_1, ONLY           :  ONE

      CHARACTER( 2*BYTE)              :: MODULUS            ! Character to print out for error

      INTEGER(LONG)                   :: JERR    = 0        ! Local erro count

      REAL(DOUBLE)                    :: EPS1               ! A small number to compare real zero
      REAL(DOUBLE)                    :: DEN                ! (1 - NU12*NU21)
      REAL(DOUBLE)                    :: NU21               ! Poissons ratio = N12*(E2/E1)

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      EPS1 = DABS(EPSIL(1))
 
! Make sure E1 and E2 > 0.

      IF (DABS(E1) < EPS1) THEN
         JERR = JERR + 1
         MODULUS = 'E1'
         WRITE(ERR,1115) SETID,MODULUS,E1,EPS1
         WRITE(F06,1115) SETID,MODULUS,E1,EPS1
      ENDIF

      IF (DABS(E2) < EPS1) THEN
         JERR = JERR + 1
         MODULUS = 'E2'
         WRITE(ERR,1115) SETID,MODULUS,E2,EPS1
         WRITE(F06,1115) SETID,MODULUS,E2,EPS1
      ENDIF

! Make sure (1 - NU12*NU21) > 0.

      IF (DABS(E2) > ZERO) THEN
         NU21 = NU12*(E1/E2)
         DEN = ONE - NU12*NU21
         IF (DABS(DEN) < EPS1) THEN
            JERR = JERR + 1
            WRITE(ERR,1116) SETID,DEN,EPS1
            WRITE(F06,1116) SETID,DEN,EPS1
         ENDIF
      ENDIF

! If JERR > 0 set fatal error flag

      IF (JERR > 0) THEN
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

! **********************************************************************************************************************************
 1115 FORMAT(' *ERROR  1115: MAT8 MATERIAL BULK DATA ENTRY ',A,' HAS MODULUS ',A,' EQUAL TO ',1ES11.2,'. MUST BE > ',1ES11.2)

 1116 FORMAT(' *ERROR  1116: MAT8 MATERIAL BULK DATA ENTRY ',A,' HAS POISSON RATIO NU12 SUCH THAT 1 - NU12*NU21 = ',1ES11.2,       &
                          '. MUST BE > ',1ES11.2)

! **********************************************************************************************************************************
      END SUBROUTINE MAT8_VALUE_CHECK

      END SUBROUTINE BD_MAT8
