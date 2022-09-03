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

      SUBROUTINE BD_MAT1 ( CARD, LARGE_FLD_INP )

! Processes MAT1 Bulk Data Cards.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LMATL, MRMATLC, NMATL, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_MATL_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, TWO
      USE PARAMS, ONLY                :  EPSIL, SUPINFO, SUPWARN
      USE MODEL_STUF, ONLY            :  MATL, RMATL

      USE BD_MAT1_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_MAT1'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=LEN(CARD))        :: CARDP             ! Parent card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_E           ! The field that contains E (Young's modulus)
      CHARACTER(LEN(JCARD))           :: JCARD_G           ! The field that contains G (shear modulus)
      CHARACTER(LEN(JCARD))           :: JCARD_NU          ! The field that contains NU (Poisson's ratio)
 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATL_ID   = 0     ! The ID for this MAT1 (field 2)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_MATL_BEGEND

      REAL(DOUBLE)                    :: R8INP              ! A real input value read

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! MAT1 Bulk Data Card routine

!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Material ID     MATL(nmatl,1)
!           Material type   MATL(nmatl,2) (e.g. 1 indicates MAT1 entry)
!    3      Elas. Mod.- E  RMATL(nmatl,1)
!    4      Shear.Mod.- G  RMATL(nmatl,2)
!    5      Poiss.Rat.-NU  RMATL(nmatl,3)
!    6      Mass Den.- RHO RMATL(nmatl,4)
!    7      Exp. Coef.- A  RMATL(nmatl,5)
!    8      Ref. Temp.- T  RMATL(nmatl,6)
!    9      Damp.Coef.-GE  RMATL(nmatl,7)
! on optional second card:
!    2      Ten. Lim. -ST  RMATL(nmatl,8)
!    3      Com.  Lim.-SC  RMATL(nmatl,9)
!    4      Shr. Lim. -SS  RMATL(nmatl,10)

      CARDP = CARD

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
         MATL(NMATL,2) = 1                                 ! Type is 1 for MAT1 card
      ENDIF

      DO J = 1,7                                           ! Read 7 fields of real data on the parent card
         R8INP = ZERO
         CALL R8FLD ( JCARD(J+2), JF(J+2), R8INP )
         IF (IERRFL(J+2) == 'N') THEN
            RMATL(NMATL,J) = R8INP
         ENDIF
      ENDDO

      JCARD_E  = JCARD(3)
      JCARD_G  = JCARD(4)
      JCARD_NU = JCARD(5)

! Check on reasonable E, G, NU and calculate values for fields that were left blank

      IF ((IERRFL(3) == 'N') .AND. (IERRFL(4) == 'N') .AND. (IERRFL(5) == 'N')) THEN
         CALL MAT1_VALUE_CHECK
      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      DO J=8,MRMATLC                                       ! Null optional data on 2nd, 3rd cards:
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

         DO J=8,10                                         ! Read optional data
            CALL R8FLD ( JCARD(J-6), JF(J-6), RMATL(NMATL,J) )
         ENDDO

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-4
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )! Issue warning if fields 5, 6, 7, 8, 9 not blank
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
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)


! **********************************************************************************************************************************

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE MAT1_VALUE_CHECK

      INTEGER(LONG)                   :: MAT1_WERR(5) = (/0, 0, 0, 0, 0/)
                                                            ! Warning error indicators

      REAL(DOUBLE)                    :: FACTOR             ! E/(2*(1+NU)*G)

! **********************************************************************************************************************************
! Check for reasonable input NU value if NU field was not blank

      IF (JCARD_NU(1:) /= ' ') THEN
         IF ((RMATL(NMATL,3) < ZERO) .OR. (RMATL(NMATL,3) > HALF)) THEN
            MAT1_WERR(1) = 1
         ENDIF
      ENDIF   

! If E, G, NU fields are all input, make sure 1-|E/(2*(1+NU)*G)| is greater than .01

      IF ((JCARD_E(1:) /= ' ') .AND. (JCARD_G(1:) /= ' ') .AND. (JCARD_NU(1:) /= ' ')) THEN
         FACTOR = RMATL(NMATL,1)/(TWO*(ONE + RMATL(NMATL,3))*RMATL(NMATL,2))
         IF (DABS(ONE - FACTOR) >= .01D0) THEN
            MAT1_WERR(2) = 1
         ENDIF
      ENDIF

! Calc E, G or NU if all were not input. Note E = G = 0 is a fatal error

      IF (JCARD_E(1:) /= ' ') THEN                                           ! E was input

         IF (JCARD_G(1:) /= ' ') THEN                                        !        G was input
            IF (JCARD_NU(1:) /= ' ') THEN                                    !               NU was input
               CONTINUE
            ELSE                                                             !               NU was not input. Calc NU
               RMATL(NMATL,3) = RMATL(NMATL,1)/(TWO*RMATL(NMATL,2)) - ONE
               WRITE(F06,1382) MATL(NMATL,1),RMATL(NMATL,3)
               IF ((RMATL(NMATL,3) < ZERO) .OR. (RMATL(NMATL,3) > HALF)) THEN
                  MAT1_WERR(3) = 1
               ENDIF
            ENDIF
         ELSE                                                                !        G was not input
            IF (JCARD_NU(1:) /= ' ') THEN                                    !               NU was input. Calc G
               RMATL(NMATL,2) = RMATL(NMATL,1)/(TWO*(ONE + RMATL(NMATL,3)))
               WRITE(ERR,1383) MATL(NMATL,1),RMATL(NMATL,2)
               WRITE(F06,1383) MATL(NMATL,1),RMATL(NMATL,2)
            ELSE                                                             !               NU was not input. Set G, NU = 0
               RMATL(NMATL,2) = ZERO
               RMATL(NMATL,3) = ZERO
               WRITE(ERR,1384) MATL(NMATL,1),RMATL(NMATL,2), RMATL(NMATL,3)
               WRITE(F06,1384) MATL(NMATL,1),RMATL(NMATL,2), RMATL(NMATL,3)
            ENDIF
         ENDIF

      ELSE                                                                   ! E was not input

         IF (JCARD_G(1:) /= ' ') THEN                                        !        G was input
            IF (JCARD_NU(1:) /= ' ') THEN                                    !               NU was input. Calc E
               RMATL(NMATL,1) = TWO*RMATL(NMATL,2)*(ONE + RMATL(NMATL,3)) 
               WRITE(ERR,1385) MATL(NMATL,1),RMATL(NMATL,1)
               WRITE(F06,1385) MATL(NMATL,1),RMATL(NMATL,1)
            ELSE                                                             !               NU was not input. Set E, NU = 0 
               RMATL(NMATL,1) = ZERO
               RMATL(NMATL,3) = ZERO
               WRITE(ERR,1386) MATL(NMATL,1),RMATL(NMATL,1),RMATL(NMATL,3)
               WRITE(F06,1386) MATL(NMATL,1),RMATL(NMATL,1),RMATL(NMATL,3)
            ENDIF
         ELSE                                                                !        G was not input (error: E or G must be input)
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1119) MATL(NMATL,1) 
            WRITE(F06,1119) MATL(NMATL,1) 
         ENDIF

      ENDIF

! Check MAT1_WARN ERROR and write messages if needed

      IF ((MAT1_WERR(1) /=0) .OR. (MAT1_WERR(2) /=0) .OR. (MAT1_WERR(3) /=0)) THEN
         WRITE(ERR,101) CARDP
         IF (ECHO == 'NONE  ') THEN
            IF (SUPWARN == 'N') THEN
               WRITE(F06,101) CARDP
            ENDIF
         ENDIF
      ENDIF

      IF (MAT1_WERR(1) /= 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1391) RMATL(NMATL,3),MATL(NMATL,1)
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1391) RMATL(NMATL,3),MATL(NMATL,1)
         ENDIF
      ENDIF

      IF (MAT1_WERR(2) /= 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1393)
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1393)
         ENDIF
      ENDIF

      IF (MAT1_WERR(3) /= 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1392) RMATL(NMATL,3),MATL(NMATL,1)
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1392) RMATL(NMATL,3),MATL(NMATL,1)
         ENDIF
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1119 FORMAT(' *ERROR  1119: CANNOT HAVE E AND G ZERO ON MATERIAL ENTRY ',I8)

 1382 FORMAT(' *INFORMATION: MAT1 ENTRY ',I8,' HAD FIELD FOR NU BLANK. MYSTRAN CALCULATED NU = ',1ES13.6)

 1383 FORMAT(' *INFORMATION: MAT1 ENTRY ',I8,' HAD FIELD FOR G  BLANK. MYSTRAN CALCULATED G  = ',1ES13.6)

 1384 FORMAT(' *INFORMATION: MAT1 ENTRY ',I8,' HAD FIELD FOR G AND NU  BLANK. MYSTRAN SET G         = ',1ES13.6,' NU = ',1ES13.6)

 1385 FORMAT(' *INFORMATION: MAT1 ENTRY ',I8,' HAD FIELD FOR E  BLANK. MYSTRAN CALCULATED E  = ',1ES13.6)

 1386 FORMAT(' *INFORMATION: MAT1 ENTRY ',I8,' HAD FIELD FOR E AND NU BLANK. MYSTRAN SET E         = ',1ES13.6,' NU = ',1ES13.6)

 1391 FORMAT(' *WARNING    : UNREASONABLE VALUE OF NU = ',1ES10.3,' INPUT ON MAT1 ENTRY ID = ',I8)

 1392 FORMAT(' *WARNING    : UNREASONABLE VALUE OF NU= ',1ES10.3,' CALCULATED FROM E AND G ON MAT1 ENTRY ID = ',I8)

 1393 FORMAT(' *WARNING    : VALUES INPUT FOR E, G AND NU ARE SUCH THAT |1 - E/(2*(1+NU)*G)| >= .01 (MAT2 IS RECOMMENDED IN THESE',&
                           ' CASES)')

! **********************************************************************************************************************************
      END SUBROUTINE MAT1_VALUE_CHECK

      END SUBROUTINE BD_MAT1
