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
  
      SUBROUTINE BD_EIGRL ( CARD, LARGE_FLD_INP, EIGFND )
  
! Processes EIGRL Bulk Data Cards. Reads and checks data and write data to file LINK1M.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1M
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPM4
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_EIG_BEGEND
      USE MODEL_STUF, ONLY            :  CC_EIGR_SID, EIG_COMP, EIG_CRIT, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_LANCZOS_NEV_DELT,      &
                                         EIG_METH, EIG_MSGLVL, EIG_LAP_MAT_TYPE, EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM,  &
                                         EIG_SID, EIG_SIGMA, EIG_VECS, MAXMIJ, MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      USE BD_EIGRL_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_EIGRL'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(1*BYTE), INTENT(INOUT):: EIGFND            ! ='Y' if this EIGR card is the one called for in Case Control
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER( 1*BYTE)              :: USE_THIS_EIG      ! ='Y' if this is the EIGR meth requested in CC
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD

      INTEGER(LONG)                   :: I4INP             ! An integer*4 value read
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_EIG_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! EIGR Bulk Data Card routine
  
!  Card 1:
 
!  Field   Item                   Description                          Type
!  -----   ------------           -----------                          ----
!   2      EIG_SID               EIGRL set ID                         Integer
!   3      EIG_FRQ1              Lower bound on freq in search        Real   , >= 0.
!   4      EIG_FRQ2              Upper bound on freq in search        Real   , >= 0.
!   5      EIG_N2                Desired number of roots              Integer, >= 0
!   6      EIG_MSGLVL            Message level for subr Lanczos       Integer, >= 0
!   7      EIG_NCVFACL           (see MODEL_STUF for explanation)     Integer, >= 1
!   8      EIG_SIGMA             Lanczos shift eigen                  Real           
!   9      EIG_NORM              Renormalization method (MASS or MAX) Char
  
! Continuation entry:

!   1      EIG_MODE              Lanczos "mode" (dsband)              Integer, 2 or 3
!   2      EIG_LAP_MAT_TYPE      LAPACK matrix type (DGB, DPB)        Char
!   3      EIG_LANCZOS_NEV_DELT  Number to add to est num roots       Integer >= 0

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      JERR = 0
      USE_THIS_EIG = 'N'
      CALL I4FLD ( JCARD(2), JF(2), EIG_SID )              ! Read set ID and check if it is one requested in Case Control
      IF (IERRFL(2) == 'N') THEN
         IF (EIG_SID == CC_EIGR_SID) THEN
            IF (EIGFND == 'Y') THEN
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
               WRITE(ERR,1117) JCARD(1),JCARD(2)
               WRITE(F06,1117) JCARD(1),JCARD(2)
            ELSE
               EIGFND = 'Y'
               USE_THIS_EIG = 'Y'
            ENDIF
         ELSE
            RETURN
         ENDIF
      ELSE
         JERR = JERR + 1
      ENDIF

      CALL R8FLD ( JCARD(3), JF(3), EIG_FRQ1 )             ! Read field 3: lower  frequency of search range

      CALL R8FLD ( JCARD(4), JF(4), EIG_FRQ2 )             ! Read field 4: higher frequency of search range

      CALL I4FLD ( JCARD(5), JF(5), EIG_N2 )               ! Read field 5: number of desired roots

      IF (JCARD(6)(1:) /= ' ') THEN                        ! Read field 6: MSGLVL
         CALL I4FLD ( JCARD(6), JF(6), I4INP )
         IF (IERRFL(6) == 'N') THEN
            EIG_MSGLVL = I4INP
         ENDIF
      ENDIF

      IF (JCARD(7)(1:) /= ' ') THEN                        ! Read field 7: EIG_NCVFACL
         CALL I4FLD ( JCARD(7), JF(7), I4INP )
         IF (IERRFL(7) == 'N') THEN
            EIG_NCVFACL = I4INP
            IF (JCARD(7)(1:) /= ' ') THEN
               IF (EIG_MODE < 1) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1137) JF(7), EIG_NCVFACL
                  WRITE(F06,1137) JF(7), EIG_NCVFACL
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      CALL R8FLD ( JCARD(8), JF(8), EIG_SIGMA )            ! Read field 8: Lanczos shift freq

      IF (JCARD(9)(1:) /= ' ') THEN                        ! Read field 9: renormalization method if field is not blank
      CALL CHAR_FLD ( JCARD(9), JF(9), EIG_NORM )
         IF ((EIG_NORM == 'MASS    ') .OR. (EIG_NORM == 'MAX     ') .OR. (EIG_NORM == 'NONE    ')) THEN
            CONTINUE
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1107) EIG_NORM
            WRITE(F06,1107) EIG_NORM
         ENDIF
      ENDIF

! Check that the above data read meets requirements.

      CALL EIGRL_DATA_CHECK

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields 

      IF((IERRFL(2) == 'Y') .OR. (IERRFL(3) == 'Y') .OR. & ! Increment JERR if there were errors reading any of the data fields
         (IERRFL(4) == 'Y') .OR. (IERRFL(5) == 'Y') .OR. &
         (IERRFL(6) == 'Y') .OR. (IERRFL(7) == 'Y') .OR. &
         (IERRFL(8) == 'Y') .OR. (IERRFL(9) == 'Y')) THEN
         JERR = JERR + 1 
      ENDIF

! Second Card only required if user wants other than default EIG_MODE, EIG_LAP_MAT_TYPE, or EIG_LANCZOS_FUDGE_FAC:
  
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      IF (ICONT == 1) THEN

         IF (JCARD(2)(1:) /= ' ') THEN                     ! Read field 2: "mode" (see IPARAM(7) in ARPACK subr dsband)
            CALL I4FLD ( JCARD(2), JF(2), I4INP )
            IF (IERRFL(2) == 'N') THEN
               EIG_MODE = I4INP
               IF (JCARD(2)(1:) /= ' ') THEN
                  IF ((EIG_MODE /= 2) .AND. (EIG_MODE /= 3)) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1108) JF(2), EIG_MODE
                     WRITE(F06,1108) JF(2), EIG_MODE
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         IF (JCARD(3)(1:) /= ' ') THEN                     ! Read field 3: LAPACK matrix type (DGB or DPB)
            CALL LEFT_ADJ_BDFLD ( JCARD(3) )
            IF (JCARD(3)(1:) /= ' ') THEN
               IF      (JCARD(3)(1:3) == 'DGB') THEN
                  EIG_LAP_MAT_TYPE = 'DGB     '
               ELSE IF (JCARD(3)(1:3) == 'DPB') THEN
                  EIG_LAP_MAT_TYPE = 'DPB     '
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1109) JCARD(3)
                  WRITE(F06,1109) JCARD(3)
               ENDIF
            ENDIF
         ENDIF

         IF (JCARD(4)(1:) /= ' ') THEN                     ! Read field 4: 
            CALL I4FLD ( JCARD(4), JF(4), I4INP )
            IF (IERRFL(4) == 'N') THEN
               EIG_LANCZOS_NEV_DELT = I4INP
            ENDIF
         ENDIF

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,0,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2,3
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,4,5,6,7,8,9 )! Issue warning if fields 4-9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ENDIF

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      EIG_CRIT = ONEPM4                                    ! Use this until code is changed to read a value from the EIGRL entry
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

! Write eigen extract data to file LINK1M if there were no errors and if this is the set ID requested in Case Control

      IF ((JERR == 0) .AND. (USE_THIS_EIG == 'Y')) THEN

         EIG_METH      = 'LANCZOS'
         EIG_N1        = 1
         EIG_GRID      = 0
         EIG_COMP      = 0
         EIG_VECS      = 'Y'

         NUM_FAIL_CRIT = 0                                 ! Following have not been determined yet but write values to L1M anyway
         MAXMIJ        = ZERO
         MIJ_ROW       = 0
         MIJ_COL       = 0

         CALL WRITE_L1M

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1107 FORMAT(' *ERROR  1107: NORMALIZATION FACTOR ON EIGRL ENTRY MUST BE MASS, MAX or NONE. VALUE INPUT IS = ',A)

 1108 FORMAT(' *ERROR  1108: LANCZOS SOLUTION "MODE" IN FIELD ',I2,' MUST BE 2 OR 3. VALUE INPUT IS = ',I8)

 1109 FORMAT(' *ERROR  1109: LAPACK MATRIX TYPE SHOULD BE "DGB" OR "DPB". VALUE INPUT IS = ',A)

 1117 FORMAT(' *ERROR  1117: ',A,' ENTRY WITH SET ID = ',A,' IS A DUPLICATE SET ID.')

 1137 FORMAT(' *ERROR  1137: FIELD ',I2,' ON THE EIGRL ENTRY MUST BE >= 1 BUT WAS = ',I8)

! *********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE EIGRL_DATA_CHECK

      IMPLICIT NONE

! **********************************************************************************************************************************
      IF ((JCARD(3)(1:) == ' ') .AND. (JCARD(4)(1:) == ' ') .AND. (JCARD(5)(1:) == ' ')) THEN
         WRITE(ERR,1113) JF(3), JF(5), 'EIGRL', EIG_SID
         WRITE(F06,1113) JF(3), JF(5), 'EIGRL', EIG_SID
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF

      IF (JCARD(5)(1:) /= ' ') THEN                        ! Search criteria must be mode number so check EIG_N1,2
         IF (EIG_N2 < 0) THEN
            WRITE(ERR,1103) JF(5), 'EIGRL', EIG_SID
            WRITE(F06,1103) JF(5), 'EIGRL', EIG_SID
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
         RETURN

      ELSE                                                 ! Search criteria must be frequency so check EIG_FRQ1,2

         IF (JCARD(3)(1:) == ' ') THEN
            EIG_FRQ1 = ZERO
         ENDIF
         IF (EIG_FRQ1 < ZERO) THEN
            WRITE(ERR,1103) JF(3), 'EIGRL', EIG_SID
            WRITE(F06,1103) JF(3), 'EIGRL', EIG_SID
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
         IF (EIG_FRQ2 < EIG_FRQ1) THEN
            WRITE(ERR,1105) JF(3), JF(4), 'EIGRL', EIG_SID, JF(4), JF(3)
            WRITE(F06,1105) JF(3), JF(4), 'EIGRL', EIG_SID, JF(4), JF(3)
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
         IF (DABS(EIG_FRQ2 - EIG_FRQ1) == ZERO) THEN
            WRITE(ERR,1111) JF(3), JF(4), 'EIGRL', EIG_SID
            WRITE(F06,1111) JF(3), JF(4), 'EIGRL', EIG_SID
            FATAL_ERR = FATAL_ERR + 1
         ENDIF

      ENDIF

      RETURN

! **********************************************************************************************************************************
 1113 FORMAT(' *ERROR  1113: FIELDS ',I2,' -',I2,' OF ',A,I8,' CANNOT ALL BE BLANK. THERE IS NO DEFINITION FOR AN EIGENVAL ',      &
                            'SEARCH RANGE')

 1103 FORMAT(' *ERROR  1103: ILLEGAL ENTRY IN FIELD ',I2,' OF ',A,I8,'. ENTRY MUST BE > 0 OR BLANK')

 1105 FORMAT(' *ERROR  1105: ILLEGAL ENTRY IN FIELD ',I2,' OR ',I2,' OF ',A,I8,' ENTRY.',                                          &
                           ' FIELD ',I2,' ENTRY MUST BE GREATER THAN FIELD ',I2,' ENTRY')

 1111 FORMAT(' *ERROR  1111: ILLEGAL ENTRY IN FIELD ',I2,' OR ',I2,' OF ',A,I8,' ENTRY.',                                          &
                           ' FREQ RANGE, |F2 - F1| MUST BE > 0')

! **********************************************************************************************************************************

      END SUBROUTINE EIGRL_DATA_CHECK

      END SUBROUTINE BD_EIGRL
