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
  
      SUBROUTINE BD_EIGR ( CARD, LARGE_FLD_INP, EIGFND )
  
! Processes EIGR Bulk Data Cards. Reads and checks data and write data to file LINK1M.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1M
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_EIG_BEGEND
      USE MODEL_STUF, ONLY            :  CC_EIGR_SID
      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_CRIT_DEF, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_METH, EIG_MSGLVL,     &
                                         EIG_LAP_MAT_TYPE, EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM, EIG_SID, EIG_SIGMA,    &
                                         EIG_VECS, MAXMIJ, MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      USE BD_EIGR_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_EIGR'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(1*BYTE), INTENT(INOUT):: EIGFND            ! ='Y' if this EIGR card is the one called for in Case Control
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CHRINP            ! Char data in one field of this entry
      CHARACTER( 1*BYTE)              :: USE_THIS_EIG      ! ='Y' if this is the EIGR meth requested in CC

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
 
!  Field   Item           Description                     Type
!  -----   ------------   -----------                     ----
!   2      EIG_SID        EIGR set ID                     Integer
!   3      EIG_METH       EIGR method                     Char
!   4      EIG_FRQ1       Lower bound on freq in search   Real
!   5      EIG_FRQ2       Upper bound on freq in search   Real, > EIG_FRQ1
!   6      EIG_N1         1st mode number                 Integer, >= 0, default = 1
!   7      EIG_N2         2nd mode number                 Integer, 1 < EIG_N2 <= EIG_N1
!   8      EIG_VECS       Are eigenvecs requested         Char (def = Y)
!   9      EIG_CRIT       Criteria for ortho check        Real >= 0. or blank (used for orthog. check)
!  Required card 2:
  
!  Field   Item           Description                     Type           
!  -----   ------------   -------------                   ----
!   2      EIG_NORM       Type of eigenvec normalization  Char
!   3      EIG_GRID       Grid to normailze on            Integer
!   4      EIG_COMP       DOF comp to normalize on        Integer
!   5      EIG_SIGMA      Shift eigen                     Real           
 

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

      CALL LEFT_ADJ_BDFLD ( JCARD(3) )
      CALL CHAR_FLD ( JCARD(3), JF(3), CHRINP )            ! Read METHOD and check for valid entry
      IF ((IERRFL(3) == 'N') .AND. (USE_THIS_EIG == 'Y')) THEN
         IF      (CHRINP(1:4) == 'GIV ') THEN
            EIG_METH = 'GIV'
         ELSE IF (CHRINP(1:4) == 'INV ') THEN
            EIG_METH = 'INV'
         ELSE IF (CHRINP(1:4) == 'MGIV') THEN
            EIG_METH = 'MGIV'
         ELSE
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1164) CHRINP
            WRITE(F06,1164) CHRINP
         ENDIF
      ENDIF
 
      CALL R8FLD ( JCARD(4), JF(4), EIG_FRQ1 )             ! Read lower  frequency of search range
      CALL R8FLD ( JCARD(5), JF(5), EIG_FRQ2 )             ! Read higher frequency of search range
      CALL I4FLD ( JCARD(6), JF(6), EIG_N1 )               ! Read 1st mode number
      CALL I4FLD ( JCARD(7), JF(7), EIG_N2 )               ! Read 2nd mode number
      CALL LEFT_ADJ_BDFLD ( JCARD(8) )                     ! Read indicator of whether eigenvec output is desired
      IF (JCARD(8)(1:1) == 'N') THEN
         EIG_VECS = 'N'
      ELSE
         EIG_VECS = 'Y'
      ENDIF
      IF (JCARD(9)(1:) /= ' ') THEN
         CALL R8FLD ( JCARD(9), JF(9), EIG_CRIT )          ! Read orthogonality check criteria
      ELSE
         EIG_CRIT = EIG_CRIT_DEF
      ENDIF

! Check that the above data read meets requirements.

      CALL EIGR_DATA_CHECK

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields 

      IF ((IERRFL(2) == 'Y') .OR. (IERRFL(3) == 'Y') .OR. &! Increment JERR if there were errors reading any of the data fields
          (IERRFL(4) == 'Y') .OR. (IERRFL(5) == 'Y') .OR. &
          (IERRFL(6) == 'Y') .OR. (IERRFL(7) == 'Y') .OR. &
          (IERRFL(8) == 'Y') .OR. (IERRFL(9) == 'Y')) THEN
         JERR = JERR + 1 
      ENDIF

! Second Card only required if user wants other than default renormalization of eigenvectors:
  
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN
         CALL CHAR_FLD ( JCARD(2), JF(2), EIG_NORM )
         IF ((EIG_NORM == 'MASS    ') .OR. (EIG_NORM == 'MAX     ') .OR. (EIG_NORM == 'POINT   ') .OR. (EIG_NORM == 'NONE    '))THEN
            CONTINUE
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1165) EIG_NORM
            WRITE(F06,1165) EIG_NORM
         ENDIF
         IF (EIG_NORM == 'POINT   ') THEN
            CALL I4FLD ( JCARD(3), JF(3), EIG_GRID )
            CALL I4FLD ( JCARD(4), JF(4), EIG_COMP )
            IF(IERRFL(4) == 'N') THEN 
               IF ((EIG_COMP < 1) .OR. (EIG_COMP > 6)) THEN
                  JERR = JERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1118) JF(4), EIG_SID, EIG_COMP
                  WRITE(F06,1118) JF(4), EIG_SID, EIG_COMP
               ENDIF
            ENDIF
            
            IF ((IERRFL(3) == 'Y') .OR. (IERRFL(4) == 'Y')) THEN
               JERR = JERR + 1
            ENDIF
         ENDIF

         IF (EIG_METH == 'INV     ') THEN
            CALL R8FLD ( JCARD(5), JF(5), EIG_SIGMA )
         ELSE
            EIG_SIGMA = ZERO
         ENDIF

         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-5
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )! Issue warning if fields 6-9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ENDIF

! Write eigen extract data to file LINK1M if there were no errors and if this is the set ID requested in Case Control

      IF ((JERR == 0) .AND. (USE_THIS_EIG == 'Y')) THEN

         EIG_LAP_MAT_TYPE = '   '
         EIG_MODE         = 0
         EIG_MSGLVL       = 0
         EIG_NCVFACL      = 0

         NUM_FAIL_CRIT    = 0                              ! Following have not been determined yet but write values to L1M anyway
         MAXMIJ           = ZERO
         MIJ_ROW          = 0
         MIJ_COL          = 0

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

 1118 FORMAT(' *ERROR  1118: DOF COMPONENT NUMBER IN FIELD ',I3,' OF EIGR CONTINUATION ENTRY WITH ID = ',I8,' MUST BE A SINGLE',   &
                           ' DIGIT 1-6'                                                                                            &
                    ,/,14X,' BUT VALUE IS = ',I8) 

 1164 FORMAT(' *ERROR  1164: METHOD MUST BE GIV, MGIV, OR INV ON EIGR ENTRY. VALUE IS ',A)

 1165 FORMAT(' *ERROR  1165: NORMALIZATION FACTOR ON EIGR CONTINUATION ENTRY MUST BE MASS, MAX, POINT or NONE. VALUE INPUT IS = ',A)
 
 1117 FORMAT(' *ERROR  1117: ',A,' ENTRY WITH SET ID = ',A,' IS A DUPLICATE SET ID.')

! *********************************************************************************************************************************

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE EIGR_DATA_CHECK

      IMPLICIT NONE

      CHARACTER( 9*BYTE)              :: EIG_SEARCH_CRIT   ! Search criteria (by freg or mode number)

! **********************************************************************************************************************************
      IF ((JCARD(4)(1:) == ' ') .AND. (JCARD(5)(1:) == ' ') .AND. (JCARD(6)(1:) == ' ') .AND.(JCARD(7)(1:) == ' ')) THEN
         WRITE(ERR,1113) JF(4), JF(7), JCARD(1), JCARD(2)
         WRITE(F06,1113) JF(4), JF(7), JCARD(1), JCARD(2)
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF

      EIG_SEARCH_CRIT(1:) = ' '
      
      IF (JCARD(7)(1:) /= ' ') THEN                        ! Search criteria must be mode number so check EIG_N1,2
         EIG_SEARCH_CRIT = 'MODE NMBR'
         IF (JCARD(6)(1:) == ' ') THEN
            EIG_N1 = 1
         ENDIF
         IF (EIG_N1 < 0) THEN
            WRITE(ERR,1103) JF(6), JCARD(1), JCARD(2)
            WRITE(F06,1103) JF(6), JCARD(1), JCARD(2)
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
         IF (EIG_N2 < EIG_N1) THEN
            WRITE(ERR,1105) JF(6), JF(7), JCARD(1), JCARD(2), JF(7), JF(6)
            WRITE(F06,1105) JF(6), JF(7), JCARD(1), JCARD(2), JF(7), JF(6)
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ELSE                                                 ! There is no EIG_N2 so make sure that there is no EIG_N1
         IF (JCARD(6)(1:) /= ' ') THEN
            WRITE(ERR,1106) JCARD(1), JCARD(2)
            WRITE(F06,1106) JCARD(1), JCARD(2)
            FATAL_ERR = FATAL_ERR + 1
            IF (EIG_N1 < 0) THEN
               WRITE(ERR,1103) JF(6), JCARD(1), JCARD(2)
               WRITE(F06,1103) JF(6), JCARD(1), JCARD(2)
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
         ENDIF
      ENDIF

      IF (EIG_SEARCH_CRIT == 'MODE NMBR') THEN
         RETURN
      ELSE                                                 ! Search criteria must be frequency so check EIG_FRQ1,2
         IF (JCARD(4)(1:) == ' ') THEN
            EIG_FRQ1 = ZERO
         ENDIF
         IF (EIG_FRQ1 < ZERO) THEN
            WRITE(ERR,1103) JF(4), JCARD(1), JCARD(2)
            WRITE(F06,1103) JF(4), JCARD(1), JCARD(2)
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
         IF (EIG_FRQ2 < EIG_FRQ1) THEN
            WRITE(ERR,1105) JF(4), JF(5), JCARD(1), JCARD(2), JF(5), JF(4)
            WRITE(F06,1105) JF(4), JF(5), JCARD(1), JCARD(2), JF(5), JF(4)
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1103 FORMAT(' *ERROR  1103: ILLEGAL ENTRY IN FIELD ',I2,' OF ',A,A,' ENTRY. ENTRY MUST BE > 0 OR BLANK')

 1105 FORMAT(' *ERROR  1105: ILLEGAL ENTRY IN FIELD ',I2,' OR ',I2,' OF ',A,A,' ENTRY.',                                           &
                           ' FIELD ',I2,' ENTRY MUST BE GREATER THAN FIELD ',I2,' ENTRY')

 1106 FORMAT(' *ERROR  1106: ILLEGAL ENTRY ON ',A,A,' ENTRY. CANNOT HAVE N1 DEFINED IN FIELD 6 WITH FIELD 7 (N2) BLANK')

 1113 FORMAT(' *ERROR  1113: FIELDS ',I2,' -',I2,' OF ',A,A,' ENTRY CANNOT ALL BE BLANK. THERE IS NO DEFINITION FOR A SEARCH RANGE')

! **********************************************************************************************************************************

      END SUBROUTINE EIGR_DATA_CHECK

      END SUBROUTINE BD_EIGR
