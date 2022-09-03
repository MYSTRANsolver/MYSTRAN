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
  
      SUBROUTINE BD_MPC ( CARD, LARGE_FLD_INP, CC_MPC_FND )
  
! Processes MPC Bulk Data Cards. Writes MPC card data to file L1S
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1S
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LMPC, LSUB, MMPC, NMPC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_MPC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  MPCSET, MPC_SIDS
 
      USE BD_MPC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_MPC'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_MPC_FND        ! ='Y' if this MPC is a set requested in Case Control
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: MPC_COMP(MMPC)    ! Array of GRID displ comp values (1-6) found on this logical MPC card
      INTEGER(LONG)                   :: MPC_GRID(MMPC)    ! Array of GRID ID's found on this logical MPC card
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: NUM_TRIPLES       ! Counter on number of pairs of grid/comp/coeff triplets on this MPC
!                                                            card. Must be <= MMPC which was counted in subr BD_MPC0
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: SETID             ! Set ID for this LOAD Bulk Data card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_MPC_BEGEND
 
      REAL(DOUBLE)                    :: MPC_COEFF(MMPC)   ! Array of MPC coeff values found on this MPC logical card

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! MPC Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      SID             MPC set ID
!    3      DEP_GRD         Dependent grid
!    4      DCOMP           Component for dependent grid 
!    5      DVAL            Coeff (value) for dep grid/comp 
!    6      IND_GRD         1st independent grid
!    7      ICOMP           Component for 1st indep grid 
!    8      IVAL            Coeff (value) for 1st indep grid/comp
 
! 1st continuation card:
! 
!   FIELD   ITEM            EXPLANATION
!   -----   ------------    -------------
!    3      IND_GRD         2nd independent grid
!    4      ICOMP           Component for 2nd indep grid 
!    5      IVAL            Coeff (value) for 2nd indep grid/comp
!    6      IND_GRD         3rd independent grid
!    7      ICOMP           Component for 3rd indep grid 
!    8      IVAL            Coeff (value) for 3rd indep grid/comp
 
! Subsequent con't cards follow the same patterm as the 1st
 
 
! Initialize arrays

      DO J=1,MMPC
         MPC_GRID(J)  = 0
         MPC_COMP(J)  = 0
         MPC_COEFF(J) = ZERO
      ENDDO 

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NMPC = NMPC+1
!xx   IF (NMPC > LMPC) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LMPC
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LMPC
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read MPC set ID in field 2

      CALL I4FLD ( JCARD(2), JF(2), SETID )
      IF (IERRFL(2) == 'N') THEN
         IF (SETID == MPCSET) THEN
            CC_MPC_FND = 'Y'
         ENDIF
         MPC_SIDS(NMPC) = SETID
      ELSE
         JERR = JERR + 1
      ENDIF
 
! Read and check dependent grid/comp/coeff in fields 3, 4, 5 on parent card. If fields 3, 4 or 5 are blank then dep
! grid, comp, or coeff was not input which is an error (must have dependent grid/comp/coeff)

      NUM_TRIPLES = 0

      IF ((JCARD(3)(1:) == ' ') .OR. (JCARD(4)(1:) == ' ') .OR. (JCARD(5)(1:) == ' ')) THEN
         WRITE(ERR,1126)
         WRITE(F06,1126)
         FATAL_ERR = FATAL_ERR + 1
      ELSE                                                 ! Increment NUM_TRIPLES and check for overflow
         NUM_TRIPLES = NUM_TRIPLES + 1
         IF (NUM_TRIPLES > MMPC) THEN
            WRITE(ERR,1120) SUBR_NAME,JCARD(1),LMPC
            WRITE(F06,1120) SUBR_NAME,JCARD(1),LMPC
            CALL OUTA_HERE ( 'Y' )
         ENDIF
           
         CALL I4FLD (JCARD(3),JF(3),MPC_GRID(NUM_TRIPLES)) ! Read parent card field 3: dependent grid
         IF (IERRFL(3) == 'Y') THEN
            JERR = JERR + 1
         ENDIF

         CALL I4FLD ( JCARD(4), JF(4), I4INP )             ! Read parent card field 4: components at dependent grid
         IF ( IERRFL(4) == 'N' ) THEN
            IF ((I4INP >= 0) .AND. (I4INP <= 6)) THEN
               MPC_COMP(NUM_TRIPLES) = I4INP
            ELSE 
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1124) JF(4),JCARD(1),JCARD(2),JF(4),JCARD(4)
               WRITE(F06,1124) JF(4),JCARD(1),JCARD(2),JF(4),JCARD(4)
            ENDIF
         ELSE
            JERR = JERR + 1
         ENDIF

         CALL R8FLD (JCARD(5),JF(5),MPC_COEFF(NUM_TRIPLES))! Read parent card field 5: dependent grid MPC coefficient
         IF (IERRFL(5) == 'Y') THEN
            JERR = JERR + 1
         ENDIF

      ENDIF

! Read and check independent grid/comp/coeff in fields 6, 7, 8 on parent card. We skip these 3 fields if all are blank

      IF ((JCARD(6)(1:) == ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN
         CONTINUE
      ELSE                                                 ! Increment NUM_TRIPLES and check for overflow
         NUM_TRIPLES = NUM_TRIPLES + 1
         IF (NUM_TRIPLES > MMPC) THEN
            WRITE(ERR,1120) SUBR_NAME,JCARD(1),LMPC
            WRITE(F06,1120) SUBR_NAME,JCARD(1),LMPC
            CALL OUTA_HERE ( 'Y' )
         ENDIF
           
         CALL I4FLD (JCARD(6),JF(6),MPC_GRID (NUM_TRIPLES))! Read parent card field 6: independent grid
         IF (IERRFL(6) == 'Y') THEN
            JERR = JERR + 1
         ENDIF

         CALL I4FLD ( JCARD(7), JF(7), I4INP )             ! Read parent card field 7: components at independent grid
         IF ( IERRFL(7) == 'N' ) THEN
            IF ((I4INP >= 0) .AND. (I4INP <= 6)) THEN
               MPC_COMP(NUM_TRIPLES) = I4INP
            ELSE 
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1124) JF(7),JCARD(1),JCARD(2),JF(7),JCARD(7)
               WRITE(F06,1124) JF(7),JCARD(1),JCARD(2),JF(7),JCARD(7)
            ENDIF
         ELSE
            JERR = JERR + 1
         ENDIF

         CALL R8FLD (JCARD(8),JF(8),MPC_COEFF(NUM_TRIPLES))! Read parent card field 8: independent grid MPC coefficient
         IF (IERRFL(8) == 'Y') THEN
            JERR = JERR + 1
         ENDIF

      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,5,6,0,8,0 )     ! Make sure there are no imbedded blanks (except 4,7: comps & 9: blank)
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! Read and check data on optional continuation cards

      DO
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN

            DO J=1,2
 
               IF ((JCARD(3*J)(1:) == ' ') .AND. (JCARD(3*J+1)(1:) == ' ') .AND. (JCARD(3*J+2)(1:) == ' ')) THEN
                  CONTINUE
               ELSE                                        ! Increment NUM_TRIPLES and check for overflow
                  NUM_TRIPLES = NUM_TRIPLES + 1
                  IF (NUM_TRIPLES > MMPC) THEN
                     WRITE(ERR,1120) SUBR_NAME,JCARD(1),LMPC
                     WRITE(F06,1120) SUBR_NAME,JCARD(1),LMPC
                     CALL OUTA_HERE ( 'Y' )
                  ENDIF
           
                  CALL I4FLD (JCARD(3*J),JF(3*J),MPC_GRID (NUM_TRIPLES))! Read independent grid
                  IF (IERRFL(3*J) == 'Y') THEN
                     JERR = JERR + 1
                  ENDIF
                                                           ! Read cont card: components at independent grid
                  CALL I4FLD ( JCARD(3*J+1), JF(3*J+1), I4INP )
                  IF ( IERRFL(3*J+1) == 'N' ) THEN
                     IF ((I4INP >= 0) .AND. (I4INP <= 6)) THEN
                        MPC_COMP(NUM_TRIPLES) = I4INP
                     ELSE 
                        JERR      = JERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1124) JF(3*J+1),JCARD(1),JCARD(2),JF(3*J+1),JCARD(3*J+1)
                        WRITE(F06,1124) JF(3*J+1),JCARD(1),JCARD(2),JF(3*J+1),JCARD(3*J+1)
                     ENDIF
                  ELSE
                     JERR = JERR + 1
                  ENDIF

                  CALL R8FLD (JCARD(3*J+2),JF(3*J+2),MPC_COEFF(NUM_TRIPLES))! Read independent grid MPC coefficient
                  IF (IERRFL(3*J+2) == 'Y') THEN
                     JERR = JERR + 1
                  ENDIF

               ENDIF

            ENDDO 

            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,5,6,0,8,0)! Make sure there are no imbedded blanks (except 4,7: comps & 9: blank)
            CALL CARD_FLDS_NOT_BLANK(JCARD,0,0,0,0,0,0,0,9)! Issue warning if field 9 not blank
            CALL CRDERR ( CARD )                           ! CRDERR prints errors found when reading fields

            CYCLE
         ELSE
            EXIT
         ENDIF

      ENDDO 
  
! Write data to file L1S

      IF (JERR == 0) THEN
         WRITE(L1S) SETID
         WRITE(L1S) NUM_TRIPLES
         DO J=1,NUM_TRIPLES
            WRITE(L1S) MPC_GRID(J), MPC_COMP(J), MPC_COEFF(J)
         ENDDO
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1120 FORMAT(' *ERROR  1120: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY TRIPLETS OF GRID/COMPONENT/MPC COEFF ON BULK DATA MPC CARD WITH SET ID = ',I8                &
                    ,/,14X,' LIMIT IS = ',I12)

 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')


 1126 FORMAT(' *ERROR  1126: FIELDS 3, 4 AND 5 ON MPC ENTRY (DEFINING THE DEPENDENT GRID, COMPONENT, MPC COEFF) MUST NOT BE BLANK')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_MPC
