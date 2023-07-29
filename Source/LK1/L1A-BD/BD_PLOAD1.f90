! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

      SUBROUTINE BD_PLOAD1 ( CARD, CC_LOAD_FND )

      ! Processes PLOAD1 Bulk Data Cards. Reads and checks data and then writes CARD to file LINK1Q for later processing
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1Q
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSUB, NPCARD, NSUB
      USE SCONTR, ONLY                :  LPLOAD, LPLOAD1, NPLOAD, NPLOAD1
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PLOAD1_BEGEND
      USE MODEL_STUF, ONLY            :  PRESS_SIDS, SUBLOD

      USE BD_PLOAD1_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PLOAD1'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD               ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2)! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)          ! The 10 fields of characters making up CARD
      CHARACTER(8*BYTE)               :: TOKEN              ! The 1st 8 characters from a JCARD
      CHARACTER(8*BYTE)               :: TOKTYP             ! The type of token in a field of parent card. Output from subr TOKCHK
 
      CHARACTER(8*BYTE)               :: TYPE               ! The type flag
      CHARACTER(8*BYTE)               :: SCALE              ! The scale flag
      INTEGER(LONG)                   :: SETID              ! Load set ID on PLOADi card
      INTEGER(LONG)                   :: EID                ! Element ID
      REAL(DOUBLE)                    :: X1, X2             ! x locations
      REAL(DOUBLE)                    :: P1, P2             ! pressures/loads
      INTEGER(LONG)                   :: J                  ! DO loop index
      INTEGER(LONG)                   :: JERR               ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PLOAD1_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! +--------+-----+------+------+-------+-----+-------+-----+-------+
! |   1    |  2  |  3   |  4   |   5   |  6  |   7   |  8  |   9   |
! +========+=====+======+======+=======+=====+=======+=====+=======+
! | PLOAD1 | SID | EID  | TYPE | SCALE | X1  |  P1   |  X2 |  P2   |
! +--------+-----+------+------+-------+-----+-------+-----+-------+
! | PLOAD1 | 25  | 1065 |  MY  | FRPR  | 0.2 | 2.5E3 | 0.8 | 3.5E3 |
! +--------+-----+------+------+-------+-----+-------+-----+-------+

! PLOAD1 Bulk Data card check
 
!   FIELD   ITEM        
!   -----   ------------
!    2      SID
!    3      Element ID
!    4      TYPE (FX, FY, FZ, FXE, FYE, FZE, 
!                 MX, MY, MZ, MXE, MYE, MZE)
!    5      SCALE (LE, FR, LEPR, FRPR)
!    6      X1
!    7      P1
!    8      X2
!    9      P2
 
 
      ! Make JCARD from CARD
      JERR = 0
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
      ! Check for overflow and increment NPLOAD1
      NPLOAD1 = NPLOAD1+1
      NPLOAD  = NPLOAD+1

      ! Check if load set ID on pressure card matches a Case Control request
      CALL I4FLD ( JCARD(2), JF(2), SETID )
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NSUB
            IF (SETID == SUBLOD(J,1)) THEN
               CC_LOAD_FND(J,1) = 'Y'
            ENDIF
         ENDDO
         PRESS_SIDS(NPLOAD) = SETID
      ELSE
         JERR = JERR + 1
      ENDIF

      ! EID
      CALL I4FLD ( JCARD(3), JF(3), EID )
      IF (EID <= 0) THEN
          JERR      = JERR + 1
          FATAL_ERR = FATAL_ERR + 1
          WRITE(ERR,1152) JCARD(1), JCARD(2)  ! TODO: not sure on JCARD(1), JCARD(2)
          WRITE(F06,1152) JCARD(1), JCARD(2)  ! TODO: not sure on JCARD(1), JCARD(2)
      ENDIF

      ! Read X1, P1, X2, P2 values
      CALL R8FLD(JCARD(6), JF(6), X1)
      CALL R8FLD(JCARD(7), JF(7), P1)
      CALL R8FLD(JCARD(8), JF(8), X2)
      CALL R8FLD(JCARD(9), JF(9), P2)

      IF ((JCARD(1)(1:7) == 'PLOAD1 ') .OR. (JCARD(1)(1:7) == 'PLOAD1*')) THEN
         ! 4: TYPE (FX, FY, FZ, FXE, FYE, FZE,
         !          MX, MY, MZ, MXE, MYE, MZE)
         TYPE = ''
         TOKEN = JCARD(4)(1:8)                             ! Only send the 1st 8 chars of this JCARD. It has been left justified
         CALL TOKCHK(TOKEN, TOKTYP)
         IF ((TOKTYP=='FX      ') .OR. (TOKTYP=='FY      ') .OR. (TOKTYP=='FZ      ') .OR.   &
             (TOKTYP=='FXE     ') .OR. (TOKTYP=='FYE     ') .OR. (TOKTYP=='FZE     ') .OR.   &
             (TOKTYP=='MX      ') .OR. (TOKTYP=='MY      ') .OR. (TOKTYP=='MZ      ') .OR.   &
             (TOKTYP=='MXE     ') .OR. (TOKTYP=='MYE     ') .OR. (TOKTYP=='MZE     ')) THEN
             TYPE = TOKTYP
         ELSE
             JERR      = JERR + 1
             FATAL_ERR = FATAL_ERR + 1
             WRITE(ERR,1129) JCARD(1), JCARD(2)  ! TODO: not sure on JCARD(1), JCARD(2)
             WRITE(F06,1129) JCARD(1), JCARD(2)  ! TODO: not sure on JCARD(1), JCARD(2)
         ENDIF

         ! 5: SCALE (LE, FR, LEPR, FRPR)
         SCALE = ''
         TOKEN = JCARD(5)(1:8)                             ! Only send the 1st 8 chars of this JCARD. It has been left justified
         CALL TOKCHK (TOKEN, TOKTYP)
         IF ((TOKTYP=='LE      ') .OR. (TOKTYP=='FR      ') .OR. (TOKTYP=='LEPR    ') .OR. (TOKTYP=='LEFR    ')) THEN
             SCALE = TOKTYP
         ELSE
             JERR      = JERR + 1
             FATAL_ERR = FATAL_ERR + 1
             WRITE(ERR,1129) JCARD(1), JCARD(2)  ! TODO: not sure on JCARD(1), JCARD(2)
             WRITE(F06,1129) JCARD(1), JCARD(2)  ! TODO: not sure on JCARD(1), JCARD(2)
         ENDIF

         CALL BD_IMBEDDED_BLANK(JCARD,2,3,4,5,6,7,8,9)     ! Make sure that there are no imbedded blanks in fields 2-9
         CALL CRDERR(CARD)                                 ! CRDERR prints errors found when reading fields
      ENDIF
 
      ! Write data to file L1Q
      IF (JERR == 0) THEN
         WRITE(L1Q) CARD
      ENDIF

      NPCARD = NPCARD + 1

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
 1129 FORMAT(' *ERROR  1129: INVALID PLOAD1 ',A, A)

 1152 FORMAT(' *ERROR  1152: ON ',A,A,' ELEM IDs MUST BE > 0')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PLOAD1
