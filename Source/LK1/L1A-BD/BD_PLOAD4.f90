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
  
      SUBROUTINE BD_PLOAD4 ( CARD, CC_LOAD_FND )
  
! Processes PLOAD4 Bulk Data Cards. Reads and checks data and then writes CARD to file LINK1Q for later processing
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1Q
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPLOAD, LSUB, NPCARD, NPLOAD,             &
                                         NPLOAD4_3D, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PLOAD4_BEGEND
      USE MODEL_STUF, ONLY            :  PRESS_SIDS, SUBLOD

      USE BD_PLOAD4_USE_IFs
 
      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PLOAD4'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD               ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2)! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)          ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: ELID1,ELID2        ! Elem ID's on parent card. If "THRU" not in field 8, ELID2 is no present
      INTEGER(LONG)                   :: I4INP              ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: J                  ! DO loop index
      INTEGER(LONG)                   :: JERR               ! Error count
      INTEGER(LONG)                   :: SETID              ! Load set ID on PLOADi card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PLOAD4_BEGEND
  
      REAL(DOUBLE)                    :: R8INP              ! A value read from input file that should be a real value
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PLOAD4 Bulk Data card check. 
! Note that if both fields 8 and 9 are blank this is for a plate elem (and this does not need to be verified)
 
! Format 1:
! --------
!   FIELD   ITEM          Description
!   -----   ----       ------------------
!    2      SID        Load set ID
!    3      ELID       Element ID
!    4      P1         Pressure at grid 1
!    5      P2         Pressure at grid 2
!    6      P3         Pressure at grid 3
!    7      P4         Pressure at grid 4 (not used unless elem is a CHEXA, CQUAD)
!    8      G1         ID of grid connected to a corner of the face (3D elems only)
!    9      G3 or G4   G3 is the ID of a grid connected to a corner diagonally opposite to G1 on the same face of a CHEXA
!                      G4 is the ID of a CTETRA grid located at the corner (not on the face being loaded). CTETRA only

! Format 2: (QUAD4, TRIA3 elems only)
! --------
!   FIELD   ITEM          Description
!   -----   ----       ------------------
!    2      SID        Load set ID
!    3      ELID1      Element ID of 1st elem
!    4      P1         Pressure at grid 1
!    5      P2         Pressure at grid 2
!    6      P3         Pressure at grid 3
!    7      P4         Pressure at grid 4 (not used unless elem is a QUAD4)
!    8      "THRU"
!    9      ELID2      Element ID of 1st elem. Elems in the range ELID1 through ELID2 will be loaded.
 
 
! Make JCARD from CARD and up the count on NPLOAD
 
      JERR = 0
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
      NPLOAD = NPLOAD+1

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
 
 
! Read data in fields 3-7 (same for either format). Only need to make sure data is correct format. We don't need values here

      ELID1 = 0                                             ! Element ID
      CALL I4FLD ( JCARD(3), JF(3), I4INP )
      IF (IERRFL(3) == 'N') THEN
         ELID1 = I4INP      
      ENDIF

      IF (JCARD(4)(1:) /= ' ') THEN
         CALL R8FLD ( JCARD(4), JF(4), R8INP )
      ELSE
         JERR      = JERR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1198) JF(4), JCARD(1), JCARD(2)
         WRITE(F06,1198) JF(4), JCARD(1), JCARD(2)
      ENDIF

      IF (JCARD(5)(1:) /= ' ') THEN                        ! Pressure at grid 2 and/or default to pressure at grid 1
         CALL R8FLD ( JCARD(5), JF(5), R8INP )
      ENDIF

      IF (JCARD(6)(1:) /= ' ') THEN                        ! Pressure at grid 3 and/or default to pressure at grid 1
         CALL R8FLD ( JCARD(6), JF(6), R8INP )
      ENDIF

      IF (JCARD(7)(1:) /= ' ') THEN                        ! Pressure at grid 4 and/or default to pressure at grid 1 
         CALL R8FLD ( JCARD(7), JF(7), R8INP )
      ENDIF

! Format 2:
! --------
      CALL LEFT_ADJ_BDFLD ( JCARD(8) )
      IF (JCARD(8)(1:4) == 'THRU') THEN

         ELID2 = 0
         CALL I4FLD ( JCARD(9), JF(9), I4INP )
         IF (IERRFL(9) == 'N') THEN
            ELID2 = I4INP
            IF (ELID2 >= ELID1) THEN
               ELID2 = I4INP
            ELSE
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1128) JCARD(1), JCARD(2)
               WRITE(F06,1128) JCARD(1), JCARD(2)
            ENDIF      
         ENDIF

! Format 1:
! --------                                                 ! If flds 8,9 are not blank and fld 8 /= THRU this must be for a 3D elem
      ELSE IF ((JCARD(8)(1:) /= ' ') .AND. (JCARD(9)(1:) /= ' ')) THEN

         jerr = jerr + 1
         fatal_err = fatal_err + 1
         Write(err,99)
         Write(f06,99)
         return

         CALL I4FLD ( JCARD(8), JF(8), I4INP )
         CALL I4FLD ( JCARD(9), JF(9), I4INP )
         NPLOAD4_3D = NPLOAD4_3D + 1                       ! Increment count of number of solid elems that have PLOAD4 pressure def

      ENDIF

! Check for card error and fields blank

      IF (JCARD(8)(1:4) == 'THRU') THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-7
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )! Issue warning if fieldS 8, 9 not blank
      ELSE
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-9
      ENDIF
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

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
   99 FORMAT(' *ERROR      : CODE NOT WRITTEN YET FOR PLOAD4 OPTION USING G1 AND G3/G4')

 1128 FORMAT(' *ERROR  1128: ON ',A,A,' THE IDs MUST BE IN INCREASING ORDER FOR THRU OPTION')
 
 1152 FORMAT(' *ERROR  1152: ON ',A,A,' ELEM IDs MUST BE > 0')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1198 FORMAT(' *ERROR  1198: FIELD ',I2,' ON ',A,A,' CANNOT BE BLANK')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PLOAD4
