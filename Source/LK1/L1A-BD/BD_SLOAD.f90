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
  
      SUBROUTINE BD_SLOAD ( CARD, CC_LOAD_FND )
  
! Processes SLOAD Bulk Data Cards. Also, the set ID is written to array SLOAD_SIDS which is checked
! in subroutine LOADB to make sure that all set ID's requested in Case Control were found in the Bulk Data.
! A record is written to file LINK1W for each SLOAD Bulk Data pairs of set ID/force mag with the following data:

!   SETID, scalar point, load mag
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1W
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LFORCE, LSUB, NFORCE, NSLOAD, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SLOAD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE MODEL_STUF, ONLY            :  SLOAD_SIDS, SUBLOD
 
      USE BD_SLOAD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SLOAD'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD                ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2) ! 'Y' if B.D SLOAD card w/ same set ID (SID) as C.C. LOAD = SID
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: GRID_NO(3)          ! Grid ID  on the FORCE/MOMENT card
      INTEGER(LONG)                   :: I                   ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0       ! A local error count
      INTEGER(LONG)                   :: NUM_PAIRS = 0       ! Bumber of pairs of SPOINT/FMAG on a SLOAD entry (can be up to 3)
      INTEGER(LONG)                   :: SETID     = 0       ! Set ID on the FORCE/MOMENT card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SLOAD_BEGEND
  
      REAL(DOUBLE)                    :: FMAG(3)             ! Force magnitude
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! SLOAD Bulk Data Card routine
 
!   FIELD   ITEM           VARIABLE
!   -----   ------------   -------------
!    2      Load set ID    SLOAD_SIDS(I), SETID
!    3      Scalar point   GRID_NO
!    4      Load magnitude FMAG

! Fields (3,4) can be repeated in fields (5,6) and (7,8)
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

! Initialize

      DO I=1,3
         GRID_NO(I) = 0
         FMAG(I)    = ZERO
      ENDDO

! Read and check data
 
      CALL I4FLD ( JCARD(2), JF(2), SETID )
      IF (IERRFL(2) == 'N') THEN
         DO I=1,NSUB
            IF (SETID == SUBLOD(I,1)) THEN
               CC_LOAD_FND(I,1) = 'Y'
            ENDIF
         ENDDO
      ENDIF
 
      NUM_PAIRS = 0
      DO I=1,3
         IF (JCARD(3+2*(I-1))(1:) /= ' ') THEN
            NSLOAD = NSLOAD + 1
            NUM_PAIRS = NUM_PAIRS + 1
            CALL I4FLD ( JCARD(3+2*(I-1)), JF(3+2*(I-1)), GRID_NO(I) )
            CALL R8FLD ( JCARD(4+2*(I-1)), JF(4+2*(I-1)), FMAG(I) )
            SLOAD_SIDS(NSLOAD) = SETID
         ENDIF
      ENDDO  

      IF      (NUM_PAIRS == 1) THEN                        ! Check for imbedded blanks in fields where there is data
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )
      ELSE IF (NUM_PAIRS == 2) THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,0,0,0 )
      ELSE IF (NUM_PAIRS == 3) THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,0 )
      ENDIF
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      DO I=2,8                                             ! Set JERR if any errors reading above data
         IF (IERRFL(I) == 'Y') THEN
            JERR = JERR + 1
         ENDIF
      ENDDO
       
! Write data to file LINK1W if there were no errors

      IF (JERR == 0) THEN
         DO I=1,NUM_PAIRS
            WRITE(L1W) SETID, GRID_NO(I), FMAG(I)
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
  101 FORMAT(A)

 1137 FORMAT(' *WARNING    : ',A,' ENTRY WITH SET ID = ',A,' HAS ZERO COMPONENTS')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SLOAD
