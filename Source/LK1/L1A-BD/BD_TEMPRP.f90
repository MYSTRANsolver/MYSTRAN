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
  
      SUBROUTINE BD_TEMPRP ( CARD, LARGE_FLD_INP, CC_LOAD_FND )
  
! Processes TEMP Bulk Data Cards and writes CARD to file LINK1K for later processing
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1K
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSUB, MTDAT_TEMPRB, MTDAT_TEMPP1, NSUB,   &
                                         NTCARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_TEMPRP_BEGEND
      USE MODEL_STUF, ONLY            :  SUBLOD
 
      USE BD_TEMPRP_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_TEMPRP'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD                ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2) ! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP       ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD               ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CARD_NAME           ! Field 1 of the parent card
      CHARACTER( 8*BYTE)              :: FIELDS              ! Descriptor of fields in error for output error purposes
      CHARACTER( 1*BYTE)              :: INTEGERS    = 'N'   ! 'Y' if fields 2-9 of a cont card have integer values (or blank)
      CHARACTER( 1*BYTE)              :: KEEP_IT     = 'N'   ! 'Y' if this is a TEMPRB or TEMPP1 we need to write to LINK1K
      CHARACTER( 1*BYTE)              :: THRU_3      = 'N'   ! 'Y' if field 3 of cont. card is "THRU"
      CHARACTER( 1*BYTE)              :: THRU_6      = 'N'   ! 'Y' if field 6 of cont. card is "THRU"
      CHARACTER( 8*BYTE)              :: TOKEN               ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP(10)          ! Character description of a JCARD (output from subr TOKCHK
 
      INTEGER(LONG)                   :: CONT_CARD_NUM  = 0  ! Count of continuation cards (used for output error messages)
      INTEGER(LONG)                   :: ELID, ELID1, ELID2  ! Element ID's
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: I4INP1, I4INP2      ! Values read by subr I4FLD when reading a CARD field
      INTEGER(LONG)                   :: ICONT               ! Indicator whether next card read was a continuation of the parent
      INTEGER(LONG)                   :: IERR                ! Error count
      INTEGER(LONG)                   :: NFLD                ! No. of fields of temperature data (depends on type of CARD)
      INTEGER(LONG)                   :: SID         = 0     ! Set ID read from CARD
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_TEMPRP_BEGEND
  
      REAL(DOUBLE)                    :: RTEMP               ! Real value of a temperature
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  TEMPRB and TEMPP1 Bulk Data card check
 
!    FIELD   ITEM          
!    -----   ------------  
!     2      SID
!     3      ELID1
!     4-5    TB, TP for TEMPP1
!     4-9    TBA, TBB, TP1A, TP1B, TP2A, TP2B for TEMPRB
!  On optional continuation cards (two options for specifying data):
!  Option 1, specify individual elements:
!     2-9    ELIDi
!  Option 2, specify elems with "THRU": ELID2 thru ELIDI and ELIDJ thru ELIDK
!     2      ELID2
!     3      "THRU"
!     4      ELIDI
!     5      ELIDJ
!     6      "THRU"
!     7      ELIDK
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read and check data
 
      KEEP_IT = 'N'                                        ! Check if load set ID on TEMP card matches a Case Control request
      CALL I4FLD ( JCARD(2), JF(2), I4INP1 )
      IF (IERRFL(2) == 'N') THEN
         SID = I4INP1
         DO I=1,NSUB
            IF (SID == SUBLOD(I,2)) THEN
               CC_LOAD_FND(I,2) = 'Y'
               KEEP_IT     = 'Y'
            ENDIF
         ENDDO
      ENDIF
         
      CARD_NAME = JCARD(1)                                 ! Set NFLD based on whether this is a TEMPRB or TEMPP1
      IF      (JCARD(1)(1:6) == 'TEMPRB') THEN
         NFLD = MTDAT_TEMPRB
      ELSE IF (JCARD(1)(1:6) == 'TEMPP1') THEN
         NFLD = MTDAT_TEMPP1
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-5
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )  ! Issue warning if fields 6, 7, 8, 9 not blank
      ENDIF

      CALL I4FLD ( JCARD(3), JF(3), I4INP1 )               ! Read elem ID in field 3
      IF (IERRFL(3) == 'N') THEN
         ELID = I4INP1
         IF (ELID <=0) THEN
            WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', ELID
            WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', ELID
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDIF
      DO J=1,NFLD                                          ! Read NFLD fields of real data
         CALL R8FLD ( JCARD(J+3), JF(J+3), RTEMP )
      ENDDO   
   
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      IF (KEEP_IT == 'Y') THEN                             ! Write parent card data to file LINK1K
         WRITE(L1K) CARD
         NTCARD = NTCARD + 1
      ENDIF
  
! Optional continuation cards:
 
      CONT_CARD_NUM = 0
      DO
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
         IF (ICONT == 1) THEN

            CONT_CARD_NUM = CONT_CARD_NUM + 1

            IF (KEEP_IT == 'Y') THEN
  
! First check for the 2 options on specifying data on continuation card. Either all data are ELID's or the THRU option
! is used in which case field 3 and/or 6 should have "THRU".
  
               IERR = 0
               DO I=2,9
                  TOKEN = JCARD(I)(1:8)                    ! Only send the 1st 8 chars of this JCARD. It has been left justified
                  CALL TOKCHK ( TOKEN, TOKTYP(I) )
               ENDDO 
 
               INTEGERS = 'N'
               DO I=2,9
                  IF ((TOKTYP(I) == 'INTEGER ') .OR. (TOKTYP(I) == 'BLANK   ')) THEN
                     INTEGERS = 'Y'
                     CYCLE
                  ELSE
                     INTEGERS = 'N'
                     EXIT
                  ENDIF
               ENDDO

               IF (INTEGERS == 'N') THEN                   ! Check for "ELID1 THRU ELID2" plus, possibly, "ELID3 THRU ELID4" 
                  IF ((TOKTYP(2) == 'INTEGER ') .AND. (TOKTYP(3) == 'THRU    ') .AND. (TOKTYP(4) == 'INTEGER ')) THEN
                     THRU_3 = 'Y'
                     IF ((TOKTYP(5) == 'INTEGER ') .AND. (TOKTYP(6) == 'THRU    ') .AND. (TOKTYP(7) == 'INTEGER ')) THEN
                        THRU_6 = 'Y'                       ! This is valid input (ELID1 THRU ELID2)
                     ELSE IF ((TOKTYP(5) == 'BLANK   ') .AND. (TOKTYP(6) == 'BLANK   ') .AND. (TOKTYP(7) == 'BLANK   ')) THEN
                        THRU_6 = 'N'                       ! This is valid input (fields 5,6,7 blank)
                     ELSE
                        THRU_6 = 'E'                       ! This is an error, so increment IERR
                        FATAL_ERR = FATAL_ERR+1
                        IERR = IERR + 1
                        FIELDS = '5, 6, 7 '
                        WRITE(ERR,1190) FIELDS,CARD_NAME,SID,CONT_CARD_NUM,JCARD(5),JCARD(6),JCARD(7)
                        WRITE(F06,1190) FIELDS,CARD_NAME,SID,CONT_CARD_NUM,JCARD(5),JCARD(6),JCARD(7)
                     ENDIF
                  ELSE
                     THRU_3 = 'E'                          ! This is an error, so increment IERR
                     FATAL_ERR = FATAL_ERR+1
                     IERR = IERR + 1
                     FIELDS = '2, 3, 4 '
                     WRITE(ERR,1190) FIELDS,CARD_NAME,SID,CONT_CARD_NUM,JCARD(2),JCARD(3),JCARD(4)
                     WRITE(F06,1190) FIELDS,CARD_NAME,SID,CONT_CARD_NUM,JCARD(2),JCARD(3),JCARD(4)
                     IF ((TOKTYP(5) == 'INTEGER ') .AND. (TOKTYP(6) == 'THRU    ') .AND. (TOKTYP(7) == 'INTEGER ')) THEN
                        THRU_6 = 'Y'                       ! This is valid input (ELID1 THRU ELID2)
                     ELSE IF ((TOKTYP(5) == 'BLANK   ') .AND. (TOKTYP(6) == 'BLANK   ') .AND. (TOKTYP(7) == 'BLANK   ')) THEN
                        THRU_6 = 'N'                       ! This is valid input (fields 5,6,7 blank)
                     ELSE
                        THRU_6 = 'E'                       ! This is an error, so increment IERR
                        FATAL_ERR = FATAL_ERR+1
                        IERR = IERR + 1
                        FIELDS = '5, 6, 7 '
                        WRITE(ERR,1190) FIELDS,CARD_NAME,SID,CONT_CARD_NUM,JCARD(5),JCARD(6),JCARD(7)
                        WRITE(F06,1190) FIELDS,CARD_NAME,SID,CONT_CARD_NUM,JCARD(5),JCARD(6),JCARD(7)
                     ENDIF
                  ENDIF
               ENDIF

               IF (INTEGERS == 'Y') THEN                   ! Fields 2-9 of cont card have integers (or blank)
                  DO J=2,9
                     IF (JCARD(J)(1:) == ' ') CYCLE
                     CALL I4FLD ( JCARD(J), JF(J), I4INP1 )
                     IF (IERRFL(J) == 'N') THEN
                        ELID = I4INP1
                        IF (ELID <= 0) THEN
                           FATAL_ERR = FATAL_ERR+1
                           WRITE(ERR,1166) CARD_NAME,SID,CONT_CARD_NUM,J
                           WRITE(F06,1166) CARD_NAME,SID,CONT_CARD_NUM,J
                        ENDIF
                     ENDIF
                  ENDDO   
               ELSE                                        ! THRU_3 must be 'Y' or we wouldn't have gotten here 
                  CALL I4FLD ( JCARD(2), JF(2), I4INP1 )
                  CALL I4FLD ( JCARD(4), JF(4), I4INP2 )
                  IF ((IERRFL(2) == 'N') .AND. (IERRFL(4) == 'N')) THEN
                     ELID1 = I4INP1
                     ELID2 = I4INP2
                     IF (ELID2 < ELID1) THEN
                        FATAL_ERR = FATAL_ERR+1
                        IERR = IERR + 1
                        FIELDS = '2 and 4 '
                        WRITE(ERR,1153) FIELDS,CARD_NAME,SID,CONT_CARD_NUM
                        WRITE(F06,1153) FIELDS,CARD_NAME,SID,CONT_CARD_NUM
                     ENDIF
                     IF (ELID1 <= 0) THEN
                        FATAL_ERR = FATAL_ERR+1
                        IERR = IERR + 1
                        WRITE(ERR,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(2)
                        WRITE(F06,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(2)
                     ENDIF
                     IF (ELID2 <= 0) THEN
                        FATAL_ERR = FATAL_ERR+1
                        IERR = IERR + 1
                        WRITE(ERR,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(4)
                        WRITE(F06,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(4)
                     ENDIF
                  ENDIF
                  IF (THRU_6 == 'Y') THEN                  ! THRU_6 must be 'Y' or 'N' or we wouldn't have gotten here 
                     CALL I4FLD ( JCARD(5), JF(5), I4INP1 )
                     CALL I4FLD ( JCARD(7), JF(7), I4INP2 )
                     IF ((IERRFL(5) == 'N') .AND. (IERRFL(7) == 'N')) THEN
                        ELID1 = I4INP1
                        ELID2 = I4INP2
                        IF (ELID2 < ELID1) THEN
                           FATAL_ERR = FATAL_ERR+1
                           FIELDS = '5 and 7 '
                           IERR = IERR + 1
                           WRITE(ERR,1153) FIELDS,CARD_NAME,SID,CONT_CARD_NUM
                           WRITE(F06,1153) FIELDS,CARD_NAME,SID,CONT_CARD_NUM
                        ENDIF
                        IF (ELID1 <= 0) THEN
                           FATAL_ERR = FATAL_ERR+1
                           IERR = IERR + 1
                           WRITE(ERR,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(5)
                           WRITE(F06,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(5)
                        ENDIF
                        IF (ELID2 <= 0) THEN
                           FATAL_ERR = FATAL_ERR+1
                           IERR = IERR + 1
                           WRITE(ERR,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(7)
                           WRITE(F06,1166) CARD_NAME,SID,CONT_CARD_NUM,JF(7)
                        ENDIF
                     ENDIF
                  ENDIF  
               ENDIF

               IF (INTEGERS == 'Y') THEN
                  CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 ) 
               ELSE
                  CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,5,0,7,0,0 ) ! Fields 3 and 6 are "THRU"
                  CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )
               ENDIF
               CALL CRDERR ( CARD )

               IF (IERR == 0) THEN
                  WRITE(L1K) CARD
                  NTCARD = NTCARD + 1
               ENDIF
            ELSE
               CYCLE
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1153 FORMAT(' *ERROR  1153: INVALID DATA IN FIELDS ',A,' OF ',A,I8,' CONT ENTRY NUMBER ',I8,'.'                                   &
                    ,/,14X,' ELEM IDs MUST BE IN INCREASING ORDER FOR "THRU" OPTION')
  
 1166 FORMAT(' *ERROR  1166: ELEMENT ID ON ',A,I8,' CONT ENTRY NUMBER ',I8,' FIELD ',I3,' MUST BE > 0')

 1190 FORMAT(' *ERROR  1190: INVALID DATA IN FIELDS ',A,' OF ',A,I8,' CONTI ENTRY NUMBER ',I8                                      &
                    ,/,14X,' THOSE 3 FIELDS MUST BE IN THE FORM: "EID1 THRU EID2", BUT IT IS = "',3A,'"')

 1192 FORMAT(' *ERROR  1192: ID IN FIELD ',I3,' OF ',A,A,' MUST BE ',A,' BUT IS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_TEMPRP
