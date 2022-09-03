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
  
      SUBROUTINE BD_LOAD ( CARD, LARGE_FLD_INP, CC_LOAD_FND )
  
! Processes LOAD Bulk Data Cards. Reads and checks data and enters data into arrays LOAD_SIDS and LOAD_FACS

!  1) Load set ID's from the LOAD Bulk Data card are entered into array LOAD_SIDS
!  2) Scale factors (overall for this LOAD card and individual for each set ID) are entered into array LOAD_FACS
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LLOADR, LSUB, NLOAD, LLOADC, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_LOAD_BEGEND
      USE MODEL_STUF, ONLY            :  LOAD_SIDS, LOAD_FACS, SUBLOD
 
      USE BD_LOAD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_LOAD'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD                ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2) ! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: NAME                ! JCARD(1) from parent entry
 
      INTEGER(LONG)                   :: I,J,K               ! DO loop index
      INTEGER(LONG)                   :: NUM_PAIRS           ! Counter on number of pairs of set ID's and scale factors on LOAD card
      INTEGER(LONG)                   :: ICONT     = 0       ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0       ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: SETID               ! Set ID for this LOAD Bulk Data card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_LOAD_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! LOAD Bulk Data Card routine
 
!   FIELD   ITEM            ARRAY ELEMENT
!   -----   ------------    -------------
!    2      SID              LOAD_SIDS(nload,1)
!    3      S0               LOAD_FACS(nload,1)
!    4      S1               LOAD_FACS(nload,2)
!    5      L1               LOAD_SIDS(nload,2)
!    6      S2               LOAD_FACS(nload,3)
!    7      L2               LOAD_SIDS(nload,3)
!    8      S3               LOAD_FACS(nload,4)
!    9      L3               LOAD_SIDS(nload,4)
 
! Optiona continuation cards:
! 
!   FIELD   ITEM            ARRAY ELEMENT
!   -----   ------------    -------------
!    2      S4               LOAD_FACS(nload,5)
!    3      L4               LOAD_SIDS(nload,5)
!    4      S5               LOAD_FACS(nload,6)
!    5      L5               LOAD_SIDS(nload,6)
!    6      S6               LOAD_FACS(nload,7)
!    7      L6               LOAD_SIDS(nload,7)
!    8      S7               LOAD_FACS(nload,8)
!    9      L7               LOAD_SIDS(nload,8)
 
! Subsequent con't cards follow the same patterm as the 1st
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
      NAME = JCARD(1)

! Check for overflow

      NLOAD = NLOAD+1
!xx   IF (NLOAD > LLOADR) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LLOADR
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LLOADR
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), LOAD_SIDS(NLOAD,1) )   ! Read set ID for this LOAD Bulk Data card
      IF (IERRFL(2) == 'N') THEN
         SETID = LOAD_SIDS(NLOAD,1)
         DO I=1,NSUB
            IF (SETID == SUBLOD(I,1)) THEN
               CC_LOAD_FND(I,1) = 'Y'
            ENDIF
         ENDDO
      ENDIF   
 
      CALL R8FLD ( JCARD(3), JF(3), LOAD_FACS(NLOAD,1) )   ! Read overall scale factor on LOAD card

      NUM_PAIRS = 1                                        ! Read pairs of load mags and load ID's on parent card.
!                                                          ! 1st "pair" is for the load set ID on this LOAD B.D. entry
      DO J=4,8,2
         IF ((JCARD(J)(1:) == ' ') .AND. (JCARD(J+1)(1:) == ' ')) THEN
            CYCLE                                          ! CYCLE if a pair is blank
         ELSE
            NUM_PAIRS = NUM_PAIRS + 1
            IF (NUM_PAIRS > LLOADC) THEN
               WRITE(ERR,1139) SETID, NAME, NAME, LLOADC
               WRITE(F06,1139) SETID, NAME, NAME, LLOADC
               CALL OUTA_HERE ( 'Y' )                       ! Coding error, so quit
            ENDIF            
            CALL R8FLD ( JCARD(J)  , JF(J)  , LOAD_FACS(NLOAD,NUM_PAIRS) )
            CALL I4FLD ( JCARD(J+1), JF(J+1), LOAD_SIDS(NLOAD,NUM_PAIRS) )
            DO K=1,NUM_PAIRS-1                             ! Check for duplicate set ID's
               IF (LOAD_SIDS(NLOAD,NUM_PAIRS) == LOAD_SIDS(NLOAD,K)) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1140) LOAD_SIDS(NLOAD,NUM_PAIRS),NAME,SETID
                  WRITE(F06,1140) LOAD_SIDS(NLOAD,NUM_PAIRS),NAME,SETID
               ENDIF
            ENDDO 
            IF (IERRFL(J+1) == 'N') THEN
               IF (LOAD_SIDS(NLOAD,NUM_PAIRS) <= 0) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1176) SETID,J+1,LOAD_SIDS(NLOAD,NUM_PAIRS)
                  WRITE(F06,1176) SETID,J+1,LOAD_SIDS(NLOAD,NUM_PAIRS)
               ENDIF
            ENDIF
         ENDIF
      ENDDO  

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
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
            DO J=2,8,2
               IF ((JCARD(J)(1:) == ' ') .AND. (JCARD(J+1)(1:) == ' ')) THEN
                  CYCLE
               ELSE
                  NUM_PAIRS = NUM_PAIRS + 1
                  IF (NUM_PAIRS > LLOADC) THEN
                     WRITE(ERR,1139) SETID, NAME, NAME, LLOADC
                     WRITE(F06,1139) SETID, NAME, NAME, LLOADC
                     CALL OUTA_HERE ( 'Y' )                 ! Coding error, so quit
                  ENDIF            
                  CALL R8FLD ( JCARD(J)  , JF(J)  , LOAD_FACS(NLOAD,NUM_PAIRS) )
                  CALL I4FLD ( JCARD(J+1), JF(J+1), LOAD_SIDS(NLOAD,NUM_PAIRS) )
                  DO K=1,NUM_PAIRS-1                       ! Check for duplicate set ID's
                     IF (LOAD_SIDS(NLOAD,NUM_PAIRS) == LOAD_SIDS(NLOAD,K)) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1140) LOAD_SIDS(NLOAD,NUM_PAIRS),NAME,SETID
                        WRITE(F06,1140) LOAD_SIDS(NLOAD,NUM_PAIRS),NAME,SETID
                     ENDIF
                  ENDDO 
                  IF (IERRFL(J+1) == 'N') THEN
                     IF (LOAD_SIDS(NLOAD,NUM_PAIRS) <= 0) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1176) SETID,J+1,LOAD_SIDS(NLOAD,NUM_PAIRS)
                        WRITE(F06,1176) SETID,J+1,LOAD_SIDS(NLOAD,NUM_PAIRS)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
 
            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9)! Make sure that there are no imbedded blanks in fields 2-9
            CALL CRDERR ( CARD )                           ! CRDERR prints errors found when reading fields

            CYCLE
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
 1139 FORMAT(' *ERROR  1139: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY PAIRS OF ',A,' SET IDs ON BULK DATA ',A,' ENTRY WITH SET ID = ',I8                           &
                    ,/,14X,' LIMIT IS = ',I12)

 1140 FORMAT(' *ERROR  1140: A DUPLICATE SET ID = ',I8,' WAS FOUND ON ',A,' BULK DATA ENTRY ID = ',I8)

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1176 FORMAT(' *ERROR  1176: LOAD SET ID ON BULK DATA LOAD ENTRY ID = ',I8,' IN FIELD ',I3,' IS = ',I8,'. MUST BE > 0')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_LOAD
