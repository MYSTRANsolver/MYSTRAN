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
  
      SUBROUTINE BD_SPCADD ( CARD, LARGE_FLD_INP, CC_SPC_FND )
  
! Processes SPCADD Bulk Data Cards. Reads and checks data and enters data into array SPCADD_SIDS

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSPCADDR, LSUB, NSPCADD, LSPCADDC, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SPCADD_BEGEND
      USE MODEL_STUF, ONLY            :  SPCADD_SIDS, SPCSET, SUBLOD
 
      USE BD_SPCADD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SPCADD'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_SPC_FND        ! 'Y' if B.D  card w/ same set ID as C.C. SPC = SID
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 3*BYTE)              :: NAME1   = 'SPC'   ! 
      CHARACTER( 6*BYTE)              :: NAME2   = 'SPCADD'! 
 
      INTEGER(LONG)                   :: I,J,K             ! DO loop index
      INTEGER(LONG)                   :: NUM_SETIDS        ! Counter on number of set ID's on SPCADD card 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: SETID             ! Set ID for this SPCADD Bulk Data card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SPCADD_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! SPCADD Bulk Data Card routine
 
!   FIELD   ITEM            ARRAY ELEMENT
!   -----   ------------    -------------
!    2      SPCADD set ID    SPCADD(nspcadd, 1)
!    3      SPC/SPC1 set ID  SPCADD(nspcadd, 2)
!    4      SPC/SPC1 set ID  SPCADD(nspcadd, 3)
!    5      SPC/SPC1 set ID  SPCADD(nspcadd, 4)
!    6      SPC/SPC1 set ID  SPCADD(nspcadd, 5)
!    7      SPC/SPC1 set ID  SPCADD(nspcadd, 6)
!    8      SPC/SPC1 set ID  SPCADD(nspcadd, 7)
!    9      SPC/SPC1 set ID  SPCADD(nspcadd, 8)
 
! 1st continuation card:
! 
!    2      SPC/SPC1 set ID  SPCADD(nspcadd, 9)
!    3      SPC/SPC1 set ID  SPCADD(nspcadd,10)
!    4      SPC/SPC1 set ID  SPCADD(nspcadd,11)
!    5      SPC/SPC1 set ID  SPCADD(nspcadd,12)
!    6      SPC/SPC1 set ID  SPCADD(nspcadd,13)
!    7      SPC/SPC1 set ID  SPCADD(nspcadd,14)
!    8      SPC/SPC1 set ID  SPCADD(nspcadd,15)
!    9      SPC/SPC1 set ID  SPCADD(nspcadd,16)
 
! Subsequent con't cards follow the same patterm as the 1st
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
      NSPCADD = NSPCADD+1
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), SETID )                ! Read set ID for this SPCADD Bulk Data card
      IF (IERRFL(2) == 'N') THEN
         SPCADD_SIDS(NSPCADD,1) = SETID
         DO I=1,NSUB
            IF (SETID == SPCSET) THEN
               CC_SPC_FND = 'Y'
            ENDIF
         ENDDO
      ENDIF   
 
      NUM_SETIDS = 1                                       ! Read SPCADD ID's on parent card.
      DO J=3,9
         IF (JCARD(J)(1:) == ' ') THEN
            CYCLE                                          ! CYCLE if a set ID field is blank
         ELSE
            NUM_SETIDS = NUM_SETIDS + 1
            IF (NUM_SETIDS > LSPCADDC) THEN
               WRITE(ERR,1139) SETID, NAME1, NAME2, LSPCADDC
               WRITE(F06,1139) SETID, NAME1, NAME2, LSPCADDC
               CALL OUTA_HERE ( 'Y' )                              ! Coding error, so quit
            ENDIF            
            CALL I4FLD ( JCARD(J), JF(J),  SPCADD_SIDS(NSPCADD,NUM_SETIDS) )
            DO K=1,NUM_SETIDS-1                            ! Check for duplicate set ID's
               IF (SPCADD_SIDS(NSPCADD,NUM_SETIDS) == SPCADD_SIDS(NSPCADD,K)) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1140) SPCADD_SIDS(NSPCADD,NUM_SETIDS),NAME2,SETID
                  WRITE(F06,1140) SPCADD_SIDS(NSPCADD,NUM_SETIDS),NAME2,SETID
               ENDIF
            ENDDO 
            IF (IERRFL(J) == 'N') THEN
               IF (SPCADD_SIDS(NSPCADD,NUM_SETIDS) <= 0) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1175) NAME1,NAME2,SETID,J,SPCADD_SIDS(NSPCADD,NUM_SETIDS)
                  WRITE(F06,1175) NAME1,NAME2,SETID,J,SPCADD_SIDS(NSPCADD,NUM_SETIDS)
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
            DO J=2,9
               IF (JCARD(J)(1:) == ' ') THEN
                  CYCLE
               ELSE
                  NUM_SETIDS = NUM_SETIDS + 1
                  IF (NUM_SETIDS > LSPCADDC) THEN
                     WRITE(ERR,1139) SETID,NAME1,NAME2,LSPCADDC
                     WRITE(F06,1139) SETID,NAME1,NAME2,LSPCADDC
                     CALL OUTA_HERE ( 'Y' )                        ! Coding error, so quit
                  ENDIF            
                  CALL I4FLD ( JCARD(J), JF(J),  SPCADD_SIDS(NSPCADD,NUM_SETIDS) )
                  DO K=1,NUM_SETIDS-1                       ! Check for duplicate set ID's
                     IF (SPCADD_SIDS(NSPCADD,NUM_SETIDS) == SPCADD_SIDS(NSPCADD,K)) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1140) SPCADD_SIDS(NSPCADD,NUM_SETIDS),NAME2,SETID
                        WRITE(ERR,1140) SPCADD_SIDS(NSPCADD,NUM_SETIDS),NAME2,SETID
                     ENDIF
                  ENDDO 
                  IF (IERRFL(J) == 'N') THEN
                     IF (SPCADD_SIDS(NSPCADD,NUM_SETIDS) <= 0) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1175) NAME1,NAME2,SETID,J,SPCADD_SIDS(NSPCADD,NUM_SETIDS)
                        WRITE(F06,1175) NAME1,NAME2,SETID,J,SPCADD_SIDS(NSPCADD,NUM_SETIDS)
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

 1175 FORMAT(' *ERROR  1175: ',A,' SET ID ON BULK DATA ',A,' ENTRY ID = ',I8,' IN FIELD ',I3,' IS = ',I8,'. MUST BE > 0')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SPCADD
