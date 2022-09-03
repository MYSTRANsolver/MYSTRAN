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
  
      SUBROUTINE BD_MPCADD ( CARD, LARGE_FLD_INP, CC_MPC_FND )
  
! Processes MPCADD Bulk Data Cards. Reads and checks data and enters data into array MPCADD_SIDS

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LMPCADDR, LSUB, NMPCADD, LMPCADDC, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_MPCADD_BEGEND
      USE MODEL_STUF, ONLY            :  MPCADD_SIDS, MPCSET, SUBLOD
 
      USE BD_MPCADD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_MPCADD'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_MPC_FND        ! 'Y' if B.D  card w/ same set ID as C.C. MPC = SID
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 3*BYTE)              :: NAME1   = 'MPC'   ! Name for output error message use
      CHARACTER( 6*BYTE)              :: NAME2   = 'MPCADD'! Name for output error message use
 
      INTEGER(LONG)                   :: I,J,K             ! DO loop index
      INTEGER(LONG)                   :: NUM_SETIDS        ! Counter on number of set ID's on MPCADD card. The SID defining this
!                                                            MPCADD card is counted  as the first one. 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: SETID             ! Set ID for this MPCADD Bulk Data card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_MPCADD_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! MPCADD Bulk Data Card routine
 
!   FIELD   ITEM            ARRAY ELEMENT
!   -----   ------------    -------------
!    2      MPCADD set ID    MPCADD(nspcadd, 1)
!    3      MPC/MPC1 set ID  MPCADD(nspcadd, 2)
!    4      MPC/MPC1 set ID  MPCADD(nspcadd, 3)
!    5      MPC/MPC1 set ID  MPCADD(nspcadd, 4)
!    6      MPC/MPC1 set ID  MPCADD(nspcadd, 5)
!    7      MPC/MPC1 set ID  MPCADD(nspcadd, 6)
!    8      MPC/MPC1 set ID  MPCADD(nspcadd, 7)
!    9      MPC/MPC1 set ID  MPCADD(nspcadd, 8)
 
! 1st continuation card:
! 
!    2      MPC/MPC1 set ID  MPCADD(nspcadd, 9)
!    3      MPC/MPC1 set ID  MPCADD(nspcadd,10)
!    4      MPC/MPC1 set ID  MPCADD(nspcadd,11)
!    5      MPC/MPC1 set ID  MPCADD(nspcadd,12)
!    6      MPC/MPC1 set ID  MPCADD(nspcadd,13)
!    7      MPC/MPC1 set ID  MPCADD(nspcadd,14)
!    8      MPC/MPC1 set ID  MPCADD(nspcadd,15)
!    9      MPC/MPC1 set ID  MPCADD(nspcadd,16)
 
! Subsequent con't cards follow the same patterm as the 1st
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NMPCADD = NMPCADD+1
!xx   IF (NMPCADD > LMPCADDR) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LMPCADDR
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LMPCADDR
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), SETID )                ! Read set ID for this MPCADD Bulk Data card
      IF (IERRFL(2) == 'N') THEN
         MPCADD_SIDS(NMPCADD,1) = SETID
         DO I=1,NSUB
            IF (SETID == MPCSET) THEN
               CC_MPC_FND = 'Y'
            ENDIF
         ENDDO
      ENDIF   
 
      NUM_SETIDS = 1                                       ! Read MPCADD ID's on parent card.
      DO J=3,9
         IF (JCARD(J)(1:) == ' ') THEN
            CYCLE                                          ! CYCLE if a set ID field is blank
         ELSE
            NUM_SETIDS = NUM_SETIDS + 1
            IF (NUM_SETIDS > LMPCADDC) THEN
               WRITE(ERR,1139) SETID,NAME1,NAME2,LMPCADDC
               WRITE(F06,1139) SETID,NAME1,NAME2,LMPCADDC
               CALL OUTA_HERE ( 'Y' )                      ! Coding error, so quit
            ENDIF            
            CALL I4FLD ( JCARD(J), JF(J),  MPCADD_SIDS(NMPCADD,NUM_SETIDS) )
            DO K=1,NUM_SETIDS-1                            ! Check for duplicate set ID's
               IF (MPCADD_SIDS(NMPCADD,NUM_SETIDS) == MPCADD_SIDS(NMPCADD,K)) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1140) MPCADD_SIDS(NMPCADD,NUM_SETIDS),NAME2,SETID
                  WRITE(F06,1140) MPCADD_SIDS(NMPCADD,NUM_SETIDS),NAME2,SETID
               ENDIF
            ENDDO 
            IF (IERRFL(J) == 'N') THEN
               IF (MPCADD_SIDS(NMPCADD,NUM_SETIDS) <= 0) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1175) NAME1,NAME2,SETID,J,MPCADD_SIDS(NMPCADD,NUM_SETIDS)
                  WRITE(F06,1175) NAME1,NAME2,SETID,J,MPCADD_SIDS(NMPCADD,NUM_SETIDS)
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
                  IF (NUM_SETIDS > LMPCADDC) THEN
                     WRITE(ERR,1139) SETID,NAME1,NAME2,LMPCADDC
                     WRITE(F06,1139) SETID,NAME1,NAME2,LMPCADDC
                     CALL OUTA_HERE ( 'Y' )                        ! Coding error, so quit
                  ENDIF            
                  CALL I4FLD ( JCARD(J), JF(J),  MPCADD_SIDS(NMPCADD,NUM_SETIDS) )
                  DO K=1,NUM_SETIDS-1                       ! Check for duplicate set ID's
                     IF (MPCADD_SIDS(NMPCADD,NUM_SETIDS) == MPCADD_SIDS(NMPCADD,K)) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1140) MPCADD_SIDS(NMPCADD,NUM_SETIDS),NAME2,SETID
                        WRITE(ERR,1140) MPCADD_SIDS(NMPCADD,NUM_SETIDS),NAME2,SETID
                     ENDIF
                  ENDDO 
                  IF (IERRFL(J) == 'N') THEN
                     IF (MPCADD_SIDS(NMPCADD,NUM_SETIDS) <= 0) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1175) NAME1,NAME2,SETID,J,MPCADD_SIDS(NMPCADD,NUM_SETIDS)
                        WRITE(F06,1175) NAME1,NAME2,SETID,J,MPCADD_SIDS(NMPCADD,NUM_SETIDS)
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
 
      END SUBROUTINE BD_MPCADD
