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
 
      SUBROUTINE BD_CORD ( CARD, LARGE_FLD_INP )
 
! Processes CORD1C, CORD1R, CORD1S and CORD2C, CORD2R, CORD2S Bulk Data Cards
!  1) Sets coord type  (0 {2R},1 {2C},2 {2S}) and enters it into array CORD
!  2) Reads coord system ID and reference ID  and enters it into array CORD
!  3) Reads coord data into array RCORD

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, IERRFL, JCARD_LEN, JF, LCORD, NCORD, NCORD1, NCORD2, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CORD_BEGEND
      USE MODEL_STUF, ONLY            :  CORD, RCORD
 
      USE BD_CORD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CORD'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CORD_CID          ! Field 2 of CORD card (coord sys ID)
      CHARACTER(LEN(JCARD))           :: CORD_NAME         ! Name of coors sys
 
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CORD_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CORD1R Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Cord Type       CORD(ncord,1) 11 is CORD1R, 21 is CORD2R, 22 is CORD2C, 23 is CORD2S 
!    2      CID             CORD(ncord,2)
!    3      GA              Temporarily put into CORD(ncord,3)
!    3      GB              Temporarily put into CORD(ncord,4)
!    3      GC              Temporarily put into CORD(ncord,5)
!    3      RID             CORD(ncord,3) ref sys for grid A (will be entered later when GRID array is sorted and we can find GA 
!    4      RID             CORD(ncord,4) ref sys for grid B (will be entered later when GRID array is sorted and we can find GB 
!    5      RID             CORD(ncord,5) ref sys for grid C (will be entered later when GRID array is sorted and we can find GC 


! CORD2C, CORD2R, CORD2S Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
! on first card:
!    1      Cord Type       CORD(ncord,1) =02 {2R},12 {2C},22 {2S}
!    2      CID             CORD(ncord,2)
!    3      RID             CORD(ncord,3)
!    4      A1             RCORD(ncord,1)
!    5      A2             RCORD(ncord,2)
!    6      A3             RCORD(ncord,3)
!    7      B1             RCORD(ncord,4)
!    8      B2             RCORD(ncord,5)
!    9      B3             RCORD(ncord,6)
! on required second card:
!    2      C1             RCORD(ncord,7)
!    3      C2             RCORD(ncord,8)
!    4      C3             RCORD(ncord,9)
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      CORD_NAME = JCARD(1)
  
! ---------------------------------------------------------------------------------------------------------------------------------
      IF (CORD_NAME(1:5) == 'CORD1') THEN
                                                           ! Read data for 1st coord system defined on this entry
         NCORD1 = NCORD1 + 1
         NCORD  = NCORD  + 1

         CORD_CID = JCARD(2)
         IF      (JCARD(1)(1:6) == 'CORD1R') THEN
            CORD(NCORD,1) = 11
         ELSE IF (JCARD(1)(1:6) == 'CORD1C') THEN
            CORD(NCORD,1) = 12
         ELSE IF (JCARD(1)(1:6) == 'CORD1S') THEN
            CORD(NCORD,1) = 13
         ENDIF
 
         CALL I4FLD ( JCARD(2), JF(2), I4INP )             ! Read CID and make sure it is > 0 (cannot define 0, or basic, system)
         IF (IERRFL(2) == 'N') THEN
            IF (I4INP < 0) THEN                            ! --- CID cannot be negative
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1169) JF(2), CORD_NAME, JCARD(2), JCARD(2)
               WRITE(F06,1169) JF(2), CORD_NAME, JCARD(2), JCARD(2)
            ELSE IF (I4INP == 0) THEN                      ! --- CID cannot be 0 (can't define basic)
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1170) JF(2), CORD_NAME, JCARD(2), JCARD(2)
               WRITE(F06,1170) JF(2), CORD_NAME, JCARD(2), JCARD(2)
            ELSE                                           ! --- CID is OK
               CORD(NCORD,2) = I4INP
            ENDIF
         ENDIF

         CALL I4FLD ( JCARD(3), JF(3), I4INP )             ! --- Read GA. If > 0 put it, temporarily, into CORD(ncord,3)
         IF (IERRFL(3) == 'N') THEN
            IF (I4INP >= 0) THEN
               CORD(NCORD,3) = I4INP
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1169) JF(3), CORD_NAME, CORD_CID, JCARD(3)
               WRITE(F06,1169) JF(3), CORD_NAME, CORD_CID, JCARD(3)
            ENDIF
         ENDIF

         CALL I4FLD ( JCARD(4), JF(4), I4INP )             ! --- Read GB. If > 0 put it, temporarily, into CORD(ncord,4)
         IF (IERRFL(4) == 'N') THEN
            IF (I4INP >= 0) THEN
               CORD(NCORD,4) = I4INP
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1169) JF(4), CORD_NAME, CORD_CID, JCARD(4)
               WRITE(F06,1169) JF(4), CORD_NAME, CORD_CID, JCARD(4)
            ENDIF
         ENDIF

         CALL I4FLD ( JCARD(5), JF(5), I4INP )             ! --- Read GC. If > 0 put it, temporarily, into CORD(ncord,5)
         IF (IERRFL(5) == 'N') THEN
            IF (I4INP >= 0) THEN
               CORD(NCORD,5) = I4INP
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1169) JF(5), CORD_NAME, CORD_CID, JCARD(5)
               WRITE(F06,1169) JF(5), CORD_NAME, CORD_CID, JCARD(5)
            ENDIF
         ENDIF


         IF (JCARD(6)(1:) == ' ') THEN                     ! There is only 1 CORD1 defined on this entry

            CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,0,0,0,0 )
            CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )
            CALL CRDERR ( CARD )

         ELSE                                              ! A 2nd CORD1 is on this entry since field 6 is non-lank

            NCORD1 = NCORD1 + 1
            NCORD  = NCORD  + 1

            CORD_CID = JCARD(6)
            CORD(NCORD,1) = 11

            CALL I4FLD ( JCARD(6), JF(6), I4INP )          ! Read CID and make sure it is > 0 (cannot define 0, or basic, system)
            IF (IERRFL(6) == 'N') THEN
               IF (I4INP < 0) THEN                         ! --- CID cannot be negative
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1169) JF(6), CORD_NAME, JCARD(6), JCARD(6)
                  WRITE(F06,1169) JF(6), CORD_NAME, JCARD(6), JCARD(6)
               ELSE IF (I4INP == 0) THEN                   ! --- CID cannot be 0 (can't define basic)
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1170) JF(6), CORD_NAME, JCARD(6), JCARD(6)
                  WRITE(F06,1170) JF(6), CORD_NAME, JCARD(6), JCARD(6)
               ELSE                                        ! --- CID is OK
                  CORD(NCORD,2) = I4INP
               ENDIF
            ENDIF

            CALL I4FLD ( JCARD(7), JF(7), I4INP )          ! --- Read GA. If > 0 put it, temporarily, into CORD(ncord,7)
            IF (IERRFL(7) == 'N') THEN
               IF (I4INP >= 0) THEN
                  CORD(NCORD,3) = I4INP
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1169) JF(7), CORD_NAME, CORD_CID, JCARD(7)
                  WRITE(F06,1169) JF(7), CORD_NAME, CORD_CID, JCARD(7)
               ENDIF
            ENDIF

            CALL I4FLD ( JCARD(8), JF(8), I4INP )          ! --- Read GB. If > 0 put it, temporarily, into CORD(ncord,8)
            IF (IERRFL(8) == 'N') THEN
               IF (I4INP >= 0) THEN
                  CORD(NCORD,4) = I4INP
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1169) JF(8), CORD_NAME, CORD_CID, JCARD(8)
                  WRITE(F06,1169) JF(8), CORD_NAME, CORD_CID, JCARD(8)
               ENDIF
            ENDIF

            CALL I4FLD ( JCARD(9), JF(9), I4INP )          ! --- Read GC. If > 0 put it, temporarily, into CORD(ncord,9)
            IF (IERRFL(9) == 'N') THEN
               IF (I4INP >= 0) THEN
                  CORD(NCORD,5) = I4INP
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1169) JF(9), CORD_NAME, CORD_CID, JCARD(9)
                  WRITE(F06,1169) JF(9), CORD_NAME, CORD_CID, JCARD(9)
               ENDIF
            ENDIF

            CALL BD_IMBEDDED_BLANK   ( JCARD,0,0,0,0,6,7,8,9 )
            CALL CRDERR ( CARD )

         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (CORD_NAME(1:5) == 'CORD2') THEN

         NCORD2 = NCORD2 + 1
         NCORD  = NCORD  + 1

         CORD_CID = JCARD(2)

         IF      (JCARD(1)(1:6) == 'CORD2R') THEN
            CORD(NCORD,1) = 21
         ELSE IF (JCARD(1)(1:6) == 'CORD2C') THEN
            CORD(NCORD,1) = 22
         ELSE IF (JCARD(1)(1:6) == 'CORD2S') THEN
            CORD(NCORD,1) = 23
         ENDIF
 
         CALL I4FLD ( JCARD(2), JF(2), I4INP )             ! Read CID and make sure it is > 0 (cannot define 0, or basic, system)
         IF (IERRFL(2) == 'N') THEN
            IF (I4INP < 0) THEN                            ! --- CID cannot be negative
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1169) JF(2), CORD_NAME, JCARD(2), JCARD(2)
               WRITE(F06,1169) JF(2), CORD_NAME, JCARD(2), JCARD(2)
            ELSE IF (I4INP == 0) THEN                      ! --- CID cannot be 0 (can't define basic)
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1170) JF(2), CORD_NAME, JCARD(2), JCARD(2)
               WRITE(F06,1170) JF(2), CORD_NAME, JCARD(2), JCARD(2)
            ELSE                                           ! --- CID is OK
               CORD(NCORD,2) = I4INP
            ENDIF
         ENDIF

         CALL I4FLD ( JCARD(3), JF(3), I4INP )             ! Read RID and make sure it is >= 0
         IF (IERRFL(3) == 'N') THEN
            IF (I4INP >= 0) THEN
               CORD(NCORD,3) = I4INP
            ELSE                                           ! --- RID cannot be negative
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1169) JF(3), CORD_NAME, JCARD(3), JCARD(3)
               WRITE(F06,1169) JF(3), CORD_NAME, JCARD(3), JCARD(3)
            ENDIF
         ENDIF

         DO J = 1,6                                        ! Read real data on parent card
            CALL R8FLD ( JCARD(J+3), JF(J+3), RCORD(NCORD,J) )
         ENDDO
      
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )
         CALL CRDERR ( CARD )
 
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )              ! Read 2nd card
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            CALL R8FLD ( JCARD(2), JF(2), RCORD(NCORD,7) )
            CALL R8FLD ( JCARD(3), JF(3), RCORD(NCORD,8) )
            CALL R8FLD ( JCARD(4), JF(4), RCORD(NCORD,9) )

            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )
            CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )
            CALL CRDERR ( CARD )

         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1136) CORD_NAME, CORD_CID
            WRITE(F06,1136) CORD_NAME, CORD_CID
         ENDIF
 
! ----------------------------------------------------------------------------------------------------------------------------------
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1136 FORMAT(' *ERROR  1136: REQUIRED CONTINUATION FOR ',A,' ID = ',A,' MISSING')
 
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1169 FORMAT(' *ERROR  1169: FIELD ',I3,' ON ',A,' ID ',A,' CANNOT BE NEGATIVE. VALUE IS = ',A)

 1170 FORMAT(' *ERROR  1170: FIELD ',I3,' ON ',A,' ID ',A,' CANNOT BE 0 (CANNOT DEFINE BASIC SYSTEM). VALUE IS = ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CORD
