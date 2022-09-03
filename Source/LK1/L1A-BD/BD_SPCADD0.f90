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
  
      SUBROUTINE BD_SPCADD0 ( CARD, LARGE_FLD_INP, ISPCADD )
  
! Processes SPCADD Bulk Data Cards to count the number of SPC (or SPC1) set ID's on this logical SPCADD card. The calling routine
! determines the max number od set ID's over all SPCADD cards in the data deck
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SPCADD0_BEGEND
      USE MODEL_STUF, ONLY            :  SPCADD_SIDS

      USE BD_SPCADD0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SPCADD0'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG), INTENT(OUT)      :: ISPCADD           ! Count of number of SPC or SPC1 set ID's defined on the SPCADD
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SPCADD0_BEGEND
 
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
 
! ISPCADD will count the number of SPC or SPC1 set ID's on this SPCADD card. It starts with 1 since there is the SPCADD set ID.
 
! Count SPC/SPC1 set ID's on parent card

      ISPCADD = 1
      DO J=3,9
         IF (JCARD(J)(1:) == ' ') THEN
            CYCLE
         ELSE
            ISPCADD = ISPCADD + 1
         ENDIF
      ENDDO 
 
! Count SPC/SPC1 set ID's on optional continuation cards
  
      DO 
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC0  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC20 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            DO J=2,9
               IF (JCARD(J)(1:) == ' ') THEN
                  CYCLE
               ELSE
                  ISPCADD = ISPCADD + 1
               ENDIF
            ENDDO  
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
  
      END SUBROUTINE BD_SPCADD0
