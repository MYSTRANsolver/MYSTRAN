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
  
      SUBROUTINE BD_LOAD0 ( CARD, LARGE_FLD_INP, ILOAD )
  
! Processes LOAD Bulk Data Cards to determine the number of pairs of load SID's and magnitudes on the LOAD B.D. card. Blank entries
! for pairs are not counted (specifically, if the load SID is 0 the pair is not counted).
! The number of pairs defined on this LOAD card will be returned to the calling routine so that the max number of pairs
! over all LOAD cards can be determined.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_LOAD0_BEGEND

      USE BD_LOAD0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_LOAD0'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG), INTENT(OUT)      :: ILOAD             ! Count of no. real load factors on this card. Starts with 1
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_LOAD0_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! LOAD Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      SID             LOAD set ID
!    3      S0              Overall scale factor
!    4      S1              Scale factor for set whose ID is L1
!    5      L1              Set ID of a FORCE, MOMENT or GRAV load 
!    6      S2              Scale factor for set whose ID is L2
!    7      L2              Set ID of a FORCE, MOMENT or GRAV load 
!    8      S3              Scale factor for set whose ID is L3
!    9      L3              Set ID of a FORCE, MOMENT or GRAV load 
 
! 1st continuation card:
! 
!   FIELD   ITEM            EXPLANATION
!   -----   ------------    -------------
!    2      S4              Scale factor for set whose ID is L4   
!    3      L4              Set ID of a FORCE, MOMENT or GRAV load  
!    4      S5              Scale factor for set whose ID is L5 
!    5      L5              Set ID of a FORCE, MOMENT or GRAV load 
!    6      S6              Scale factor for set whose ID is L6
!    7      L6              Set ID of a FORCE, MOMENT or GRAV load 
!    8      S7              Scale factor for set whose ID is L7 
!    9      L7              Set ID of a FORCE, MOMENT or GRAV load 
 
! Subsequent continuation cards follow the same pattern as the 1st continuation card

 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! ILOAD will count the number of real load factors on this LOAD card. It starts with 1 since there is an overall factor on the LOAD
! card, and continues as factors for the individual loads defined on the card are read
 
! Count pairs of load ID's/factors on parent card

      ILOAD = 1
      DO J=5,9,2
         IF ((JCARD(J)(1:) == ' ') .AND. (JCARD(J+1)(1:) == ' ')) THEN
            CYCLE
         ELSE
            ILOAD = ILOAD + 1
         ENDIF
      ENDDO 
 
! Count pairs of load ID's/factors on optional continuation cards
  
      DO 
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC0  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC20 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            DO J=2,8,2
               IF ((JCARD(J)(1:) == ' ') .AND. (JCARD(J+1)(1:) == ' ')) THEN
                  CYCLE
               ELSE
                  ILOAD = ILOAD + 1
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
  
      END SUBROUTINE BD_LOAD0
