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
  
      SUBROUTINE BD_RBE30 ( CARD, LARGE_FLD_INP, IRBE3 )
  
! Processes RBE3 Bulk Data Cards to estimate the number of triplets of grid/comp/coeff on logical RBE3 B.D.card.
! The number of triplets defined on this RBE3 card will be returned to the calling routine so that the max number of triplets
! over all RBE3 cards can be determined. This conservative estimate will be based on counting all non blank fields except ones with 
! a floating point number
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RBE30_BEGEND

      USE BD_RBE30_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RBE30'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! Type of TOKEN returned from subr TOKCHK
 
      INTEGER(LONG), INTENT(OUT)      :: IRBE3             ! Count of number of grid/comp/coeff triplets on this RBE3 logical card
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RBE30_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RBE3 Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      ELID            RBE3 elem ID
!    3      blank
!    4      REFGRID         Ref grid point ID (this DOF will go into M-set)
!    5      REFC            Ref component number (1-6) (comp of REFGRID) 
!    6      WT1             Weighting factor for 1st component of displ
!    7      C1              Comp number for associated with WT1 
!    8      G1,1            Grid associated with WT1
!    9      G1,2 or next WT or blank

! on optional continuation entries:
!    2-9    contain either a WT, Ci, or blank (to be conservative say that every field that is not floating pt is a grid and
!           therefore represents another triplet of (grid/comp/weight)


! Subsequent entries have the same format where a WTi is specified followed by a displ component Ci followed by a list of grids that
! have the WTi for that component
 
! Max number of grids on parent entry can be 2

      IRBE3 = 2

! Count continuation entries
  
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
               TOKEN = JCARD(J)(1:8)                       ! Only send the 1st 8 chars of this JCARD. It has been left justified
               CALL TOKCHK ( TOKEN, TOKTYP )
               IF (TOKTYP /= 'FL PT   ') THEN
                  IRBE3 = IRBE3 + 1
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
  
      END SUBROUTINE BD_RBE30
