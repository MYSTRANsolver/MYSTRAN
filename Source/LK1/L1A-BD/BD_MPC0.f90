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
  
      SUBROUTINE BD_MPC0 ( CARD, LARGE_FLD_INP, IMPC )
  
! Processes MPC Bulk Data Cards to determine the number of triplets of grid/comp/coeff on logical MPC B.D.card. Blank entries
! for triplets are not counted.
! The number of triplets defined on this MPC card will be returned to the calling routine so that the max number of triplets
! over all MPC cards can be determined.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_MPC0_BEGEND

      USE BD_MPC0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_MPC0'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG), INTENT(OUT)      :: IMPC              ! Count of number of grid/comp/coeff triplets on this MPC logical card
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_MPC0_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! MPC Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      SID             MPC set ID
!    3      DEP_GRD         Dependent grid
!    4      DCOMP           Component for dependent grid 
!    5      DVAL            Coeff (value) for dep grid/comp 
!    6      IND_GRD         1st independent grid
!    7      ICOMP           Component for 1st indep grid 
!    8      IVAL            Coeff (value) for 1st indep grid/comp
 
! 
!   FIELD   ITEM            EXPLANATION
!   -----   ------------    -------------
!    3      IND_GRD         2nd independent grid
!    4      ICOMP           Component for 2nd indep grid 
!    5      IVAL            Coeff (value) for 2nd indep grid/comp
!    6      IND_GRD         3rd independent grid
!    7      ICOMP           Component for 3rd indep grid 
!    8      IVAL            Coeff (value) for 3rd indep grid/comp
 
! Subsequent continuation cards follow the same pattern as the 1st continuation card
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! IMPC will count the number of grid/comp/coeff triplets. We will start with 1 since there must be a dependent grid/comp/coeff
! defined in fields 3, 4, 5 and count the independent triplets (the 1st of which is on the parent card in fields 6, 7, 8)
 
      IMPC = 1
      IF ((JCARD(6)(1:) /= ' ') .OR. (JCARD(7)(1:) /= ' ') .OR. (JCARD(8)(1:) /= ' ')) THEN
         IMPC = IMPC + 1
      ENDIF
 
! Count triplets of grid/comp/coeff on optional continuation cards
  
      DO 
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC0  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC20 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            DO J=3,8,3                                     ! Count up to 2 triplets per cont card (fields 3,4,5 and fields 6,7,8)
               IF ((JCARD(J)(1:) /= ' ') .OR. (JCARD(J+1)(1:) /= ' ') .OR. (JCARD(J+2)(1:) /= ' ')) THEN
                  CYCLE
               ELSE
                  IMPC = IMPC + 1
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
  
      END SUBROUTINE BD_MPC0
