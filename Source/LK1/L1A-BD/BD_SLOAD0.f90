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
  
      SUBROUTINE BD_SLOAD0 ( CARD, NUM_PAIRS )
  
! Processes SLOAD Bulk Data Cards to count the number of SLOAD points/force mags on one entry

!   SETID, scalar point, load mag
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, IERRFL, JCARD_LEN, JF
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SLOAD0_BEGEND
 
      USE BD_SLOAD0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SLOAD0'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD                ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of characters making up CARD
 
      INTEGER(LONG), INTENT(OUT)      :: NUM_PAIRS           ! Number of pairs of SPOINT/force MAG on a SLOAD entry (can be up to 3)
      INTEGER(LONG)                   :: I                   ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SLOAD0_BEGEND
  
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

      NUM_PAIRS = 0
      DO I=1,3
         IF (JCARD(3+2*(I-1))(1:) /= ' ') THEN
            NUM_PAIRS = NUM_PAIRS + 1
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
 
      END SUBROUTINE BD_SLOAD0
