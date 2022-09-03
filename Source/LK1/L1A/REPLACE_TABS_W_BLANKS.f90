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
 
      SUBROUTINE REPLACE_TABS_W_BLANKS ( CARD )
 
! Searches input CARD for tab characters and replaces them with 1 white space character. Used primarily for Exec Control and 
! Case Control entries (but not Bulk Data entries - which are handled differently).

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC 
      USE SUBR_BEGEND_LEVELS, ONLY    :  REPLACE_TABS_W_BLANKS_BEGEND

      USE REPLACE_TABS_W_BLANKS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REPLACE_TABS_W_BLANKS'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! Input entry character line
      CHARACTER(LEN=LEN(CARD))        :: CARD0             ! Temporary CARD

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REPLACE_TABS_W_BLANKS_BEGEND
		INTEGER(LONG)                   :: I                 ! DO loop index
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Strip all tab chars from input CARD

         CARD0(1:) = ' '
         DO I=1,LEN(CARD)
            IF (CARD(I:I) == ACHAR(9)) THEN
               CARD0(I:I) = ' '
            ELSE
               CARD0(I:I) = CARD(I:I)
            ENDIF
         ENDDO
         CARD(1:) = CARD0(1:)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE REPLACE_TABS_W_BLANKS

