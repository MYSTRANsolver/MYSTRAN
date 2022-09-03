! ###############################################################################################################################
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

   MODULE CSHIFT_Interface

   INTERFACE

      SUBROUTINE CSHIFT ( CARD_IN, CHAR, CARD_SHIFTED, CHAR_COL, IERR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CSHIFT_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*) , INTENT(IN)            :: CARD_IN           ! Input Case Control card
      CHARACTER(LEN=LEN(CARD_IN)) , INTENT(OUT):: CARD_SHIFTED      ! C.C. card shifted to begin in 1st nonblank col after CHAR_COL
      CHARACTER(1*BYTE), INTENT(IN)            :: CHAR              ! Character to find in CARD
 
      INTEGER(LONG), INTENT(OUT)               :: IERR              ! Error indicator. If CHAR not found, IERR set to 1
      INTEGER(LONG), INTENT(OUT)               :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG), PARAMETER                 :: SUBR_BEGEND = CSHIFT_BEGEND

      END SUBROUTINE CSHIFT

   END INTERFACE

   END MODULE CSHIFT_Interface

