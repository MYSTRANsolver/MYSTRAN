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
 
      SUBROUTINE MKJCARD ( CALLING_SUBR, CARD, JCARD )
 
! Routine to create JCARD, a set of 10 CHAR fields (each of size JCARD_LEN), from input char CARD
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE SCONTR, ONLY                :  JCARD_LEN
 
      USE MKJCARD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*)  , INTENT(IN)       :: CALLING_SUBR      ! Subr that called this one
      CHARACTER(LEN=*)  , INTENT(IN)       :: CARD              ! A MYSTRAN data card
      CHARACTER(LEN=JCARD_LEN), INTENT(OUT):: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                        :: I
      INTEGER(LONG)                        :: K1,K2             ! Range for setting CARD = JCARD
 
! **********************************************************************************************************************************
      DO I=1,10
         K1 = JCARD_LEN*(I-1) + 1
         K2 = K1 + JCARD_LEN - 1
         JCARD(I)(1:JCARD_LEN) = CARD(K1:K2)
      ENDDO 
      RETURN

! **********************************************************************************************************************************

! **********************************************************************************************************************************

      END SUBROUTINE MKJCARD
