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
 
      SUBROUTINE CHAR_FLD ( JCARDI, IFLD, CHAR_INP )
 
! Reads a field of CHARACTER data that can be 1 to LEN(JCARDI) chars in length
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  IERRFL, FATAL_ERR
 
      USE CHAR_FLD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)       :: JCARDI            ! The field of characters to read
      CHARACTER(LEN(JCARDI)), INTENT(OUT):: CHAR_INP          ! The character variable to read
 
      INTEGER(LONG), INTENT(IN)          :: IFLD              ! Field (2 - 9) of a Bulk Data card to read
      INTEGER(LONG)                      :: IOCHK             ! IOSTAT error value from READ
 
! **********************************************************************************************************************************
      CHAR_INP(1:) = ' '

      READ(JCARDI,'(A)',IOSTAT=IOCHK) CHAR_INP

      IF      (IOCHK < 0) THEN                             ! EOF/EOR during read

         IERRFL(IFLD) = 'Y'
         FATAL_ERR    = FATAL_ERR + 1

      ELSE IF (IOCHK == 0) THEN                            ! READ was OK

         IERRFL(IFLD) = 'N'

      ELSE IF (IOCHK > 0) THEN                             ! Error during READ

         IERRFL(IFLD) = 'Y'
         FATAL_ERR    = FATAL_ERR + 1

      ENDIF
 
      RETURN
 
! **********************************************************************************************************************************
 
      END SUBROUTINE CHAR_FLD
