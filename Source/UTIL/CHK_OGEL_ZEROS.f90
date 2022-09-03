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
 
      SUBROUTINE CHK_OGEL_ZEROS ( NUM )
 
! If a value in OGEL is -0.0, change it to +0.0 

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE SCONTR, ONLY                :  MOGEL
      USE CONSTANTS_1, ONLY           :  ZERO
      USE LINK9_STUFF, ONLY           :  OGEL

      IMPLICIT NONE
 
      INTEGER(LONG)                   :: I, J              ! DO loop indices
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows in OGEL to check
 
! *********************************************************************************************************************************

      DO I=1,NUM
         DO J=1,MOGEL
            IF (OGEL(I,J) == -ZERO) THEN
               OGEL(I,J) = ZERO
            ENDIF
         ENDDO
      ENDDO

! **********************************************************************************************************************************

      END SUBROUTINE CHK_OGEL_ZEROS

