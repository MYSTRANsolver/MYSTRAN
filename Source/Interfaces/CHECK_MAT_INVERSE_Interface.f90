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

   MODULE CHECK_MAT_INVERSE_Interface

   INTERFACE

      SUBROUTINE CHECK_MAT_INVERSE ( MAT_NAME, A, AI, NSIZE )


      USE PENTIUM_II_KIND, ONLY        : BYTE, DOUBLE, LONG
      USE IOUNT1, ONLY                 : ERR, F06
      USE CONSTANTS_1, ONLY            : ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                 :  EPSIL

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Name of input matrix

      INTEGER(LONG), INTENT(IN)       :: NSIZE             ! Row/col size of input matrices
      REAL(DOUBLE) , INTENT(IN)       :: A(NSIZE,NSIZE)    ! Matrix to invert
      REAL(DOUBLE) , INTENT(IN)       :: AI(NSIZE,NSIZE)   ! Inverse of A

      END SUBROUTINE CHECK_MAT_INVERSE

   END INTERFACE

   END MODULE CHECK_MAT_INVERSE_Interface

