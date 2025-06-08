! #################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
      SUBROUTINE QUAD8_ADD_TO_B ( B, POINT, COL, SCALAR, TENSOR )

! Add the UT of the 3x3 tensor times the scalar to a column of the B for sampling point POINT.
 
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE

      IMPLICIT NONE 
      
      REAL(DOUBLE) , INTENT(INOUT)    :: B(6,6*8,4)
      REAL(DOUBLE) , INTENT(IN)       :: SCALAR
      REAL(DOUBLE) , INTENT(IN)       :: TENSOR(3,3)

      INTEGER(LONG), INTENT(IN)       :: POINT
      INTEGER(LONG), INTENT(IN)       :: COL

! **********************************************************************************************************************************

      B(1, COL, POINT) = B(1, COL, POINT) + SCALAR * TENSOR(1,1)
      B(2, COL, POINT) = B(2, COL, POINT) + SCALAR * TENSOR(2,2)
      B(3, COL, POINT) = B(3, COL, POINT) + SCALAR * TENSOR(3,3)
      B(4, COL, POINT) = B(4, COL, POINT) + SCALAR * TENSOR(1,2)
      B(5, COL, POINT) = B(5, COL, POINT) + SCALAR * TENSOR(2,3)
      B(6, COL, POINT) = B(6, COL, POINT) + SCALAR * TENSOR(1,3)

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE QUAD8_ADD_TO_B
