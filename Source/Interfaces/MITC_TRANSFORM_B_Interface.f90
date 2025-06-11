! ###############################################################################################################################
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

   MODULE MITC_TRANSFORM_B_Interface

   INTERFACE

      SUBROUTINE MITC_TRANSFORM_B ( TRANSFORM, B )

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP
      USE CONSTANTS_1, ONLY           :  ZERO

      IMPLICIT NONE 

      REAL(DOUBLE),  INTENT(INOUT)    :: B(6,6*ELGP)
      REAL(DOUBLE),  INTENT(IN)       :: TRANSFORM(3,3)
      REAL(DOUBLE)                    :: B_TRANSFORMED(6,6*ELGP)
      REAL(DOUBLE)                    :: FACTOR

      INTEGER(LONG)                   :: I,J,K,L           ! Tensor indices
      INTEGER(LONG)                   :: INDEX1(6)         ! Mapping of 6x1 vector index to 3x3 tensor first index.
      INTEGER(LONG)                   :: INDEX2(6)         ! Mapping of 6x1 vector index to 3x3 tensor second index.
      INTEGER(LONG)                   :: ROWS(3,3)         ! Mapping of 3x3 tensor indices to 6x1 vector index
      INTEGER(LONG)                   :: ROW
      INTEGER(LONG)                   :: IJ_ROW

      INTRINSIC                       :: DSQRT

      END SUBROUTINE MITC_TRANSFORM_B

   END INTERFACE

   END MODULE MITC_TRANSFORM_B_Interface

