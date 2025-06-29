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
      SUBROUTINE TRANSFORM_SHELL_STR ( T, STR_VEC, SHR_FAC )

! Transforms a shell stress or strain vector with separate membrane, bending, and transverse shear terms.
! Use SHR_FAC = ONE for stress
! Use SHR_FAC = TWO for engineering strain to make it convert to tensor strain for transforming

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO

      IMPLICIT NONE 

      REAL(DOUBLE),  INTENT(IN)       :: T(3,3)
      REAL(DOUBLE),  INTENT(INOUT)    :: STR_VEC(9)
      REAL(DOUBLE),  INTENT(IN)       :: SHR_FAC
      REAL(DOUBLE)                    :: STR_TENSOR(3,3)
      REAL(DOUBLE)                    :: DUM33(3,3)


! **********************************************************************************************************************************

                                                           ! Membrane and transverse shear
      STR_TENSOR(1,1) = STR_VEC(1)           ; STR_TENSOR(1,2) = STR_VEC(3) / SHR_FAC ;   STR_TENSOR(1,3) = STR_VEC(7) / SHR_FAC
      STR_TENSOR(2,1) = STR_VEC(3) / SHR_FAC ; STR_TENSOR(2,2) = STR_VEC(2)           ;   STR_TENSOR(2,3) = STR_VEC(8) / SHR_FAC
      STR_TENSOR(3,1) = STR_VEC(7) / SHR_FAC ; STR_TENSOR(3,2) = STR_VEC(8) / SHR_FAC ;   STR_TENSOR(3,3) = ZERO

      CALL MATMULT_FFF (STR_TENSOR, TRANSPOSE(T), 3, 3, 3, DUM33 )
      CALL MATMULT_FFF (T, DUM33, 3, 3, 3, STR_TENSOR )

      STR_VEC(1) = STR_TENSOR(1,1)
      STR_VEC(2) = STR_TENSOR(2,2)
      STR_VEC(3) = STR_TENSOR(1,2) * SHR_FAC
      STR_VEC(7) = STR_TENSOR(1,3) * SHR_FAC
      STR_VEC(8) = STR_TENSOR(2,3) * SHR_FAC

                                                           ! Bending
      STR_TENSOR(1,1) = STR_VEC(4)           ;   STR_TENSOR(1,2) = STR_VEC(6) / SHR_FAC ;   STR_TENSOR(1,3) = ZERO
      STR_TENSOR(2,1) = STR_VEC(6) / SHR_FAC ;   STR_TENSOR(2,2) = STR_VEC(5)           ;   STR_TENSOR(2,3) = ZERO
      STR_TENSOR(3,1) = ZERO                 ;   STR_TENSOR(3,2) = ZERO                 ;   STR_TENSOR(3,3) = ZERO

      CALL MATMULT_FFF (STR_TENSOR, TRANSPOSE(T), 3, 3, 3, DUM33 )
      CALL MATMULT_FFF (T, DUM33, 3, 3, 3, STR_TENSOR )

      STR_VEC(4) = STR_TENSOR(1,1)
      STR_VEC(5) = STR_TENSOR(2,2)
      STR_VEC(6) = STR_TENSOR(1,2) * SHR_FAC


      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE TRANSFORM_SHELL_STR
