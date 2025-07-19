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
      SUBROUTINE MITC_TRANSFORM_B ( TRANSFORM, B )

! Transform the strain-displacement matrix (tensor components) to a different basis.
! Equivalent to the sum
!    B_kl = A_ki Alj B~_ij

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP
      USE CONSTANTS_1, ONLY           :  ZERO

      IMPLICIT NONE 

      REAL(DOUBLE),  INTENT(INOUT)    :: B(6,6*ELGP)
      REAL(DOUBLE),  INTENT(IN)       :: TRANSFORM(3,3)
      REAL(DOUBLE)                    :: B_TRANSFORMED(6,6*ELGP)

      INTEGER(LONG)                   :: I,J,K,L           ! Tensor indices
      INTEGER(LONG)                   :: INDEX1(6)         ! Mapping of 6x1 vector index to 3x3 tensor first index.
      INTEGER(LONG)                   :: INDEX2(6)         ! Mapping of 6x1 vector index to 3x3 tensor second index.
      INTEGER(LONG)                   :: ROWS(3,3)         ! Mapping of 3x3 tensor indices to 6x1 vector index
      INTEGER(LONG)                   :: ROW
      INTEGER(LONG)                   :: IJ_ROW

      INTRINSIC                       :: DSQRT


! **********************************************************************************************************************************

      INDEX1 = (/ 1, 2, 3, 1, 2, 1 /)
      INDEX2 = (/ 1, 2, 3, 2, 3, 3 /)
      
      ROWS = RESHAPE((/ 1, 4, 6, 4, 2, 5, 6, 5, 3 /), SHAPE(ROWS))
      
      B_TRANSFORMED(:,:) = ZERO
      
      DO ROW=1,6
        K = INDEX1(ROW)
        L = INDEX2(ROW)
        DO I=1,3
          DO J=1,3
            IJ_ROW = ROWS(I,J)
            B_TRANSFORMED(ROW,:) = B_TRANSFORMED(ROW,:) + TRANSFORM(K,I) * TRANSFORM(L,J) * B(IJ_ROW,:)
          ENDDO
        ENDDO
      ENDDO

      B(:,:) = B_TRANSFORMED(:,:)

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_TRANSFORM_B
