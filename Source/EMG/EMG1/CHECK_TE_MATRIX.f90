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

      SUBROUTINE CHECK_TE_MATRIX ( TE_IN, NAME_IN )

! Checks TE_IN(t)*TE_IN to see if the element transformation matrix times its transpose is an identity matrix

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG

      USE CHECK_TE_MATRIX_USE_IFs                          ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Name for output purposes

      INTEGER(LONG)                   :: I,J               ! DO loop indices

      REAL(DOUBLE), INTENT(IN)        :: TE_IN(3,3)        ! Input TE matrix
      REAL(DOUBLE)                    :: DUM1(3,3)         ! Dummy matrix in a matrix multiply
      REAL(DOUBLE)                    :: DUM2(3,3)         ! Dummy matrix in a matrix multiply

! **********************************************************************************************************************************
      DO I=1,3
         DO J=1,3
            DUM1(I,J) = TE_IN(I,J)
         ENDDO
      ENDDO

      CALL MATMULT_FFF_T ( DUM1, TE_IN, 3, 3, 3, DUM2 )
      WRITE(BUG,1) NAME_IN, NAME_IN
      DO I=1,3
         WRITE(BUG,2) (DUM2(I,J),J=1,3)
      ENDDO
      WRITE(BUG,*)

      RETURN

! **********************************************************************************************************************************
    1 FORMAT('  Check on ',A,'(t)*',A,' to see if it is the identity matrix:',/,                                                   &
             '  -----------------------------------------------------------------')

    2 FORMAT(3(1ES14.6))

      END SUBROUTINE CHECK_TE_MATRIX
