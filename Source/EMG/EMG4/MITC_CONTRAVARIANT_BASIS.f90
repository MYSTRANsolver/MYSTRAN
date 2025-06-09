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
      SUBROUTINE MITC_CONTRAVARIANT_BASIS ( G, G_CONTRA )
 
! Calculates the contravariant basis vectors g^1, g^2, g^3 to the specified covariant basis vectors
! g_r, g_s, g_t. This is the inverse Jacobian matrix where g_r/s/t are the columns of the Jacobian matrix.
! G(:,1) is g_r, etc.
! G_CONTRA(:,1) is g^1, etc.

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE

      USE CROSS_Interface
      
      IMPLICIT NONE 

      REAL(DOUBLE) , INTENT(IN)       :: G(3,3)            ! Covariant basis vectors
      REAL(DOUBLE) , INTENT(OUT)      :: G_CONTRA(3,3)     ! Contravariant basis vectors
      REAL(DOUBLE)                    :: DUM1(3)
      REAL(DOUBLE)                    :: DETJ


! **********************************************************************************************************************************

                                                           ! DET(J) = g_r . (g_s x g_t)
      CALL CROSS(G(:,2), G(:,3), DUM1)
      DETJ = G(1,1)*DUM1(1) + G(2,1)*DUM1(2) + G(3,1)*DUM1(3)

      CALL CROSS(G(:,2), G(:,3), G_CONTRA(:,1))
      CALL CROSS(G(:,3), G(:,1), G_CONTRA(:,2))
      CALL CROSS(G(:,1), G(:,2), G_CONTRA(:,3))

      G_CONTRA = G_CONTRA / DETJ

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_CONTRAVARIANT_BASIS
