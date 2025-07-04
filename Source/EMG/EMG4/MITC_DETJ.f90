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
      FUNCTION MITC_DETJ ( R, S, T )

! Calculates the Jacobian determinant in basic coordinates at a point in isoparametric coordinates.
 
      USE PENTIUM_II_KIND, ONLY       :  DOUBLE

      USE MITC_COVARIANT_BASIS_Interface
      USE CROSS_Interface

      IMPLICIT NONE 
      
      REAL(DOUBLE)                    :: MITC_DETJ
      REAL(DOUBLE) , INTENT(IN)       :: R
      REAL(DOUBLE) , INTENT(IN)       :: S
      REAL(DOUBLE) , INTENT(IN)       :: T
      REAL(DOUBLE)                    :: G(3,3)            ! covariant basis vectors in basic coordinates
      REAL(DOUBLE)                    :: DUM1(3)

! **********************************************************************************************************************************
      
      CALL MITC_COVARIANT_BASIS( R, S, T, G )

      !DET(J) = G_R . (G_S x G_T)
      CALL CROSS(G(:,2), G(:,3), DUM1)
      MITC_DETJ = G(1,1)*DUM1(1) + G(2,1)*DUM1(2) + G(3,1)*DUM1(3)

      RETURN


! **********************************************************************************************************************************
  
      END FUNCTION MITC_DETJ
