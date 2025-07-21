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
      FUNCTION MITC4_CARTESIAN_LOCAL_BASIS ( R, S, T )
 
! Reference [2]:
!  MITC4 paper "A continuum mechanics based four-node shell element for general nonlinear analysis" 
!     by Dvorkin and Bathe

! First index of the result (row) is a vector component in basic coordinates (x,y,z)
! Second index of the result (column) is basis vector (x_l, y_l, normal)

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE

      USE MITC_COVARIANT_BASIS_Interface
      USE CROSS_Interface

      IMPLICIT NONE 

      REAL(DOUBLE)                    :: MITC4_CARTESIAN_LOCAL_BASIS(3,3)
      REAL(DOUBLE) , INTENT(IN)       :: R
      REAL(DOUBLE) , INTENT(IN)       :: S
      REAL(DOUBLE) , INTENT(IN)       :: T
      REAL(DOUBLE)                    :: G(3,3)
      REAL(DOUBLE)                    :: X(3)
      REAL(DOUBLE)                    :: Y(3)
      REAL(DOUBLE)                    :: Z(3)

! **********************************************************************************************************************************
      
      CALL MITC_COVARIANT_BASIS( R, S, T, G )
      
      Z = G(:,3)
      Z = Z / DSQRT(DOT_PRODUCT(Z, Z))

      CALL CROSS(G(:,2), Z, X)
      X = X / DSQRT(DOT_PRODUCT(X, X))

      CALL CROSS(Z, X, Y)

      MITC4_CARTESIAN_LOCAL_BASIS(:,1) = X
      MITC4_CARTESIAN_LOCAL_BASIS(:,2) = Y
      MITC4_CARTESIAN_LOCAL_BASIS(:,3) = Z

      RETURN

! **********************************************************************************************************************************
  
      END FUNCTION MITC4_CARTESIAN_LOCAL_BASIS
