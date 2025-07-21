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
      FUNCTION MITC8_CARTESIAN_LOCAL_BASIS ( R, S )
 
! Finds the basis vectors of the cartesian local coordinate system expressed in the basic coordinate system.
! This is defined the same way as the material coordinate system in Simcenter Nastran with THETA=0.
! This definition is chosen because it is uniform over the surface of the element so stress and strain outputs
! can be interpolated/extrapolated in this coordinate system then transformed to the local element coordinate
! system at the corner grid points and center for output.
!
! First index of the result (row) is a vector component in basic coordinates (x,y,z)
! Second index of the result (column) is basis vector (x_l, y_l, normal)

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, XEB, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO

      USE MITC_SHAPE_FUNCTIONS_Interface
      USE CROSS_Interface

      IMPLICIT NONE 
      
      INTEGER(LONG)                   :: I                 ! DO loop indices

      REAL(DOUBLE)                    :: MITC8_CARTESIAN_LOCAL_BASIS(3,3)
      REAL(DOUBLE) , INTENT(IN)       :: R
      REAL(DOUBLE) , INTENT(IN)       :: S
      REAL(DOUBLE)                    :: PSH(ELGP)       
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: E_XI(3)
      REAL(DOUBLE)                    :: E_ETA(3)
      REAL(DOUBLE)                    :: Z_REF(3)
      REAL(DOUBLE)                    :: R_G1G2(3)
      REAL(DOUBLE)                    :: X(3)
      REAL(DOUBLE)                    :: Y(3)
      REAL(DOUBLE)                    :: Z(3)


! **********************************************************************************************************************************
      
 
      CALL MITC_SHAPE_FUNCTIONS(R, S, PSH, DPSHG)
      
                                                           ! Unit normal to the reference plane
      CALL CROSS(XEB(3,:) - XEB(1,:), XEB(4,:) - XEB(2,:), Z_REF)
      Z_REF = Z_REF / DSQRT(DOT_PRODUCT(Z_REF, Z_REF))

                                                           ! Project R_G1G2 onto the reference plane
      R_G1G2 = XEB(2,:) - XEB(1,:)
      R_G1G2 = R_G1G2 - Z_REF * DOT_PRODUCT(R_G1G2, Z_REF) / DOT_PRODUCT(Z_REF, Z_REF)

                                                           ! Unit normal to shell surface (Z)
      E_XI(:)=ZERO
      E_ETA(:)=ZERO
      DO I=1,ELGP
        E_XI(:) = E_XI(:) + XEB(I,:) * DPSHG(1,I)
        E_ETA(:) = E_ETA(:) + XEB(I,:) * DPSHG(2,I)
      ENDDO
      CALL CROSS(E_XI, E_ETA, Z)
      Z = Z / DSQRT(DOT_PRODUCT(Z, Z))

                                                           ! Y tangent to the surface
      CALL CROSS(Z, R_G1G2, Y)
      Y = Y / DSQRT(DOT_PRODUCT(Y, Y))

                                                           ! Rotate the projected R_G1G2 about Y to be tangent to the surface
      CALL CROSS(Y, Z, X)

      MITC8_CARTESIAN_LOCAL_BASIS(:,1) = X
      MITC8_CARTESIAN_LOCAL_BASIS(:,2) = Y
      MITC8_CARTESIAN_LOCAL_BASIS(:,3) = Z


      RETURN


! **********************************************************************************************************************************
  
      END FUNCTION MITC8_CARTESIAN_LOCAL_BASIS
