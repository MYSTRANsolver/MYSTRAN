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
! This is the element coordinate system (x_l, y_l, normal) and is defined the same way as in MSC Nastran.
! First index of the result is a vector component in basic coordinates (x,y,z)
! Second index of the result is basis vector (x_l, y_l, normal)

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, XEB, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE SHP2DQ_Interface
      USE CROSS_Interface
      USE OUTA_HERE_Interface

      IMPLICIT NONE 
      
      INTEGER(LONG)                   :: I                 ! DO loop indices

      REAL(DOUBLE)                    :: MITC8_CARTESIAN_LOCAL_BASIS(3,3)
      REAL(DOUBLE) , INTENT(IN)       :: R
      REAL(DOUBLE) , INTENT(IN)       :: S
      REAL(DOUBLE)                    :: PSH(ELGP)       
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: E_XI(3)
      REAL(DOUBLE)                    :: E_ETA(3)
      REAL(DOUBLE)                    :: A(3)
      REAL(DOUBLE)                    :: B(3)
      REAL(DOUBLE)                    :: X_L_ACB(3)
      REAL(DOUBLE)                    :: Y_L_ACB(3)
      REAL(DOUBLE)                    :: T(3,3)

      INTRINSIC                       :: DSQRT


! **********************************************************************************************************************************
      
 
      IF (TYPE(1:5) == 'QUAD8') THEN

        CALL SHP2DQ ( 0, 0, ELGP, 'MITC8_CARTESIAN_LOCAL_BASIS', '', 0, R, S, 'N', PSH, DPSHG )

      ELSE

        WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
        WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF


                                                           ! e_ξ(r, s) = d/dR X = sum over nodes[ dN/dR X ]
                                                           ! e_η(r, s) = d/dS X = sum over nodes[ dN/dS X ]
      E_XI(:)=ZERO
      E_ETA(:)=ZERO
      DO I=1,ELGP
        E_XI(:) = E_XI(:) + XEB(I,:) * DPSHG(1,I)
        E_ETA(:) = E_ETA(:) + XEB(I,:) * DPSHG(2,I)
      ENDDO

                                                           !Normalize e_ξ and e_η
      E_XI = E_XI / DSQRT(DOT_PRODUCT(E_XI, E_XI))
      E_ETA = E_ETA / DSQRT(DOT_PRODUCT(E_ETA, E_ETA))

                                                           ! A = bisection of e_ξ and e_η
      A = E_XI + E_ETA
      A = A / DSQRT(DOT_PRODUCT(A, A))

                                                           ! B = common normal of e_ξ and e_η
      CALL CROSS(E_XI, E_ETA, B)
      B = B / DSQRT(DOT_PRODUCT(B, B))

                                                           ! x_l and y_l in the A C B coordinate system.
      X_L_ACB = (/ ONE/DSQRT(TWO), -ONE/DSQRT(TWO), ZERO /)
      Y_L_ACB = (/ ONE/DSQRT(TWO),  ONE/DSQRT(TWO), ZERO /)

                                                           ! Rotation matrix from A C B to basic coordinates
      T(:,1) = A
      CALL CROSS(B, A, T(:,2))
      T(:,3) = B
                                                           
                                                           ! Transform x_l and y_l to the basic coordinate system
      MITC8_CARTESIAN_LOCAL_BASIS(:,1) = MATMUL(T, X_L_ACB)
      MITC8_CARTESIAN_LOCAL_BASIS(:,2) = MATMUL(T, Y_L_ACB)
      MITC8_CARTESIAN_LOCAL_BASIS(:,3) = B


      RETURN


! **********************************************************************************************************************************
  
      END FUNCTION MITC8_CARTESIAN_LOCAL_BASIS
