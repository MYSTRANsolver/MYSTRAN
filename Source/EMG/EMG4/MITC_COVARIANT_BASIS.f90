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
      SUBROUTINE MITC_COVARIANT_BASIS ( R, S, T, G )
 
! Calculates g_r, g_s, g_t in XEL element coordinates.
! These are also the columns of the Jacobian matrix.
! G(:,1) is g_r, etc.

! Ref [3] SesamX blog https://www.sesamx.io/blog/shell_finite_element/

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, XEL
      USE CONSTANTS_1, ONLY           :  ZERO, TWO
      USE MITC_STUF, ONLY             :  DIRECTOR, DIR_THICKNESS

      USE MITC_SHAPE_FUNCTIONS_Interface

      IMPLICIT NONE 
      
      INTEGER(LONG)                   :: GP                ! Element grid point number

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: G(3,3)            ! basis vector in basic coordinates
      REAL(DOUBLE)                    :: PSH(ELGP)       
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to xi and eta.

! **********************************************************************************************************************************

      CALL MITC_SHAPE_FUNCTIONS(R, S, PSH, DPSHG)

      G(:,:) = ZERO

      ! Interpolate from the values at nodes
      DO GP=1,ELGP
         ! g_r(r, s, t) = dX/dr = d/dr X + t/2 * d/dr (hv)
         !     = sum over nodes[ dN/dr X + t/2 * dN/dr (hv) ]
         G(:,1) = G(:,1) + XEL(GP,:) * DPSHG(1,GP) + DIRECTOR(GP,:) * T/TWO * DPSHG(1,GP) * DIR_THICKNESS(GP)
         G(:,2) = G(:,2) + XEL(GP,:) * DPSHG(2,GP) + DIRECTOR(GP,:) * T/TWO * DPSHG(2,GP) * DIR_THICKNESS(GP)
         ! Interpolate director vector * thickness.
         G(:,3) = G(:,3) + DIRECTOR(GP,:) * DIR_THICKNESS(GP) / TWO * PSH(GP)
      ENDDO
   
      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_COVARIANT_BASIS
