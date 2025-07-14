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
 
! Calculates g_r, g_s, g_t in global coordinates.
! These are also the columns of the Jacobian matrix.
! G(:,1) is g_r, etc.


      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, XEB, EPROP, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO

      USE MITC_SHAPE_FUNCTIONS_Interface
      USE MITC_GP_RS_Interface
      USE MITC_DIRECTOR_VECTOR_Interface

      IMPLICIT NONE 
      
      INTEGER(LONG)                   :: GP                ! Element grid point number

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: G(3,3)            ! basis vector in basic coordinates
      REAL(DOUBLE)                    :: PSH(ELGP)       
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to xi and eta.
      REAL(DOUBLE)                    :: DIRECTOR(3)       ! Director vector
      REAL(DOUBLE)                    :: GP_RS(2,ELGP)     ! Isoparametric coordinates of the nodes
      REAL(DOUBLE)                    :: RS_THICKNESS      ! Element thickness at R,S
      REAL(DOUBLE)                    :: THICKNESS(ELGP)   ! Element thickness at grid points

! **********************************************************************************************************************************


      ! Thickness is treated as uniform.
      ! To allow grid point thicknesses, this should be interpolated to midside nodes and at (R,S).
      DO GP=1,ELGP
         THICKNESS(GP) = EPROP(1)
      ENDDO
      RS_THICKNESS = EPROP(1)

      CALL MITC_SHAPE_FUNCTIONS(R, S, PSH, DPSHG)

      G(:,:) = ZERO

      GP_RS = MITC_GP_RS()

      ! Interpolate from the values at nodes
      ! g_r(r, s, t) = dX/dr = d/dr X + t/2 * d/dr (hv)
      !     = sum over nodes[ dN/dr X + t/2 * dN/dr (hv) ]
      DO GP=1,ELGP
         DIRECTOR = MITC_DIRECTOR_VECTOR(GP_RS(1,GP), GP_RS(2,GP))
         G(:,1) = G(:,1) + XEB(GP,:) * DPSHG(1,GP) + DIRECTOR * T/TWO * DPSHG(1,GP) * THICKNESS(GP)
         G(:,2) = G(:,2) + XEB(GP,:) * DPSHG(2,GP) + DIRECTOR * T/TWO * DPSHG(2,GP) * THICKNESS(GP)
      ENDDO


      IF(.TRUE.) THEN
         ! Interpolate director vector from nodes.
         ! This is different from evaluating it directly at R,S but it might still be OK.
         !
         ! Using this, the MITC_DIRECTOR_VECTOR function can be changed to only return values at nodes, not R,S.
         ! That will enable grid point normals more easily.
         
         DO GP=1,ELGP
            DIRECTOR = MITC_DIRECTOR_VECTOR(GP_RS(1,GP), GP_RS(2,GP))
            G(:,3) = G(:,3) + DIRECTOR * PSH(GP)
         ENDDO
                                                           ! Normalize to half thickness at R,S.
         G(:,3) = G(:,3) / DSQRT(DOT_PRODUCT(G(:,3), G(:,3))) * RS_THICKNESS/TWO
      
      ELSE

         DIRECTOR = MITC_DIRECTOR_VECTOR(R, S)
         G(:,3) = DIRECTOR * RS_THICKNESS/TWO
      
      ENDIF






      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_COVARIANT_BASIS
