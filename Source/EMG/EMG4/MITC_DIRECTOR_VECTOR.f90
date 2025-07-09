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
      FUNCTION MITC_DIRECTOR_VECTOR ( R, S )
 
! Calculates the director vector in basic coordinates at a point in isoparametric coordinates.

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, XEB, TYPE
      USE CONSTANTS_1, ONLY           :  ZERO
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE SHP2DQ_Interface
      USE CROSS_Interface
      USE OUTA_HERE_Interface

      IMPLICIT NONE 
      
      INTEGER(LONG)                   :: I,J               ! DO loop indices

      REAL(DOUBLE)                    :: MITC_DIRECTOR_VECTOR(3)
      REAL(DOUBLE) , INTENT(IN)       :: R
      REAL(DOUBLE) , INTENT(IN)       :: S
      REAL(DOUBLE)                    :: PSH(ELGP)       
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: TANGENT_R(3)
      REAL(DOUBLE)                    :: TANGENT_S(3)
      REAL(DOUBLE)                    :: NORMAL(3)
      REAL(DOUBLE)                    :: DUM(2,ELGP)

      INTRINSIC                       :: DSQRT


! **********************************************************************************************************************************
      
! Choose the director vector to be normal to the nodal surface everywhere.
! This isn't required for MITC and could be averaged from adjacent elements.
 
      IF ((TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'QUAD8')) THEN

         CALL SHP2DQ ( 0, 0, ELGP, 'MITC_DIRECTOR_VECTOR', '', 0, R, S, 'N', PSH, DPSHG )

                                                           ! Change node numbering to match Bathe's MITC4+ paper.
         IF (TYPE(1:5) == 'QUAD4') THEN
            DUM = DPSHG
            DPSHG(:,1) = DUM(:,3)
            DPSHG(:,2) = DUM(:,4)
            DPSHG(:,3) = DUM(:,1)
            DPSHG(:,4) = DUM(:,2)
         ENDIF

      ELSE

         WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
         WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

      DO I=1,3
         TANGENT_R(I)=ZERO
         TANGENT_S(I)=ZERO
      ENDDO

      ! TANGENT_R(r, s) = dX/dR = d/dR X = sum over nodes[ dN/dR X ]
      ! TANGENT_S(r, s) = dX/dS = d/dS X = sum over nodes[ dN/dS X ]
      DO I=1,ELGP
         DO J=1,3
            TANGENT_R(J) = TANGENT_R(J) + XEB(I,J) * DPSHG(1,I)
            TANGENT_S(J) = TANGENT_S(J) + XEB(I,J) * DPSHG(2,I)
         ENDDO
      ENDDO

      CALL CROSS(TANGENT_R, TANGENT_S, NORMAL)

      NORMAL = NORMAL / DSQRT(DOT_PRODUCT(NORMAL, NORMAL))
      
      MITC_DIRECTOR_VECTOR = NORMAL

      RETURN


! **********************************************************************************************************************************
  
      END FUNCTION MITC_DIRECTOR_VECTOR
