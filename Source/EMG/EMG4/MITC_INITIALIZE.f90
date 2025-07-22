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
      SUBROUTINE MITC_INITIALIZE ()

! Initialize element variables in MITC_STUF.

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, EPROP, XEL, BGRID, GRID_SNORM, TYPE, TE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MITC_STUF, ONLY             :  DIRECTOR, DIR_THICKNESS, GP_RS 
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE MATMULT_FFF_Interface

      IMPLICIT NONE 

      INTEGER(LONG)                   :: GP                ! Element grid point number
      INTEGER(LONG)                   :: I,J               ! DO loop indices

      REAL(DOUBLE)                    :: PSH(ELGP)       
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: TANGENT_R(3)
      REAL(DOUBLE)                    :: TANGENT_S(3)
      REAL(DOUBLE)                    :: NORMAL(3)         ! Intermediate value of some normal.

                                                           ! Normal defined by node positions at each node
      REAL(DOUBLE)                    :: MIDSURFACE_NORMAL(ELGP,3)
      REAL(DOUBLE)                    :: THICKNESS_FACTOR

! **********************************************************************************************************************************

! ----------------------------------------------------------------------------------------------------------------------------------
! R, S coordiantes of each node

      IF (TYPE(1:5) == 'QUAD4') THEN

         ! Bathe's node numbering relationship to R,S coordinates.
         GP_RS(1,1) =  ONE
         GP_RS(1,2) = -ONE
         GP_RS(1,3) = -ONE
         GP_RS(1,4) =  ONE
  
         GP_RS(2,1) =  ONE
         GP_RS(2,2) =  ONE
         GP_RS(2,3) = -ONE
         GP_RS(2,4) = -ONE

      ELSEIF (TYPE(1:5) == 'QUAD8') THEN

         GP_RS(1,1) = -ONE
         GP_RS(1,2) =  ONE
         GP_RS(1,3) =  ONE
         GP_RS(1,4) = -ONE
         GP_RS(1,5) =  ZERO
         GP_RS(1,6) =  ONE
         GP_RS(1,7) =  ZERO
         GP_RS(1,8) = -ONE
  
         GP_RS(2,1) = -ONE
         GP_RS(2,2) = -ONE
         GP_RS(2,3) =  ONE
         GP_RS(2,4) =  ONE
         GP_RS(2,5) = -ONE
         GP_RS(2,6) =  ZERO
         GP_RS(2,7) =  ONE
         GP_RS(2,8) =  ZERO

      ELSE

         WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
         WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF


! ----------------------------------------------------------------------------------------------------------------------------------
! Director vector at each node

      DO GP=1,ELGP

         CALL MITC_SHAPE_FUNCTIONS(GP_RS(1,GP), GP_RS(2,GP), PSH, DPSHG)

         TANGENT_R(:)=ZERO
         TANGENT_S(:)=ZERO

         ! TANGENT_R(r, s) = dX/dR = d/dR X = sum over nodes[ dN/dR X ]
         ! TANGENT_S(r, s) = dX/dS = d/dS X = sum over nodes[ dN/dS X ]
         DO I=1,ELGP
            DO J=1,3
               TANGENT_R(J) = TANGENT_R(J) + XEL(I,J) * DPSHG(1,I)
               TANGENT_S(J) = TANGENT_S(J) + XEL(I,J) * DPSHG(2,I)
            ENDDO
         ENDDO

         CALL CROSS(TANGENT_R, TANGENT_S, NORMAL)

         NORMAL = NORMAL / DSQRT(DOT_PRODUCT(NORMAL, NORMAL))

         MIDSURFACE_NORMAL(GP,:) = NORMAL

         NORMAL = GRID_SNORM(BGRID(GP),:)
                                     
                                                           ! Use the midsurface normal unless SNORM exists and it's
                                                           ! a linear element.
         IF (ANY(NORMAL /= ZERO) .AND. (TYPE(1:5) == 'QUAD4')) THEN
                                                           ! Transform SNORM from basic to XEL element coordinates.
            CALL MATMULT_FFF(TE, NORMAL, 3, 3, 1, DIRECTOR(GP,:))
         ELSE
            DIRECTOR(GP,:) = MIDSURFACE_NORMAL(GP,:)
         ENDIF

      ENDDO

! ----------------------------------------------------------------------------------------------------------------------------------
! Thickness in the direction of the director vector at each node

      ! Thickness is treated as uniform.
      ! To allow grid point thicknesses, this should be interpolated to midside nodes.
      DO GP=1,ELGP

         ! If SNORM is used, the director vector may be different from the midsurface normal where thickness is defined
         ! by the user. Scale the thickness to make it in the direction of the director vector.
         THICKNESS_FACTOR = DOT_PRODUCT(DIRECTOR(GP,:), MIDSURFACE_NORMAL(GP,:))

                                                           ! Error if the angle betwen them is greater than ~=89 degrees.
                                                           ! 90 degrees would divide by zero and >90 degrees would be
                                                           ! inside-out.
         IF(THICKNESS_FACTOR < 0.01) THEN                  
            WRITE(ERR,*) ' *ERROR: SNORM IS TOO FAR FROM MIDSURFACE NORMAL'
            WRITE(F06,*) ' *ERROR: SNORM IS TOO FAR FROM MIDSURFACE NORMAL'
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
         
         DIR_THICKNESS(GP)   = EPROP(1) / THICKNESS_FACTOR

      ENDDO

! ----------------------------------------------------------------------------------------------------------------------------------

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_INITIALIZE
