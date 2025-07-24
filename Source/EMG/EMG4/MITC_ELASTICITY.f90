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
      FUNCTION MITC_ELASTICITY ()

! Returns the 6x6 elasticity matrix.
! In the element coordinate system for QUAD4.
! In the material coordinate system for QUAD8.
 
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  EM, ET, EPROP
      USE CONSTANTS_1, ONLY           :  ZERO

      IMPLICIT NONE 
  
      INTEGER(LONG)                   :: I,J               ! DO loop indices

      REAL(DOUBLE)                    :: MITC_ELASTICITY(6,6)


! **********************************************************************************************************************************

                                                           ! Convert 2D material elasticity matrices to 3D.
      MITC_ELASTICITY(1,1) = EM(1,1)
      MITC_ELASTICITY(1,2) = EM(1,2)
      MITC_ELASTICITY(1,3) = ZERO
      MITC_ELASTICITY(1,4) = EM(1,3)
      MITC_ELASTICITY(1,5) = ZERO
      MITC_ELASTICITY(1,6) = ZERO

      MITC_ELASTICITY(2,2) = EM(2,2)
      MITC_ELASTICITY(2,3) = ZERO
      MITC_ELASTICITY(2,4) = EM(2,3)
      MITC_ELASTICITY(2,5) = ZERO
      MITC_ELASTICITY(2,6) = ZERO

      MITC_ELASTICITY(3,3) = ZERO
      MITC_ELASTICITY(3,4) = ZERO
      MITC_ELASTICITY(3,5) = ZERO
      MITC_ELASTICITY(3,6) = ZERO

      MITC_ELASTICITY(4,4) = EM(3,3)
      MITC_ELASTICITY(4,5) = ZERO
      MITC_ELASTICITY(4,6) = ZERO

      MITC_ELASTICITY(5,5) = ET(2,2) * EPROP(3)
      MITC_ELASTICITY(5,6) = ET(2,1) * EPROP(3)

      MITC_ELASTICITY(6,6) = ET(1,1) * EPROP(3)

      DO I=2,6                                             ! Copy UT to LT because it's symmetric.
         DO J=1,I-1
            MITC_ELASTICITY(I,J) = MITC_ELASTICITY(J,I)
         ENDDO 
      ENDDO   

      RETURN

! **********************************************************************************************************************************
  
      END FUNCTION MITC_ELASTICITY
