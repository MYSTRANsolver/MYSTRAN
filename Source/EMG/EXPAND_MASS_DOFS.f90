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
      SUBROUTINE EXPAND_MASS_DOFS ( M_1DOF )

! Copies per-node mass values to all 3 of each node's translational DOFs in the element mass matrix ME.

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE, LONG
      USE MODEL_STUF, ONLY            :  ELGP, ME
      USE CONSTANTS_1, ONLY           :  ZERO

      IMPLICIT NONE 

      INTEGER(LONG)                   :: I,J,L
      INTEGER(LONG)                   :: KI, KJ            ! For converting grid point number to element DOF number

      REAL(DOUBLE), INTENT(INOUT)     :: M_1DOF(ELGP,ELGP)


! **********************************************************************************************************************************

      ME(:,:) = ZERO

                                                           ! Copy to all 3 translational DOFs of each node.
      DO I=1,ELGP
         DO J=1,ELGP
            KI = (I-1) * 6
            KJ = (J-1) * 6
            DO L=1,3
               ME(KI + L, KJ + L) = M_1DOF(I,J)
            ENDDO
         ENDDO
      ENDDO

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE EXPAND_MASS_DOFS
