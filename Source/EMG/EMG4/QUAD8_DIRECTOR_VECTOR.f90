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
      FUNCTION QUAD8_DIRECTOR_VECTOR ( XI, ETA )
 
! Calculates the director vector in basic coordinates at a point in isoparametric coordinates.

      USE MODEL_STUF, ONLY            :  ELGP

      REAL(DOUBLE)                    :: QUAD8_DIRECTOR_VECTOR(3)
      REAL(DOUBLE) , INTENT(IN)       :: XI
      REAL(DOUBLE) , INTENT(IN)       :: ETA
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)! Derivatives of shape functions with respect to xi and eta.


! **********************************************************************************************************************************
      
! Choose the director vector to be normal to the nodal surface everywhere.
! This isn't required for MITC and could be averaged from adjacent elements.
 
 !victor todo I don't need PSH. see if fortran allows passing some sort of nothing for an INTENT OUT parameter.
      CALL SHP2DQ ( 0, 0, ELGP, 'QUAD8_DIRECTOR_VECTOR', '', 0, XI, ETA, 'N', PSH, DPSHG )

!victor todo continue working out director vector.
!victor todo interface


      RETURN


! **********************************************************************************************************************************
  
      END FUNCTION QUAD8_DIRECTOR_VECTOR
