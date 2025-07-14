! ###############################################################################################################################
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

   MODULE MITC_SHAPE_FUNCTIONS_Interface

   INTERFACE

      SUBROUTINE MITC_SHAPE_FUNCTIONS ( R, S, PSH, DPSHG )
 

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP

      USE SHP2DQ_Interface
      USE OUTA_HERE_Interface

      IMPLICIT NONE 

      REAL(DOUBLE) , INTENT(IN)       :: R,S               ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: PSH(ELGP)         ! Shape functions
      REAL(DOUBLE) , INTENT(OUT)      :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      
      END SUBROUTINE MITC_SHAPE_FUNCTIONS

   END INTERFACE

   END MODULE MITC_SHAPE_FUNCTIONS_Interface

