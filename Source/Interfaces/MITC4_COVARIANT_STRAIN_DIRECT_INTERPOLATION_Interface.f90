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

   MODULE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface

   INTERFACE

      SUBROUTINE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION ( R, S, T, X_R, X_S, X_D, MEMBRANE, BENDING, B )
      
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP

      IMPLICIT NONE 
      
      REAL(DOUBLE) , INTENT(IN)       :: R,S,T             ! Isparametric coordinates
      REAL(DOUBLE) , INTENT(IN)       :: X_R(3)            ! Characteristic geometry vector x_r
      REAL(DOUBLE) , INTENT(IN)       :: X_S(3)            ! Characteristic geometry vector x_s
      REAL(DOUBLE) , INTENT(IN)       :: X_D(3)            ! Characteristic geometry vector x_d (distortion vector)
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix.

      LOGICAL      , INTENT(IN)       :: MEMBRANE          ! If true, generate membrane parts of B
      LOGICAL      , INTENT(IN)       :: BENDING           ! If true, generate bending parts of B


      END SUBROUTINE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION

   END INTERFACE

   END MODULE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION_Interface

