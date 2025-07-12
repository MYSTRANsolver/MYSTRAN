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

   MODULE MITC4_B_Interface

   INTERFACE

      SUBROUTINE MITC4_B ( R, S, T, MEMBRANE, BENDING, SHEAR, B )
      
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP

      REAL(DOUBLE) , INTENT(IN)       :: R, S, T           ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix

      LOGICAL      , INTENT(IN)       :: MEMBRANE          ! If true, generate membrane parts of B (rows 1,2,4)
      LOGICAL      , INTENT(IN)       :: BENDING           ! If true, generate bending parts of B (rows 1,2,4)
      LOGICAL      , INTENT(IN)       :: SHEAR             ! If true, generate shear parts of B (rows 5,6)

      END SUBROUTINE MITC4_B

   END INTERFACE

   END MODULE MITC4_B_Interface

