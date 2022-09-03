! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
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

   MODULE ROD1_Interface

   INTERFACE

      SUBROUTINE ROD1 ( OPT, L, AREA, JTOR, SCOEFF, E, G, ALPHA, TREF )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NTSUB, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ROD1_BEGEND
      USE CONSTANTS_1, ONLY           :  TWO
      USE MODEL_STUF, ONLY            :  DT, KE, PTE, SE1, STE1

      IMPLICIT NONE 
 
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROD1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: ALPHA             ! Coefficient of thermal expansion
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Cross-sectional area
      REAL(DOUBLE) , INTENT(IN)       :: E                 ! Youngs modulus
      REAL(DOUBLE) , INTENT(IN)       :: G                 ! Shear modulus
      REAL(DOUBLE) , INTENT(IN)       :: JTOR              ! Torsional constant
      REAL(DOUBLE) , INTENT(IN)       :: L                 ! Elem length
      REAL(DOUBLE) , INTENT(IN)       :: SCOEFF            ! Stress recovery coeff for torsion
      REAL(DOUBLE) , INTENT(IN)       :: TREF              ! Element reference temperature
      REAL(DOUBLE)                    :: C01               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: C02               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: CT0               ! Intermediate variable used in calc thermal loads, PTE
  
      END SUBROUTINE ROD1

   END INTERFACE

   END MODULE ROD1_Interface

