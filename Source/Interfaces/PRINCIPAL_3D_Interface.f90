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

   MODULE PRINCIPAL_3D_Interface

   INTERFACE

      SUBROUTINE PRINCIPAL_3D ( STR, PRINCIPAL_STR, MEAN, VONMISES, SIG_OCT, TAU_OCT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, TWO, THREE
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  PRINCIPAL_3D_BEGEND
 
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PRINCIPAL_3D_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STR(6)             ! Stress or strain vector
      REAL(DOUBLE), INTENT(OUT)       :: MEAN               ! Mean stresses or strains
      REAL(DOUBLE), INTENT(OUT)       :: PRINCIPAL_STR(3)   ! Principal stresses or strains
      REAL(DOUBLE), INTENT(OUT)       :: SIG_OCT            ! Octrahedral normal stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: TAU_OCT            ! Octrahedral shear  stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: VONMISES           ! Octahedral stress or strain
 
      END SUBROUTINE PRINCIPAL_3D

   END INTERFACE

   END MODULE PRINCIPAL_3D_Interface

