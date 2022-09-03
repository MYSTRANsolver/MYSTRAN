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

   MODULE PRINCIPAL_2D_Interface

   INTERFACE

      SUBROUTINE PRINCIPAL_2D ( SX, SY, SXY, ANGLE, SMAJOR, SMINOR, SXYMAX, MEAN, VONMISES )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, QUARTER, HALF, TWO, ONEPM6, FORTY5, CONV_RAD_DEG 
      USE SUBR_BEGEND_LEVELS, ONLY    :  PRINCIPAL_2D_BEGEND
 
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PRINCIPAL_2D_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: SX                 ! Normal x stress or strain
      REAL(DOUBLE), INTENT(IN)        :: SY                 ! Normal y stress or strain
      REAL(DOUBLE), INTENT(IN)        :: SXY                ! Shear stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: ANGLE              ! Angle of principal stresses or strain
      REAL(DOUBLE), INTENT(OUT)       :: MEAN               ! Mean stresses or strain
      REAL(DOUBLE), INTENT(OUT)       :: SMAJOR             ! Major principal stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: SMINOR             ! Minor principal stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: SXYMAX             ! Max shear stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: VONMISES           ! von Mises stress or strain
      REAL(DOUBLE), PARAMETER         :: EPS2    = ONEPM6   ! Small number to compare with ADENR, ANUMR when calculating ANGLE
 
      END SUBROUTINE PRINCIPAL_2D

   END INTERFACE

   END MODULE PRINCIPAL_2D_Interface

