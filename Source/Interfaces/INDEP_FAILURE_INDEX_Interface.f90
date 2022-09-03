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

   MODULE INDEP_FAILURE_INDEX_Interface

   INTERFACE

      SUBROUTINE INDEP_FAILURE_INDEX ( STREi, STRNi, STRE_ALLOWABLES, STRN_ALLOWABLES, FAILURE_INDEX )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  FAILURE_THEORY
      USE SUBR_BEGEND_LEVELS, ONLY    :  INDEP_FAILURE_INDEX_BEGEND

      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = INDEP_FAILURE_INDEX_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STRE_ALLOWABLES(9)! Allowable stresses (incl tension and compr for normal stresses)
      REAL(DOUBLE), INTENT(IN)        :: STRN_ALLOWABLES(9)! Allowable strains (incl tension and compr for normal stresses)
      REAL(DOUBLE), INTENT(IN)        :: STREi(6)          ! 6 components of strain
      REAL(DOUBLE), INTENT(IN)        :: STRNi(6)          ! 6 components of stress
      REAL(DOUBLE), INTENT(OUT)       :: FAILURE_INDEX     ! Failure index (scalar value)

      END SUBROUTINE INDEP_FAILURE_INDEX

   END INTERFACE

   END MODULE INDEP_FAILURE_INDEX_Interface

