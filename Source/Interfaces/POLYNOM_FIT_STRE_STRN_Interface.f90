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

   MODULE POLYNOM_FIT_STRE_STRN_Interface

   INTERFACE

      SUBROUTINE POLYNOM_FIT_STRE_STRN ( STR_IN, NROW, NCOL, STR_OUT, STR_OUT_PCT_ERR, STR_OUT_ERR_INDEX, PCT_ERR_MAX )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, THREE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  EID, ELGP, TYPE, XEL
      USE PARAMS, ONLY                :  Q4SURFIT, QUAD4TYP
      USE SUBR_BEGEND_LEVELS, ONLY    :  POLYNOM_FIT_STRE_STRN_BEGEND

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: NCOL                ! Number of cols in arrays STR_IN, STR_OUT (1 more than the num of elem
      INTEGER(LONG), INTENT(IN)       :: NROW                ! Number of rows in arrays STR_IN, STR_OUT

      INTEGER(LONG), INTENT(OUT)      :: STR_OUT_ERR_INDEX(MAX_STRESS_POINTS)
      INTEGER(LONG), PARAMETER        :: IORD = 2            ! Gaussian integration order
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = POLYNOM_FIT_STRE_STRN_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STR_IN(NROW,NCOL)   ! Input stress/strain vals. NROW are num of diff stress/strain vals and
      REAL(DOUBLE), INTENT(OUT)       :: STR_OUT(NROW,NCOL)  ! Output stress/strain vals. NROW are num of diff stress/strain vals
      REAL(DOUBLE), INTENT(OUT)       :: PCT_ERR_MAX         ! Max value from array PCT_ERR (max poly fit err at any input data pt)

      REAL(DOUBLE), INTENT(OUT)       :: STR_OUT_PCT_ERR(MAX_STRESS_POINTS)

      END SUBROUTINE POLYNOM_FIT_STRE_STRN

   END INTERFACE

   END MODULE POLYNOM_FIT_STRE_STRN_Interface

