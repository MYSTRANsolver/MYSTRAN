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

   MODULE MATADD_FFF_Interface

   INTERFACE

      SUBROUTINE MATADD_FFF ( A, B, NROW, NCOL, ALPHA, BETA, ITRNSPB, C)

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATADD_FFF_BEGEND
 
      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: NROW              ! Number of rows in matrces A, B, C
      INTEGER(LONG), INTENT(IN)       :: NCOL              ! Number of cols in matrces A, B, C
      INTEGER(LONG), INTENT(IN)       :: ITRNSPB           ! Transpose indicator for matrix B
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_FFF_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: A(NROW,NCOL)      ! Input  matrix A
      REAL(DOUBLE) , INTENT(IN)       :: B(NROW,NCOL)      ! Input  matrix B
      REAL(DOUBLE) , INTENT(IN)       :: ALPHA             ! Scalar multiplier for matrix A
      REAL(DOUBLE) , INTENT(IN)       :: BETA              ! Scalar multiplier for matrix B

      REAL(DOUBLE) , INTENT(OUT)      :: C(NROW,NCOL)      ! Output matrix C
 
      END SUBROUTINE MATADD_FFF

   END INTERFACE

   END MODULE MATADD_FFF_Interface

