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

   MODULE MATMULT_FFF_T_Interface

   INTERFACE

      SUBROUTINE MATMULT_FFF_T ( A, B, NROWA, NCOLA, NCOLB, C )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATMULT_FFF_T_BEGEND
 
      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: NROWA             ! No. rows in input matrix A (NOT A')
      INTEGER(LONG), INTENT(IN)       :: NCOLA             ! No. cols in input matrix A (NOT A')
      INTEGER(LONG), INTENT(IN)       :: NCOLB             ! No. cols in input matrix B 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATMULT_FFF_T_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: A(NROWA,NCOLA)    ! Input  matrix A
      REAL(DOUBLE) , INTENT(IN)       :: B(NROWA,NCOLB)    ! Input  matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NCOLA,NCOLB)    ! Output matrix C
 
      END SUBROUTINE MATMULT_FFF_T

   END INTERFACE

   END MODULE MATMULT_FFF_T_Interface

