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

   MODULE MATMULT_SFF_Interface

   INTERFACE

      SUBROUTINE MATMULT_SFF ( MAT_A_NAME, NROWS_A, NCOLS_A, NTERM_A, SYM_A, I_A, J_A, A, MAT_B_NAME, NROWS_B, NCOLS_B, B,         &

                               WRITE_SC1, MAT_C_NAME, CONS, C )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATMULT_SFF_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_ALG_ARRAYS, ONLY     :  AROW, J_AROW
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME        ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME        ! Name of matrix C
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_A             ! ='Y' if matrix A is input symmetric (terms on and above diag only)
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_SC1         ! ='Y' if we need to write to screen to advance 1 line before write msg

      INTEGER(LONG), INTENT(IN )      :: NROWS_B           ! Number of rows in input matrix B
      INTEGER(LONG), INTENT(IN )      :: NCOLS_A           ! NUMBER OF COLS IN INPUT MATRIX A
      INTEGER(LONG), INTENT(IN )      :: NCOLS_B           ! Number of cols in input matrix B
      INTEGER(LONG), INTENT(IN )      :: NROWS_A           ! Number of rows in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_A           ! Number of nonzero terms in input  matrix A
      INTEGER(LONG), INTENT(IN )      :: I_A(NROWS_A+1)    ! I_A(I+1) - I_A(I) = num nonzeros in row I of matrix A (CRS)
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)      ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATMULT_SFF_BEGEND
       
      REAL(DOUBLE) , INTENT(IN )      :: A(NTERM_A)        ! Nonzero values in matrix A
      REAL(DOUBLE) , INTENT(IN )      :: B(NROWS_B,NCOLS_B)! Real values in matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NROWS_A,NCOLS_B)! Real values in matrix c
      REAL(DOUBLE) , INTENT(IN )      :: CONS              ! Constant multiplier in cons*A*B to get C

      END SUBROUTINE MATMULT_SFF

   END INTERFACE

   END MODULE MATMULT_SFF_Interface

