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

   MODULE MATADD_SSS_NTERM_Interface

   INTERFACE

      SUBROUTINE MATADD_SSS_NTERM ( NROWS, MAT_A_NAME, NTERM_A, I_A, J_A, SYM_A, MAT_B_NAME, NTERM_B, I_B, J_B, SYM_B,             &

                                           MAT_C_NAME, NTERM_C )
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_ALG_ARRAYS, ONLY     :  LOGICAL_VEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATADD_SSS_NTERM_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME        ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME        ! Name of matrix C
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_A             ! Flag for whether matrix A is stored sym (terms on and above diag)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_B             ! Flag for whether matrix B is stored sym (terms on and above diag)
      CHARACTER( 2*BYTE)              :: ALG               ! Which algorithm is used in solving for the terms in a row of C

      INTEGER(LONG), INTENT(IN )      :: NROWS             ! Number of rows in input matrices A and B
      INTEGER(LONG), INTENT(IN )      :: NTERM_A           ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B           ! Number of nonzero terms in input matrix B
      INTEGER(LONG), INTENT(IN )      :: I_A(NROWS+1)      ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG), INTENT(IN )      :: I_B(NROWS+1)      ! I_B(I+1) - I_B(I) = no. terms in row I of matrix B
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)      ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NTERM_B)      ! Col no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(OUT)      :: NTERM_C           ! Number of nonzero terms in output matrix C
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_SSS_NTERM_BEGEND
       
      END SUBROUTINE MATADD_SSS_NTERM

   END INTERFACE

   END MODULE MATADD_SSS_NTERM_Interface

