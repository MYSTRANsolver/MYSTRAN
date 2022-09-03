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

   MODULE MATMULT_SSS_NTERM_Interface

   INTERFACE

      SUBROUTINE MATMULT_SSS_NTERM ( MAT_A_NAME, NROW_A, NTERM_A, SYM_A, I_A, J_A,                                                 &

                                     MAT_B_NAME, NCOL_B, NTERM_B, SYM_B, J_B, I_B, AROW_MAX_TERMS, MAT_C_NAME, NTERM_C )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATMULT_SSS_NTERM_BEGEND
      USE SPARSE_ALG_ARRAYS, ONLY     :  J_AROW
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME             ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME             ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME             ! Name of matrix C
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_A                  ! ='Y' if matrix A is input sym (terms on and above diag only)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_B                  ! ='Y' if matrix B is input sym (terms on and above diag only)

      INTEGER(LONG), INTENT(IN )      :: NCOL_B                 ! Number of cols in input matrix B
      INTEGER(LONG), INTENT(IN )      :: NROW_A                 ! Number of rows in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_A                ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B                ! Number of nonzero terms in input matrix B
      INTEGER(LONG), INTENT(IN )      :: I_A(NROW_A+1)          ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)           ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NCOL_B+1)          ! J_B(I+1) - J_B(I) = no. terms in row I of matrix B
      INTEGER(LONG), INTENT(IN )      :: I_B(NTERM_B)           ! Row no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(OUT)      :: AROW_MAX_TERMS         ! Max number of terms in any row of A
      INTEGER(LONG), INTENT(OUT)      :: NTERM_C                ! Number of nonzero terms in output matrix C
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATMULT_SSS_NTERM_BEGEND
       
      END SUBROUTINE MATMULT_SSS_NTERM

   END INTERFACE

   END MODULE MATMULT_SSS_NTERM_Interface

