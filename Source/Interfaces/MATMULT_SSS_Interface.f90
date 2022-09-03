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

   MODULE MATMULT_SSS_Interface

   INTERFACE

      SUBROUTINE MATMULT_SSS ( MAT_A_NAME, NROW_A, NTERM_A, SYM_A, I_A, J_A, A,                                                    &

                               MAT_B_NAME, NCOL_B, NTERM_B, SYM_B, J_B, I_B, B, AROW_MAX_TERMS, MAT_C_NAME, CONS,                  &
                                                   NTERM_C,        I_C, J_C, C )
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATMULT_SSS_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME            ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME            ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME            ! Name of matrix C
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_A                 ! ='Y' if matrix A is input symmetric (terms on and above diag only)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_B                  ! ='Y' if matrix B is input sym (terms on and above diag only)

      INTEGER(LONG), INTENT(IN )      :: AROW_MAX_TERMS        ! Max number of terms in any row of A
      INTEGER(LONG), INTENT(IN )      :: NCOL_B                ! Number of cols in input matrix B
      INTEGER(LONG), INTENT(IN )      :: NROW_A                ! Num rows in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_A               ! Num non 0's in input  matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B               ! Num non 0's in input  matrix B
      INTEGER(LONG), INTENT(IN )      :: NTERM_C               ! Size of arrays J_C and C (MUST be determined by subr MATMULT_SSS)
      INTEGER(LONG), INTENT(IN )      :: I_A(NROW_A+1)         ! I_A(I+1) - I_A(I) = num nonzeros in row I of matrix A (CRS format)
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)          ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NCOL_B+1)         ! J_B(I+1) - J_B(I) = num nonzeros in col I of matrix B (CCS format)
      INTEGER(LONG), INTENT(IN )      :: I_B(NTERM_B)          ! Row no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(OUT)      :: I_C(NROW_A+1)         ! I_C(I+1) - I_C(I) = num nonzeros in row I of matrix C (CRS format)
      INTEGER(LONG), INTENT(OUT)      :: J_C(NTERM_C)          ! Col no's for nonzero terms in matrix C
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATMULT_SSS_BEGEND
       
      REAL(DOUBLE) , INTENT(IN )      :: CONS                  ! Constant multiplier in cons*A*B to get C
      REAL(DOUBLE) , INTENT(IN )      :: A(NTERM_A)            ! Nonzero values in matrix A
      REAL(DOUBLE) , INTENT(IN )      :: B(NTERM_B)            ! Nonzero values in matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NTERM_C)            ! Nonzero values in matrix C
      END SUBROUTINE MATMULT_SSS

   END INTERFACE

   END MODULE MATMULT_SSS_Interface

