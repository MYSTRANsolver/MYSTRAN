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

   MODULE CRS_NONSYM_TO_CRS_SYM_Interface

   INTERFACE

      SUBROUTINE CRS_NONSYM_TO_CRS_SYM ( NAME_A, NROW_A, NTERM_A, I_A, J_A, A, NAME_B, NTERM_B, I_B, J_B, B )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  CRS_NONSYM_TO_CRS_SYM_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_A            ! Name of input matrix
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_B            ! Name of output matrix
 
      INTEGER(LONG), INTENT(IN)       :: NROW_A            ! Number of rows in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM_A           ! Number of nonzero terms in input  matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM_B           ! Number of nonzero terms in output matrix, B
      INTEGER(LONG), INTENT(IN)       :: I_A(NROW_A+1)     ! I_A(I+1) - I_A(I) are the number of nonzeros in A row I
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM_A)      ! Col numbers for nonzero terms in A
      INTEGER(LONG), INTENT(OUT)      :: I_B(NROW_A+1)     ! I_B(I+1) - I_B(I) are the num of nonzeros in B row I
      INTEGER(LONG), INTENT(OUT)      :: J_B(NTERM_B)      ! Col numbers for nonzero terms in B
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CRS_NONSYM_TO_CRS_SYM_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: A(NTERM_A)        ! Real nonzero values in input  matrix A
      REAL(DOUBLE) , INTENT(OUT)      :: B(NTERM_B)        ! Real nonzero values in output matrix B

      END SUBROUTINE CRS_NONSYM_TO_CRS_SYM

   END INTERFACE

   END MODULE CRS_NONSYM_TO_CRS_SYM_Interface

