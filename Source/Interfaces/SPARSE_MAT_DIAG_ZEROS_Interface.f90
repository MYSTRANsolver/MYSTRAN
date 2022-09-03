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

   MODULE SPARSE_MAT_DIAG_ZEROS_Interface

   INTERFACE

       SUBROUTINE SPARSE_MAT_DIAG_ZEROS ( NAME, NROWS_A, NTERM_A, I_A, J_A, NUM_A_DIAG_ZEROS )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_MAT_DIAG_ZEROS_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: NAME               ! Name of input matrix

      INTEGER(LONG), INTENT(IN)       :: NROWS_A            ! Number of rows in input matrix A
      INTEGER(LONG), INTENT(IN)       :: NTERM_A            ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN)       :: I_A(NROWS_A+1)     ! Array of row no's for terms in input matrix A
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM_A)       ! Array of col no's for terms in input matrix A
      INTEGER(LONG), INTENT(OUT)      :: NUM_A_DIAG_ZEROS   ! Number of zero diagonal terms in input matrix A
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_MAT_DIAG_ZEROS_BEGEND

      END SUBROUTINE SPARSE_MAT_DIAG_ZEROS

   END INTERFACE

   END MODULE SPARSE_MAT_DIAG_ZEROS_Interface

