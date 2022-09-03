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

   MODULE MATTRNSP_SS_Interface

   INTERFACE

      SUBROUTINE MATTRNSP_SS ( NROWA, NCOLA, NTERM, MAT_A_NAME, I_A, J_A, A, MAT_AT_NAME, I_AT, J_AT, AT )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATTRNSP_SS_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix to be transposed
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_AT_NAME       ! Name of matrix that is transposed
 
      INTEGER(LONG), INTENT(IN)       :: NCOLA             ! Number of cols in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NROWA             ! Number of rows in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM             ! Number of nonzero terms in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: I_A(NROWA+1)      ! I_A(I+1) - I_A(I) are the number of nonzeros in A row I
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM)        ! Col numbers for nonzero terms in A
      INTEGER(LONG), INTENT(OUT)      :: I_AT(NCOLA+1)     ! I_AT(I+1) - I_AT(I) are the num of nonzeros in AT row I
      INTEGER(LONG), INTENT(OUT)      :: J_AT(NTERM)       ! Col numbers for nonzero terms in AT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATTRNSP_SS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: A(NTERM)          ! Real nonzero values in input  matrix A
      REAL(DOUBLE) , INTENT(OUT)      :: AT(NTERM)         ! Real nonzero values in output matrix AT

      END SUBROUTINE MATTRNSP_SS

   END INTERFACE

   END MODULE MATTRNSP_SS_Interface

