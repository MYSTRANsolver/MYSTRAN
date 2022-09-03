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

   MODULE PARTITION_SS_NTERM_Interface

   INTERFACE

      SUBROUTINE PARTITION_SS_NTERM ( MAT_A_NAME, NTERM_A, NROW_A, NCOL_A, SYM_A, I_A, J_A                                         &

                                    , ROW_PART_VEC, COL_PART_VEC, VAL_ROWS, VAL_COLS, AROW_MAX_TERMS                               &
                                    , MAT_B_NAME, NTERM_B, SYM_B )                
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SPARSE_ALG_ARRAYS, ONLY     :  ALG, J_AROW
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARTITION_SS_NTERM_BEGEND
 
      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)      ! This causes a carriage return simulating the "+" action in a FORMAT
 
      CHARACTER(LEN=*), INTENT(IN )   :: SYM_A                  ! 'Y' if input matrix is symmetric (and terms below diag
      CHARACTER(LEN=*), INTENT(IN )   :: SYM_B                  ! 'Y' if all terms (incl below diag) are to be output for
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_B_NAME             ! Name of partitioned output matrix
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_A_NAME             ! Name of input matrix to be partitioned

      INTEGER(LONG), INTENT(IN )      :: NTERM_A                ! No. terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: NROW_A                 ! No. rows in MATIN
      INTEGER(LONG), INTENT(IN )      :: NCOL_A                 ! No. cols in MATIN
      INTEGER(LONG), INTENT(IN )      :: VAL_ROWS               ! Value in ROW_PART_VEC to look for for partitioning rows  
      INTEGER(LONG), INTENT(IN )      :: VAL_COLS               ! Value in COL_PART_VEC to look for for partitioning cols
      INTEGER(LONG), INTENT(IN )      :: I_A(NROW_A+1)          ! Starting locations in MATIN for each row
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)           ! Col number for each MATIN input matrix term
      INTEGER(LONG), INTENT(IN )      :: ROW_PART_VEC(NROW_A)   ! Row partitioning vector (1's and 2's)
      INTEGER(LONG), INTENT(IN )      :: COL_PART_VEC(NCOL_A)   ! Col partitioning vector (1's and 2's)
      INTEGER(LONG), INTENT(OUT)      :: AROW_MAX_TERMS         ! Max number of terms in any row of A
      INTEGER(LONG), INTENT(OUT)      :: NTERM_B                ! No. terms that go into MATOUT (from subr PARTITION_SS_NTERM)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PARTITION_SS_NTERM_BEGEND

      END SUBROUTINE PARTITION_SS_NTERM

   END INTERFACE

   END MODULE PARTITION_SS_NTERM_Interface

