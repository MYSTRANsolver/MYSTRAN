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

   MODULE PARTITION_FF_Interface

   INTERFACE

      SUBROUTINE PARTITION_FF ( MAT_A_NAME, NROW_A, NCOL_A, A, ROW_PART_VEC, COL_PART_VEC, VAL_ROWS, VAL_COLS,                     &

                                MAT_B_NAME, NROW_B, NCOL_B, B )                
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARTITION_FF_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME             ! Name of partitioned output matrix
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME             ! Name of input matrix to be partitioned
      INTEGER(LONG)   , INTENT(IN)    :: NCOL_A                 ! No. cols in A
      INTEGER(LONG)   , INTENT(IN)    :: NROW_A                 ! No. rows in A
      INTEGER(LONG)   , INTENT(IN)    :: COL_PART_VEC(NCOL_A)   ! Col partitioning vector (1's and 2's)
      INTEGER(LONG)   , INTENT(IN)    :: ROW_PART_VEC(NROW_A)   ! Row partitioning vector (1's and 2's)
      INTEGER(LONG)   , INTENT(IN)    :: VAL_ROWS               ! Value in ROW_PART_VEC to look for for partitioning rows  
      INTEGER(LONG)   , INTENT(IN)    :: VAL_COLS               ! Value in COL_PART_VEC to look for for partitioning cols
      INTEGER(LONG)   , INTENT(IN)    :: NCOL_B                 ! No. cols in B
      INTEGER(LONG)   , INTENT(IN)    :: NROW_B                 ! No. rows in B
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = PARTITION_FF_BEGEND
       
      REAL(DOUBLE)    , INTENT(IN )   :: A(NROW_A,NCOL_A)       ! Input  matrix

      REAL(DOUBLE)    , INTENT(OUT)   :: B(NROW_B,NCOL_B)       ! Output matrix

      END SUBROUTINE PARTITION_FF

   END INTERFACE

   END MODULE PARTITION_FF_Interface

