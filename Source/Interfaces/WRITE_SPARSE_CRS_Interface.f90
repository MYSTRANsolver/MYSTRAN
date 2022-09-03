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

   MODULE WRITE_SPARSE_CRS_Interface

   INTERFACE

      SUBROUTINE WRITE_SPARSE_CRS ( MAT_NAME, ROW_SET, COL_SET, NTERM_A, NROWS_A, I_AXX, J_AXX, AXX )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SPARSTOR, TINY
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_SPARSE_CRS_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: COL_SET           ! Set designator for cols of matrix
      CHARACTER(LEN=*), INTENT(IN)    :: ROW_SET           ! Set designator for rows of matrix
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Input matrix descriptor

      INTEGER(LONG), INTENT(IN)       :: NTERM_A           ! No. of terms in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: NROWS_A           ! No. of rows  in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: I_AXX(NROWS_A+1)  ! Array of starting indices for the 1-st term in rows of AXX
      INTEGER(LONG), INTENT(IN)       :: J_AXX(NTERM_A)    ! Array of col no's for terms in matrix AXX
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_SPARSE_CRS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: AXX(NTERM_A)      ! Array of terms in matrix AXX
 
      END SUBROUTINE WRITE_SPARSE_CRS

   END INTERFACE

   END MODULE WRITE_SPARSE_CRS_Interface

