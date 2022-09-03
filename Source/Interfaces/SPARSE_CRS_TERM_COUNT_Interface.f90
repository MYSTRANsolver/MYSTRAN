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

   MODULE SPARSE_CRS_TERM_COUNT_Interface

   INTERFACE

      SUBROUTINE SPARSE_CRS_TERM_COUNT ( NROWS, NTERM_IN, MATIN_NAME, I_MATIN, J_MATIN, NTERM_OUT )

                                                        
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_CRS_TERM_COUNT_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME          ! Name of input matrix
 
      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_IN            ! Number of nonzero terms in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)    ! I_MATIN(I+1) - I_MATIN(I) are the number of nonzeros in MATIN row I
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_IN)   ! Col numbers for nonzero terms in MATIN
      INTEGER(LONG), INTENT(OUT)      :: NTERM_OUT           ! Number of nonzero terms in output matrix, MATOUT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_CRS_TERM_COUNT_BEGEND

      END SUBROUTINE SPARSE_CRS_TERM_COUNT

   END INTERFACE

   END MODULE SPARSE_CRS_TERM_COUNT_Interface

