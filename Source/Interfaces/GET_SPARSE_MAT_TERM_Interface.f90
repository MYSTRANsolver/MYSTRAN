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

   MODULE GET_SPARSE_MAT_TERM_Interface

   INTERFACE

      SUBROUTINE GET_SPARSE_MAT_TERM ( MATIN_NAME, I_MATIN, J_MATIN, MATIN, IROW, JCOL, N, NTERMS, MATIN_VAL )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO 
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_SPARSE_MAT_TERM_BEGEND

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN )   :: MATIN_NAME        ! Name of input matrix MATIN

      INTEGER(LONG), INTENT(IN)       :: N                 ! Row/col size of MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzero terms in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: IROW              ! Row index of the term to retrieve from sparse MATIN
      INTEGER(LONG), INTENT(IN)       :: JCOL              ! Col index of the term to retrieve from sparse MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(N+1)      ! Indices of the beginning terms in each row for MATIN values
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERMS)   ! Col numbers of nonzero term in MATIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_SPARSE_MAT_TERM_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERMS)     ! Real vals in sparse matrix MATIN
      REAL(DOUBLE) , INTENT(OUT)      :: MATIN_VAL

      END SUBROUTINE GET_SPARSE_MAT_TERM

   END INTERFACE

   END MODULE GET_SPARSE_MAT_TERM_Interface

