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

   MODULE CNT_NONZ_IN_FULL_MAT_Interface

   INTERFACE

      SUBROUTINE CNT_NONZ_IN_FULL_MAT ( MATIN_NAME, MATIN, NROWS, NCOLS, SYM, NTERM_NONZERO, SMALL )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL, SUPINFO, TINY
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG 
      USE SUBR_BEGEND_LEVELS, ONLY    :  CNT_NONZ_IN_FULL_MAT_BEGEND

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME        ! Name of the matrix
      CHARACTER(LEN=*), INTENT(IN)    :: SYM               ! See above ('ALL' or 'UTR')

      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in the matrix
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in the matrix
      INTEGER(LONG), INTENT(OUT)      :: NTERM_NONZERO     ! Number of nonzero (or significant) values in the matrix
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CNT_NONZ_IN_FULL_MAT_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NROWS,NCOLS)! Input full matrix
      REAL(DOUBLE) , INTENT(OUT)      :: SMALL             ! Filter for small terms

      END SUBROUTINE CNT_NONZ_IN_FULL_MAT

   END INTERFACE

   END MODULE CNT_NONZ_IN_FULL_MAT_Interface

