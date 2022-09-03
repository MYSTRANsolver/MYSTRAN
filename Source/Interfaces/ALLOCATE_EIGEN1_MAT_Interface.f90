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

   MODULE ALLOCATE_EIGEN1_MAT_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_EIGEN1_MAT ( NAME, NROWS, NCOLS, CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, TOT_MB_MEM_ALLOC
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_EIGEN1_MAT_BEGEND
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL, EIGEN_VEC, GEN_MASS, MODE_NUM, MEFFMASS, MPFACTOR_N6, MPFACTOR_NR
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name of the matrix to be allocated in sparse format
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows to allocate to matrix NAME
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols to allocate to matrix NAME
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_EIGEN1_MAT_BEGEND
 
      END SUBROUTINE ALLOCATE_EIGEN1_MAT

   END INTERFACE

   END MODULE ALLOCATE_EIGEN1_MAT_Interface

