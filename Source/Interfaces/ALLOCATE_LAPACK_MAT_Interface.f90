! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

   MODULE ALLOCATE_LAPACK_MAT_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_LAPACK_MAT ( NAME, NROWS, NCOLS, CALLING_SUBR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONEPP6
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  WINAMEM
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_LAPACK_MAT_BEGEND
      USE ARPACK_MATRICES_1 , ONLY    :  IWORK, RFAC, RESID, SELECT, VBAS, WORKD, WORKL
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND, BBAND, LAPACK_S, RES

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Name of matrix to be allocated
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in array to be allocated
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in array to be allocated
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_LAPACK_MAT_BEGEND
 
      END SUBROUTINE ALLOCATE_LAPACK_MAT

   END INTERFACE

   END MODULE ALLOCATE_LAPACK_MAT_Interface

