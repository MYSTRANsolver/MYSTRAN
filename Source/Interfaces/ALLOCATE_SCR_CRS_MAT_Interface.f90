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

   MODULE ALLOCATE_SCR_CRS_MAT_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_SCR_CRS_MAT ( NAME, NROWS, NTERMS, CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_SCR_CRS_MAT_BEGEND
      USE SCRATCH_MATRICES , ONLY     :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CRS3, J_CRS3, CRS3

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name (used for output error message)
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(6*BYTE)               :: NAMEO             ! Array name (used for output error message)
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows for matrix CRSi
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzero terms that will be in matrix CRSi
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_SCR_CRS_MAT_BEGEND

      END SUBROUTINE ALLOCATE_SCR_CRS_MAT

   END INTERFACE

   END MODULE ALLOCATE_SCR_CRS_MAT_Interface

