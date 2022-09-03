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

   MODULE DEALLOCATE_SCR_MAT_Interface

   INTERFACE

      SUBROUTINE DEALLOCATE_SCR_MAT ( NAME_IN )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC          
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_SCR_MAT_BEGEND
      USE SCRATCH_MATRICES , ONLY     :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CRS3, J_CRS3, CRS3,  &
                                         I_CCS1, J_CCS1, CCS1, I_CCS2, J_CCS2, CCS2, I_CCS3, J_CCS3, CCS3

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name (used for output error message)
      CHARACTER(6*BYTE)               :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_SCR_MAT_BEGEND
 
      END SUBROUTINE DEALLOCATE_SCR_MAT

   END INTERFACE

   END MODULE DEALLOCATE_SCR_MAT_Interface

