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

   MODULE CHECK_BAR_MOIs_Interface

   INTERFACE

      SUBROUTINE CHECK_BAR_MOIs ( NAME, ID, I1, I2, I12, IERR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC 
      USE PARAMS, ONLY                :  EPSIL, SUPINFO
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  CHECK_BAR_MOIs_BEGEND

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Either PBAR, PBARL or PBEAM
      CHARACTER(LEN=*), INTENT(IN)    :: ID                ! Character value of the bar's ID

      INTEGER(LONG), INTENT(OUT)      :: IERR              ! Error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CHECK_BAR_MOIs_BEGEND
 
      REAL(DOUBLE), INTENT(INOUT)     :: I1                ! MOI of the bar or beam
      REAL(DOUBLE), INTENT(INOUT)     :: I2                ! MOI of the bar or beam
      REAL(DOUBLE), INTENT(INOUT)     :: I12               ! MOI of the bar or beam

      END SUBROUTINE CHECK_BAR_MOIs

   END INTERFACE

   END MODULE CHECK_BAR_MOIs_Interface

