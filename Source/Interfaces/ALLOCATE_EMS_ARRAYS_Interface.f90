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

   MODULE ALLOCATE_EMS_ARRAYS_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_EMS_ARRAYS ( CALLING_SUBR )  

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, ONEPP6
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LINKNO, LTERM_MGGE, NDOFG, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE EMS_ARRAYS, ONLY            :  EMS, EMSCOL, EMSKEY, EMSPNT
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_EMS_ARRAYS_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER( 6*BYTE)              :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_EMS_ARRAYS_BEGEND

      END SUBROUTINE ALLOCATE_EMS_ARRAYS

   END INTERFACE

   END MODULE ALLOCATE_EMS_ARRAYS_Interface

