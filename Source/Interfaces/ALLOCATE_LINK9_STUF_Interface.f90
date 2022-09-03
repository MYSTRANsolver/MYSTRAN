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

   MODULE ALLOCATE_LINK9_STUF_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_LINK9_STUF ( CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, ONEPP6
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MELGP, MMSPRNT, MOGEL, TOT_MB_MEM_ALLOC
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_LINK9_STUF_BEGEND
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, EID_OUT_ARRAY, FTNAME, MAXREQ, MSPRNT, OGEL, POLY_FIT_ERR,                 &
                                         POLY_FIT_ERR_INDEX
 
      IMPLICIT NONE
 
      CHARACTER(24*BYTE)              :: NAME              ! Array name (used for output error message)
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_LINK9_STUF_BEGEND
 
      END SUBROUTINE ALLOCATE_LINK9_STUF

   END INTERFACE

   END MODULE ALLOCATE_LINK9_STUF_Interface

