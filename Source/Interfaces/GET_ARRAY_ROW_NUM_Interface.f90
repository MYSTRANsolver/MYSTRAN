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

   MODULE GET_ARRAY_ROW_NUM_Interface

   INTERFACE

      SUBROUTINE GET_ARRAY_ROW_NUM ( ARRAY_NAME, CALLING_SUBR, ASIZE, ARRAY, EXT_ID, ROW_NUM )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE, TWO
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_ARRAY_ROW_NUM_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: ARRAY_NAME        ! Name of array to be searched
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Name of subr that called this one

      INTEGER(LONG), INTENT(IN)       :: ASIZE             ! Size of ARRAY
      INTEGER(LONG), INTENT(IN)       :: ARRAY(ASIZE)      ! Array to search
      INTEGER(LONG), INTENT(IN)       :: EXT_ID            ! External (actual) ID to find in ARRAY
      INTEGER(LONG), INTENT(OUT)      :: ROW_NUM           ! Internal ID (row in ARRAY) where EXT_ID exists
      INTEGER(LONG)                   :: HI, LO            ! Used to bound the range of N where EXT_ID is expected to be found
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_ARRAY_ROW_NUM_BEGEND
 
      END SUBROUTINE GET_ARRAY_ROW_NUM

   END INTERFACE

   END MODULE GET_ARRAY_ROW_NUM_Interface

