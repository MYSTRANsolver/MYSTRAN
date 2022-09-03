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

   MODULE OUTA_HERE_Interface

   INTERFACE

      SUBROUTINE OUTA_HERE ( WRITE_TO_L1A ) 

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE IOUNT1, ONLY                :  BUGOUT, F04, F06FIL, SC1, WRT_LOG,                                                        &
                                         BUGSTAT, BUGSTAT_OLD, ERRSTAT, ERRSTAT_OLD, F04STAT, F04STAT_OLD,                         &
                                         OP2STAT, PCHSTAT, L1ASTAT 

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LINKNO, WARN_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  OUTA_HERE_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_TO_L1A      ! Y/N indicator of whether to call subr WRITE_L1A

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OUTA_HERE_BEGEND

      END SUBROUTINE OUTA_HERE

   END INTERFACE

   END MODULE OUTA_HERE_Interface

