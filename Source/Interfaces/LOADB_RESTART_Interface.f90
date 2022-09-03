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

   MODULE LOADB_RESTART_Interface

   INTERFACE

      SUBROUTINE LOADB_RESTART

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, ECHO, FATAL_ERR, JCARD_LEN, JF, PROG_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADB_RESTART_BEGEND 
 
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: NUM_PARMS = 25      ! Number of PARAM entries allowed in RESTART
      INTEGER(LONG), PARAMETER        :: NUM_DEB   = 28      ! Number of DEBUG entries allowed in RESTART

      CHARACTER( 7*BYTE), PARAMETER   :: END_CARD    = 'ENDDATA'
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADB_RESTART_BEGEND
 
      END SUBROUTINE LOADB_RESTART

   END INTERFACE

   END MODULE LOADB_RESTART_Interface

