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

   MODULE LOADC_Interface

   INTERFACE

      SUBROUTINE LOADC

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUGOUT, ERR, F04, F06, IN1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_ENTRY_LEN, ENFORCED, FATAL_ERR, WARN_ERR, NSUB, NTSUB, PROG_NAME,        &
                                         RESTART, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADC_BEGEND
      USE MODEL_STUF, ONLY            :  CC_EIGR_SID, MEFFMASS_CALC, MPCSET, MPCSETS, MPFACTOR_CALC, SCNUM, SPCSET, SPCSETS, SUBLOD
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRN_LOC, STRE_LOC
 
      IMPLICIT NONE
 
      CHARACTER(10*BYTE), PARAMETER   :: END_CARD    = 'BEGIN BULK'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADC_BEGEND
 
      END SUBROUTINE LOADC

   END INTERFACE

   END MODULE LOADC_Interface

