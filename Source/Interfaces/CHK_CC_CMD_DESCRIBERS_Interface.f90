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

   MODULE CHK_CC_CMD_DESCRIBERS_Interface

   INTERFACE

      SUBROUTINE CHK_CC_CMD_DESCRIBERS ( WHAT, NUM_WORDS )

 
      USE PENTIUM_II_KIND, ONLY        :  BYTE, LONG
      USE IOUNT1, ONLY                 :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                 :  BLNK_SUB_NAM, CC_CMD_DESCRIBERS, ECHO, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                 :  TSEC
      USE CC_OUTPUT_DESCRIBERS, ONLY   :  STRN_LOC, STRN_OPT, STRE_LOC, STRE_OPT
      USE PARAMS, ONLY                 :  SUPWARN 
      USE SUBR_BEGEND_LEVELS, ONLY     :  CHK_CC_CMD_DESCRIBERS_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER         :: NUM_POSS_CCD = 31 ! Number of possible CC command describers (incl all MSC ones as well)
      INTEGER(LONG), PARAMETER         :: NUM_OUT_TYP  =  9 ! Number of OUTPUT_TYPE's

      CHARACTER(LEN=*), INTENT(IN)     :: WHAT              ! What Case Control output is this call for (e.g. 'DISP')

      INTEGER(LONG), INTENT(IN)        :: NUM_WORDS         ! Number of words we need to check in CC_CMD_DESCRIBERS
      INTEGER(LONG), PARAMETER         :: SUBR_BEGEND = CHK_CC_CMD_DESCRIBERS_BEGEND
 
      END SUBROUTINE CHK_CC_CMD_DESCRIBERS

   END INTERFACE

   END MODULE CHK_CC_CMD_DESCRIBERS_Interface

