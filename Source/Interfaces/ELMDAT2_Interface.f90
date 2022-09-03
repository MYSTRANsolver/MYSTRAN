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

   MODULE ELMDAT2_Interface

   INTERFACE

      SUBROUTINE ELMDAT2 ( INT_ELEM_ID, OPT, WRITE_WARN )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LPDAT, MPRESS, MDT, MTDAT_TEMPRB, NSUB, NTSUB 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMDAT_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, QUARTER, THIRD
      USE MODEL_STUF, ONLY            :  BGRID, DT, ELGP, ETYPE, GTEMP, PDATA, PPNT, PTYPE, PRESS, TDATA, TPNT, TYPE

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: OPT(6)            ! Array of EMG option indicators
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID for which
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMDAT_BEGEND

      END SUBROUTINE ELMDAT2

   END INTERFACE

   END MODULE ELMDAT2_Interface

