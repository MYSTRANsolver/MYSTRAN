! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

   MODULE EMG_Interface

   INTERFACE

      SUBROUTINE EMG ( INT_ELEM_ID, OPT, WRITE_WARN, CALLING_SUBR, WRT_BUG_THIS_TIME )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MBUG, MEDAT0_CUSERIN, MELDOF, MEMATC, MOFFSET, NSUB, NTSUB
      USE SCONTR, ONLY                :  DEDAT_Q4_MATANG_KEY, DEDAT_Q4_POFFS_KEY, DEDAT_Q4_SHELL_KEY, DEDAT_Q4_THICK_KEY,          &
                                         DEDAT_T3_MATANG_KEY, DEDAT_T3_POFFS_KEY, DEDAT_T3_SHELL_KEY, DEDAT_T3_THICK_KEY,          &
                                         WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  EMG_BEGEND
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD, CONV_RAD_DEG, ZERO
      USE MODEL_STUF, ONLY            :  CAN_ELEM_TYPE_OFFSET, EDAT, EID, EPNT, ETYPE, ISOLID, MATANGLE, NUM_EMG_FATAL_ERRS,       &
                                         PCOMP_PROPS, PLY_NUM, TE_IDENT, THETAM, TYPE, XEL

      IMPLICIT NONE

      CHARACTER(LEN=*)  , INTENT(IN)  :: CALLING_SUBR       ! Name of subr that called this one
      CHARACTER( 1*BYTE), INTENT(IN)  :: OPT(6)             ! Array of EMG option indicators explained above
      CHARACTER(LEN=*)  , INTENT(IN)  :: WRITE_WARN         ! If 'Y" write warning messages, otherwise do not
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME  ! If 'Y' then write to BUG file if WRT_BUG array says to
      CHARACTER( 1*BYTE)              :: RED_INT_SHEAR = 'N'! If 'Y', use Gaussian weighted average of B matrices for shear terms
 
      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      INTEGER(LONG)                   :: INT41,INT42        ! An integer used in getting MATANGLE
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EMG_BEGEND
 
      END SUBROUTINE EMG

   END INTERFACE

   END MODULE EMG_Interface

