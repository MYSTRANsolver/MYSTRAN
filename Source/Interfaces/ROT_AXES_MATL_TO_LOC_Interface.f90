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

   MODULE ROT_AXES_MATL_TO_LOC_Interface

   INTERFACE

      SUBROUTINE ROT_AXES_MATL_TO_LOC ( WRITE_WARN )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  ALPVEC, CORD, ISOLID, EB, EBM, EM, ES, ET, MTRL_TYPE, NUM_EMG_FATAL_ERRS, QUAD_DELTA,     &
                                         RCORD, TE, THETAM, TYPE
      USE PARAMS, ONLY                :  EPSIL
      USE DEBUG_PARAMETERS
      USE SUBR_BEGEND_LEVELS, ONLY    :  ROT_AXES_MATL_TO_LOC_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y' write warning messages, otherwise do not

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROT_AXES_MATL_TO_LOC_BEGEND

      END SUBROUTINE ROT_AXES_MATL_TO_LOC

   END INTERFACE

   END MODULE ROT_AXES_MATL_TO_LOC_Interface

