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

   MODULE ELMGM2_Interface

   INTERFACE

      SUBROUTINE ELMGM2 ( WRITE_WARN )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEFE, MEWE, MELGP, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMGM2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, TWO
      USE PARAMS, ONLY                :  EPSIL, QUADAXIS, SUPWARN
      USE MODEL_STUF, ONLY            :  AGRID, BMEANT, EID, ELGP, EMG_IFE, EMG_IWE, EMG_RWE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS,     &
                                         HBAR, MXWARP, QUAD_DELTA, QUAD_GAMMA, QUAD_THETA, TE, TE_IDENT, TYPE, WARP_WARN, XEB, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: DIAG_GRID1        ! Used for error output purposes
      INTEGER(LONG)                   :: DIAG_GRID2        ! Used for error output purposes
      INTEGER(LONG)                   :: SIDE_GRID1        ! Used for error output purposes
      INTEGER(LONG)                   :: SIDE_GRID2        ! Used for error output purposes
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMGM2_BEGEND

      END SUBROUTINE ELMGM2

   END INTERFACE

   END MODULE ELMGM2_Interface

