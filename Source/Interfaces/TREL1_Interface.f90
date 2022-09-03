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

   MODULE TREL1_Interface

   INTERFACE

      SUBROUTINE TREL1 ( OPT, WRITE_WARN )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEWE, NSUB, NTSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TREL1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TENTH, ONE, TWO, THREE, TWELVE
      USE PARAMS, ONLY                :  SUPWARN
      USE MODEL_STUF, ONLY            :  EID, ELDOF, EMG_IWE, EMG_RWE, INTL_MID, KE, MASS_PER_UNIT_AREA, ME,                       &
                                         NUM_EMG_FATAL_ERRS, PCOMP_LAM, PCOMP_PROPS, SHELL_B, TYPE, XEB, XEL
      USE MODEL_STUF, ONLY            :  BENSUM, SHRSUM, PHI_SQ, PSI_HAT, XTB, XTL
 
      IMPLICIT NONE 
  
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TREL1_BEGEND

      REAL(DOUBLE)                    :: M0                ! An intermediate variable used in calc elem mass, ME
  
      CHARACTER(LEN=1), PARAMETER     :: MN4T_QD   = 'N'
      INTEGER(LONG)   , PARAMETER     :: TRIA_NUM  = 1
      REAL(DOUBLE)    , PARAMETER     :: PSI       = 0.0D0

      END SUBROUTINE TREL1

   END INTERFACE

   END MODULE TREL1_Interface

