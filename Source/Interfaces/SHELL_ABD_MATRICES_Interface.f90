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

   MODULE SHELL_ABD_MATRICES_Interface

   INTERFACE

      SUBROUTINE SHELL_ABD_MATRICES ( INT_ELEM_ID, WRITE_WARN )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC, MRMATLC, MPCOMP_PLIES, MPCOMP0, MRPCOMP_PLIES, MRPCOMP0, &
                                         WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, THIRD, HALF, THREE, TWELVE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL, IORQ1M, QUAD4TYP, PCOMPEQ, PCMPTSTM, SHRFXFAC, SUPWARN, TSTM_DEF

      USE MODEL_STUF, ONLY            :  ALPVEC, EB, EBM, EM, ET, EDAT, EID, EMAT, EPNT, EPROP, ETYPE, FAILURE_THEORY, FCONV,      &
                                         FCONV_SHEAR_THICK, INTL_MID, INTL_PID, MASS_PER_UNIT_AREA, MATL, MEPROP, MTRL_TYPE,       &
                                         NUM_EMG_FATAL_ERRS, NUM_PLIES, PLY_NUM, PCOMP, PCOMP_LAM, PCOMP_PROPS, RPCOMP, PSHEL,     &
                                         RPSHEL, RHO, RMATL, SHELL_A, SHELL_B, SHELL_D, SHELL_T, SHELL_AALP, SHELL_BALP,           &
                                         SHELL_DALP, SHELL_TALP, SHELL_T_MOD, THETA_PLY, TPLY, TYPE, ULT_STRE, ULT_STRN, ZPLY, ZS

      USE SUBR_BEGEND_LEVELS, ONLY    :  SHELL_ABD_MATRICES_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHELL_ABD_MATRICES_BEGEND

      END SUBROUTINE SHELL_ABD_MATRICES

   END INTERFACE

   END MODULE SHELL_ABD_MATRICES_Interface

