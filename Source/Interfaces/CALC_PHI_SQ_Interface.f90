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

   MODULE CALC_PHI_SQ_Interface

   INTERFACE

      SUBROUTINE CALC_PHI_SQ ( IERROR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEFE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_PHI_SQ_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWELVE
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, CBMIN4T, EPSIL, PCMPTSTM, QUAD4TYP
      USE MODEL_STUF, ONLY            :  BENSUM, EID, EMG_IFE, EMG_RFE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS, INTL_MID, PHI_SQ,  &
                                         PCOMP_PROPS, PLY_NUM, PSI_HAT, SHRSUM, TPLY, TYPE
      IMPLICIT NONE 

      INTEGER(LONG), INTENT(OUT)      :: IERROR            ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_PHI_SQ_BEGEND

      END SUBROUTINE CALC_PHI_SQ

   END INTERFACE

   END MODULE CALC_PHI_SQ_Interface

