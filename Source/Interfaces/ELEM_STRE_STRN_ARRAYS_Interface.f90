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

   MODULE ELEM_STRE_STRN_ARRAYS_Interface

   INTERFACE

      SUBROUTINE ELEM_STRE_STRN_ARRAYS ( STR_PT_NUM )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, INT_SC_NUM, JTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELEM_STRE_STRN_ARRAYS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, one, four
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, BE2, BE3, DT, EM, EB, ES, ET, ELDOF, PEL, PHI_SQ, STRAIN, STRESS, SUBLOD,    &
                                         TREF, TYPE, UEL, SE1, SE2, SE3, STE1, STE2, STE3
      USE DEBUG_PARAMETERS
      USE PARAMS, ONLY                :  STR_CID

      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: STR_PT_NUM        ! Which point (3rd index in SEi matrices) this call is for
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELEM_STRE_STRN_ARRAYS_BEGEND
 
      REAL(DOUBLE)                    :: DUM31(3)          ! Array used in an intermediate calc
      REAL(DOUBLE)                    :: DUM32(3)          ! Array used in an intermediate calc
      REAL(DOUBLE)                    :: DUM33(3)          ! Array used in an intermediate calc

      END SUBROUTINE ELEM_STRE_STRN_ARRAYS

   END INTERFACE

   END MODULE ELEM_STRE_STRN_ARRAYS_Interface

