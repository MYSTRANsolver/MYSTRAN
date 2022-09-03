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

   MODULE ROT_COMP_ELEM_AXES_Interface

   INTERFACE

      SUBROUTINE ROT_COMP_ELEM_AXES ( IPLY, THETA, DIRECTION )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC 
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD, ZERO, HALF, ONE, TWO, FOUR
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  ALPVEC, EB, EM, ET, EBM, INTL_MID, MTRL_TYPE, STRESS, STRAIN, T1P, T1M, T1T, T2P, T2M, T2T

      USE SUBR_BEGEND_LEVELS, ONLY    :  ROT_COMP_ELEM_AXES_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: DIRECTION         ! =1-2, rotate from ply to elem mat'l axes (when gen ABD matrices)
      INTEGER(LONG), INTENT(IN)       :: IPLY              ! Ply number
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROT_COMP_ELEM_AXES_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: THETA             ! Orient angle of long dir of ply i wrt matl axis for the composite elem
 
      END SUBROUTINE ROT_COMP_ELEM_AXES

   END INTERFACE

   END MODULE ROT_COMP_ELEM_AXES_Interface

