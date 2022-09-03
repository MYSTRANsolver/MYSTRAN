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

   MODULE PARAM_CORDS_ACT_CORDS_Interface

   INTERFACE

      SUBROUTINE PARAM_CORDS_ACT_CORDS ( NROW, IORD, XEP, XEA )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  TYPE, XEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARAM_CORDS_ACT_CORDS_BEGEND

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: IORD              ! Gaussian integration order to be used in obtaining the PSH shape fcns
      INTEGER(LONG), INTENT(IN)       :: NROW              ! Number of rows in XEP, XEA
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PARAM_CORDS_ACT_CORDS_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: XEP(NROW,3)       ! Parametric coords of NCOL points
      REAL(DOUBLE), INTENT(OUT)       :: XEA(NROW,3)       ! Actual local element coords corresponding to XEP

      END SUBROUTINE PARAM_CORDS_ACT_CORDS

   END INTERFACE

   END MODULE PARAM_CORDS_ACT_CORDS_Interface

