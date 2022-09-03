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

   MODULE WRITE_MEFFMASS_Interface

   INTERFACE

      SUBROUTINE WRITE_MEFFMASS

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_MEFFMASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, ONE_HUNDRED, PI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, MEFFMASS
      USE MODEL_STUF, ONLY            :  MEFM_RB_MASS, LABEL, STITLE, TITLE
      USE PARAMS, ONLY                :  EPSIL, GRDPNT, MEFMCORD, MEFMGRID, MEFMLOC, SUPINFO, WTMASS
  
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_MEFFMASS_BEGEND

      END SUBROUTINE WRITE_MEFFMASS

   END INTERFACE

   END MODULE WRITE_MEFFMASS_Interface

