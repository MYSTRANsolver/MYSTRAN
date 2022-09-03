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

   MODULE RENORM_ON_MASS_Interface

   INTERFACE

      SUBROUTINE RENORM_ON_MASS ( NVC, EPS1 )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NDOFL, BLNK_SUB_NAM, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL, SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  RENORM_ON_MASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE EIGEN_MATRICES_1 , ONLY     :  GEN_MASS, EIGEN_VEC
      USE MODEL_STUF, ONLY            :  EIG_NORM, MAXMIJ, MIJ_COL, MIJ_ROW
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
  
      IMPLICIT NONE
  
      INTEGER(LONG), INTENT(IN)       :: NVC               ! Number of eigenvectors to be renormalized.
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RENORM_ON_MASS_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: EPS1              ! Small number to compare variables against zero

      END SUBROUTINE RENORM_ON_MASS

   END INTERFACE

   END MODULE RENORM_ON_MASS_Interface

