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

   MODULE CALC_GEN_MASS_Interface

   INTERFACE

      SUBROUTINE CALC_GEN_MASS


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFL, NTERM_KLLDn, NTERM_MLLn, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_GEN_MASS_BEGEND
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS, EIGEN_VEC
      USE MODEL_STUF, ONLY            :  EIG_CRIT, MAXMIJ, MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT
      USE SPARSE_MATRICES, ONLY       :  I_KLLDn, J_KLLDn, KLLDn, I_MLLn, J_MLLn, MLLn
      USE SPARSE_MATRICES, ONLY       :  SYM_MLLn
      USE LAPACK_BLAS_AUX

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_GEN_MASS_BEGEND

      REAL(DOUBLE)                    :: MAX               ! Temporary variable used in finding MAXMIJ
      REAL(DOUBLE)                    :: MIJ               ! The i,j-th value from gen. mass matrix. Used to find MAXMIJ

      END SUBROUTINE CALC_GEN_MASS

   END INTERFACE

   END MODULE CALC_GEN_MASS_Interface

