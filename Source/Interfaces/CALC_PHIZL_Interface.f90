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

   MODULE CALC_PHIZL_Interface

   INTERFACE

      SUBROUTINE CALC_PHIZL

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFL, NDOFR,                                                    &
                                         NTERM_DLR, NTERM_PHIZL, NTERM_PHIZL1, NTERM_PHIZL2 , NTERM_MLL, NTERM_MLR, NTERM_MRL,     &
                                         NUM_CB_DOFS, NVEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTPHIZL, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VEC, EIGEN_VAL
      USE SPARSE_MATRICES, ONLY       :  SYM_MLL, SYM_MLR, SYM_MRL, SYM_KLL, SYM_DLR, SYM_PHIZL  , SYM_PHIZL1  , SYM_PHIZL2  

      USE SPARSE_MATRICES, ONLY       :  I_MLL   , J_MLL   , MLL   , I_MLR   , J_MLR   , MLR   , I_MRL   , J_MRL   , MRL,          &
                                         I_KLL   , J_KLL   , KLL   , I_DLR   , J_DLR   , DLR   ,                                   &
                                         I_PHIZL , J_PHIZL , PHIZL , I_PHIZL1, J_PHIZL1, PHIZL1, I_PHIZL2, J_PHIZL2, PHIZL2

      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CRS3, J_CRS3, CRS3, I_CCS1, J_CCS1, CCS1 

      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_PHIZL_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_PHIZL_BEGEND

      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix

      END SUBROUTINE CALC_PHIZL

   END INTERFACE

   END MODULE CALC_PHIZL_Interface

