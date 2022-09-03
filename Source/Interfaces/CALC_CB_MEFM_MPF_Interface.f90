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

   MODULE CALC_CB_MEFM_MPF_Interface

   INTERFACE

      SUBROUTINE CALC_CB_MEFM_MPF

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR,                                                                  &
                                         NDOFL, NDOFR, NTERM_MPF0 , NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTs_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  MPFOUT
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_CB_MEFM_MPF_BEGEND
      USE RIGID_BODY_DISP_MATS, ONLY  :  TR6_MEFM
      USE SPARSE_MATRICES, ONLY       :  I_MPF0 , J_MPF0 , MPF0 , SYM_MPF0
      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VEC, GEN_MASS, MEFFMASS, MPFACTOR_NR, MPFACTOR_N6

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_CB_MEFM_MPF_BEGEND

      END SUBROUTINE CALC_CB_MEFM_MPF

   END INTERFACE

   END MODULE CALC_CB_MEFM_MPF_Interface

