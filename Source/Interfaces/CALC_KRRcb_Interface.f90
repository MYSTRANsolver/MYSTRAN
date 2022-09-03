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

   MODULE CALC_KRRcb_Interface

   INTERFACE

      SUBROUTINE CALC_KRRcb

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FACTORED_MATRIX, FATAL_ERR, KRRcb_SDIA,                                     &
                                         NDOFL, NDOFR, NTERM_DLR, NTERM_KRL, NTERM_KRR, NTERM_KRRcb, NTERM_KRRcbs
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  SPARSTOR, SUPINFO
      USE SPARSE_MATRICES , ONLY      :  SYM_DLR, SYM_KRL, SYM_KRR
      USE SPARSE_MATRICES , ONLY      :  I_KRL, J_KRL, KRL, I_KRR, J_KRR, KRR, I_DLR, J_DLR, DLR,                                  &
                                         I_KRRcb, J_KRRcb, KRRcb, I_KRRcbs, J_KRRcbs, KRRcbs
      USE SCRATCH_MATRICES
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_KRRcb_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_KRRcb_BEGEND

      END SUBROUTINE CALC_KRRcb

   END INTERFACE

   END MODULE CALC_KRRcb_Interface

