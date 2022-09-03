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

   MODULE CALC_MRRcb_Interface

   INTERFACE

      SUBROUTINE CALC_MRRcb

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFL, NDOFR, NTERM_DLR, NTERM_MLL, NTERM_MRL, NTERM_MRR,        &
                                         NTERM_MRRcb, NTERM_MRRcbn
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  SPARSTOR, WTMASS
      USE RIGID_BODY_DISP_MATS, ONLY  :  TR6_MEFM
      USE MODEL_STUF, ONLY            :  MEFM_RB_MASS
      USE SPARSE_MATRICES , ONLY      :  SYM_DLR, SYM_MLL, SYM_MRL, SYM_MRR, SYM_MRRcb

      USE SPARSE_MATRICES , ONLY      :  I_MLL  , J_MLL  , MLL  , I_MRL  , J_MRL  , MRL  , I_MRR  , J_MRR  , MRR  ,                &
                                         I_DLR  , J_DLR  , DLR  , I_DLRt , J_DLRt , DLRt , I_MRRcb, J_MRRcb, MRRcb,                &
                                         SYM_MRRcb

      USE SCRATCH_MATRICES
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_MRRcb_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_MRRcb_BEGEND

      END SUBROUTINE CALC_MRRcb

   END INTERFACE

   END MODULE CALC_MRRcb_Interface

