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

   MODULE MERGE_MXX_Interface

   INTERFACE

      SUBROUTINE MERGE_MXX  

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NVEC, NTERM_MRRcb, NTERM_MRRcbn, NTERM_MRN, NTERM_MXX,    &
                                         NTERM_MXXn
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  PRTMXX, SPARSTOR
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS
      USE SPARSE_MATRICES, ONLY       :  SYM_MRRcbn, SYM_MRN  , SYM_MXX  , SYM_MXXn
      USE SPARSE_MATRICES, ONLY       :  I_MRRcb, J_MRRcb, MRRcb, I_MRRcbn, J_MRRcbn, MRRcbn, I_MRN  , J_MRN  , MRN  ,             &
                                         I_MXX  , J_MXX  , MXX  , I_MXXn  , J_MXXn  , MXXn
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_MXX_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_MXX_BEGEND

      END SUBROUTINE MERGE_MXX

   END INTERFACE

   END MODULE MERGE_MXX_Interface

