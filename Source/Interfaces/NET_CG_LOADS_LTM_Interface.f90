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

   MODULE NET_CG_LOADS_LTM_Interface

   INTERFACE

      SUBROUTINE NET_CG_LOADS_LTM

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NTERM_MRRcbn, NTERM_MRN, NTERM_CG_LTM, NUM_CB_DOFS
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTCGLTM, WTMASS
      USE RIGID_BODY_DISP_MATS, ONLY  :  TR6_CG, TR6_0
      USE MODEL_STUF, ONLY            :  MCG
      USE OUTPUT4_MATRICES, ONLY      :  RBM0
      USE SPARSE_MATRICES, ONLY       :  SYM_MRN   , SYM_MRRcbn, SYM_CG_LTM  
      USE SPARSE_MATRICES, ONLY       :  I_MRRcbn  , J_MRRcbn  , MRRcbn   ,  I_MRN      , J_MRN      , MRN      ,                  &
                                         I_CG_LTM  , J_CG_LTM  , CG_LTM   

      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CCS1, J_CCS1, CCS1

      USE SUBR_BEGEND_LEVELS, ONLY    :  NET_CG_LOADS_LTM_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = NET_CG_LOADS_LTM_BEGEND

      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix

      END SUBROUTINE NET_CG_LOADS_LTM

   END INTERFACE

   END MODULE NET_CG_LOADS_LTM_Interface

