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

   MODULE DEALLOCATE_COL_VEC_Interface

   INTERFACE

      SUBROUTINE DEALLOCATE_COL_VEC ( NAME )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_COL_VEC_BEGEND
      USE OUTPUT4_MATRICES, ONLY      :  OU4_MAT_COL_GRD_COMP, OU4_MAT_ROW_GRD_COMP
      USE COL_VECS, ONLY              :  UG_COL, UN_COL, UM_COL, UF_COL, US_COL, UA_COL, UO_COL, UO0_COL, UR_COL, UL_COL, YSe,     &
                                         FG_COL, FN_COL, FM_COL, FF_COL, FS_COL, FA_COL, FO_COL, FL_COL, FR_COL,                   &
                                         FG_COL, PG_COL, PM_COL, PS_COL, PL_COL, QGm_COL, QGr_COL, QGs_COL, QM_COL, QN_COL, QR_COL,&
                                         QS_COL, QSYS_COL

      USE COL_VECS, ONLY              :  PHIXG_COL, PHIXL_COL, PHIXN_COL, PHIXGP_COL, PHIXLP_COL, PHIXNP_COL

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_COL_VEC_BEGEND
 
      END SUBROUTINE DEALLOCATE_COL_VEC

   END INTERFACE

   END MODULE DEALLOCATE_COL_VEC_Interface

