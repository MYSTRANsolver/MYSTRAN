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

   MODULE ALLOCATE_CB_GRD_OTM_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_CB_GRD_OTM ( NAME_IN )


      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC,                                                &
                                         GROUT_ACCE_BIT, GROUT_DISP_BIT, GROUT_SPCF_BIT, GROUT_MPCF_BIT,                           &
                                         IBIT, NDOFR, NGRID, NUM_CB_DOFS, NVEC,                                                    &
                                         NROWS_OTM_ACCE, NROWS_OTM_DISP, NROWS_OTM_MPCF, NROWS_OTM_SPCF,                           &
                                         NROWS_TXT_ACCE, NROWS_TXT_DISP, NROWS_TXT_MPCF, NROWS_TXT_SPCF
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE PARAMS, ONLY                :  OTMSKIP
      USE MODEL_STUF, ONLY            :  GRID, GROUT
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ACCE, OTM_DISP, OTM_MPCF, OTM_SPCF, TXT_ACCE, TXT_DISP, TXT_MPCF, TXT_SPCF
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_CB_GRD_OTM_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name of the matrix to be allocated

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_CB_GRD_OTM_BEGEND

      END SUBROUTINE ALLOCATE_CB_GRD_OTM

   END INTERFACE

   END MODULE ALLOCATE_CB_GRD_OTM_Interface

