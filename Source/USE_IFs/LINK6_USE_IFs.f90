! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

      MODULE LINK6_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE LINK6

      USE TIME_INIT_Interface
      USE OURDAT_Interface
      USE OURTIM_Interface
      USE READ_L1A_Interface
      USE OUTA_HERE_Interface
      USE ALLOCATE_EIGEN1_MAT_Interface
      USE READ_L1M_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE READ_MATRIX_1_Interface
      USE FILE_OPEN_Interface
      USE READERR_Interface
      USE FILE_CLOSE_Interface
      USE SOLVE_DLR_Interface
      USE PARTITION_VEC_Interface
      USE MERGE_PHIXA_Interface
      USE WRITE_SPARSE_CRS_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE CALC_PHIZL_Interface
      USE CALC_KRRcb_Interface
      USE MERGE_KXX_Interface
      USE CALC_MRRcb_Interface
      USE CALC_MRN_Interface
      USE MERGE_MXX_Interface
      USE INTERFACE_FORCE_LTM_Interface
      USE NET_CG_LOADS_LTM_Interface
      USE MERGE_LTM_Interface
      USE GET_SPARSE_CRS_COL_Interface
      USE CALC_CB_MEFM_MPF_Interface
      USE OUTPUT4_PROC_Interface
      USE WRITE_USERIN_BD_CARDS_Interface
      USE DEALLOCATE_EIGEN1_MAT_Interface
      USE DEALLOCATE_LAPACK_MAT_Interface
      USE WRITE_L1A_Interface
      USE CHK_ARRAY_ALLOC_STAT_Interface
      USE WRITE_ALLOC_MEM_TABLE_Interface
      USE FILE_INQUIRE_Interface

      END MODULE LINK6_USE_IFs
