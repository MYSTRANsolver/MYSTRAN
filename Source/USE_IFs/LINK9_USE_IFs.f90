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

      MODULE LINK9_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE LINK9

      USE TIME_INIT_Interface
      USE OURDAT_Interface
      USE OURTIM_Interface
      USE FILE_OPEN_Interface
      USE READ_L1A_Interface
      USE OUTA_HERE_Interface
      USE DEALLOCATE_MODEL_STUF_Interface
      USE ALLOCATE_MODEL_STUF_Interface
      USE LINK9S_Interface
      USE MAXREQ_OGEL_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE READ_MATRIX_1_Interface
      USE PARTITION_VEC_Interface
      USE PARTITION_SS_NTERM_Interface
      USE PARTITION_SS_Interface
      USE ALLOCATE_COL_VEC_Interface
      USE GET_SPARSE_CRS_COL_Interface
      USE MATTRNSP_SS_Interface
      USE ALLOCATE_EIGEN1_MAT_Interface
      USE READ_L1M_Interface
      USE DEALLOCATE_EIGEN1_MAT_Interface
      USE ALLOCATE_LINK9_STUF_Interface
      USE ALLOCATE_CB_GRD_OTM_Interface
      USE ALLOCATE_CB_ELM_OTM_Interface
      USE DEALLOCATE_COL_VEC_Interface
      USE READERR_Interface
      USE OFP1_Interface
      USE OFP2_Interface
      USE MATMULT_SFF_Interface
      USE TDOF_COL_NUM_Interface
      USE GP_FORCE_BALANCE_PROC_Interface
      USE OFP3_Interface
      USE FILE_CLOSE_Interface
      USE WRITE_OU4_FULL_MAT_Interface
      USE WRITE_GRD_OT4_Interface
      USE WRITE_ELM_OT4_Interface
      USE WRITE_MPFACTOR_Interface
      USE WRITE_MEFFMASS_Interface
      USE OUTPUT4_PROC_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE DEALLOCATE_IN4_FILES_Interface
      USE DEALLOCATE_LINK9_STUF_Interface
      USE WRITE_L1A_Interface
      USE FILE_INQUIRE_Interface
      USE CHK_ARRAY_ALLOC_STAT_Interface
      USE WRITE_ALLOC_MEM_TABLE_Interface
      USE MERGE_COL_VECS_Interface

      END MODULE LINK9_USE_IFs
