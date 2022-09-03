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

      MODULE REDUCE_N_FS_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE REDUCE_N_FS

      USE OURTIM_Interface
      USE PARTITION_VEC_Interface
      USE ALLOCATE_COL_VEC_Interface
      USE FILE_OPEN_Interface
      USE READERR_Interface
      USE OUTA_HERE_Interface
      USE FILE_CLOSE_Interface
      USE WRITE_VECTOR_Interface
      USE REDUCE_KNN_TO_KFF_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE REDUCE_MNN_TO_MFF_Interface
      USE REDUCE_PN_TO_PF_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE MATMULT_SFS_NTERM_Interface
      USE WRITE_SPARSE_CRS_Interface
      USE MATMULT_SFS_Interface
      USE WRITE_MATRIX_1_Interface
      USE DEALLOCATE_COL_VEC_Interface
      USE GET_MATRIX_DIAG_STATS_Interface
      USE ALLOCATE_RBGLOBAL_Interface
      USE TDOF_COL_NUM_Interface
      USE STIFF_MAT_EQUIL_CHK_Interface
      USE DEALLOCATE_RBGLOBAL_Interface
      USE REDUCE_KNND_TO_KFFD_Interface

      END MODULE REDUCE_N_FS_USE_IFs
