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

      MODULE SOLVE_GMN_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE SOLVE_GMN

      USE OURTIM_Interface
      USE WRITE_SPARSE_CRS_Interface
      USE PARTITION_SS_NTERM_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE PARTITION_SS_Interface
      USE OUTA_HERE_Interface
      USE ALLOCATE_L2_GMN_2_Interface
      USE WRITE_MATRIX_1_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE DEALLOCATE_L2_GMN_2_Interface
      USE ALLOCATE_FULL_MAT_Interface
      USE SPARSE_CRS_TO_FULL_Interface
      USE GET_GRID_AND_COMP_Interface
      USE ALLOCATE_SCR_CCS_MAT_Interface
      USE SPARSE_CRS_SPARSE_CCS_Interface
      USE SYM_MAT_DECOMP_SUPRLU_Interface
      USE OPNERR_Interface
      USE FILE_CLOSE_Interface
      USE GET_SPARSE_CRS_COL_Interface
      USE FBS_SUPRLU_Interface
      USE DEALLOCATE_SCR_MAT_Interface
      USE DEALLOCATE_FULL_MAT_Interface
      USE READ_MATRIX_2_Interface
      USE SORT_INT2_REAL1_Interface
      USE READ_MATRIX_1_Interface

      END MODULE SOLVE_GMN_USE_IFs
