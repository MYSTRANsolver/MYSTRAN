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

      MODULE REDUCE_KFF_TO_KAA_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE REDUCE_KFF_TO_KAA

      USE OURTIM_Interface
      USE PARTITION_SS_NTERM_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE PARTITION_SS_Interface
      USE SYM_MAT_DECOMP_LAPACK_Interface
      USE SYM_MAT_DECOMP_SUPRLU_Interface
      USE OUTA_HERE_Interface
      USE SOLVE_GOA_Interface
      USE WRITE_MATRIX_1_Interface
      USE ALLOCATE_SCR_CCS_MAT_Interface
      USE SPARSE_CRS_SPARSE_CCS_Interface
      USE MATMULT_SSS_NTERM_Interface
      USE ALLOCATE_SCR_CRS_MAT_Interface
      USE MATMULT_SSS_Interface
      USE DEALLOCATE_SCR_MAT_Interface
      USE SPARSE_CRS_TERM_COUNT_Interface
      USE CRS_NONSYM_TO_CRS_SYM_Interface
      USE MATADD_SSS_NTERM_Interface
      USE MATADD_SSS_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE ALLOCATE_FULL_MAT_Interface
      USE SPARSE_CRS_TO_FULL_Interface
      USE MATMULT_FFF_Interface
      USE DEALLOCATE_FULL_MAT_Interface
      USE MATADD_FFF_Interface
      USE CNT_NONZ_IN_FULL_MAT_Interface

      END MODULE REDUCE_KFF_TO_KAA_USE_IFs
