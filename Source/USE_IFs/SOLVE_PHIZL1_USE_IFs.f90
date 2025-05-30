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

      MODULE SOLVE_PHIZL1_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE SOLVE_PHIZL1

      USE OURTIM_Interface
      USE OPNERR_Interface
      USE FILE_CLOSE_Interface
      USE OUTA_HERE_Interface
      USE GET_SPARSE_CRS_COL_Interface
      USE FBS_LAPACK_Interface
      USE FBS_SUPRLU_Interface
      USE deallocate_sparse_mat_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE ALLOCATE_L6_2_Interface
      USE READ_MATRIX_2_Interface
      USE GET_I_MAT_FROM_I2_MAT_Interface
      USE MATTRNSP_SS_Interface
      USE COUNTER_INIT_Interface
      USE COUNTER_PROGRESS_Interface

      END MODULE SOLVE_PHIZL1_USE_IFs
