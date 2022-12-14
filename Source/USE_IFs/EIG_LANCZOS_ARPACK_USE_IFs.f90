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

      MODULE EIG_LANCZOS_ARPACK_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE EIG_LANCZOS_ARPACK

      USE OURTIM_Interface
      USE MATADD_SSS_NTERM_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE MATADD_SSS_Interface
      USE BANDSIZ_Interface
      USE OUTA_HERE_Interface
      USE ALLOCATE_LAPACK_MAT_Interface
      USE BANDGEN_LAPACK_DPB_Interface
      USE BANDGEN_LAPACK_DGB_Interface
      USE WRITE_MATRIX_BY_ROWS_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE SPARSE_MAT_DIAG_ZEROS_Interface
      USE CRS_SYM_TO_CRS_NONSYM_Interface
      USE WRITE_SPARSE_CRS_Interface
      USE ALLOCATE_EIGEN1_MAT_Interface
      USE DEALLOCATE_LAPACK_MAT_Interface
      USE GET_GRID_AND_COMP_Interface
      USE INVERT_EIGENS_Interface

      END MODULE EIG_LANCZOS_ARPACK_USE_IFs
