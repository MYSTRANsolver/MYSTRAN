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

      MODULE OFP3_ELFE_2D_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE OFP3_ELFE_2D

      USE OURTIM_Interface
      USE is_elem_pcomp_props_Interface
      USE EMG_Interface
      USE ELMDIS_Interface
      USE ELEM_STRE_STRN_ARRAYS_Interface
      USE SHELL_ENGR_FORCE_OGEL_Interface
      USE CHK_OGEL_ZEROS_Interface
      USE WRITE_ELEM_ENGR_FORCE_Interface
      USE ALLOCATE_FEMAP_DATA_Interface
      USE WRITE_FEMAP_ELFO_VECS_Interface
      USE DEALLOCATE_FEMAP_DATA_Interface

      END MODULE OFP3_ELFE_2D_USE_IFs
