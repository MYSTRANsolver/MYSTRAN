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

      MODULE MYSTRAN_USE_IFs

! USE Interface statements for all subroutines called by PROGRAM MYSTRAN

      USE TIME_INIT_Interface
      USE READ_INI_Interface
      USE OURTIM_Interface
      USE OURDAT_Interface
      USE READ_INPUT_FILE_NAME_Interface
      USE WRITE_FILNAM_Interface
      USE FILE_OPEN_Interface
      USE IS_THIS_A_RESTART_Interface
      USE MYSTRAN_FILES_Interface
      USE PROCESS_INCLUDE_FILES_Interface
      USE FILE_CLOSE_Interface
      USE LOADE0_Interface
      USE READ_L1A_Interface
      USE LINK0_Interface
      USE LINK1_Interface
      USE LINK2_Interface
      USE LINK3_Interface
      USE LINK4_Interface
      USE LINK6_Interface
      USE DEALLOCATE_RBGLOBAL_Interface
      USE LINK5_Interface
      USE OUTA_HERE_Interface
      USE LINK9_Interface
      USE RESTART_DATA_FOR_L3_Interface
      USE DEALLOCATE_NL_PARAMS_Interface
      USE FILE_INQUIRE_Interface
      USE CLOSE_OUTFILES_Interface
      USE CLOSE_LIJFILES_Interface
      USE VECTOR_NORM_Interface

      END MODULE MYSTRAN_USE_IFs
