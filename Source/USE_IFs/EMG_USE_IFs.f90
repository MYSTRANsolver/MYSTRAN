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

      MODULE EMG_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE EMG

      USE IS_ELEM_PCOMP_PROPS_Interface
      USE OURTIM_Interface
      USE ELMDAT1_Interface
      USE OUTA_HERE_Interface
      USE ELMGM1_Interface
      USE ELMGM2_Interface
      USE ELMGM3_Interface
      USE GET_MATANGLE_FROM_CID_Interface
      USE MATERIAL_PROPS_2D_Interface
      USE ROT_AXES_MATL_TO_LOC_Interface
      USE MATERIAL_PROPS_3D_Interface
      USE ELMOUT_Interface
      USE SHELL_ABD_MATRICES_Interface
      USE ELMDAT2_Interface
      USE ELAS1_Interface
      USE BREL1_Interface
      USE BUSH_Interface
      USE TREL1_Interface
      USE QDEL1_Interface
      USE HEXA_Interface
      USE PENTA_Interface
      USE TETRA_Interface
      USE KUSER1_Interface
      USE USERIN_Interface
      USE ELMOFF_Interface
      USE MITC8_Interface

      END MODULE EMG_USE_IFs
