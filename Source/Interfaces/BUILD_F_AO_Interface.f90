! ###############################################################################################################################
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

   MODULE BUILD_F_AO_Interface

   INTERFACE

      SUBROUTINE BUILD_F_AO

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFA, NDOFF, NDOFO, NTERM_GOA, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUILD_F_AO_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTDISP
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SPARSE_MATRICES, ONLY       :  I_GOA, J_GOA, GOA, SYM_GOA
      USE COL_VECS, ONLY              :  UA_COL, UF_COL, UO_COL, UO0_COL
 
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: NUMCOLS     = 1   ! Variable for number of cols of an array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUILD_F_AO_BEGEND
 
      END SUBROUTINE BUILD_F_AO

   END INTERFACE

   END MODULE BUILD_F_AO_Interface

