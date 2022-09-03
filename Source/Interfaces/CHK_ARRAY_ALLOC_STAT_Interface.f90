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

   MODULE CHK_ARRAY_ALLOC_STAT_Interface

   INTERFACE

      SUBROUTINE CHK_ARRAY_ALLOC_STAT


      USE SCONTR, ONLY                :  LINKNO
      USE IOUNT1, ONLY                :  F06, IN4FIL, IN4FIL_NUM
      USE ARPACK_MATRICES_1
      USE COL_VECS
      USE DOF_TABLES
      USE EIGEN_MATRICES_1
      USE EMS_ARRAYS
      USE FEMAP_ARRAYS
      USE FULL_MATRICES
      USE LAPACK_DPB_MATRICES
      USE LINK9_STUFF
      USE MODEL_STUF
      USE NONLINEAR_PARAMS
      USE INPUTT4_MATRICES
      USE RIGID_BODY_DISP_MATS
      USE SCRATCH_MATRICES
      USE SPARSE_ALG_ARRAYS
      USE SPARSE_MATRICES
      USE STF_ARRAYS
      USE STF_TEMPLATE_ARRAYS
      USE OUTPUT4_MATRICES

      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: NUM_NAMES = 585   ! Number of arrays to check

      END SUBROUTINE CHK_ARRAY_ALLOC_STAT

   END INTERFACE

   END MODULE CHK_ARRAY_ALLOC_STAT_Interface

