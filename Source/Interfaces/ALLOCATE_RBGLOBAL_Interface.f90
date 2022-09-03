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

   MODULE ALLOCATE_RBGLOBAL_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_RBGLOBAL ( SET, CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NDOFG, NDOFN, NDOFF, NDOFA, NDOFL, NDOFR, BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_RBGLOBAL_BEGEND
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_NSET, RBGLOBAL_FSET, RBGLOBAL_ASET, RBGLOBAL_LSET,                &
                                         TR6_CG, TR6_MEFM, TR6_0

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: SET               ! Set name of the displ matrix
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(14*BYTE)              :: NAME              ! Specific array name used for output error message
 
      INTEGER(LONG), PARAMETER        :: NCOLS       = 6   ! Number of cols in array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_RBGLOBAL_BEGEND

      END SUBROUTINE ALLOCATE_RBGLOBAL

   END INTERFACE

   END MODULE ALLOCATE_RBGLOBAL_Interface

