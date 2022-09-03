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

   MODULE REDUCE_PA_TO_PL_Interface

   INTERFACE

      SUBROUTINE REDUCE_PA_TO_PL ( PART_VEC_A_LR, PART_VEC_SUB )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFL, NDOFR, NSUB, NTERM_GOA, NTERM_PA, NTERM_PL, NTERM_PR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_PA_TO_PL_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_PA, J_PA, PA, I_PL, J_PL, PL, I_PR, J_PR, PR, I_GOA, J_GOA, GOA, I_GOAt, J_GOAt, GOAt 
      USE SPARSE_MATRICES, ONLY       :  SYM_PA, SYM_PL, SYM_PR
 
      IMPLICIT NONE
               
      INTEGER(LONG), INTENT(IN)        :: PART_VEC_A_LR(NDOFA)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG), INTENT(IN)        :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG), PARAMETER         :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER         :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER         :: SUBR_BEGEND = REDUCE_PA_TO_PL_BEGEND

      END SUBROUTINE REDUCE_PA_TO_PL

   END INTERFACE

   END MODULE REDUCE_PA_TO_PL_Interface

