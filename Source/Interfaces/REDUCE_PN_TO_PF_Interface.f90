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

   MODULE REDUCE_PN_TO_PF_Interface

   INTERFACE

      SUBROUTINE REDUCE_PN_TO_PF ( PART_VEC_N_FS, PART_VEC_SUB )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2D, LINK2D, L2D_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFN, NDOFF, NDOFS, NDOFSE, NSUB, NTERM_KFSe, NTERM_PN,         &
                                         NTERM_PF, NTERM_PFYS, NTERM_PS
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_PN_TO_PF_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE PARAMS, ONLY                :  MATSPARS
      USE SPARSE_MATRICES, ONLY       :  I_KFSe, J_KFSe, KFSe, I_PN, J_PN, PN, I_PF, J_PF, PF, I_PS, J_PS, PS, I_PF_TMP, J_PF_TMP, &
                                         PF_TMP, I_PFYS, J_PFYS, PFYS, I_PFYS1, J_PFYS1, PFYS1, I_QSYS, J_QSYS, QSYS
      USE SPARSE_MATRICES, ONLY       :  SYM_KFSe, SYM_PN, SYM_PF, SYM_PFYS, SYM_PF_TMP, SYM_PS
      USE COL_VECS, ONLY              :  YSe 
      USE FULL_MATRICES, ONLY         :  KFSe_FULL, PF_FULL, PFYS_FULL, DUM1
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
   
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM_YS_COLS = 1     ! Variable for number of cols in array YSe 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_PN_TO_PF_BEGEND

      END SUBROUTINE REDUCE_PN_TO_PF

   END INTERFACE

   END MODULE REDUCE_PN_TO_PF_Interface

