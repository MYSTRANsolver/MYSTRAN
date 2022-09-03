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

   MODULE REDUCE_KNND_TO_KFFD_Interface

   INTERFACE

      SUBROUTINE REDUCE_KNND_TO_KFFD ( PART_VEC_N_FS, PART_VEC_S_SzSe, PART_VEC_F, PART_VEC_S )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2B, LINK2B, L2B_MSG
      USE SCONTR, ONLY                :  FATAL_ERR, NDOFN, NDOFF, NDOFS, NDOFSE, NTERM_KNND, NTERM_KFFD, NTERM_KFSD, NTERM_KSSD,   &
                                         NTERM_KFSDe, NTERM_KSSDe, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KNND_TO_KFFD_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KNND, J_KNND, KNND, I_KFFD, J_KFFD, KFFD, I_KFSD, J_KFSD, KFSD, I_KFSDe, J_KFSDe, KFSDe,&
                                         I_KSFD, J_KSFD, KSFD, I_KSSD, J_KSSD, KSSD, I_KSSDe, J_KSSDe, KSSDe
      USE SPARSE_MATRICES, ONLY       :  SYM_KNND, SYM_KFFD, SYM_KFSD, SYM_KFSDe, SYM_KSSD, SYM_KSSD, SYM_KSSDe
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F(NDOFF)      ! Partitioning vector (1's for all of F set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)   ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S(NDOFS)      ! Partitioning vector (1's for all of S set) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_S_SzSe(NDOFS) ! Partitioning vector (S set into SZ and SE sets) 
      INTEGER(LONG), PARAMETER        :: NUM1        = 1        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2        ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KNND_TO_KFFD_BEGEND

      END SUBROUTINE REDUCE_KNND_TO_KFFD

   END INTERFACE

   END MODULE REDUCE_KNND_TO_KFFD_Interface

