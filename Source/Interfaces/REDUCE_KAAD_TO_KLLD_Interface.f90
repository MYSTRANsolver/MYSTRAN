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

   MODULE REDUCE_KAAD_TO_KLLD_Interface

   INTERFACE

      SUBROUTINE REDUCE_KAAD_TO_KLLD ( PART_VEC_A_LR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2K, L2L, LINK2K, LINK2L, L2K_MSG, L2L_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFL, NDOFR, NTERM_KAAD, NTERM_KLLD, NTERM_KRLD,         &
                                         NTERM_KRRD,  SOL_NAME
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KAAD_TO_KLLD_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KAAD, J_KAAD, KAAD, I_KLLD, J_KLLD, KLLD, I_KRLD, J_KRLD, KRLD, I_KRRD, J_KRRD, KRRD,   &
                                         SYM_KAAD, SYM_KLLD, SYM_KRLD, SYM_KRRD
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: PART_VEC_A_LR(NDOFA)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KAAD_TO_KLLD_BEGEND

      END SUBROUTINE REDUCE_KAAD_TO_KLLD

   END INTERFACE

   END MODULE REDUCE_KAAD_TO_KLLD_Interface

