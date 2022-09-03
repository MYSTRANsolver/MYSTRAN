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

   MODULE REDUCE_KFFD_TO_KAAD_Interface

   INTERFACE

      SUBROUTINE REDUCE_KFFD_TO_KAAD ( PART_VEC_F_AO )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2E, LINK2E, L2E_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FACTORED_MATRIX, FATAL_ERR, NDOFF, NDOFA, NDOFO, NTERM_KFFD, NTERM_KAAD,    &
                                         NTERM_KAOD, NTERM_KOOD, NTERM_KOODs, NTERM_GOA
      USE PARAMS, ONLY                :  EPSIL, KOORAT, SPARSTOR, RCONDK
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KFFD_TO_KAAD_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_KFFD, J_KFFD, KFFD, I_KAAD, J_KAAD, KAAD, I_KAOD, J_KAOD, KAOD, I_GOA, J_GOA, GOA,      &
                                         I_KOOD, I2_KOOD, J_KOOD, KOOD, I_KOODs, I2_KOODs, J_KOODs, KOODs
                                         
      USE SPARSE_MATRICES, ONLY       :  SYM_GOA, SYM_KFFD, SYM_KAAD, SYM_KAOD, SYM_KOOD
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F_AO(NDOFF)! Partitioning vector (F set into A and O sets) 

      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KFFD_TO_KAAD_BEGEND

      END SUBROUTINE REDUCE_KFFD_TO_KAAD

   END INTERFACE

   END MODULE REDUCE_KFFD_TO_KAAD_Interface

