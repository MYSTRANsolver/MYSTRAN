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

   MODULE REDUCE_KFF_TO_KAA_Interface

   INTERFACE

      SUBROUTINE REDUCE_KFF_TO_KAA ( PART_VEC_F_AO )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2E, LINK2E, L2E_MSG, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KOO_SDIA, NDOFF, NDOFA, NDOFO, NTERM_KFF,       &
                                         NTERM_KAA, NTERM_KAO, NTERM_KOO, NTERM_GOA
      USE PARAMS, ONLY                :  KOORAT, MATSPARS, SOLLIB, SPARSTOR, SPARSE_FLAVOR, RCONDK
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KFF_TO_KAA_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE FULL_MATRICES, ONLY         :  KAA_FULL, KAO_FULL, GOA_FULL, DUM1, DUM2
      USE SPARSE_MATRICES, ONLY       :  I_KFF, J_KFF, KFF, I_KAA, J_KAA, KAA, I_KAO, J_KAO, KAO, I_GOA, J_GOA, GOA,               &
                                         I_KOO, J_KOO, KOO
                                         
      USE SPARSE_MATRICES, ONLY       :  SYM_GOA, SYM_KFF, SYM_KAA, SYM_KAO, SYM_KOO
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F_AO(NDOFF)! Partitioning vector (F set into A and O sets) 

      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KFF_TO_KAA_BEGEND

      REAL(DOUBLE)                    :: SMALL               ! A number used in filtering out small numbers from a full matrix
 
      END SUBROUTINE REDUCE_KFF_TO_KAA

   END INTERFACE

   END MODULE REDUCE_KFF_TO_KAA_Interface

