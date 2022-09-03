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

   MODULE REDUCE_PG_TO_PN_Interface

   INTERFACE

      SUBROUTINE REDUCE_PG_TO_PN ( PART_VEC_G_NM, PART_VEC_SUB )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NDOFN, NDOFM, NSUB, NTERM_GMN, NTERM_PG, NTERM_PN, NTERM_PM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_PG_TO_PN_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE PARAMS, ONLY                :  EPSIL, MATSPARS
      USE SPARSE_MATRICES, ONLY       :  I_PG, J_PG, PG, I_PN, J_PN, PN, I_PM, J_PM, PM, I_GMN, J_GMN, GMN, I_GMNt, J_GMNt, GMNt 
      USE SPARSE_MATRICES, ONLY       :  SYM_GMN, SYM_PG, SYM_PN, SYM_PM
      USE FULL_MATRICES, ONLY         :  PN_FULL, PM_FULL, GMN_FULL, DUM1, DUM2
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_PG_TO_PN_BEGEND

      REAL(DOUBLE)                    :: SMALL               ! A number used in filtering out small numbers from a full matrix
 
      END SUBROUTINE REDUCE_PG_TO_PN

   END INTERFACE

   END MODULE REDUCE_PG_TO_PN_Interface

