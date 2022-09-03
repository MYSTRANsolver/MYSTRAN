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

   MODULE SYM_MAT_DECOMP_SUPRLU_Interface

   INTERFACE

      SUBROUTINE SYM_MAT_DECOMP_SUPRLU ( CALLING_SUBR, MATIN_NAME, NROWS, NTERMS, I_MATIN, J_MATIN, MATIN, INFO )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC       
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  CRS_CCS, SPARSTOR
      USE SCRATCH_MATRICES, ONLY      :  I_CCS1, J_CCS1, CCS1
      USE SuperLU_STUF, ONLY          :  SLU_FACTORS
      USE SUBR_BEGEND_LEVELS, ONLY    :  SYM_MAT_DECOMP_SUPRLU_BEGEND

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! The subr that called this subr (used for output error purposes)
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME        ! Name of matrix to be decomposed

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzeros in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)  ! Indicators of number of nonzero terms in rows of matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERMS)   ! Col numberts of nonzero terms in matrix MATIN

      INTEGER(LONG), INTENT(INOUT)    :: INFO              ! Output from SuperLU routine

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SYM_MAT_DECOMP_SUPRLU_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERMS)     ! A small number to compare real zero

      END SUBROUTINE SYM_MAT_DECOMP_SUPRLU

   END INTERFACE

   END MODULE SYM_MAT_DECOMP_SUPRLU_Interface

