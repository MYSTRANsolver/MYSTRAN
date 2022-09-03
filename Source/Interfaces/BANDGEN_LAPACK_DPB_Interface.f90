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

   MODULE BANDGEN_LAPACK_DPB_Interface

   INTERFACE

       SUBROUTINE BANDGEN_LAPACK_DPB ( MATIN_NAME, N, KD, NTERM_MATIN, I_MATIN, J_MATIN, MATIN, MATOUT, CALLING_SUBR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SPARSTOR
      USE SUBR_BEGEND_LEVELS, ONLY    :  BANDGEN_BEGEND

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR         ! Name of subr calling this one
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME           ! Name of matrix input

      INTEGER(LONG), INTENT(IN)       :: N                    ! Number of cols (or rows) of symmetric matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_MATIN          ! No. of terms in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(N+1)         ! Array of row no's for terms in matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_MATIN) ! Array of col no's for terms in matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: KD                   ! Number of sub (or super) diagonals in matrix MATIN.
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BANDGEN_BEGEND
     
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM_MATIN)   ! Array of terms in sparse matrix MATIN
      REAL(DOUBLE) , INTENT(INOUT)    :: MATOUT(KD+1,N)       ! Array of terms in band matrix MATOUT

      END SUBROUTINE BANDGEN_LAPACK_DPB

   END INTERFACE

   END MODULE BANDGEN_LAPACK_DPB_Interface

