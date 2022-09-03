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

   MODULE COND_NUM_Interface

   INTERFACE

      SUBROUTINE COND_NUM ( MATIN_NAME, N, KD, K_INORM, MATIN_FAC, RCOND )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  ITMAX
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  COND_NUM_BEGEND
      USE LAPACK_LIN_EQN_DPB

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME        ! Name of the matrix whose triang factor is input to this subr
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO      = 'U'   ! Indicates if matrix MATIN_FAC is an upper triangular factor

      INTEGER(LONG), INTENT(IN)       :: N                 ! No. cols in array MATIN_FAC
      INTEGER(LONG), INTENT(IN)       :: KD                ! No. of superdiagonals of KAA

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = COND_NUM_BEGEND

      REAL(DOUBLE),  INTENT(IN)       :: K_INORM           ! The infinity-norm of the matrix whose name is MATIN_NAME     
      REAL(DOUBLE),  INTENT(IN)       :: MATIN_FAC(KD+1,N) ! The upper triangular factor of the matrix whose name is MATIN_NAME
      REAL(DOUBLE),  INTENT(OUT)      :: RCOND             ! The recip of the condition number of matrix whose name is MATIN_NAME

      END SUBROUTINE COND_NUM

   END INTERFACE

   END MODULE COND_NUM_Interface

