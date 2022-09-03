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

   MODULE INVERT_FF_MAT_Interface

   INTERFACE

      SUBROUTINE INVERT_FF_MAT ( CALLING_SUBR, MAT_A_NAME, A, NROWS, INFO )


      USE PENTIUM_II_KIND, ONLY       :  DOUBLE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  INVERT_FF_MAT_BEGEND
      USE LAPACK_SYM_MAT_INV

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN )   :: CALLING_SUBR      ! Name of subr that called this subr
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_A_NAME        ! Name of input matrix to be inverted

      INTEGER(LONG)   , INTENT(IN)    :: NROWS             ! Row/col size of input matrix A
      INTEGER(LONG)   , INTENT(OUT)   :: INFO              ! Output from LAPACK routines to do factorization of Lapack band matrix
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = INVERT_FF_MAT_BEGEND

      REAL(DOUBLE)    , INTENT(INOUT) :: A(NROWS,NROWS)    ! Matrix to invert. Inverted matrix returned in A

      END SUBROUTINE INVERT_FF_MAT

   END INTERFACE

   END MODULE INVERT_FF_MAT_Interface

