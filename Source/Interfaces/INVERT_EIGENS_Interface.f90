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

   MODULE INVERT_EIGENS_Interface

   INTERFACE

       SUBROUTINE INVERT_EIGENS ( MLAM, N, W, Z, EIG_NUM )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  INVERT_EIGENS_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE
      USE MACHINE_PARAMS, ONLY        :  MACH_SFMIN, MACH_LARGE_NUM
      USE MODEL_STUF, ONLY            :  EIG_SIGMA
      USE LAPACK_BLAS_AUX
 
      IMPLICIT NONE
  
      INTEGER(LONG), INTENT(IN)       :: MLAM              ! Number of eigenvalues.
      INTEGER(LONG), INTENT(IN)       :: N                 ! Size of eigenvectors.
      INTEGER(LONG), INTENT(INOUT)    :: EIG_NUM(MLAM)     ! Eigenvector numbers.
      INTEGER(LONG)                   :: PM,QM             ! Indices used in reording the W and Z
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = INVERT_EIGENS_BEGEND

      REAL(DOUBLE) , INTENT(INOUT)    :: W(MLAM)           ! Eigenvalues
      REAL(DOUBLE) , INTENT(INOUT)    :: Z(N,NVEC)         ! Eigenvectors

      END SUBROUTINE INVERT_EIGENS

   END INTERFACE

   END MODULE INVERT_EIGENS_Interface

