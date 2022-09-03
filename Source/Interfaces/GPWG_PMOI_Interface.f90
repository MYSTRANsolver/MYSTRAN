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

   MODULE GPWG_PMOI_Interface

   INTERFACE

      SUBROUTINE GPWG_PMOI (MOI1, Q, INFO )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  SUPWARN, WTMASS
      USE LAPACK_STD_EIG_1
      USE SUBR_BEGEND_LEVELS, ONLY    :  GPWG_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER( 1*BYTE), PARAMETER   :: JOBZ      = 'V'   ! Indicates to solve for eigenvalues and vectors in LAPACK subr DSYEV
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO      = 'U'   ! Indicates array KBAND is the upper triangular part of KAA

      INTEGER(LONG), INTENT(OUT)      :: INFO              ! = 0:  successful exit
      INTEGER(LONG), PARAMETER        :: N         = 3     ! Order of matrix MOI1
      INTEGER(LONG), PARAMETER        :: LWORK     = 3*N-1 ! Size of array WORK
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GPWG_BEGEND + 1

      REAL(DOUBLE) , INTENT(INOUT)    :: MOI1(3,3)         ! On entry, the MOI's about c.g. in basic coords
      REAL(DOUBLE) , INTENT(OUT)      :: Q(3,3)            ! Transformation from basic to principal directions
      REAL(DOUBLE) , PARAMETER        :: ALPHA     = ONE   ! Scalar multiplier for subr DGEMM
      REAL(DOUBLE) , PARAMETER        :: BETA      = ZERO  ! Scalar multiplier for subr DGEMM
 
      END SUBROUTINE GPWG_PMOI

   END INTERFACE

   END MODULE GPWG_PMOI_Interface

