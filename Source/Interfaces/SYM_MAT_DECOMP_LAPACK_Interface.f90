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

   MODULE SYM_MAT_DECOMP_LAPACK_Interface

   INTERFACE

      SUBROUTINE SYM_MAT_DECOMP_LAPACK ( CALLING_SUBR, MATIN_NAME, MATIN_SET, NROWS, NTERMS, I_MATIN, J_MATIN, MATIN, PRT_ERRS,    &

                                         MATIN_DIAG_RAT, EQUIL_MATIN, CALC_COND_NUM, DEB_PRT, EQUED, MATIN_SDIA, K_INORM, RCOND,   &
                                         EQUIL_SCALE_FACS, INFO )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FACTORED_MATRIX, FATAL_ERR, LINKNO
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, STIME, TSEC       
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONEPP6
      USE PARAMS, ONLY                :  BAILOUT, EPSIL, MAXRATIO, SUPINFO
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND, LAPACK_S
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG, NDEBUG
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  
      USE LAPACK_LIN_EQN_DPB
      USE SUBR_BEGEND_LEVELS, ONLY    :  SYM_MAT_DECOMP_LAPACK_BEGEND

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      CHARACTER(LEN=*) , INTENT(IN)   :: CALC_COND_NUM     ! If "Y" calc RCOND (reciprocal of condition number of MATIN)
      CHARACTER(LEN=*) , INTENT(IN)   :: CALLING_SUBR      ! The subr that called this subr (used for output error purposes)
      CHARACTER(LEN=*) , INTENT(IN)   :: EQUIL_MATIN       ! If "Y" attempt to equilibrate MATIN (if it needs it)
      CHARACTER(1*BYTE), INTENT(OUT)  :: EQUED             ! 'Y' if MATIN was equilibrated in subr EQUILIBRATE (called herein)   
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_DIAG_RAT    ! If "Y" calculate max ratio of matrix diagonal to factor diagonal
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_NAME        ! Name of matrix to be decomposed
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_SET         ! Set designator for the input matrix. If it corresponds to a MYSTRAN
      CHARACTER(LEN=*) , INTENT(IN)   :: PRT_ERRS          ! If not 'N', print singularity errors

      CHARACTER( 1*BYTE), PARAMETER   :: INORM    = 'I'    ! Indicates to calculate the infinity norm via LAPACK function DLANSB
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO     = 'U'    ! Indicates upper triang part of matrix is stored
 
      INTEGER(LONG), INTENT(IN)       :: DEB_PRT(2)        ! Debug numbers to say whether to write ABAND and/or its decomp to file
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzeros in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)  ! Indicators of number of nonzero terms in rows of matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERMS)   ! Col numberts of nonzero terms in matrix MATIN

      INTEGER(LONG), INTENT(INOUT)    :: INFO              ! Output from LAPACK routine to do factorization of ABAND

      INTEGER(LONG), INTENT(OUT)      :: MATIN_SDIA        ! No. of superdiags in the MATIN upper triangle
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SYM_MAT_DECOMP_LAPACK_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERMS)     ! A small number to compare real zero
      REAL(DOUBLE) , INTENT(OUT)      :: RCOND             ! Recrip of cond no. of MATIN. Determined in  subr COND_NUM
      REAL(DOUBLE) , INTENT(OUT)      :: K_INORM           ! Inf norm of MATIN matrix (det in  subr COND_NUM)

      REAL(DOUBLE) , INTENT(OUT)      :: EQUIL_SCALE_FACS(NROWS)

      END SUBROUTINE SYM_MAT_DECOMP_LAPACK

   END INTERFACE

   END MODULE SYM_MAT_DECOMP_LAPACK_Interface

