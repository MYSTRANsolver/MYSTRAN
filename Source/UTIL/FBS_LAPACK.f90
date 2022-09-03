! ##################################################################################################################################
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
 
      SUBROUTINE FBS_LAPACK ( EQUED, NROWS, MATIN_SDIA, EQUIL_SCALE_FACS, INOUT_COL )

! FBS_LAPACK performs the forward-backward substitution to get displacements after the stiffness matrix has been decomposed using
! subr SYM_MAT_DECOMP_LAPACK

! (1) Scales the INOUT (e.g. load) vector if requested
! (2) Does the forward-backward solution to solve for a left-hand side vector given 1 right-hand side input vector (INOUT_COL)
!     The reult is returned in INOUT_COL
! (3) Scales the solution vector if requested

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LINKNO
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, STIME, TSEC       
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSERR, RCONDK
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND, LAPACK_S, RES
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG, NDEBUG
      USE MACHINE_PARAMS, ONLY        :  MACH_EPS, MACH_SFMIN
      USE LAPACK_LIN_EQN_DPB
      USE SUBR_BEGEND_LEVELS, ONLY    :  FBS_LAPACK_BEGEND

      USE SYM_MAT_DECOMP_LAPACK_USE_IFs
                      
      USE FBS_LAPACK_USE_IFs                               ! Added 2019/07/14

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FBS_LAPACK'
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: CALLED_SUBR = ' ' ! Name of a called subr (for output error purposes)
      CHARACTER(  1*BYTE), INTENT(IN) :: EQUED             ! 'Y' if MATIN was equilibrated in subr EQUILIBRATE (called herein)   
                                                           !       and the factorization (in DPBTRS) could not be completed.
      CHARACTER(  1*BYTE), PARAMETER  :: UPLO        = 'U' ! Indicates upper triang part of matrix is stored


      INTEGER(LONG), INTENT(IN)       :: MATIN_SDIA        ! No. of superdiags in the MATIN upper triangle
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in sparse matrix MATIN
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: INFO        = 0   ! Output from LAPACK routine to do factorization of ABAND
                                                           !   0:  successful exit
                                                           ! < 0:  if INFO = -i, the i-th argument had an illegal value
                                                           ! > 0:  if INFO = i, the leading minor of order i is not pos def
                                                           !       and the factorization (in DPBTRS) could not be completed.
      INTEGER(LONG), PARAMETER        :: NUM_COLS    = 1   ! Number of vectors to solve in this call
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FBS_LAPACK_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: EQUIL_SCALE_FACS(NROWS)
                                                           ! LAPACK_S values to return to calling subr

      REAL(DOUBLE) , INTENT(INOUT)    :: INOUT_COL(NROWS)    ! INOUT input  vector

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Scale the INOUT vector if requested

      IF (EQUED == 'Y') THEN
         DO I=1,NROWS
            INOUT_COL(I) = EQUIL_SCALE_FACS(I)*INOUT_COL(I)
         ENDDO 
      ENDIF

! Calculate the answer via forward/backward substitution. Subr DPBTRS returns the answer in the workspace (INOUT_COL)
! for the right-hand side (which, at entry, was INOUT_COL)

      CALL DPBTRS ( UPLO, NROWS, MATIN_SDIA, NUM_COLS, ABAND, MATIN_SDIA+1, INOUT_COL, NROWS, INFO, 'N' )

      CALLED_SUBR = 'DPBTRS'      
      IF (INFO < 0) THEN                                   ! LAPACK subr XERBLA should have reported error on an illegal argument
         WRITE(ERR,993) SUBR_NAME, CALLED_SUBR             ! in calling a LAPACK subr, so we should not have gotten here
         WRITE(F06,993) SUBR_NAME, CALLED_SUBR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Scale the solution vector if requested (now overwritten in INOUT_COL)

      IF (EQUED == 'Y') THEN
         DO I=1,NROWS
            INOUT_COL(I) = EQUIL_SCALE_FACS(I)*INOUT_COL(I)
         ENDDO 
      ENDIF

!***********************************************************************************************************************************
  993 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' LAPACK SUBR XERBLA SHOULD HAVE REPORTED AN ERROR ON AN ILLEGAL ARGUMENT IN A CALL TO LAPACK SUBR '    &
                    ,/,15X,A,' (OR A SUBR CALLED BY IT) AND THEN ABORTED')

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

!***********************************************************************************************************************************

      END SUBROUTINE FBS_LAPACK
