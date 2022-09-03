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

      SUBROUTINE COND_NUM ( MATIN_NAME, N, KD, K_INORM, MATIN_FAC, RCOND )

! Calculatess the reciprocal of the LAPACK cond number, RCOND, of a square matrix stored in LAPACK band form.
! Uses the triangular factor of the matrix, which is called MATIN_FAC.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  ITMAX
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  COND_NUM_BEGEND
      USE LAPACK_LIN_EQN_DPB

! Interface module not needed for subr DPBCON. This is "CONTAIN'ed" in module LAPACK_LIN_EQN_DPB, which is "USE'd" above

      USE COND_NUM_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'COND_NUM'
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME        ! Name of the matrix whose triang factor is input to this subr
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: CALLED_SUBR = ' ' ! Name of a called subr (for output error purposes)
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO      = 'U'   ! Indicates if matrix MATIN_FAC is an upper triangular factor

      INTEGER(LONG), INTENT(IN)       :: N                 ! No. cols in array MATIN_FAC
      INTEGER(LONG), INTENT(IN)       :: KD                ! No. of superdiagonals of KAA
      INTEGER(LONG)                   :: IWORK(N)          ! Workspace array
      INTEGER(LONG)                   :: INFO      = 0     ! Output from subr DPBCON, which calc's RCOND 
                                                           ! = 0:  successful exit
                                                           ! < 0:  if INFO = -i, the i-th arg had an illegal value

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = COND_NUM_BEGEND

      REAL(DOUBLE),  INTENT(IN)       :: K_INORM           ! The infinity-norm of the matrix whose name is MATIN_NAME     
      REAL(DOUBLE),  INTENT(IN)       :: MATIN_FAC(KD+1,N) ! The upper triangular factor of the matrix whose name is MATIN_NAME
      REAL(DOUBLE),  INTENT(OUT)      :: RCOND             ! The recip of the condition number of matrix whose name is MATIN_NAME
      REAL(DOUBLE)                    :: WORK(3*N)         ! Workspace array     

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      RCOND = ZERO

! Calculate recriprocal of the condition number of the stiffness matrix. Note: LAPACK subr XERBLA wrote message on illegal argument
! in a call to a LAPACK subr.

!xx   WRITE(SC1, * )
      CALL DPBCON( UPLO, N, KD, MATIN_FAC, KD+1, K_INORM, RCOND, WORK, IWORK, INFO, ITMAX, 'Y' )

      CALLED_SUBR = 'DPBCON  '
      IF      (INFO < 0) THEN                              ! LAPACK subr XERBLA should have reported error on an illegal argument
!                                                            in a call to a LAPACK subr, so we should not have gotten here
         WRITE(ERR,993) SUBR_NAME, CALLED_SUBR
         WRITE(F06,993) SUBR_NAME, CALLED_SUBR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 3501 FORMAT(' *INFORMATION: RECIPROCAL OF THE CONDITION NUMBER OF THE ',A,' MATRIX IS RCOND   = ',1ES13.6,                        &
                           ' Used for LAPACK error estimate, below',/)

  993 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' LAPACK SUBR XERBLA SHOULD HAVE REPORTED AN ERROR ON AN ILLEGAL ARGUMENT IN A CALL TO LAPACK SUBR '    &
                    ,/,15X,A,' (OR A SUBR CALLED BY IT) AND THEN ABORTED')

! **********************************************************************************************************************************

      END SUBROUTINE COND_NUM 