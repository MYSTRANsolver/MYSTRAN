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

      SUBROUTINE GPWG_PMOI (MOI1, Q, INFO )
 
! Jacobi solution for 3x3 eigenvalue problem used in finding principal moments of inertia for the Grid Point Weight Generator (GPWG)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  SUPWARN, WTMASS
      USE LAPACK_STD_EIG_1
      USE SUBR_BEGEND_LEVELS, ONLY    :  GPWG_BEGEND
 
      USE GPWG_PMOI_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GPWG_PMOI'
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: CALLED_SUBR = ' ' ! Name of a called subr (for output error purposes)
      CHARACTER( 1*BYTE), PARAMETER   :: JOBZ      = 'V'   ! Indicates to solve for eigenvalues and vectors in LAPACK subr DSYEV
      CHARACTER( 1*BYTE)              :: TRANSA            ! Transpose indicator for subr DGEMM
      CHARACTER( 1*BYTE)              :: TRANSB            ! Transpose indicator for subr DGEMM
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO      = 'U'   ! Indicates array KBAND is the upper triangular part of KAA

      INTEGER(LONG), INTENT(OUT)      :: INFO              ! = 0:  successful exit
!                                                            < 0:  if INFO = -i, the i-th argument had an illegal value
!                                                            > 0:  if INFO = i, the algorithm failed to converge; i off_diag
!                                                            elems of an intermediate tridiag form did not converge to zero
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: N         = 3     ! Order of matrix MOI1
      INTEGER(LONG), PARAMETER        :: LWORK     = 3*N-1 ! Size of array WORK
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GPWG_BEGEND + 1

      REAL(DOUBLE) , INTENT(INOUT)    :: MOI1(3,3)         ! On entry, the MOI's about c.g. in basic coords
!                                                            On exit , the principal MOI's in basic coords (if INFO = 0)
      REAL(DOUBLE) , INTENT(OUT)      :: Q(3,3)            ! Transformation from basic to principal directions
!                                                            Q = Z'. That is;
!                                                            U(principal) = Q*U(basic), where U is a displ vector 
      REAL(DOUBLE)                    :: Z(3,3)            ! When subr DSYEV is called, Z = input MOI1
!                                                            When subr DSYEV returns, Z = eigenvecs: PMOI = Z'*MOI1*Z
      REAL(DOUBLE)                    :: DUM(N,N)          ! Intermediate result in calculating Z'*MOI1*Z 
      REAL(DOUBLE) , PARAMETER        :: ALPHA     = ONE   ! Scalar multiplier for subr DGEMM
      REAL(DOUBLE) , PARAMETER        :: BETA      = ZERO  ! Scalar multiplier for subr DGEMM
      REAL(DOUBLE)                    :: PMOI(3)           ! Principal MOI's in a 1-D array
      REAL(DOUBLE)                    :: WORK(LWORK)       ! Workspace
 
      EXTERNAL                        :: DGEMM

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ! Initialize outputs
      INFO = 0

      DO I=1,N
         DO J=1,N
            Q(I,J) = ZERO
         ENDDO
      ENDDO

      ! Use LAPACK driver DSYEV to get all eigenvalues (the principal MOI's) and eigenvectors (returned in Z)
      DO I=1,N
         DO J=1,N
            Z(I,J) = MOI1(I,J)
            Q(I,J) = ZERO
         ENDDO
      ENDDO
 
      CALL DSYEV ( JOBZ, UPLO, N, Z, N, PMOI, WORK, LWORK, INFO )

      ! Set MOI1 matrix to zero for off-diag terms and to PMOI for diag terms.
      ! Set Q = Z' (' = transpose)

      CALLED_SUBR = 'DSTEQR'
      IF      (INFO < 0) THEN
         ! LAPACK subr XERBLA should have reported error on an illegal argument
         ! in a called LAPACK subr, so we should not have gotten here
         WRITE(ERR,993) SUBR_NAME, CALLED_SUBR
         WRITE(F06,993) SUBR_NAME, CALLED_SUBR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ELSE IF (INFO > 0) THEN
        ! No convergence in subr DSYEV
        WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1001) CALLED_SUBR, SUBR_NAME
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1001) CALLED_SUBR, SUBR_NAME
         ENDIF
         RETURN

      ELSE
         ! INFO=0, so no error
         
         ! Set Q = Z'
         DO I=1,N
            DO J=1,N
               Q(J,I) = Z(I,J)
            ENDDO
         ENDDO

         ! MOI1 <-- Z'*MOI1*Z, diags should be PMOI's, off-diags should be zero
         TRANSA = 'N'
         TRANSB = 'N'
         CALL DGEMM ( TRANSA, TRANSB, N, N, N, ALPHA, MOI1, N, Z,   N, BETA, DUM,  N )
         TRANSA = 'T'
         TRANSB = 'N'
         CALL DGEMM ( TRANSA, TRANSB, N, N, N, ALPHA, Z,    N, DUM, N, BETA, MOI1, N )

      ENDIF  
      

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  993 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' LAPACK SUBR XERBLA SHOULD HAVE REPORTED AN ERROR ON AN ILLEGAL ARGUMENT IN A CALL TO LAPACK SUBR '    &
                    ,/,15X,A,' (OR A SUBR CALLED BY IT) AND THEN ABORTED')

 1001 FORMAT(' *WARNING    : LAPACK SUBR ',A8,' CALLED BY SUBROUTINE ',A                                                           &
                    ,/,14X,' CANNOT CONVERGE IN ATTEMPTING TO FIND PRINCIPAL MOIs'                                                 &
                    ,/,14X,' THE ALGORITHM HAS FAILED TO FIND ALL THE EIGENVALUES (PRINCIPAL MOIs) IN 90 ITERATIONS'               &
                    ,/,14X,' PRINCIPAL MOIs AND PRINCIPAL DIRECTIONS CANNOT BE FOUND')
 
! **********************************************************************************************************************************

      END SUBROUTINE GPWG_PMOI
