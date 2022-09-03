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
 
      SUBROUTINE KGG_SINGULARITY_PROC ( AGRID, KGRD, NUM_ASPC_BY_COMP )
 
! Grid point singularity processor. The algorithm is based on input matrix KGRD that is the 6x6 matrix from the diagonal of KGG for
! one grid point. The 2 3x3 diagonal partitions from KGRD are checked to see if there are any singularities based on: 

!     1) Get eigenvalues and eigenvectors of each of the 3x3 matrices (one for translation and 1 for rotation). Calc the ratios of
!        the 3 eigenvales to the max value (among the 3) and, if the ratio is less than AUTOSPC_RAT, mark the DOF for AUTOSPC.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, SPC
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFSA, NGRID, NUM_PCHD_SPC1
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_INFO, AUTOSPC_RAT, EPSIL, PCHSPC1, SPC1SID, SUPINFO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  KGG_SINGULARITY_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START, TDOFI, TSET
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  GRID_ID

      USE KGG_SINGULARITY_PROC_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'KGG_SINGULARITY_PROC'
      CHARACTER( 1*BYTE)              :: AUTOSPC_SOME_COMP  ! 'Y'/'N' indicator if some component of a grid is to be AUTOSPC'd
      CHARACTER( 1*BYTE)              :: CONSTR_COMP(6)     ! If DOF i is in the S or M sets, CONSTR_COMP(i) = 'Y'. Otherwise 'N'
      CHARACTER( 1*BYTE)              :: SINGLR_COMP(6)     ! If DOF i is singular, SINGLR_COMP(i) = 'Y'. Otherwise 'N'
      CHARACTER( 6*BYTE)              :: SINGLR_COMP_CHAR   ! Array of indicators of whether a displ comp in KGRD is singular. If
!                                                             DOF's 2, 4 and 5 are each singular then SINGLR_COMP_CHAR = ' 2 45 '
      CHARACTER( 1*BYTE)              :: WRITE_HEADER       ! 'Y'/'N' indicator for writing header to F06 for debug output

      INTEGER(LONG), INTENT(IN)       :: AGRID              ! Actual grid ID for IGRID
      INTEGER(LONG), INTENT(INOUT)    :: NUM_ASPC_BY_COMP(6)! The number of DOF's AUTOSPC'd for each displ component
      INTEGER(LONG)                   :: EIGENVAL_NUM(6)    ! Array to hold the eigenvalue number used in finding a SINGLR_COMP
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM    ! Row number in array GRID_ID where AGRID is found
      INTEGER(LONG)                   :: IDOF               ! Internal DOF number         
      INTEGER(LONG)                   :: I,J,K              ! DO loop indices
      INTEGER(LONG)                   :: IGRID              ! Internal grid ID
      INTEGER(LONG)                   :: INFO               ! See subr K33_EIGENS (CONTAIN'ed herein)
      INTEGER(LONG)                   :: I2,J2              ! Index into an array
      INTEGER(LONG)                   :: NUM_COMPS          ! 6 if a physical grid,  if a scalar point (SPOINT)
      INTEGER(LONG)                   :: M_SET_COL          ! Col no. in array TDOF where the  M-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: S_SET_COL          ! Col no. in array TDOF where the  S-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: O_SET_COL          ! Col no. in array TDOF where the  O-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: R_SET_COL          ! Col no. in array TDOF where the  R-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: ROW_NUM_START      ! DOF number where TDOF data begins for a grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = KGG_SINGULARITY_PROC_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: KGRD(6,6)          ! 6x6 diagonal stiffness matrix for grid point AGRID
      REAL(DOUBLE)                    :: FAC                ! Multipling factor used in an intermediate calc
      REAL(DOUBLE)                    :: EPS1               ! Small value used in comparison to determine a real zero
      REAL(DOUBLE)                    :: K33(3,3)           ! Partition of the 6x6 stiff matrix for rotation    DOF's for a grid
      REAL(DOUBLE)                    :: K33_LAMBDAS(3)     ! Eigenvalues  of K33_ROT
      REAL(DOUBLE)                    :: K33_LAMBDA_MAX     ! Max value from  K33_ROT K33_LAMBDAS(i)
      REAL(DOUBLE)                    :: K33_VECS(3,3)      ! Eigenvectors of K33_ROT
      REAL(DOUBLE)                    :: K33_VEC_MAX        ! Max value from one eigenvector (column) of K33_VECS

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

      SINGLR_COMP_CHAR(1:6) = ' '
      DO I=1,6
         SINGLR_COMP(I) = 'N'
         CONSTR_COMP(I) = 'N'
         EIGENVAL_NUM(I) = 0
      ENDDO

      CALL TDOF_COL_NUM ( 'M ',  M_SET_COL )
      CALL TDOF_COL_NUM ( 'S ',  S_SET_COL )
      CALL TDOF_COL_NUM ( 'O ',  O_SET_COL )
      CALL TDOF_COL_NUM ( 'R ',  R_SET_COL )

      WRITE_HEADER = 'Y'

! Check individual DOF's for singularities, and set SINGLR_COMP and CONSTR_COMP for this grid

!xx   CALL CALC_TDOF_ROW_NUM  ( AGRID, ROW_NUM_START, 'N' )! Det where in TDOF (not TDOFI) the DOF data begins for AGRID
      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, IGRID )
      ROW_NUM_START = TDOF_ROW_START(IGRID)
      CALL GET_GRID_NUM_COMPS ( AGRID, NUM_COMPS, 'N' )
comps:IF (NUM_COMPS == 6) THEN                             ! Physical grid with 6 components represented in KGRD

comps6:  DO K=1,2                                          ! K=1 is for translational DOF's and K=2 is for rotational DOF's

            DO I=1,3                                       ! K33 is the 3x3 partition of KGRD for translation or rotation
               I2 = I + 3*(K - 1)
               DO J=1,3
                  J2 = J + 3*(K - 1)
                  K33(I,J) = KGRD(I2,J2)
               ENDDO
            ENDDO
                                                           ! Get the 3 eigenvalues and eigenvectors of K33
            CALL K33_EIGENS ( K33, K33_LAMBDAS, K33_VECS, INFO )

eigs_ok:    IF (INFO == 0) THEN                            ! K33_EIGENS returned with no error from LAPACK routine

               K33_LAMBDA_MAX = ZERO                       ! Find max eigenvalue (max K33_LAMBDA_MAX)
               DO I=1,3
                  IF (DABS(K33_LAMBDAS(I)) > DABS(K33_LAMBDA_MAX)) THEN
                     K33_LAMBDA_MAX = K33_LAMBDAS(I)
                  ENDIF
               ENDDO

               IF (DABS(K33_LAMBDA_MAX) > EPS1) THEN       ! If max eigenvalue > 0, use it to  normalize all eigenvalues
                  FAC = ONE/K33_LAMBDA_MAX
               ELSE                                        ! If max eigenvalue = 0, use 1.0 to normalize all eigenvalues
                  FAC = ONE
               ENDIF

               DO J=1,3                                    ! For the J-th eigenvalue (1 to 3) get any comp that needs to be SPC'd
                  IF(DABS(FAC*K33_LAMBDAS(J))<=AUTOSPC_RAT) THEN
                     K33_VEC_MAX = ZERO
                     I2 = 0
                     DO I=1,3                              ! Scan the J-th eigenvector for comp with largest absolute value
                        IF (DABS(K33_VECS(I,J)) > K33_VEC_MAX) THEN
                           K33_VEC_MAX = DABS(K33_VECS(I,J))
                           I2 = I + 3*(K - 1)              ! After this loop, I2 will be the comp number (1-6) where the eigenvector
                        ENDIF                              ! is the max absolute value
                     ENDDO
                     SINGLR_COMP(I2) = 'Y'
                     EIGENVAL_NUM(I2) = J
                     CALL CONVERT_INT_TO_CHAR ( I2, SINGLR_COMP_CHAR(I2:I2) )
                  ENDIF
               ENDDO

               DO I=1,3                                    ! Check to see if any of the 3 comps (for K=1,2) are in M, S, O or R set
                  I2 = I + 3*(K - 1)                       ! (i.e., do not AUTOSPC DOF's that are members of the M, S, O or R-sets)
                  IDOF = ROW_NUM_START + I2 - 1
                  IF ((TDOF(IDOF,M_SET_COL) /= 0) .OR. (TDOF(IDOF,S_SET_COL) /= 0) .OR.                                            &
                      (TDOF(IDOF,O_SET_COL) /= 0) .OR. (TDOF(IDOF,R_SET_COL) /= 0)) THEN
                     CONSTR_COMP(I2) = 'Y'                 ! If there are, set CONSTR_COMP to 'Y'
                  ENDIF
               ENDDO

               AUTOSPC_SOME_COMP = 'N'
               DO I=1,6
                  IF ((SINGLR_COMP(I) == 'Y') .AND. (CONSTR_COMP(I) == 'N')) THEN
                     AUTOSPC_SOME_COMP = 'Y'
                  ENDIF
               ENDDO

deb_17:        IF (DEBUG(17) > 0) THEN

                  IF ((AUTOSPC_SOME_COMP == 'Y') .OR. (DEBUG(17) > 1)) THEN

                     IF (WRITE_HEADER == 'Y') THEN
                        CALL KGG_SING_PROC_DEBUG ( 1 )
                        WRITE_HEADER = 'N'
                     ENDIF

                     CALL KGG_SING_PROC_DEBUG ( 2 )

                     DO J=1,3
                        I2 = J + 3*(K - 1)
                        IF (SINGLR_COMP(I2) == 'Y') THEN
                           CALL KGG_SING_PROC_DEBUG ( 3 )
                        ENDIF
                     ENDDO

                     WRITE(F06,*)

                  ENDIF

               ENDIF deb_17

            ENDIF eigs_ok

         ENDDO comps6

      ELSE                                                 ! Scalar point - only need to check KGRD(1,1)

         IDOF = ROW_NUM_START
         IF ((TDOF(IDOF,M_SET_COL) /= 0) .OR. (TDOF(IDOF,S_SET_COL) /= 0) .OR.                                                     &
             (TDOF(IDOF,O_SET_COL) /= 0) .OR. (TDOF(IDOF,R_SET_COL) /= 0)) THEN
            CONSTR_COMP(1) = 'Y'                           ! If this is M, S, O, R set, set CONSTR_COMP to 'Y'
         ENDIF

         IF (KGRD(1,1) < EPS1) THEN
            SINGLR_COMP(1) = 'Y'
            CALL CONVERT_INT_TO_CHAR ( 1, SINGLR_COMP_CHAR(1:1) )
         ENDIF

         AUTOSPC_SOME_COMP = 'N'
         DO I=1,6
            IF ((SINGLR_COMP(I) == 'Y') .AND. (CONSTR_COMP(I) == 'N')) THEN
               AUTOSPC_SOME_COMP = 'Y'
            ENDIF
         ENDDO

      ENDIF comps


      DO I=1,6                                             ! Change SINGLR_COMP_CHAR if a component belongs to S or M set
         IF (CONSTR_COMP(I) == 'Y') THEN
            SINGLR_COMP_CHAR(I:I) = ' '
         ENDIF
      ENDDO 

      DO I=1,6                                             ! Reset table TSET for the DOF's AUTOSPC'd and increment NUM_ASPC_BY_COMP
         IF (SINGLR_COMP_CHAR(I:I) /= ' ') THEN
            IF (AUTOSPC == 'Y') THEN
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
               TSET ( GRID_ID_ROW_NUM, I ) = 'SA'
               NDOFSA = NDOFSA + 1
               NUM_ASPC_BY_COMP(I) = NUM_ASPC_BY_COMP(I) + 1               
            ENDIF
         ENDIF
      ENDDO

      IF (AUTOSPC_INFO == 'Y') THEN                        ! Write singularity messages to output file if requested
         IF (SINGLR_COMP_CHAR /= '      ') THEN
            IF (AUTOSPC == 'Y') THEN
               WRITE(ERR,101) AGRID,(SINGLR_COMP_CHAR(I:I),I=1,6),AUTOSPC
               IF (SUPINFO == 'N') THEN
                  WRITE(F06,101) AGRID,(SINGLR_COMP_CHAR(I:I),I=1,6),AUTOSPC
               ENDIF
            ELSE
               WRITE(ERR,102) AGRID,(SINGLR_COMP_CHAR(I:I),I=1,6)
               IF (SUPINFO == 'N') THEN
                  WRITE(F06,102) AGRID,(SINGLR_COMP_CHAR(I:I),I=1,6)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 
      DO I=1,6                                             ! If requested, write SPC1 card images to text file for singulaqr DOF's
         IF (SINGLR_COMP_CHAR(I:I) /= ' ') THEN
            IF (PCHSPC1 == 'Y') THEN
               WRITE(SPC,109) SPC1SID, I, AGRID
               NUM_PCHD_SPC1 = NUM_PCHD_SPC1 + 1
            ENDIF
         ENDIF
      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *INFORMATION: GRID POINT ',I8,' HAS SINGULARITY FOR DISPL COMPONENT(S) ',6A1,'. SINCE PARAM AUTOSPC = ',A,          &
                          ', THESE WILL BE AUTOSPC''d')

  102 FORMAT(' *INFORMATION: GRID POINT ',I8,' HAS SINGULARITY FOR DISPL COMPONENT(S) ',6A1)

  109 FORMAT('SPC1    ',3I8)


! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################
 
      SUBROUTINE K33_EIGENS (K33, K33_LAMBDAS, K33_VECS, INFO )
 
! Jacobi solution for 3x3 eigenvalue problem used in finding the eigenvalues of a 3x3 diag partition of a 6x6 grid stiffness matrix
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE CONSTANTS_1, ONLY           :  ZERO
      USE LAPACK_STD_EIG_1
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: CALLED_SUBR = ' ' ! Name of a called subr (for output error purposes)
      CHARACTER( 1*BYTE), PARAMETER   :: JOBZ      = 'V'   ! Indicates to solve for eigenvalues and vectors in LAPACK subr DSYEV
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO      = 'U'   ! Indicates array A is the upper triangular part of K33

      INTEGER(LONG), INTENT(OUT)      :: INFO              ! = 0:  successful exit from subr DSYEV
!                                                            < 0:  if INFO = -i, the i-th argument had an illegal value
!                                                            > 0:  if INFO = i, the algorithm failed to converge; i off_diag
!                                                            elems of an intermediate tridiag form did not converge to zero
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: N         = 3     ! Order of matrix K33
      INTEGER(LONG), PARAMETER        :: LWORK     = 3*N-1 ! Size of array WORK

      REAL(DOUBLE) , INTENT(IN)       :: K33(N,N)          ! A 3x3 diag partition of the 6x6 stiff matrix for a grid
      REAL(DOUBLE) , INTENT(OUT)      :: K33_LAMBDAS(N)    ! The eigenvalues of input K33 (if INFO = 0)
      REAL(DOUBLE) , INTENT(OUT)      :: K33_VECS(N,N)     ! Prior to entry to DSYEV, K33_VECS is set = K33.
!                                                            On exit, K33_VECS contains the eigenvectors of K33
      REAL(DOUBLE)                    :: WORK(LWORK)       ! Workspace for subr DSYEV
 
! **********************************************************************************************************************************
! Initialize outputs

      INFO = 0

      DO I=1,N
         K33_LAMBDAS(I) = ZERO
      ENDDO

      DO I=1,N
         DO J=1,N
            K33_VECS(I,J) = ZERO
         ENDDO
      ENDDO

! Use LAPACK driver DSYEV to get all eigenvalues of K33

      DO I=1,N
         DO J=1,N
            K33_VECS(I,J) = K33(I,J)
         ENDDO
      ENDDO
 
      CALL DSYEV ( JOBZ, UPLO, N, K33_VECS, N, K33_LAMBDAS, WORK, LWORK, INFO )
      CALLED_SUBR = 'DSYEV'      

      IF      (INFO < 0) THEN                              ! LAPACK subr XERBLA should have reported error on an illegal argument
                                                           ! in a call to a LAPACK subr, which would only occur if the call to DSYEV
         WRITE(ERR,993) SUBR_NAME, CALLED_SUBR             ! was incorrect.
         WRITE(F06,993) SUBR_NAME, CALLED_SUBR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ELSE IF (INFO > 0) THEN                              ! No convergence in subr DSYEV. Return INFO > 0 for calling routine to
                                                           ! deal with
         WRITE(ERR,1612) CALLED_SUBR, SUBR_NAME
         WRITE(F06,1612) CALLED_SUBR, SUBR_NAME
         FATAL_ERR = FATAL_ERR + 1

      ENDIF

      RETURN

! **********************************************************************************************************************************
  993 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' LAPACK SUBR XERBLA SHOULD HAVE REPORTED AN ERROR ON AN ILLEGAL ARGUMENT IN A CALL TO LAPACK SUBR '    &
                    ,/,15X,A,' (OR A SUBR CALLED BY IT) AND THEN ABORTED')

 1612 FORMAT(' *ERROR  1612: LAPACK DRIVER ',A8,' CALLED BY SUBROUTINE ',A                                                         &
                    ,/,14X,' CANNOT CONVERGE IN ATTEMPTING TO FIND K33 EIGENVALUES AND EIGENVECTORS'                               &
                    ,/,14X,' THE ALGORITHM HAS FAILED TO FIND ALL THE EIGENVALUES (PRINCIPAL MOIs) IN 90 ITERATIONS')
 
! **********************************************************************************************************************************

      END SUBROUTINE K33_EIGENS

! ##################################################################################################################################
 
      SUBROUTINE KGG_SING_PROC_DEBUG ( WHAT )
 
! Debug output for KGG singularity calcs
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE LAPACK_STD_EIG_1
 
      IMPLICIT NONE
 
      INTEGER(LONG)                   :: COMP_NUM          ! Displ component number (1 - 6)
      INTEGER(LONG)                   :: II,JJ             ! DO loop indices
      INTEGER(LONG)                   :: WHAT              ! What to print out

! **********************************************************************************************************************************
      IF     ( WHAT == 1) THEN
      
         WRITE(F06,201) NDOFSA+1, AGRID                    ! Use NDOFSA+1 since NDOFSA is not incremented until later
         
      ELSE IF (WHAT == 2) THEN
      
         IF      (K == 1) THEN
            WRITE(F06,202)
            WRITE(F06,204)
         ELSE IF (K == 2) THEN
            WRITE(F06,203)
            WRITE(F06,204)
         ENDIF
         
         DO II=1,3
            COMP_NUM = II + 3*(K - 1)
            WRITE(F06,205) COMP_NUM,(K33(II,JJ),JJ=1,3),II,K33_LAMBDAS(II),(K33_VECS(II,JJ),JJ=1,3)
         ENDDO
         WRITE(F06,*)
         
      ELSE IF (WHAT == 3) THEN

         WRITE(F06,101) I2, EIGENVAL_NUM(I2), I2, EIGENVAL_NUM(I2), I2

      ENDIF

! **********************************************************************************************************************************
  101 FORMAT(' AUTOSPC comp',I2,' since (eigenvalue',I2,')/max eigenvalue < AUTOSPC_RAT (comp',I2,' is AUTOSPC''d since eigenvec', &
             I2,' is max abs for comp',I2,')')

  201 FORMAT(' ******************************************************************************************************************',&
             '*****************'                                                                                                   &
          ,/,I8, ')', ' Results from KGG_SINGULARITY_PROC for grid ',I8                                                            &
          ,/,' ------------------------------------------------------------')

  202 FORMAT(50X,'Results for translation 3x3 partitions')

  203 FORMAT(50X,' Results for rotation 3x3 partitions')

  204 FORMAT(4X,'Comp',16X,'G-set stiffness',24X,'Eigenvalues',28X,'Eigenvectors',/,93X,'1             2             3')

  205 FORMAT(5X,I2,3X,3(1ES14.6),8X,I2,1ES14.6,10X,3(1ES14.6))

! **********************************************************************************************************************************

      END SUBROUTINE KGG_SING_PROC_DEBUG

      END SUBROUTINE KGG_SINGULARITY_PROC
