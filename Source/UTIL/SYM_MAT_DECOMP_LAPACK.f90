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
 
      SUBROUTINE SYM_MAT_DECOMP_LAPACK ( CALLING_SUBR, MATIN_NAME, MATIN_SET, NROWS, NTERMS, I_MATIN, J_MATIN, MATIN, PRT_ERRS,    &
                                         MATIN_DIAG_RAT, EQUIL_MATIN, CALC_COND_NUM, DEB_PRT, EQUED, MATIN_SDIA, K_INORM, RCOND,   &
                                         EQUIL_SCALE_FACS, INFO )

! Decomposes a symmetric band matrix into triangular factors. The input matrix, MATIN, is stored in CRS sparse format and is
! converted, in this subr, to band matrix ABAND (stored in module LAPACK_DPB_MATRICES) needed for the LAPACK routines that do the
! actual work

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

      USE SYM_MAT_DECOMP_LAPACK_USE_IFs
                      
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SYM_MAT_DECOMP_LAPACK'

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: CALLED_SUBR = ' ' ! Name of a called subr (for output error purposes)
      CHARACTER(LEN=*) , INTENT(IN)   :: CALC_COND_NUM     ! If "Y" calc RCOND (reciprocal of condition number of MATIN)
      CHARACTER(LEN=*) , INTENT(IN)   :: CALLING_SUBR      ! The subr that called this subr (used for output error purposes)
      CHARACTER(LEN=*) , INTENT(IN)   :: EQUIL_MATIN       ! If "Y" attempt to equilibrate MATIN (if it needs it)
      CHARACTER(1*BYTE), INTENT(OUT)  :: EQUED             ! 'Y' if MATIN was equilibrated in subr EQUILIBRATE (called herein)   
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_DIAG_RAT    ! If "Y" calculate max ratio of matrix diagonal to factor diagonal
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_NAME        ! Name of matrix to be decomposed
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_SET         ! Set designator for the input matrix. If it corresponds to a MYSTRAN
!                                                            displ set (e.g. 'L ' set) then error messages about singulatities
!                                                            can reference the grid/comp that is singular (otherwise the row/col
!                                                            where the singularity occurs is referenced). If it is not a MYSTRAN
!                                                            set designator it should be blank
      CHARACTER(LEN=*) , INTENT(IN)   :: PRT_ERRS          ! If not 'N', print singularity errors

      CHARACTER( 1*BYTE), PARAMETER   :: INORM    = 'I'    ! Indicates to calculate the infinity norm via LAPACK function DLANSB
      CHARACTER(54*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run
      CHARACTER( 1*BYTE)              :: NONPOS_DEF        ! Indicates matrix was nonpositive definite
      CHARACTER( 1*BYTE)              :: QUIT_ON_POS_INFO  ! Indicator of whether to quit if output value of INFO is found to be > 0
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO     = 'U'    ! Indicates upper triang part of matrix is stored
 
      INTEGER(LONG), INTENT(IN)       :: DEB_PRT(2)        ! Debug numbers to say whether to write ABAND and/or its decomp to file
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzeros in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)  ! Indicators of number of nonzero terms in rows of matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERMS)   ! Col numberts of nonzero terms in matrix MATIN

      INTEGER(LONG), INTENT(INOUT)    :: INFO              ! Output from LAPACK routine to do factorization of ABAND
                                                           !   0:  successful exit
                                                           ! < 0:  if INFO = -i, the i-th argument had an illegal value
                                                           ! > 0:  if INFO = i, the leading minor of order i is not pos def
                                                           !       and the factorization (in DPBTRS) could not be completed.

      INTEGER(LONG), INTENT(OUT)      :: MATIN_SDIA        ! No. of superdiags in the MATIN upper triangle
      INTEGER(LONG)                   :: COMPV             ! Component number (1-6) of a grid DOF
      INTEGER(LONG)                   :: GRIDV             ! Grid number
      INTEGER(LONG)                   :: I                 ! DO loop index             
      INTEGER(LONG)                   :: IIMAX             ! Row/Col in MATIN where max diagonal term occurs
      INTEGER(LONG)                   :: IIMIN             ! Row/Col in MATIN where min diagonal term occurs
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SYM_MAT_DECOMP_LAPACK_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERMS)     ! A small number to compare real zero
      REAL(DOUBLE) , INTENT(OUT)      :: RCOND             ! Recrip of cond no. of MATIN. Determined in  subr COND_NUM
      REAL(DOUBLE) , INTENT(OUT)      :: K_INORM           ! Inf norm of MATIN matrix (det in  subr COND_NUM)
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero

      REAL(DOUBLE) , INTENT(OUT)      :: EQUIL_SCALE_FACS(NROWS)
                                                           ! LAPACK_S values to return to calling subr

      REAL(DOUBLE)                    :: MATIN_DIAG(NROWS) ! Diagonal terms from MATIN matrix
      REAL(DOUBLE)                    :: KRATIO            ! Ratio: MAXKII/MINKII
      REAL(DOUBLE)                    :: MAXKII            ! Maximum diagonal term in MATIN
      REAL(DOUBLE)                    :: MAXIMAX_RATIO     ! Largest of the ratios of matrix diagonal to factor diagonal
      REAL(DOUBLE)                    :: MB_TO_ALLOCATE    ! MB of memory to allocate
      REAL(DOUBLE)                    :: MINKII            ! Minimum diagonal term in MATIN
      REAL(DOUBLE)                    :: FAC_DIAG          ! Diagonal term in the tringular factor of MATIN
!xx   REAL(DOUBLE)                    :: SCOND             ! Ratio of min to max scaling factors, LAPACK_S(i), if MATIN is equil'ed.
      REAL(DOUBLE)                    :: RATIO             ! Ratio of matrix diagonal to factor diagonal
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Deallocate ABAND in case it is already allocated

      CALL DEALLOCATE_LAPACK_MAT ( 'ABAND' )

! We will abort if errors occur and if INFO, on input, is not -1

      QUIT_ON_POS_INFO = 'Y'
      IF (INFO == -1) THEN
         QUIT_ON_POS_INFO = 'N'
      ENDIF

      EPS1 = EPSIL(1)

! Determine bandwidth of matrix 

      CALL OURTIM
      MODNAM = 'CALC BANDWIDTH OF MATRIX ' // MATIN_NAME(1:)
      WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL BANDSIZ ( NROWS, NTERMS, I_MATIN, J_MATIN, MATIN_SDIA ) 
      MB_TO_ALLOCATE = (REAL(DOUBLE))*(REAL(MATIN_SDIA+1))*(REAL(NROWS))/ONEPP6
      WRITE(SC1,3094) MATIN_NAME, MATIN_SDIA+1, MB_TO_ALLOCATE
      WRITE(ERR,3002) MATIN_NAME, MATIN_SDIA+1
      IF (SUPINFO == 'N') THEN
         WRITE(F06,3002) MATIN_NAME, MATIN_SDIA+1
      ENDIF
      IF (MB_TO_ALLOCATE <= ONE) THEN
         WRITE(ERR,3003) MATIN_NAME, MB_TO_ALLOCATE
         IF (SUPINFO == 'N') THEN
            WRITE(F06,3003) MATIN_NAME, MB_TO_ALLOCATE
         ENDIF
      ELSE
         WRITE(ERR,3004) MATIN_NAME, MB_TO_ALLOCATE
         IF (SUPINFO == 'N') THEN
            WRITE(F06,3004) MATIN_NAME, MB_TO_ALLOCATE
         ENDIF
      ENDIF

! Allocate array ABAND (matrix in band form for LAPACK)

      CALL OURTIM
      MODNAM = 'ALLOCATE ARRAYS FOR LAPACK BAND FORM OF ' // MATIN_NAME(1:)
      WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL ALLOCATE_LAPACK_MAT ( 'ABAND', MATIN_SDIA+1, NROWS, SUBR_NAME )
      CALL ALLOCATE_LAPACK_MAT ( 'LAPACK_S', NROWS, 1, SUBR_NAME )

! Put MATIN matrix into ABAND form required by LAPACK band matrix.

      CALL OURTIM
      MODNAM = 'PUT INTO LAPACK BAND FORM: MATRIX ' // MATIN_NAME(1:)
      WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL BANDGEN_LAPACK_DPB ( MATIN_NAME, NROWS, MATIN_SDIA, NTERMS, I_MATIN, J_MATIN, MATIN, ABAND, SUBR_NAME )

! Output ABAND, if requested

      IF ((DEB_PRT(1) > 0) .AND. (DEB_PRT(1) <= NDEBUG)) THEN
         IF ((DEBUG(DEB_PRT(1)) == 1) .OR. (DEBUG(DEB_PRT(1)) == 3)) THEN
            CALL WRITE_MATRIX_BY_ROWS ( 'LAPACK BAND FORM FOR MATRIX ' // MATIN_NAME(1:), ABAND, MATIN_SDIA+1, NROWS, F06 )
         ENDIF
      ENDIF

! Calc the infinity norm of the matrix using the LAPACK function DLANSB. K_INORM is needed later for estimates
! of errors in the solution. Use array S for workspace in the calculation.

      IF (CALC_COND_NUM == 'Y') THEN
         CALL OURTIM
         MODNAM = 'CALC INFINITY NORM OF MATRIX ' // MATIN_NAME(1:)
         WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )
         K_INORM = DLANSB ( INORM, UPLO, NROWS, MATIN_SDIA, ABAND, MATIN_SDIA+1, LAPACK_S )
         WRITE(F06,3005) MATIN_NAME, K_INORM
      ENDIF  

! Get max & min diagonals from the original matrix. Code assumes all diag terms positive
 
      IF (MATIN_DIAG_RAT == 'Y') THEN
         CALL OURTIM
         MODNAM = 'GET MAX/MIN DIAGONALS OF MATRIX ' // MATIN_NAME(1:)
         WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

         MAXKII = ZERO
         IIMAX  = 0
         DO I=1,NROWS
            IF (ABAND(MATIN_SDIA+1,I) > MAXKII) THEN
               MAXKII = ABAND(MATIN_SDIA+1,I)
               IIMAX  = I
            ENDIF
         ENDDO
         WRITE(F06,3006) MATIN_NAME, MAXKII,IIMAX 

         MINKII = MAXKII
         IIMIN  = IIMAX
         DO I=1,NROWS
            IF (ABAND(MATIN_SDIA+1,I) < MINKII) THEN
               MINKII = ABAND(MATIN_SDIA+1,I)
               IIMIN  = I
            ENDIF
         ENDDO
         WRITE(F06,3007) MATIN_NAME, MINKII,IIMIN

         IF (DABS(MINKII) > EPS1) THEN
            KRATIO = MAXKII/MINKII
            WRITE(F06,3008) MATIN_NAME, KRATIO
         ENDIF 
      ENDIF

      EQUED = 'N'
! Equilibrate matrix, if user requested it via input arg EQUIL_MATIN
! TEMPORARILY REMOVE THIS CODE. IT WAS CAUSING ERRORS - FAILURES DUE TO RATIO OF MATRIX DIAG TO FACTOR DIAG WHEN EQUILIBRATED
!     IF (EQUIL_MATIN == 'Y') THEN
!        CALL OURTIM
!        MODNAM = 'EQUILIBRATING (IF NEEDED) MATRIX ' // MATIN_NAME(1:)
!        WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
!        CALL EQUILIBRATE( MATIN_NAME, MATIN_SET, NROWS, MATIN_SDIA, ABAND, LAPACK_S, EQUED, SCOND )
!     ENDIF

! Set equilibrate scale factors to return to calling subr. We do this since we need to deallocate LAPACK_S here since this
! subr (SYM_MAT_DECOMP_LAPACK) is called other times in MYSTRAN.

      DO I=1,NROWS
         EQUIL_SCALE_FACS(I) = LAPACK_S(I)
      ENDDO

! Deallocate LAPACK_S

      CALL DEALLOCATE_LAPACK_MAT ( 'LAPACK_S' )

! Get max & min diagonals from the equilibrated matrix. Code assumes all diag terms positive 
 
      IF ((EQUED == 'Y') .AND. (MATIN_DIAG_RAT == 'Y')) THEN
         CALL OURTIM
         MODNAM = 'GET MAX/MIN DIAGONALS OF EQUILIBRATED MATRIX' // MATIN_NAME(1:)
         WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

         MAXKII = ZERO
         IIMAX  = 0
         DO I=1,NROWS
            IF (ABAND(MATIN_SDIA+1,I) > MAXKII) THEN
               MAXKII = ABAND(MATIN_SDIA+1,I)
               IIMAX  = I
            ENDIF
         ENDDO
         WRITE(F06,3009) MATIN_NAME, MAXKII,IIMAX 

         MINKII = MAXKII
         IIMIN  = 0
         DO I=1,NROWS
            IF (ABAND(MATIN_SDIA+1,I) < MINKII) THEN
               MINKII = ABAND(MATIN_SDIA+1,I)
               IIMIN  = I
            ENDIF
         ENDDO
         WRITE(F06,3010) MATIN_NAME, MINKII,IIMIN

         IF (DABS(MINKII) > EPS1) THEN
            KRATIO = MAXKII/MINKII
            WRITE(F06,3011) MATIN_NAME, KRATIO
         ENDIF

         IF ((DEB_PRT(1) > 0) .AND. (DEB_PRT(1) <= NDEBUG)) THEN
            IF ((DEBUG(DEB_PRT(1)) == 2) .OR. (DEBUG(DEB_PRT(1)) == 3)) THEN
               CALL WRITE_MATRIX_BY_ROWS ('LAPACK BAND FORM FOR EQUILIBRATED MATRIX' // MATIN_NAME(1:), ABAND, MATIN_SDIA+1,       &
                                           NROWS, F06)
            ENDIF
         ENDIF
      
      ENDIF 

! Perform factorization of matrix. ABAND is the original matrix going into the decomp routine and is the upper triangular factor on
! exit.

      CALL OURTIM
      MODNAM = 'LAPACK TRIANGULAR FACTORIZATION OF MATRIX ' // MATIN_NAME(1:)
      WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL DPBTRF ( UPLO, NROWS, MATIN_SDIA, ABAND, MATIN_SDIA+1, INFO )

      CALLED_SUBR = 'DPBTRF'      
      IF (INFO == 0) THEN

         FACTORED_MATRIX(1:) = ' '
         FACTORED_MATRIX     = MATIN_NAME

      ELSE IF (INFO < 0) THEN                              ! LAPACK subr XERBLA should have reported error on an illegal argument
!                                                            in calling a LAPACK subr, so we should not have gotten here
         WRITE(ERR,993) SUBR_NAME, CALLED_SUBR, CALLING_SUBR
         WRITE(F06,993) SUBR_NAME, CALLED_SUBR, CALLING_SUBR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit

      ELSE IF (INFO > 0) THEN                              ! Leading minor of MATIN not positive definite

         CALL GET_GRID_AND_COMP ( MATIN_SET, INFO, GRIDV, COMPV  )

         IF (PRT_ERRS /= 'N') THEN
            WRITE(ERR,981) MATIN_NAME, CALLED_SUBR, INFO
            WRITE(F06,981) MATIN_NAME, CALLED_SUBR, INFO
            IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
               WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
               WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
            ELSE 
               WRITE(ERR,9812) INFO, CALLING_SUBR 
               WRITE(F06,9812) INFO, CALLING_SUBR
            ENDIF
         ENDIF

      ENDIF

! Output ABAND (factor of MATIN now), if requested

      IF ((DEB_PRT(2) > 0) .AND. (DEB_PRT(2) <= NDEBUG)) THEN
         IF (DEBUG(DEB_PRT(2)) == 1) THEN
            CALL WRITE_MATRIX_BY_ROWS('TRIANGULAR FACTOR IN LAPACK BAND FORM FOR MATRIX ' // MATIN_NAME(1:), ABAND, MATIN_SDIA+1,  &
                                       NROWS, F06)
         ENDIF
      ENDIF

! Calculate and print ratios of diag to factor diag (if they are zero or negative or > MAXRATIO).

      CALL OURTIM
      MODNAM = 'CALC MAX RATIO OF MATRIX DIAGONAL TO FACTOR DIAGONAL'
      WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

      DO I=1,NROWS                                         ! First, get diagonal terms from MATIN
         WRITE(SC1,12345,ADVANCE='NO') I, NROWS, CR13
         IF (I_MATIN(I) == I_MATIN(I+1)) THEN
            MATIN_DIAG(I) = ZERO
         ELSE
            MATIN_DIAG(I) = MATIN(I_MATIN(I))
         ENDIF
      ENDDO
      WRITE(SC1,*) CR13

      MAXIMAX_RATIO = -MACH_LARGE_NUM                                  ! Calc ratio of MATIN diag to factor diag
      NONPOS_DEF    = 'N'
      DO I=1,NROWS

         CALL GET_GRID_AND_COMP ( MATIN_SET, I, GRIDV, COMPV  )

         WRITE(SC1,22345,ADVANCE='NO') I, NROWS, CR13
         FAC_DIAG = ABAND(MATIN_SDIA+1,I)

         IF (FAC_DIAG <= EPS1) THEN                        ! Zero or negative factor diagonal. (MATIN is nonpositive definite)

            NONPOS_DEF = 'Y'
            IF (PRT_ERRS /= 'N') THEN
               WRITE(ERR,982) MATIN_NAME, FAC_DIAG
               WRITE(F06,982) MATIN_NAME, FAC_DIAG
               IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
                  WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
                  WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
               ELSE 
                  WRITE(ERR,9812) I, CALLING_SUBR
                  WRITE(F06,9812) I, CALLING_SUBR
               ENDIF
            ENDIF

         ELSE

            RATIO = MATIN_DIAG(I)/FAC_DIAG
!                                                          Ratio is greater than param MAXRATIO
            IF ((DABS(RATIO) > MAXRATIO) .AND. (PRT_ERRS /= 'N')) THEN
               WRITE(ERR,983) MATIN_NAME, RATIO, MAXRATIO
               WRITE(F06,983) MATIN_NAME, RATIO, MAXRATIO
               IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
                  WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
                  WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
               ELSE 
                  WRITE(ERR,9811) I, CALLING_SUBR 
                  WRITE(F06,9811) I, CALLING_SUBR
               ENDIF
            ENDIF

            IF (RATIO > MAXIMAX_RATIO) THEN                ! This is the largest of the ratios
               MAXIMAX_RATIO = MATIN_DIAG(I)/ABAND(MATIN_SDIA+1,I)
               IIMAX = I
            ENDIF

         ENDIF

      ENDDO
      WRITE(SC1,*) CR13  

      IF (NONPOS_DEF == 'N') THEN

         WRITE(ERR,984) MATIN_NAME, MAXIMAX_RATIO
         WRITE(F06,984) MATIN_NAME, MAXIMAX_RATIO
         CALL GET_GRID_AND_COMP ( MATIN_SET, IIMAX, GRIDV, COMPV  )
         IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
            WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
            WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
         ELSE 
            WRITE(ERR,9812) IIMAX, CALLING_SUBR
            WRITE(F06,9812) IIMAX, CALLING_SUBR
         ENDIF

      ENDIF

      IF ((DABS(MAXIMAX_RATIO) > MAXRATIO) .OR. (NONPOS_DEF == 'Y') .OR. (INFO > 0)) THEN
                                                           ! If BAILOUT >= 0 then quit. Otherwise, continue processing.
         IF ((BAILOUT >= 0) .AND. (QUIT_ON_POS_INFO == 'Y')) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,99999) BAILOUT
            WRITE(F06,99999) BAILOUT
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ENDIF

! If CALC_COND_NUM = 'Y', then calc the reciprocal of the condition number of the matrix. This is done in
! subr COND_NUM using LAPACK subroutine DPBCON and is used for a better estimate of solution errors later.

      RCOND = ZERO
      IF (CALC_COND_NUM == 'Y') THEN
         CALL OURTIM
         MODNAM = 'CALC RECIP OF COND NUM OF MATRIX ' // MATIN_NAME(1:)
         WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL COND_NUM ( MATIN_NAME, NROWS, MATIN_SDIA, K_INORM, ABAND, RCOND )
      ENDIF

!***********************************************************************************************************************************
  981 FORMAT(' *ERROR   981: THE FACTORIZATION OF THE MATRIX ',A,' COULD NOT BE COMPLETED BY LAPACK SUBR ',A                       &
                    ,/,14X,' THE LEADING MINOR OF ORDER ',I12,' IS NOT POSITIVE DEFINITE')

  982 FORMAT(' *ERROR   982: MATRIX ',A,' IS NONPOSITIVE DEFINITE. A DIAGONAL TERM IS ZERO OR NEGATIVE = ',1ES14.6)

  983 FORMAT(' *ERROR   983: MATRIX ',A,' HAS AN ABSOLUTE VALUE OF THE RATIO OF MATRIX DIAG TO FACTOR DIAG = ',1ES10.2,            &
                           ' (GREATER THAN BULK DATA PARAM MAXRATIO = ',1ES10.2,')'                                                &
                    ,/,14X,' THIS WILL ONLY BE A FATAL ERROR IF PARAM BAILOUT > 0')

  984 FORMAT(' *INFORMATION: THE MAXIMUM ABSOLUTE VALUE OF THE RATIO OF MATRIX DIAGONAL TO FACTOR DIAG FOR MATRIX ',A,' = ',1ES14.6)

 9811 FORMAT('               THIS IS FOR ROW AND COL IN THE MATRIX FOR GRID POINT ',I8,' COMP ',I3,'. THE CALLING SUBR WAS: ',A,/)

 9812 FORMAT('               THIS IS FOR ROW AND COL ',I8,' IN THE MATRIX. THE CALLING SUBR WAS: ',A,/)

  993 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' LAPACK SUBR XERBLA SHOULD HAVE REPORTED AN ERROR ON AN ILLEGAL ARGUMENT IN A CALL TO LAPACK SUBR: '   &
                    ,/,15X,A,/,' (OR A SUBR CALLED BY IT) AND THEN ABORTED. THE CALLING SUBR WAS: ',A)

 3002 FORMAT(' *INFORMATION: BANDWIDTH OF MATRIX ',A11,'                                        = ',I13,' (BW)',/)

 3003 FORMAT(' *INFORMATION: MEMORY REQUIRED FOR TRIANGULAR DECOMPOSITION OF MATRIX   ',A11,'   = ',F13.6,' MB ',                  &
                           '(8*NDOF*BW)',/,14X,' (using LAPACK band matrix algorithm)',/)

 3004 FORMAT(' *INFORMATION: MEMORY REQUIRED FOR TRIANGULAR DECOMPOSITION OF MATRIX   ',A11,'   = ',F13.3,' MB ',                  &
                           '(8*NDOF*BW)',/,14X,' (using LAPACK band matrix algorithm)',/)

 3005 FORMAT(' *INFORMATION: INFINITY NORM OF MATRIX ',A11,'                                    = ',1ES13.6,/)

 3006 FORMAT(' *INFORMATION: MAXIMUM DIAGONAL TERM IN MATRIX ',A11,'                            = ',1ES13.6,                       &
                           ' Occurs in row/col no. ',I8)

 3007 FORMAT(' *INFORMATION: MINIMUM DIAGONAL TERM IN MATRIX ',A11,'                            = ',1ES13.6,                       &
                           ' Occurs in row/col no. ',I8)

 3008 FORMAT(' *INFORMATION: RATIO OF MAX TO MIN DIAGONALS IN MATRIX ',A11,'                    = ',1ES13.6,/)

 3009 FORMAT(' *INFORMATION: MAXIMUM DIAGONAL TERM IN THE EQUILIBRATED MATRIX ',A11,'           = ',1ES13.6,                       &
                           ' Occurs in row/col no. ',I8)

 3010 FORMAT(' *INFORMATION: MINIMUM DIAGONAL TERM IN THE EQUILIBRATED MATRIX ',A11,'           = ',1ES13.6,                       &
                           ' Occurs in row/col no. ',I8)

 3011 FORMAT(' *INFORMATION: RATIO OF MAX TO MIN DIAGONALS IN THE EQUILIBRATED MATRIX ',A11,'   = ',1ES13.6,/)

 3092 FORMAT(1X,I2,'/',A54,8X,2X,I2,':',I2,':',I2,'.',I3)

 3094 FORMAT(5X,' Bandwidth of ',A,'  = ',I8,' and requires ',F10.3,' MB of memory')

12345 FORMAT(5X,'Getting diagonal of matrix, row ',I8,' of ',I8,A)

22345 FORMAT(5X,'Calc ratios of matrix diag to factor diag: row ',I8,' of ',I8,A)

99999 FORMAT(/,' PROCESSING TERMINATED DUE TO ABOVE MESSAGES AND BULK DATA PARAMETER BAILOUT = ',I7)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

!***********************************************************************************************************************************

      END SUBROUTINE SYM_MAT_DECOMP_LAPACK







