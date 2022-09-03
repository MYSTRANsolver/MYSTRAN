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

      SUBROUTINE GET_MATRIX_DIAG_STATS ( MAT_NAME, INPUT_SET, NROWS, NTERM, I_KIN, J_KIN, KIN, WRITE_WHAT, KIN_DIAG,               &
                                         MAX_OA_DIAG_TERM )

! (1) Gets the diagonal terms from an input stiffness matrix that is in sparse compressed row storage format
! (2) Returns the diagonal in a column matrix
! (3) If requested, prints the diagonal with summary on stats: max, mins, etc

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NGRID
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  AUTOSPC_RAT, EPSIL
      USE DOF_TABLES, ONLY            :  TDOFI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_MATRIX_DIAG_STATS_BEGEND

      USE GET_MATRIX_DIAG_STATS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_MATRIX_DIAG_STATS'
      CHARACTER(LEN=*), INTENT(IN )   :: INPUT_SET         ! Char description of MYSTRAN displ set (e.g. 'A ' or 'SG')
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_NAME          ! Name of the input matrix

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in the input matrix
      INTEGER(LONG), INTENT(IN)       :: NTERM             ! Number of nonzero terms in the input matrix
      INTEGER(LONG), INTENT(IN)       :: I_KIN(NROWS+1)    ! Indices that are used to determine where in KIN the next column begins
      INTEGER(LONG), INTENT(IN)       :: J_KIN(NTERM)      ! Col numbers of terms in KIN
      INTEGER(LONG), INTENT(IN)       :: WRITE_WHAT        ! 1 write diagonal, 2 write summary stats, 3 write both
      INTEGER(LONG)                   :: AGRID             ! Actual grid number
      INTEGER(LONG)                   :: AGRID_OLD         ! Actual grid number (used to add blank line bet grids when write diags)
      INTEGER(LONG)                   :: COMP              ! DOF component number
      INTEGER(LONG)                   :: COMPS_TO_CHECK(6) ! DOF component number numbers to check in the statistical summary:
!                                                              for OA stats             : COMPS_TO_CHECK(I) = 1,1,1,1,1,1
!                                                              for translation DOF stats: COMPS_TO_CHECK(I) = 1,1,1,0,0,0
!                                                              for rotation    DOF stats: COMPS_TO_CHECK(I) = 0,0,0,1,1,1

      INTEGER(LONG)                   :: INPUT_SET_COL     ! Col no. in array TDOF where the  INPUT_SET is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: I,J,L             ! DO loop indices
      INTEGER(LONG)                   :: K                 ! Index
      INTEGER(LONG)                   :: KIN_ROW_MAX       ! Row/Col in KIN where max diagonal term occurs
      INTEGER(LONG)                   :: KIN_ROW_OA_MAX    ! Row/Col in KIN where max OA diagonal term occurs
      INTEGER(LONG)                   :: KIN_ROW_MIN       ! Row/Col in KIN where min diagonal term occurs
      INTEGER(LONG)                   :: KIN_ROW_MINP      ! Row/Col in KIN where min diagonal term > 0 occurs
      INTEGER(LONG)                   :: NUM_DOF(3)        ! Number of rows in this matrix (overall, translation and rotation)
      INTEGER(LONG)                   :: NUM_IN_ROW        ! Number of terms in a row of KIN
      INTEGER(LONG)                   :: NUM_NEG_DIAG_TERMS! Number of negative terms on the diagonal
      INTEGER(LONG)                   :: NUM_NULL_ROWS     ! Number of null rows
      INTEGER(LONG)                   :: NUM_SMALL_TERMS   ! Number of zero terms on the diagonal
      INTEGER(LONG)                   :: TDOFI_ROW_MAX     ! Row/Col in TDOFI where MAX_DIAG_TERM is
      INTEGER(LONG)                   :: TDOFI_ROW_OA_MAX  ! Row/Col in TDOFI where MAX_OA_DIAG_TERM is
      INTEGER(LONG)                   :: TDOFI_ROW_MIN     ! Row/Col in TDOFI where MIN_DIAG_TERM is
      INTEGER(LONG)                   :: TDOFI_ROW_MINP    ! Row/Col in TDOFI where MINP_DIAG_TERM is
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_MATRIX_DIAG_STATS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: KIN(NTERM)        ! Nonzero terms in the stiffness matrix
      REAL(DOUBLE) , INTENT(OUT)      :: KIN_DIAG(NROWS)   ! Diagonal terms from KIN
      REAL(DOUBLE) , INTENT(OUT)      :: MAX_OA_DIAG_TERM  ! Maximum diagonal term in the stiffness matrix for the COMPS_TO_CHECK
      REAL(DOUBLE)                    :: EPS1              ! Small number to compare against zero
      REAL(DOUBLE)                    :: MAX_DIAG_TERM     ! Maximum diagonal term in the stiffness matrix
      REAL(DOUBLE)                    :: MIN_DIAG_TERM     ! Minimum diagonal term in the stiffness matrix
      REAL(DOUBLE)                    :: MINP_DIAG_TERM    ! Minimum diagonal term > 0 in the stiffness matrix
      REAL(DOUBLE)                    :: MAX_MAX_OA_RATIO  ! Ratio: MAX_DIAG_TERM/MAX_OA_DIAG_TERM
      REAL(DOUBLE)                    :: MIN_MAX_OA_RATIO  ! Ratio: MIN_DIAG_TERM/MAX_OA_DIAG_TERM
      REAL(DOUBLE)                    :: MINP_MAX_OA_RATIO ! Ratio: MINP_DIAG_TERM/MAX_OA_DIAG_TERM
      REAL(DOUBLE)                    :: RATIO             ! Ratio of a diagonal term in KIN to the max diag term in KIN

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Initialize outputs

      MAX_OA_DIAG_TERM = ZERO

      DO I=1,NROWS
         KIN_DIAG(I) = ZERO
      ENDDO

      CALL TDOF_COL_NUM ( INPUT_SET, INPUT_SET_COL )

! Get diagonal terms

      K = 0
      DO I=1,NROWS
         NUM_IN_ROW = I_KIN(I+1) - I_KIN(I)
         IF (I_KIN(I) == I_KIN(I+1)) THEN
            KIN_DIAG(I) = ZERO
         ELSE
            DO J=1,NUM_IN_ROW
               K = K + 1
               IF (J_KIN(K) == I) THEN
                  KIN_DIAG(I) = KIN(K)
               ENDIF
            ENDDO
         ENDIF
      ENDDO

! Determine max OA positive diagonal term

      KIN_ROW_OA_MAX   = 0
      DO I=1,NROWS
         IF (KIN_DIAG(I) > MAX_OA_DIAG_TERM) THEN
            MAX_OA_DIAG_TERM = KIN_DIAG(I)
            KIN_ROW_OA_MAX  = I
         ENDIF
      ENDDO
      IF (DABS(MAX_OA_DIAG_TERM) < EPS1) THEN
         WRITE(F06,100) MAT_NAME, MAX_OA_DIAG_TERM, SUBR_NAME
         RETURN
      ENDIF

! Calc row where max OA occurs

         K = 0
         TDOFI_ROW_OA_MAX = 0
         DO I=1,NDOFG
            IF (TDOFI(I,INPUT_SET_COL) /= 0) THEN
               K = K + 1
               IF (K == KIN_ROW_OA_MAX) THEN
                  TDOFI_ROW_OA_MAX = I
                  EXIT
               ENDIF
            ENDIF
         ENDDO

! Print diagonal terms (with ratio to max term), if requested

      IF ((WRITE_WHAT == 1) .OR. (WRITE_WHAT == 3)) THEN
      
         WRITE(F06,*)
         WRITE(F06,1001) MAT_NAME
         WRITE(F06,1002)

         AGRID_OLD = 0
         K = 0
         DO I=1,NDOFG
            IF (TDOFI(I,INPUT_SET_COL) /= 0) THEN
               K = K + 1
               AGRID = TDOFI(I,1)
               IF (DEBUG(88) == 0) THEN                    ! Write separator line between each set of grid outputs
                  IF (AGRID /= AGRID_OLD) THEN
                     AGRID_OLD = AGRID
                     WRITE(F06,*)
                  ENDIF
               ENDIF
               COMP  = TDOFI(I,2)
               RATIO = KIN_DIAG(K)/MAX_OA_DIAG_TERM
               WRITE(F06,1003) K, AGRID, COMP ,KIN_DIAG(K), RATIO
            ENDIF
         ENDDO
         WRITE(F06,*)
         
      ENDIF

! Determine statistics, if requested

      IF (WRITE_WHAT == 2) THEN
         WRITE(F06,1004) MAT_NAME
         WRITE(F06,1002)
      ENDIF

      IF ((WRITE_WHAT == 2) .OR. (WRITE_WHAT == 3)) THEN

         DO L=1,3                                          ! L=1 is for all COMPS, L=2 is for COMPS 1,2,3 and L=3 is for COMPS 4,5,6

            DO I = 1,6
               COMPS_TO_CHECK(I) = 0
            ENDDO

            NUM_DOF(L) = 0

            IF      (L == 1) THEN
               DO I = 1,6
                  COMPS_TO_CHECK(I) = 1
               ENDDO
            ELSE IF (L == 2) THEN
               DO I = 1,3
                  COMPS_TO_CHECK(I) = 1
               ENDDO
            ELSE IF (L == 3) THEN
               DO I = 4,6
                  COMPS_TO_CHECK(I) = 1
               ENDDO
            ENDIF
            
            K = 0
            MAX_DIAG_TERM = -1.0D0                         ! Determine max positive diagonal term for the COMPS_TO_CHECK
            KIN_ROW_MAX   = 0
            DO I=1,NDOFG
               IF(TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF(COMPS_TO_CHECK(TDOFI(I,2)) > 0) THEN
                     NUM_DOF(L) = NUM_DOF(L) + 1
                     IF (KIN_DIAG(K) >= MAX_DIAG_TERM) THEN
                        MAX_DIAG_TERM = KIN_DIAG(K)
                        KIN_ROW_MAX  = K
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO

            K = 0
            MINP_DIAG_TERM = MAX_DIAG_TERM                 ! Determine min positive diagonal term for the COMPS_TO_CHECK
            KIN_ROW_MINP   = KIN_ROW_MAX
            DO I=1,NDOFG
               IF(TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF(COMPS_TO_CHECK(TDOFI(I,2)) > 0) THEN
                     IF ((KIN_DIAG(K) <= MINP_DIAG_TERM) .AND. (KIN_DIAG(K) > ZERO)) THEN
                        MINP_DIAG_TERM = KIN_DIAG(K)
                        KIN_ROW_MINP   = K
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO

            K = 0
            MIN_DIAG_TERM = MAX_DIAG_TERM                  ! Determine min diagonal term for the COMPS_TO_CHECK
            KIN_ROW_MIN   = KIN_ROW_MAX
            DO I=1,NDOFG
               IF(TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF(COMPS_TO_CHECK(TDOFI(I,2)) > 0) THEN
                     IF (KIN_DIAG(K) <= MIN_DIAG_TERM) THEN
                        MIN_DIAG_TERM = KIN_DIAG(K)
                        KIN_ROW_MIN   = K
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
                                                           ! Calc ratios of max and mins to overall max term
            MAX_MAX_OA_RATIO  = MAX_DIAG_TERM/MAX_OA_DIAG_TERM
            MIN_MAX_OA_RATIO  = MIN_DIAG_TERM/MAX_OA_DIAG_TERM
            MINP_MAX_OA_RATIO = MINP_DIAG_TERM/MAX_OA_DIAG_TERM

            K = 0
            NUM_NEG_DIAG_TERMS = 0                         ! Determine number of neg terms, null rows and negligible terms 
            NUM_NULL_ROWS      = 0
            NUM_SMALL_TERMS    = 0
            DO I=1,NDOFG
               IF(TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF(COMPS_TO_CHECK(TDOFI(I,2)) > 0) THEN
                     IF (I_KIN(K) == I_KIN(K+1)) THEN
                        KIN_DIAG(K) = ZERO
                        NUM_NULL_ROWS = NUM_NULL_ROWS + 1
                     ENDIF
                     IF (KIN_DIAG(K) < ZERO) THEN
                        NUM_NEG_DIAG_TERMS = NUM_NEG_DIAG_TERMS + 1
                     ENDIF
                     IF (DABS(KIN_DIAG(K)) < AUTOSPC_RAT*MAX_OA_DIAG_TERM) THEN
                        NUM_SMALL_TERMS = NUM_SMALL_TERMS + 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO

            K = 0                                          ! Calc row where max occurs
            TDOFI_ROW_MAX = 0
            DO I=1,NDOFG
               IF (TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF (K == KIN_ROW_MAX) THEN
                     TDOFI_ROW_MAX = I
                     EXIT
                  ENDIF
               ENDIF
            ENDDO

            K = 0                                          ! Calc row where min positive occurs
            TDOFI_ROW_MINP = 0
            DO I=1,NDOFG
               IF (TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF (K == KIN_ROW_MINP) THEN
                     TDOFI_ROW_MINP = I
                     EXIT
                  ENDIF
               ENDIF
            ENDDO

            K = 0                                          ! Calc row where min occurs
            TDOFI_ROW_MIN = 0
            DO I=1,NDOFG
               IF (TDOFI(I,INPUT_SET_COL) /= 0) THEN
                  K = K + 1
                  IF (K == KIN_ROW_MIN) THEN
                     TDOFI_ROW_MIN = I
                     EXIT
                  ENDIF
               ENDIF
            ENDDO

            IF      (L == 1) THEN
               WRITE(F06,1005) NUM_DOF(L), INPUT_SET
            ELSE IF (L == 2) THEN
               WRITE(F06,5005) NUM_DOF(L), INPUT_SET
            ELSE IF (L == 3) THEN
               WRITE(F06,6005) NUM_DOF(L), INPUT_SET
            ENDIF

            IF (L == 1) THEN
               IF (TDOFI_ROW_OA_MAX /= 0) THEN
                  WRITE(F06,1006) KIN_ROW_MAX    , TDOFI(TDOFI_ROW_OA_MAX,1), TDOFI(TDOFI_ROW_OA_MAX,2), MAX_OA_DIAG_TERM
               ELSE
                  WRITE(F06,2006)
               ENDIF
            ELSE
               IF (TDOFI_ROW_MAX /= 0) THEN
                  WRITE(F06,3006) KIN_ROW_MAX    , TDOFI(TDOFI_ROW_MAX,1), TDOFI(TDOFI_ROW_MAX,2), MAX_DIAG_TERM, MAX_MAX_OA_RATIO
               ELSE
                  WRITE(F06,4006)
               ENDIF
            ENDIF

            IF (TDOFI_ROW_MINP /= 0) THEN
               WRITE(F06,1007) KIN_ROW_MINP, TDOFI(TDOFI_ROW_MINP,1), TDOFI(TDOFI_ROW_MINP,2), MINP_DIAG_TERM,         &
                               MINP_MAX_OA_RATIO
            ELSE
               WRITE(F06,2007)
            ENDIF

            IF (TDOFI_ROW_MIN /= 0) THEN
               WRITE(F06,1008) KIN_ROW_MIN    , TDOFI(TDOFI_ROW_MIN,1), TDOFI(TDOFI_ROW_MIN,2), MIN_DIAG_TERM    , MIN_MAX_OA_RATIO
            ELSE
               WRITE(F06,2008)
            ENDIF

            WRITE(F06,1009) NUM_NEG_DIAG_TERMS
            WRITE(F06,1010) NUM_NULL_ROWS
                                                           ! NUM_SMALL_TERMS does not include NUM_NEG_TERMS so don't subtract it off
            WRITE(F06,1011) AUTOSPC_RAT, NUM_SMALL_TERMS - NUM_NULL_ROWS
            WRITE(F06,*)

         ENDDO

         WRITE(F06,1012) AUTOSPC_RAT

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  100 FORMAT(' *INFORMATION: MAX DIAGONAL TERM IN THE ',A,' MATRIX IS = ',1ES9.2,' WHICH IS TOO SMALL FOR CALCULATION OF MATRIX',  &
                         ' STATS',/,14X,' IN SUBR ',A)

 1001 FORMAT(1X,/,37X,'OUTPUT OF DIAGONALS OF ',A,' MATRIX AND THEIR RATIOS TO MAX DIAGONAL TERM',/)

 1002 FORMAT(45X,'DOF NO     GRID  COMP      DIAGONAL      DIAG/MAXDIAG')

 1003 FORMAT(43X,I8,1X,I8,I6,2(1ES16.6))

 1004 FORMAT(1X,/,47X,'STATISTICS ON THE DIAGONAL TERMS OF THE ',A,' MATRIX',/)

 1005 FORMAT(' Overall stats for ',I6,1X,A,'set DOF''s',6X,'-------- --------     -   -------------   -------------',/,1X,         &
             '------------------------------------')

 5005 FORMAT(' Stats for ',I6,1X,A,'set translation DOF''s',/,1X,'----------------------------------------')

 6005 FORMAT(' Stats for ',I6,1X,A,'set rotation DOF''s',/,1X,'-------------------------------------')

 1006 FORMAT(' Max positive diag term (OA max)         : ',I8,1X,I8,I6,2(1ES16.6))

 3006 FORMAT(' Max positive diag term & ratio to OA max: ',I8,1X,I8,I6,2(1ES16.6))

 1007 FORMAT(' Min positive diag term & ratio to OA max: ',I8,1X,I8,I6,2(1ES16.6))

 1008 FORMAT(' Min (any)    diag term & ratio to OA max: ',I8,1X,I8,I6,2(1ES16.6))

 1009 FORMAT(' Num of negative terms on the diagonal   : ',I8)

 1010 FORMAT(' Num of null rows                        : ',I8)

 1011 FORMAT(' Num of diag terms <',1ES9.2,'*(OA max)** : ',I8)
 
 1012 FORMAT(' ** Ratio of diag term to OA max is less than AUOTSPC_RAT = ',1ES13.6,' (defined on PARAM AUTOSPC by user or as',    &
                ' default value)',/,'    (excluding null rows and negative terms)',/)

 2006 FORMAT(' Max positive diag term (OA max)         : ')

 4006 FORMAT(' Max positive diag term (OA max)         : ')

 2007 FORMAT(' Min positive diag term & ratio to OA max: ')

 2008 FORMAT(' Min (any)    diag term & ratio to OA max: ')

! **********************************************************************************************************************************

      END SUBROUTINE GET_MATRIX_DIAG_STATS
