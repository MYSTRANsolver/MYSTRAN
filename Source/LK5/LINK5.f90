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
 
      SUBROUTINE LINK5
 
! LINK5 takes the L-set displacements solved for in LINK3 (statics) or LINK4 (eigenvalues) and builds it back up to the G-set.
! See Appendix B to the MYSTRAN User's Reference Guide for an explanation of how this is done.

! In addition, for Craig-Bampton model generation (SOL = GEN CB MODEL or 31), array PHIXA is expanded to G-set size
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, ERR, F04, F06, L1H, L2A, L2E, L2F, L3A, L5A, L5B, SC1
      USE IOUNT1, ONLY                :  LINK1H, LINK2A, LINK2E, LINK2F, LINK3A, LINK5A, LINK5B
      USE IOUNT1, ONLY                :  L1H_MSG, L2A_MSG, L2E_MSG, L2F_MSG, L3A_MSG, L5A_MSG, L5B_MSG
      USE IOUNT1, ONLY                :  ERRSTAT, L1HSTAT, L2ESTAT, L2FSTAT, L3ASTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, COMM, FATAL_ERR, LINKNO, MBUG, NDOFA, NDOFF, NDOFG, NDOFL, NDOFM,           &
                                         NDOFN, NDOFO, NDOFR, NDOFS, NDOFSE, NGRID, NSUB, NTERM_GMN, NTERM_GOA, NTERM_PO,          &
                                         NUM_CB_DOFS, NUM_EIGENS, NVEC, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  EIGNORM2, SUPINFO, SUPWARN
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL, EIGEN_VEC, GEN_MASS, MODE_NUM
      USE FULL_MATRICES, ONLY         :  PHIZG_FULL
      USE SPARSE_MATRICES, ONLY       :  I_GMN, J_GMN, GMN, I_GOA, J_GOA, GOA
      USE OUTPUT4_MATRICES, ONLY      :  NUM_OU4_REQUESTS
      USE MISC_MATRICES, ONLY         :  UG_T123_MAT
      USE COL_VECS, ONLY              :  UG_COL, YSe, UO0_COL, UL_COL 
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID, INV_GRID_SEQ, EIG_COMP, EIG_GRID, EIG_NORM, MAXMIJ, MIJ_COL, MIJ_ROW

      USE LINK5_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: VEC_SIGN_CHG(NDOFL) ! Indicators of whether user wants to change sign of an eigenvector

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LINK5'
      CHARACTER( 1*BYTE)              :: CLOSE_IT          ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close a file or not 
      CHARACTER( 8*BYTE)              :: CLOSE_STAT        ! What to do with file when it is closed
      CHARACTER( 1*BYTE)              :: DO_IT             ! If 'Y' execute some code
      CHARACTER( 1*BYTE)              :: MIJ_COL_FOUND='N' ! 'Y' if MIJ_ROW is processed as a solution vector in this LINK
      CHARACTER( 1*BYTE)              :: MIJ_ROW_FOUND='N' ! 'Y' if MIJ_ROW is processed as a solution vector in this LINK
      CHARACTER(54*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run
      CHARACTER( 1*BYTE)              :: READ_NTERM        ! 'Y' or 'N' Input to subr READ_MATRIX_1 
      CHARACTER( 1*BYTE)              :: OPND              ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to open  a file or not 
      CHARACTER( 1*BYTE)              :: READ_UO0          ! If 'Y' then read UO0 data from file L2F
 
      INTEGER(LONG)                   :: COL_NUM           ! Arg passed to subr BUILD_A_LR
      INTEGER(LONG)                   :: EIG_NORM_GSET_DOF ! A-set DOF no. for EIG_GRID/EIG_COMP 
      INTEGER(LONG)                   :: EIGNORM2_ERR        ! Error indicator for reading param EIGNORM2 data
      INTEGER(LONG)                   :: G_SET_COL         ! Col number in TDOF, TDOFI where G-set DOF's exist
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Error count
      INTEGER(LONG)                   :: IGRID             ! Internal grid numbER for EIG_GRID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: NUM_COMPS         ! 6 if GRID_NUM is an physical grid, 1 if an SPOINT
      INTEGER(LONG)                   :: NUM_SOLNS    = 0  ! No. of solutions to process (e.g. NSUB for STATICS)
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: P_LINKNO          ! Prior LINK no's that should have run before this LINK can execute
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file

      REAL(DOUBLE)                    :: MIJ_COL_SCALE=ZERO! Scale fac for a col of gen mass matrix to renorm MAXMIJ from LINK4
      REAL(DOUBLE)                    :: MIJ_ROW_SCALE=ZERO! Scale fac for a col of gen mass matrix to renorm MAXMIJ from LINK4
      REAL(DOUBLE)                    :: PHI_SCALE_FAC     ! Scale factor that the eigenvector was renormalized to in subr RENORM

! **********************************************************************************************************************************
      LINKNO = 5

! Set time initializing parameters

      CALL TIME_INIT

! Initialize WRT_BUG

      DO I=0,MBUG-1
         WRT_BUG(I) = 0
      ENDDO

! Get date and time, write to screen

      CALL OURDAT
      CALL OURTIM
      WRITE(SC1,152) LINKNO

! Make units for writing errors the screen until we open output files

      OUNT(1) = SC1
      OUNT(2) = SC1

! Make units for writing errors the error file and output file

      OUNT(1) = ERR
      OUNT(2) = F06

! Write info to text files
  
      WRITE(F06,150) LINKNO
      IF (WRT_LOG > 0) THEN
         WRITE(F04,150) LINKNO
      ENDIF
      WRITE(ERR,150) LINKNO

! Read LINK1A file
 
      CALL READ_L1A ( 'KEEP', 'Y' )
! Check COMM for successful completion of prior LINKs

      IF (SOL_NAME(1:7) == 'STATICS') THEN

         P_LINKNO = 3
         IF (COMM(P_LINKNO) /= 'C') THEN
            WRITE(SC1,9998) P_LINKNO,P_LINKNO,LINKNO
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

         P_LINKNO = 4
         IF (COMM(P_LINKNO) /= 'C') THEN
            WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
            WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN

         P_LINKNO = 6
         IF (COMM(P_LINKNO) /= 'C') THEN
            WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
            WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ELSE IF (SOL_NAME(1:8) == 'BUCKLING') THEN

         IF      (LOAD_ISTEP == 1) THEN
            P_LINKNO = 3
            IF (COMM(P_LINKNO) /= 'C') THEN
               WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
               WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF
         ELSE IF (LOAD_ISTEP == 2) THEN
            P_LINKNO = 4
            IF (COMM(P_LINKNO) /= 'C') THEN
               WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
               WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,5002) SUBR_NAME, LOAD_ISTEP
            WRITE(F06,5002) SUBR_NAME, LOAD_ISTEP
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ELSE IF (SOL_NAME(1:8) == 'NLSTATIC') THEN

         P_LINKNO = 3
         IF (COMM(P_LINKNO) /= 'C') THEN
            WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
            WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ELSE

         WRITE(ERR,5001) SOL_NAME
         WRITE(F06,5001) SOL_NAME
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

! **********************************************************************************************************************************
! Allocate arrays for YSe, GMN, GOA

      CALL OURTIM
      MODNAM = 'ALLOCATE SEVERAL ARRAYS'
      WRITE(SC1,5092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL ALLOCATE_SPARSE_MAT ( 'GMN', NDOFM, NTERM_GMN, SUBR_NAME )
      CALL ALLOCATE_SPARSE_MAT ( 'GOA', NDOFO, NTERM_GOA, SUBR_NAME )
      CALL ALLOCATE_COL_VEC ( 'YSe' , NDOFS, SUBR_NAME )
 
! Read GMN matrix if there are MPC's

      IF (NTERM_GMN > 0) THEN

         CALL OURTIM
         MODNAM = 'READ GMN MATRIX'
         WRITE(SC1,5092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         READ_NTERM = 'Y'
         OPND       = 'N'
         CLOSE_IT   = 'Y'
         CALL READ_MATRIX_1 ( LINK2A, L2A, OPND, CLOSE_IT, 'KEEP', L2A_MSG, 'GMN', NTERM_GMN, READ_NTERM, NDOFM                    &
                            , I_GMN, J_GMN, GMN )
      ENDIF 

! Read GOA matrix if there are omitted DOFs

      IF (NTERM_GOA > 0) THEN
         CALL OURTIM
         MODNAM = 'READ GOA MATRIX'
         WRITE(SC1,5092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         READ_NTERM = 'Y'
         OPND       = 'N'
         CLOSE_IT   = 'Y'
!        CLOSE_STAT = L2ESTAT
         CLOSE_STAT = 'KEEP'
         CALL READ_MATRIX_1 ( LINK2E, L2E, OPND, CLOSE_IT, CLOSE_STAT, L2E_MSG, 'GOA', NTERM_GOA, READ_NTERM, NDOFO                &
                            , I_GOA, J_GOA, GOA )
      ENDIF

! Read enforced displ's

      IF (NDOFSE > 0) THEN

         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN

            CALL FILE_OPEN ( L1H, LINK1H, OUNT, 'OLD', L1H_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

            CALL OURTIM
            MODNAM = 'READ YSe ENFORCED DISPLACEMENTS'
            WRITE(SC1,5092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

            IERROR = 0
            DO I=1,NDOFSE
               READ(L1H,IOSTAT=IOCHK) YSe(I)
               IF (IOCHK /= 0) THEN
                  IERROR = IERROR + 1
                  REC_NO = I
                  CALL READERR ( IOCHK, LINK1H, L1H_MSG, REC_NO, OUNT, 'Y' )
               ENDIF
            ENDDO
            IF (IERROR /= 0) THEN
               WRITE(ERR,9995) LINKNO,IERROR
               WRITE(F06,9995) LINKNO,IERROR
               CALL OUTA_HERE ( 'Y' )
            ENDIF

            CALL FILE_CLOSE ( L1H, LINK1H, L1HSTAT, 'Y' )

         ENDIF

      ENDIF

! Read eigenvalue data from file L1M if this is an eigenvalue problem. Check to see if user wants to change sign of any vector.
! EIGEN_VAL was not deallocated in LINK4 (see LINK4 comment 01/11/19) so we do not allocate it here anymore

      DO_IT = 'N'
      IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
         DO_IT = 'Y'
      ENDIF
      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         DO_IT = 'Y'
      ENDIF

      IF (DO_IT == 'Y') THEN
         IERROR = 0
         CALL ALLOCATE_EIGEN1_MAT ( 'MODE_NUM' , NUM_EIGENS, 1, SUBR_NAME ) 
!xx      CALL ALLOCATE_EIGEN1_MAT ( 'EIGEN_VAL', NUM_EIGENS, 1, SUBR_NAME ) 
         CALL ALLOCATE_EIGEN1_MAT ( 'GEN_MASS' , NUM_EIGENS, 1, SUBR_NAME ) 
         CALL READ_L1M ( IERROR )

         IF (IERROR /= 0) THEN
            WRITE(ERR,9995) LINKNO,IERROR
            WRITE(F06,9995) LINKNO,IERROR
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ENDIF

! Open file that has L-set displs, or eigenvectors ('MODES') or PHIZL ('GEN CB MODEL')

      IF (NDOFL > 0) THEN
         CALL FILE_OPEN ( L3A, LINK3A, OUNT, 'OLD', L3A_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

! Open file for writing displs to.
 
      CALL FILE_CLOSE ( L5A, LINK5A, 'KEEP', 'Y' )
      CALL FILE_OPEN  ( L5A, LINK5A, OUNT, 'REPLACE', L5A_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
 
! Open file that has UO0

      IF (NTERM_PO > 0) THEN
         CALL FILE_OPEN ( L2F, LINK2F, OUNT, 'OLD', L2F_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

! Set NUM_SOLNS for use in loop (below) to get outputs for each subcase/solution vector

      IF      ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
         NUM_SOLNS = NSUB
      ELSE IF (SOL_NAME(1:5) == 'MODES') THEN
         NUM_SOLNS = NVEC
      ELSE IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         IF (LOAD_ISTEP == 1) THEN
            NUM_SOLNS = 1
         ELSE IF (LOAD_ISTEP == 2) THEN
            NUM_SOLNS = NVEC
         ENDIF
      ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
         NUM_SOLNS = NUM_CB_DOFS
      ENDIF

! Allocate memory to EIGEN_VEC so that we can write the G-set eigenvectors to it for possible OUTPUT4 later

      IF (SOL_NAME(1:5) == 'MODES') THEN
         CALL ALLOCATE_EIGEN1_MAT ( 'EIGEN_VEC', NDOFG, NUM_SOLNS, SUBR_NAME )
      ENDIF

! Allocate memory for CB matrix PHIZG_FULL

      IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
         CALL ALLOCATE_FULL_MAT ( 'PHIZG_FULL', NDOFG, NUM_CB_DOFS, SUBR_NAME )
      ENDIF

! Check for renorm = MAX or POINT.

      DO_IT = 'N'
      IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
         DO_IT = 'Y'
      ENDIF
      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         DO_IT = 'Y'
      ENDIF

      IF (DO_IT == 'Y') THEN
         EIG_NORM_GSET_DOF = 0
         IGRID = 0
         IF (EIG_NORM == 'POINT   ') THEN                  ! User requested to renormalize eigenvectors on POINT 
  
            CALL TDOF_COL_NUM ( 'G ',  G_SET_COL )
i_do:       DO I=1,NDOFG
               IF (TDOF(I,1) == EIG_GRID) THEN
                  IGRID = TDOF(I,3)
                  EIG_NORM_GSET_DOF = TDOF(I,G_SET_COL) + EIG_COMP - 1
                  EXIT i_do
               ENDIF
            ENDDO i_do 

            IF (IGRID > 0) THEN
               WRITE(ERR,5102) EIG_GRID, EIG_COMP
               IF (SUPINFO == 'N') THEN
                  WRITE(F06,5102) EIG_GRID, EIG_COMP
               ENDIF
            ELSE
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,5111) EIG_GRID
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,5111) EIG_GRID
               ENDIF
               PHI_SCALE_FAC = ONE
            ENDIF

         ELSE IF (EIG_NORM == 'MAX     ') THEN             ! User requested to renormalize eigenvectors on MAX

            WRITE(ERR,5103)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,5103)
            ENDIF

         ELSE                                              ! No renorm needed here (was done in LINK4 if not POINT or MASS)

            WRITE(ERR,5104) 
            IF (SUPINFO == 'N') THEN
               WRITE(F06,5104) 
            ENDIF
  
         ENDIF
         
      ENDIF

      IF ((EIGNORM2 == 'Y') .AND. (NVEC > 0)) THEN
         CALL READ_EIGNORM2
      ENDIF

! **********************************************************************************************************************************
      READ_UO0 = 'Y'
      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         READ_UO0 = 'N'
      ENDIF

! Begin loop for reading L-set displs and building up to G-set displs one subcase/solution vector at a time

j_do: DO J = 1,NUM_SOLNS

         CALL ALLOCATE_COL_VEC ('UL_COL', NDOFL, SUBR_NAME)! Allocate array UL_COL

         CALL OURTIM                                       ! Read UL displs for the current subcase/vector from LINK3A 
         IF     ((SOL_NAME(1: 7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
            MODNAM = 'READ  L-SET DISPLACEMENTS                      Subcase'
         ELSE IF (SOL_NAME(1: 5) == 'MODES') THEN
            MODNAM = 'READ  L-SET EIGENVECTORS                       Vector'
         ELSE IF (SOL_NAME(1: 8) == 'BUCKLING') THEN
            IF (LOAD_ISTEP == 1) THEN
               MODNAM = 'READ  L-SET DISPLACEMENTS                      Subcase'
            ELSE IF (LOAD_ISTEP == 2) THEN
               MODNAM = 'READ  L-SET EIGENVECTORS                       Vector'
            ENDIF
         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
            MODNAM = 'READ  L-SET CB VECTORS (PHIZL)                 CB vec'
         ENDIF
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC

         REC_NO = 0
         IERROR = 0
         DO I=1,NDOFL
            REC_NO = REC_NO + 1
            READ(L3A,IOSTAT=IOCHK) UL_COL(I)               ! For CB, a col of PHIZL. So UG_COL, calc'd in this subr, is a PHIZG col
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK3A, L3A_MSG, REC_NO, OUNT, 'Y' )
               IERROR = IERROR + 1
            ENDIF
         ENDDO

         IF (IERROR /= 0) THEN
            WRITE(ERR,9995) LINKNO,IERROR
            WRITE(F06,9995) LINKNO,IERROR
            CALL OUTA_HERE ( 'Y' )
         ENDIF
                                                           ! Build UA from UL and UR
         CALL ALLOCATE_COL_VEC ( 'UA_COL', NDOFA, SUBR_NAME )
         CALL ALLOCATE_COL_VEC ( 'UR_COL', NDOFR, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UA DISPLS FROM UL, UR:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         COL_NUM = 0
         IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
            IF ((J > NDOFR+nvec) .AND. (J <= NUM_CB_DOFS)) THEN
               COL_NUM = J
            ENDIF
         ENDIF
         CALL BUILD_A_LR ( COL_NUM )
         CALL DEALLOCATE_COL_VEC ( 'UL_COL' )
         CALL DEALLOCATE_COL_VEC ( 'UR_COL' )

                                                           ! Solve for UO and Build UF from UA and UO
         CALL ALLOCATE_COL_VEC ( 'UF_COL' , NDOFF, SUBR_NAME )
         CALL ALLOCATE_COL_VEC ( 'UO_COL' , NDOFO, SUBR_NAME )
         CALL ALLOCATE_COL_VEC ( 'UO0_COL', NDOFO, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UF DISPLS FROM UA, UO:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         IF (READ_UO0 == 'Y') THEN
            IF (NDOFO > 0) THEN
               IF (NTERM_PO > 0) THEN      
                  CALL OURTIM
                  MODNAM = '  READ UO0 DISPLS,                                "'
                  WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC 
    
                  IERROR = 0
                  DO I=1,NDOFO
                     READ(L2F,IOSTAT=IOCHK) UO0_COL(I)
                     IF (IOCHK /= 0) THEN
                        REC_NO = I+1
                        CALL READERR ( IOCHK, LINK2F, L2F_MSG, REC_NO, OUNT, 'Y' )
                        IERROR = IERROR + 1
                     ENDIF
                  ENDDO
                  IF (IERROR /= 0) THEN
                     WRITE(ERR,9995) LINKNO,IERROR
                     WRITE(F06,9995) LINKNO,IERROR
                     CALL OUTA_HERE ( 'Y' )
                  ENDIF
               ELSE
                  DO I=1,NDOFO
                     UO0_COL(I) = ZERO
                  ENDDO 
               ENDIF
            ENDIF
         ENDIF
         CALL BUILD_F_AO
         CALL DEALLOCATE_COL_VEC ( 'UA_COL' )
         CALL DEALLOCATE_COL_VEC ( 'UO_COL' )
         CALL DEALLOCATE_COL_VEC ( 'UO0_COL' )
                                                           ! Build UN from UF and US
         CALL ALLOCATE_COL_VEC ( 'UN_COL', NDOFN , SUBR_NAME)
         CALL ALLOCATE_COL_VEC ( 'US_COL', NDOFS, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UN DISPLS FROM UF, US:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         CALL BUILD_N_FS
         CALL DEALLOCATE_COL_VEC ( 'UF_COL' )
         CALL DEALLOCATE_COL_VEC ( 'US_COL' )
                                                           ! Build UG from UN and UM
         CALL DEALLOCATE_COL_VEC ( 'UG_COL' )
         CALL ALLOCATE_COL_VEC ( 'UG_COL', NDOFG, SUBR_NAME )
         CALL ALLOCATE_COL_VEC ( 'UM_COL', NDOFM, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UG DISPLS FROM UN, UM:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         CALL BUILD_G_NM
         CALL DEALLOCATE_COL_VEC ( 'UN_COL' )
         CALL DEALLOCATE_COL_VEC ( 'UM_COL' )

         IERROR = 0
!xx      IF ((SOL_NAME(1:8) == 'DIFFEREN') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
            IF (ALLOCATED(UG_T123_MAT)) THEN
               CALL DEALLOCATE_MISC_MAT ( 'UG_T123_MAT' )
            ENDIF
            CALL ALLOCATE_MISC_MAT ( 'UG_T123_MAT', NGRID, 3, SUBR_NAME )
            CALL GET_UG_123_IN_GRD_ORD ( IERROR )
!xx      ENDIF
         IF (IERROR /= 0) THEN
            WRITE(ERR,9995) LINKNO,IERROR
            WRITE(F06,9995) LINKNO,IERROR
            CALL OUTA_HERE ( 'Y' )
         ENDIF

         IF      (SOL_NAME(1: 5) == 'MODES'       ) THEN   ! For modes, write UG_COL to jth col of EIGEN_VEC (for later OUTPUT4)
            DO I=1,NDOFG
               EIGEN_VEC(I,J)  = UG_COL(I)
            ENDDO
         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! For CB   , write UG_COL to jth col of PHiZG     (for later OUTPUT4)
            DO I=1,NDOFG
               PHIZG_FULL(I,J) = UG_COL(I)
            ENDDO
         ENDIF
                                                           ! Renorm eigenvecs, if requested
         IF (J <= NVEC) THEN

            DO_IT = 'N'
            IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
               DO_IT = 'Y'
            ENDIF
            IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
               DO_IT = 'Y'
            ENDIF

            IF (DO_IT == 'Y') THEN
               IF ((EIG_NORM == 'POINT   ') .OR. (EIG_NORM == 'MAX     ')) THEN
                  CALL RENORM (J, EIG_GRID, EIG_COMP, EIG_NORM, EIG_NORM_GSET_DOF, GEN_MASS(J), PHI_SCALE_FAC )
                  IF      (J == MIJ_ROW) THEN
                     MIJ_ROW_FOUND = 'Y'
                     MIJ_ROW_SCALE = PHI_SCALE_FAC
                  ELSE IF (J == MIJ_COL) THEN
                     MIJ_COL_FOUND = 'Y'
                     MIJ_COL_SCALE = PHI_SCALE_FAC
                  ENDIF
                  CALL WRITE_L1M                           ! Need to update GEN_MASS in L1M if vecs are renorm'd here
               ENDIF

            ENDIF

         ENDIF
                                                           ! See if user requested eigenvector sign changes
         IF ((EIGNORM2 == 'Y') .AND. (EIGNORM2_ERR == 0)) THEN
            IF (VEC_SIGN_CHG(J)) THEN
               DO I=1,NDOFG
                  UG_COL(I) = -UG_COL(I)
               ENDDO
            ENDIF
         ENDIF

         CALL OURTIM                                       ! Write UG displs for this subcase to file LINK5A
         MODNAM = 'WRITE UG DISPLS TO FILE,                          "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         WRITE(SC1, * )                                    ! Separator between UG_COL calcs
         DO I=1,NDOFG
            WRITE(L5A) UG_COL(I)                           ! For CB this is a col of PHIZG (which is never processed as an array)
         ENDDO

      ENDDO j_do                                           ! End of loop on NUM_SOLNS

! If CB soln, expand PHIXA to G-set size and write to file unit L5B

     IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                                                           ! Open file for writing cols of PHIXG
         CALL FILE_OPEN ( L5B, LINK5B, OUNT, 'REPLACE', L5B_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
 
         CALL DEALLOCATE_COL_VEC ( 'UG_COL' )
         CALL EXPAND_PHIXA_TO_PHIXG                        ! Expand PHIXA to PHIXG and write cols to file L5B
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PHIXA', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PHIXA' )

      ENDIF

! Code to write PHIZG in full format

      IF ((DEBUG(55) == 2) .OR. (DEBUG(55) == 3)) THEN
         IF (SOL_NAME == 'GEN CB MODEL') THEN
            WRITE(F06,*)
            WRITE(F06,99885)
            WRITE(F06,99886) (J,J=1,NUM_CB_DOFS)
            L = 0
            DO I=1,NGRID
               CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
               DO K=1,NUM_COMPS
                  L = L + 1
                  IF (K == 1) THEN
                     WRITE(F06,99887) TDOFI(L,1), K, (PHIZG_FULL(L,J),J=1,NUM_CB_DOFS)
                  ELSE
                     WRITE(F06,99888)             K, (PHIZG_FULL(L,J),J=1,NUM_CB_DOFS)
                  ENDIF
               ENDDO
               WRITE(F06,*)
            ENDDO
            WRITE(F06,*)
         ENDIF
      ENDIF

! **********************************************************************************************************************************
! Write eigen analysis summary if we have renormed vectors here in LINK5

      DO_IT = 'N'
      IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
         DO_IT = 'Y'
      ENDIF
      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         DO_IT = 'Y'
      ENDIF

      IF (DO_IT == 'Y') THEN
         IF ((EIG_NORM == 'POINT   ') .OR. (EIG_NORM == 'MAX     ')) THEN

            IF ((MIJ_ROW_FOUND == 'Y') .AND. (MIJ_COL_FOUND == 'Y')) THEN
               MAXMIJ = MAXMIJ/(MIJ_ROW_SCALE * MIJ_COL_SCALE)
            ELSE
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,5101) LINKNO
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,5101) LINKNO
               ENDIF
            ENDIF

            CALL EIG_SUMMARY
         ENDIF
      ENDIF

! Close files

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN
         CLOSE_STAT = 'KEEP'
      ELSE
!xx      CLOSE_STAT = L2FSTAT
         CLOSE_STAT = 'KEEP'
      ENDIF
      CALL FILE_CLOSE ( L2F, LINK2F, CLOSE_STAT, 'Y' )

      IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
         CALL FILE_CLOSE ( L3A, LINK3A, 'KEEP', 'Y' )
      ELSE
         CALL FILE_CLOSE ( L3A, LINK3A, L3ASTAT, 'Y' )
      ENDIF

      CALL FILE_CLOSE ( L5A, LINK5A, 'KEEP', 'Y' )
      CALL FILE_CLOSE ( L5B, LINK5B, 'KEEP', 'Y' )

! Call OUTPUT4 processor to process output requests for OUTPUT4 matrices generated in this link

      IF (NUM_OU4_REQUESTS > 0) THEN
         CALL OURTIM
         MODNAM = 'WRITE OUTPUT4 NATRICES      '
         WRITE(SC1,5092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         WRITE(F06,*)
         CALL OUTPUT4_PROC ( SUBR_NAME )
      ENDIF

! Deallocate arrays (except EIGEN_VAL, may be needed later)

!xx   WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
      WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GMN      ', CR13  ;   CALL DEALLOCATE_SPARSE_MAT ( 'GMN' )
      WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GOA      ', CR13  ;   CALL DEALLOCATE_SPARSE_MAT ( 'GOA' )
      WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MODE_NUM ', CR13  ;   CALL DEALLOCATE_EIGEN1_MAT ( 'MODE_NUM' ) 
!xx   WRITE(SC1,12345,ADVANCE='NO') '       Deallocate EIGEN_VAL', CR13  ;   CALL DEALLOCATE_EIGEN1_MAT ( 'EIGEN_VAL' ) 
      WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GEN_MASS ', CR13  ;   CALL DEALLOCATE_EIGEN1_MAT ( 'GEN_MASS' )
      CALL DEALLOCATE_COL_VEC    ( 'YSe' )
      CALL DEALLOCATE_EIGEN1_MAT ( 'EIGEN_VEC' )  
 
! Process is now complete so set COMM(LINKNO)
  
      COMM(LINKNO) = 'C'

! Write data to L1A
      CALL WRITE_L1A ( 'KEEP', 'Y', 'Y' )
  
! Check allocation status of allocatable arrays, if requested

      IF (DEBUG(100) > 0) THEN
         CALL CHK_ARRAY_ALLOC_STAT
         IF (DEBUG(100) > 1) THEN
            CALL WRITE_ALLOC_MEM_TABLE ( 'at the end of '//SUBR_NAME )
         ENDIF
      ENDIF

! Write LINK5 end to F04, F06

      CALL OURTIM
      IF (WRT_LOG > 0) THEN
         WRITE(F04,151) LINKNO
      ENDIF
      WRITE(F06,151) LINKNO

! Close files
  
      IF (( DEBUG(193) == 5) .OR. (DEBUG(193) == 999)) THEN
         CALL FILE_INQUIRE ( 'near end of LINK5' )
      ENDIF

! Write LINK5 end to screen
      WRITE(SC1,153) LINKNO

! **********************************************************************************************************************************
  101 format(32767(1es22.14))

  150 FORMAT(/,' >> LINK',I3,' BEGIN',/)

  151 FORMAT(/,' >> LINK',I3,' END',/)

  152 FORMAT(/,' >> LINK',I3,' BEGIN')

  153 FORMAT(  ' >> LINK',I3,' END')

 5001 FORMAT(' *ERROR  5001: INVALID SOLUTION NAME: SOL = ',A)

 5002 FORMAT(' *ERROR  5002: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                     ,/,14X,'VARIABLE LOAD_ISTEP MUST BE 1 OR 2 BUT VALUE IS = ',I8)

 5092 FORMAT(1X,I2,'/',A54,8X,2X,I2,':',I2,':',I2,'.',I3)

 5093 FORMAT(1X,I2,'/',A54,I8,2X,I2,':',I2,':',I2,'.',I3)

 5094 FORMAT(/,' >> LINK',I2,' END',19X,I2,':',I2,':',I2,'.',I3,/)

 5101 FORMAT(' *WARNING    : THE LARGEST OFF-DIAGONAL GENERALIZED MASS TERM REPORTED IN THE EIGENVALUE ANALYSIS SUMMARY CANNOT BE' &
                    ,/,14X,' RESCALED IN LINK ',I2,' BASED ON THE EIGENVALUE RENORMALIZATION REQUESTED.'                           &
                    ,/,14X,' IT IS RENORMALIZED BASED ON UNIT GENERALIZED MASS',/)

 5102 FORMAT(' *INFORMATION: ATTEMPTING TO RENORMALIZE EIGENVECTORS BASED ON GRID POINT-COMPONENT ',2I8                            &
                    ,/,14X,' THE RENORMALIZATION WILL BE DONE FOR ALL EIGENVECTORS WHOSE AMPLITUDE IS NONZERO AT THIS DOF',/)

 5103 FORMAT(' *INFORMATION: EIGENVECTORS WILL BE RENORMALIZED BASED ON MAX VALUE = 1.0',/)

 5104 FORMAT(' *INFORMATION: EIGENVECTORS WERE NORMALIZED TO MASS IN LINK4',/)

 5111 FORMAT(' *WARNING    : REQUEST TO NORMALIZE EIGENVECTORS BASED ON GRID POINT ',I8,', HOWEVER, THIS NOT A VALID GRID POINT.'  &
                    ,/,14X,' EIGENVECTORS WILL NOT BE RENORMALIZED',/)

 9995 FORMAT(/,' PROCESSING ENDED IN LINK ',I3,' DUE TO ABOVE ',I8,' ERRORS')

 9998 FORMAT(' *ERROR  9998: COMM ',I3,' INDICATES UNSUCCESSFUL LINK ',I2,' COMPLETION.'                                           &
                    ,/,14X,' FATAL ERROR - CANNOT START LINK ',I2)

98001 FORMAT(41X,'R E A L   E I G E N V A L U E S')

98002 FORMAT(82X,'BEFORE RENORMALIZATION OF EIGENVECTORS')

98003 FORMAT(3X,' MODE  EXTRACTION     EIGENVALUE            RADIANS              CYCLES            GENERALIZED         GENERALIZED&
&        ',/,3X,'NUMBER   ORDER                                                                        MASS              STIFFNESS'&
          ,/)

98004 FORMAT(1X,2I8,5(1ES20.6))

99885 FORMAT(82X,'MATRIX PHIZG',/,82X,'------------')                                                                             

99886 FORMAT(5X,32676(I14))                                                                                                       

99887 FORMAT(I8,'-',I1,32767(1ES14.6))                                                                                            

99888 FORMAT(8X,'-',I1,32767(1ES14.6))                                                                                            

12345 FORMAT(A,10X,A)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE READ_EIGNORM2

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  EIN, EINFIL, ERR, F06
      USE SCONTR, ONLY                :  IERRFL, JCARD_LEN

      IMPLICIT NONE

      LOGICAL                         :: FILE_EXIST      

      CHARACTER(JCARD_LEN*BYTE)       :: DATA_FIELD
      CHARACTER(80*BYTE)              :: TITLE             ! First record in EINFIL

      INTEGER(LONG)                   :: IOCHK             ! Vaue of IOSTAT in file open
      INTEGER(LONG)                   :: NUM_CHANGED       ! Number of eigenvectors to have their sign changed
      INTEGER(LONG)                   :: REC_NUM           ! Number of the record read from file
      INTEGER(LONG)                   :: VEC_NUM           ! Number of a eigenvector read from EINFIL
      INTEGER(LONG)                   :: VECS_CHANGED(NVEC)! Numbers of eigenvectors to have their sign changed

! **********************************************************************************************************************************
      DO I=1,NDOFL
         VEC_SIGN_CHG(I) = .FALSE.
      ENDDO

      EIGNORM2_ERR = 0
      INQUIRE ( FILE=EINFIL, EXIST=FILE_EXIST )

      IF (FILE_EXIST) THEN

         EIGNORM2_ERR = 0

         OPEN (EIN, FILE=EINFIL, STATUS='OLD', IOSTAT=IOCHK)

         IF (IOCHK == 0) THEN                              ! File was opened successfully

            WRITE(ERR,*)
            WRITE(ERR,5996) EIGNORM2
            IF (SUPINFO == 'N') THEN
               WRITE(F06,*)
               WRITE(F06,5996) EIGNORM2
            ENDIF
            CALL WRITE_FILNAM ( EINFIL, F06, 15 )
            READ(EIN,'(A)') TITLE

            NUM_CHANGED = 0

            REC_NUM = 0
            DO

               READ(EIN,'(A)',IOSTAT=IOCHK) DATA_FIELD   ;   REC_NUM = REC_NUM + 1

               IF (IOCHK == 0) THEN

                  IF (DATA_FIELD(1:3) == 'END') EXIT

                  CALL I4FLD (DATA_FIELD(1:JCARD_LEN), 1, VEC_NUM )
                  IF (IERRFL(1) == 'N') THEN
                     IF ((VEC_NUM >= 1) .AND. (VEC_NUM <= NVEC)) THEN
                        VEC_SIGN_CHG(VEC_NUM) = .TRUE.
                        NUM_CHANGED = NUM_CHANGED + 1
                        VECS_CHANGED(NUM_CHANGED) = VEC_NUM
                     ELSE
                        WRITE(ERR,5992) REC_NUM, DATA_FIELD, NVEC
                        WRITE(F06,5992) REC_NUM, DATA_FIELD, NVEC
                     ENDIF
                  ELSE
                     WRITE(ERR,5995) REC_NUM, DATA_FIELD
                     WRITE(F06,5995) REC_NUM, DATA_FIELD
                     EXIT
                  ENDIF

                  CYCLE

               ELSE

                  WRITE(ERR,5991)
                  CALL WRITE_FILNAM ( EINFIL, ERR, 15 )
                  WRITE(ERR,59911)

                  WRITE(F06,5991)
                  CALL WRITE_FILNAM ( EINFIL, F06, 15 )
                  WRITE(F06,59911)

                  EIGNORM2_ERR = EIGNORM2_ERR + 1
                  EXIT

               ENDIF

               IF (REC_NUM > NDOFL) THEN
                  EIGNORM2_ERR = EIGNORM2_ERR + 1
                  WRITE(ERR,5998) EINFIL, REC_NUM, NVEC
                  WRITE(F06,5998) EINFIL, REC_NUM, NVEC
               ENDIF

            ENDDO

         ELSE                                              ! File not opened successfully, so no vecs will have signs changed

            WRITE(ERR,5997)
            CALL WRITE_FILNAM ( EINFIL, ERR, 15 )
            WRITE(ERR,59971)

            WRITE(F06,5997)
            CALL WRITE_FILNAM ( EINFIL, F06, 15 )
            WRITE(F06,59971)

            EIGNORM2_ERR = EIGNORM2_ERR + 1

         ENDIF

      ELSE

         WRITE(ERR,5990)
         CALL WRITE_FILNAM ( EINFIL, ERR, 15 )
         WRITE(ERR,59901)

         WRITE(F06,5990)
         CALL WRITE_FILNAM ( EINFIL, F06, 15 )
         WRITE(F06,59901)

         EIGNORM2_ERR = EIGNORM2_ERR + 1

      ENDIF

      IF (EIGNORM2_ERR == 0) THEN
         WRITE(ERR,*)
         WRITE(ERR,5993)
         WRITE(ERR,5994) (VECS_CHANGED(I),I=1,NUM_CHANGED)
         WRITE(ERR,*)
         IF (SUPINFO == 'N') THEN
            WRITE(F06,*)
            WRITE(F06,5993)
            WRITE(F06,5994) (VECS_CHANGED(I),I=1,NUM_CHANGED)
            WRITE(F06,*)
         ENDIF
      ELSE
         WRITE(F06,5999)
         CALL WRITE_FILNAM ( EINFIL, F06, 15 )
      ENDIF

      RETURN

! **********************************************************************************************************************************
 5990 FORMAT(' *WARNING    : PARAMETER EIGNORM2 REQUESTS THAT SOME EIGENVECTORS ARE TO HAVE THEIR SIGN CHANGED, BUT FILE:')

59901 FORMAT('               WHICH SHOULD HAVE THE NUMBERS OF THE VECTORS TO HAVE THEIR SIGN CHANGED CANNOT BE FOUND')

 5991 FORMAT(' *WARNING    : EOF/ERR READING FILE:')

59911 FORMAT('               ON RECORD NUMBER ',I8,'. FIELD THAT HAS ERROR = "',A,'"')

 5992 FORMAT(' *WARNING    : ON REC NUM ',I8,' EIGENVEC ',A,' FOR SIGN CHANGE WAS OUT OF RANGE.',                                  &
                           ' MUST BE 1 TO NVEC = ',I8,'. VALUE IGNORED')

 5993 FORMAT(' *INFORMATION: THE FOLLOWING EIGENVECTORS HAVE HAD THEIR SIGN CHANGED:')

 5994 FORMAT(10I8)

 5995 FORMAT(' *WARNING    : ERROR READING RECORD NUMBER ',I8,' = ',A,'. REMAINDER OF EIGENVECTOR SIGN CHANGE FILE IGNORED')

 5996 FORMAT(' *INFORMATION: BASED ON PARAMETER EIGNORM2 = ',A,' SOME EIGENVECS WILL HAVE THEIR SIGN CHANGED AS DEFINED IN FILE:')

 5997 FORMAT(' *WARNING    : PARAMETER EIGNORM2 REQUESTS THAT SOME EIGENVECTORS ARE TO HAVE THEIR SIGN CHANGED, BUT FILE:')

59971 FORMAT('               WHICH SHOULD HAVE THE NUMBERS OF THE VECTORS TO HAVE THEIR SIGN CHANGED COULD NOT BE OPENED')

 5998 FORMAT(' *WARNING    : ERROR READING FILE: ',A                                                                               &
                    ,/,14X,' ATTEMPTING TO READ RECORD NUMBER ',I8,' BUT THERE SHOULD ONLY BE NVEC = ',I8,' RECORDS')

 5999 FORMAT(' *WARNING    : NO EIGENVECTORS WILL HAVE THEIR SIGN CHANGED DUE TO ABOVE ERRORS READING FILE:')

! **********************************************************************************************************************************

      END SUBROUTINE READ_EIGNORM2

      END SUBROUTINE LINK5
