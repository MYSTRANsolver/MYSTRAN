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

      SUBROUTINE GP_FORCE_BALANCE_PROC ( JVEC, IHDR )

! Processes grid point force balance output requests for one subcase. The effects of all forces on grids are included so totals
! should be zero

      USE PENTIUM_II_KIND, ONLY       :  BYTE, SHORT, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ANS, ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, GROUT_GPFO_BIT, IBIT, INT_SC_NUM, JTSUB, NDOFG, NDOFM, MELDOF, NDOFO, NDOFR,&
                                         NELE, NGRID, NUM_CB_DOFS, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GP_FORCE_BALANCE_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE_HUNDRED
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  AGRID, EID, ELGP, ESORT1, ETYPE, NUM_EMG_FATAL_ERRS, GRID, GRID_ELEM_CONN_ARRAY, GRID_ID, &
                                         GROUT, LABEL, PLY_NUM, PEG, PTE, SCNUM, STITLE, SUBLOD, TITLE, TYPE
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY
      USE COL_VECS, ONLY              :  FG_COL, PG_COL, QGm_COL, QGs_COL, QGr_COL, UG_COL
      USE PARAMS, ONLY                :  EPSIL

      USE GP_FORCE_BALANCE_PROC_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GP_FORCE_BALANCE_PROC'
      CHARACTER(13*BYTE)              :: CHAR_PCT(6)       ! Character representation of MEFFMASS sum percents of total model mass
      CHARACTER(LEN=*) , INTENT(IN)   :: IHDR              ! Indicator of whether to write an output header
      CHARACTER( 1*BYTE), PARAMETER   :: COORD_SYS   = 'G' ! Subr TRANSFORM_NODE_FORCES will calc elem node forces in global coords
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr ELMTLB (to tell it what to transform)
      CHARACTER(128*BYTE)             :: TITLEI, STITLEI, LABELI ! title, subtitle, label

      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG)                   :: AELEM             ! Actual element ID
      INTEGER(LONG)                   :: BNDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BNDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BNDY_DOF_NUM       ! DOF number for BNDY_GRID/BNDY_COMP
      INTEGER(LONG)                   :: BEG_ROW_NUM       ! Row num in PEG where we begin to get the 6 values of elem load KE*UE
      INTEGER(LONG)                   :: G_CID             ! Global coord system for GRID_NUM
      INTEGER(LONG)                   :: GRID_NUM          ! Actual grid point number
      INTEGER(LONG)                   :: G_SET_COL         ! Col number in TDOF where the G-set DOF's exist
      INTEGER(LONG)                   :: GDOF              ! G-set DOF number
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices
      INTEGER(LONG)                   :: IB                ! Intermediate value used in determining NREQ
      INTEGER(LONG)                   :: IELE              ! Internal element ID
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: NREQ              ! Number of grids for which G.P. force balance is requested
      INTEGER(LONG)                   :: NUM_COMPS         ! Either 6 or 1 depending on whether grid is a physical grid or a SPOINT
      INTEGER(LONG)                   :: NUM_CONN_ELEMS    ! The number of elements that are connected to a specific grid
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: TDOF_ROW          ! Row no. in array TDOF to find GDOF DOF number
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GP_FORCE_BALANCE_PROC_BEGEND

      INTEGER(SHORT), DIMENSION(1)    :: Udd = (/0220/)    ! 0220 is the 4 digit ASCII code for a capital U double-dot

      REAL(DOUBLE)                    :: DUM_KE(MELDOF,MELDOF)
      REAL(DOUBLE)                    :: FG1(6)            ! The 6 vals from FG_COL (grid inertia forces) for 1 grid point
      REAL(DOUBLE)                    :: PG1(6)            ! The 6 vals from PG_COL (grid applied loads) for 1 grid point
      REAL(DOUBLE)                    :: MAX_ABS(6)        ! The 6 abs largest values of any force item in the 6 components
      REAL(DOUBLE)                    :: MAX_ABS_PCT(6)    ! Modal mass as % of total mass
      REAL(DOUBLE)                    :: PEG1(6)           ! 6 vals of elem load for one grid point of an elem
      REAL(DOUBLE)                    :: PTE1(6)           ! 6 vals from thermal load, PTE, for one grid point, one elem
      REAL(DOUBLE)                    :: PTET(6)           ! 6 vals from thermal load, PTE, for one grid point, all elems
      REAL(DOUBLE)                    :: QGm1(6)           ! 6 vals from QGm_COL (G-set MPC constraint forces) for one grid point
      REAL(DOUBLE)                    :: QGr1(6)           ! 6 vals from QGr_COL (R-set I/F forces expanded to G-set) for 1 grid pt
      REAL(DOUBLE)                    :: QGs1(6)           ! 6 vals from QGs_COL (S-set SPC forces expanded to G-set) for 1 grid pt
      REAL(DOUBLE)                    :: TOTALS(6)         ! 6 totals of FG1, PG1, QGs1, QGm1, PTET, PEG1 (should be zero's)

      INTRINSIC IAND

      integer(long)                   :: max_abs_grid(6)    ! Grid where max-abs for GP force balance totals exists
      real(double)                    :: max_abs_all_grds(6)! The 6 max-abs from GP force balance totals

      INTEGER(LONG)                   :: DEVICE_CODE        ! 
      INTEGER(LONG)                   :: ANALYSIS_CODE      !      
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=1,6
         FG1(I)         = ZERO
         PG1(I)         = ZERO
         MAX_ABS(I)     = ZERO
         MAX_ABS_PCT(I) = ZERO
         PEG1(I)        = ZERO
         PTE1(I)        = ZERO
         PTET(I)        = ZERO
         QGM1(I)        = ZERO
         QGR1(I)        = ZERO
         QGS1(I)        = ZERO
         TOTALS(I)      = ZERO
      ENDDO     

      CALL CHK_COL_ARRAYS_ZEROS

! Write output headers.
      ANALYSIS_CODE = -1
      IF (IHDR == 'Y') THEN
         WRITE(F06,*)
         WRITE(F06,*)
         IF    (SOL_NAME(1:7) == 'STATICS') THEN
            ANALYSIS_CODE = 1
            WRITE(F06,9101) SCNUM(JVEC)

         ELSE IF (SOL_NAME(1:5) == 'MODES') THEN
            ANALYSIS_CODE = 2
            WRITE(F06,9102) JVEC

         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

            IF ((JVEC <= NDOFR) .OR. (JVEC >= NDOFR+NVEC)) THEN 
               IF (JVEC <= NDOFR) THEN
                  BNDY_DOF_NUM = JVEC
               ELSE
                  BNDY_DOF_NUM = JVEC-(NDOFR+NVEC)
               ENDIF
               CALL GET_GRID_AND_COMP ( 'R ', BNDY_DOF_NUM, BNDY_GRID, BNDY_COMP  )
            ENDIF

            IF       (JVEC <= NDOFR) THEN
               WRITE(F06,9103) JVEC, NUM_CB_DOFS, 'acceleration', BNDY_GRID, BNDY_COMP
            ELSE IF ((JVEC > NDOFR) .AND. (JVEC <= NDOFR+NVEC)) THEN
               WRITE(F06,9105) JVEC, NUM_CB_DOFS, JVEC-NDOFR
            ELSE
               WRITE(F06,9103) JVEC, NUM_CB_DOFS, 'displacement', BNDY_GRID, BNDY_COMP
            ENDIF

         ENDIF


         TITLEI = TITLE(INT_SC_NUM)
         STITLEI = STITLE(INT_SC_NUM)
         LABELI = LABEL(INT_SC_NUM)
         IF (TITLE(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,9799) TITLE(INT_SC_NUM)
         ENDIF

         IF (STITLE(INT_SC_NUM)(1:) /= ' ') THEN
            WRITE(F06,9799) STITLE(INT_SC_NUM)
         ENDIF

         IF (LABEL(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,9799) LABEL(INT_SC_NUM)
         ENDIF

         WRITE(F06,*)
 
         ! write f06 header
         IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
            WRITE(F06,8999)
         ELSE
            WRITE(F06,9200)
         ENDIF

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,*)
            IF    (SOL_NAME(1:7) == 'STATICS') THEN
               WRITE(ANS,9101) SCNUM(JVEC)

            ELSE IF (SOL_NAME(1:5) == 'MODES') THEN
               WRITE(ANS,9102) JVEC

            ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

               IF ((JVEC <= NDOFR) .OR. (JVEC >= NDOFR+NVEC)) THEN 
                  IF (JVEC <= NDOFR) THEN
                     BNDY_DOF_NUM = JVEC
                  ELSE
                     BNDY_DOF_NUM = JVEC-(NDOFR+NVEC)
                  ENDIF
                  CALL GET_GRID_AND_COMP ( 'R ', BNDY_DOF_NUM, BNDY_GRID, BNDY_COMP  )
               ENDIF

               IF       (JVEC <= NDOFR) THEN
                  WRITE(ANS,9103) JVEC, NUM_CB_DOFS, 'acceleration', BNDY_GRID, BNDY_COMP
               ELSE IF ((JVEC > NDOFR) .AND. (JVEC <= NDOFR+NVEC)) THEN
                  WRITE(ANS,9105) JVEC, NUM_CB_DOFS, JVEC-NDOFR
               ELSE
                  WRITE(ANS,9103) JVEC, NUM_CB_DOFS, 'displacement', BNDY_GRID, BNDY_COMP
               ENDIF

            ENDIF

            WRITE(ANS,*)

            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(ANS,8999)
            ELSE
               WRITE(ANS,9200)
            ENDIF

         ENDIF

      ENDIF
 
! Process grid point force balance output requests. 
      DO I=1,6
         FG1(I)     = ZERO
         PG1(I)     = ZERO
         PTET(I)    = ZERO
         QGs1(I)    = ZERO
         QGm1(I)    = ZERO
         PEG1(I)    = ZERO
         MAX_ABS(I) = ZERO
      ENDDO 

      IF (DEBUG(192) > 0) THEN                             ! If DEBUG 192 = 1, GPFO summary will be printed out to F06
         DO L=1,6                                          ! (1) Initialize the max valueof the 6 TOTAL components
            MAX_ABS_ALL_GRDS(L) = ZERO
         ENDDO
i_do1:   DO I=1,NGRID                                      ! (2) Set initial value for the grid at which the max occurs so that if
                                                           !     all TOTAL values are 0 we set the grid for max at the initial grid
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_GPFO_BIT))
            IF (IB > 0) THEN
               DO L=1,6
                  MAX_ABS_GRID(L) = GRID(I,1)
               ENDDO
               EXIT i_do1
            ENDIF
         ENDDO i_do1
      ENDIF

      NREQ = 0
      DO I=1,NGRID
         IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_GPFO_BIT))
         NREQ = NREQ + 1
      ENDDO

      DEVICE_CODE = 1
      !GPFB_NROWS = 0
      !CALL CALCULATE_GPFB_NROWS(GPFB_NROWS)
      !CALL BUILD_GPFB(GPFB_NROWS)
      !WRITE(OP2) (GRID(I,1)*10+DEVICE_CODE, I=1,NGRID)

!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages
      DO I=1,NGRID
         WRITE(SC1,12345,ADVANCE='NO') I, NREQ, CR13
         IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_GPFO_BIT))
         GRID_NUM  = GRID(I,1)
         CALL GET_GRID_NUM_COMPS ( GRID_NUM, NUM_COMPS, SUBR_NAME )

         IF ((IB > 0) .AND. (NUM_COMPS == 6)) THEN         ! Do not do force balance for SPOINT's
            G_CID = GRID(I,3)
            WRITE(F06,9201) GRID_NUM, G_CID
            WRITE(F06,9202)
            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,9201) GRID_NUM, G_CID
               WRITE(ANS,9202)
            ENDIF
                                                           ! Get element equiv thermal loads so we can sub them from applied loads
            OPT(1) = 'N'                                   ! OPT(1) is for calc of ME
            OPT(2) = 'Y'                                   ! OPT(2) is for calc of PTE
            OPT(3) = 'N'                                   ! OPT(3) is for calc of SEi, STEi
            OPT(4) = 'N'                                   ! OPT(4) is for calc of KE-linear
            OPT(5) = 'N'                                   ! OPT(5) is for calc of PPE
            OPT(6) = 'N'                                   ! OPT(6) is for calc of KE-diff stiff
            DO J=1,6
               PTET(J) = ZERO
            ENDDO
            IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
               NUM_CONN_ELEMS = GRID_ELEM_CONN_ARRAY(I,2)
               DO J=1,NUM_CONN_ELEMS
                  AELEM = GRID_ELEM_CONN_ARRAY(I,2+J)
                  CALL GET_ARRAY_ROW_NUM ( 'ESORT1', SUBR_NAME, NELE, ESORT1, AELEM, IELE )
                  CALL GET_ELGP ( IELE )
                  CALL GET_ELEM_AGRID_BGRID ( IELE, 'N' )
                  DO K=1,ELGP
                     IF (AGRID(K) == GRID_NUM) THEN
                        PLY_NUM = 0                        ! 'N' in call to EMG means do not write to BUG file
                        CALL EMG ( IELE, OPT, 'N', SUBR_NAME, 'N' )
                        IF (ETYPE(IELE)(1:4) /= 'ELAS') THEN
                           CALL ELEM_TRANSFORM_LBG ( 'PTE', dum_ke, PTE )
                        ENDIF
                        DO L=1,6
                           BEG_ROW_NUM = 6*(K-1) + 1
                           PTE1(L) = PTE(BEG_ROW_NUM + L - 1,JTSUB)
                        ENDDO
                        DO L=1,6
                           PTET(L) = PTET(L) + PTE1(L)
                        ENDDO 
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF

            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_NUM, IGRID )
            ROW_NUM_START = TDOF_ROW_START(IGRID)
            CALL GET_GRID_NUM_COMPS ( GRID_NUM, NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
               TDOF_ROW = ROW_NUM_START + J - 1
               GDOF = TDOF(TDOF_ROW,G_SET_COL)
               IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
                  FG1(J)  = FG_COL(GDOF)
               ENDIF
               PG1(J)  = PG_COL(GDOF)
               QGs1(J) = QGs_COL(GDOF)
               IF (NDOFM > 0) THEN
                  QGm1(J) = QGm_COL(GDOF)
               ENDIF                                       ! PTET = 0 unless INT_SC_NUM is a thermal subcase (see above)
               IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                  QGr1(J) = QGr_COL(GDOF)
                  TOTALS(J) = -FG1(J) + PG1(J) - PTET(J) + QGs1(J) + QGm1(J) + QGr1(J)
               ELSE
                  TOTALS(J) = -FG1(J) + PG1(J) - PTET(J) + QGs1(J) + QGm1(J)
               ENDIF
            ENDDO
            WRITE(F06,9203) ( PG1(J),J=1,6)
            IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
               WRITE(F06,9204) (-PTET(J),J=1,6)
            ENDIF
            WRITE(F06,9205) (QGs1(J),J=1,6)
            WRITE(F06,9206) (QGm1(J),J=1,6)

            IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
               DO J=1,6
                  IF(FG1(J) == ZERO) THEN  ;  FG1(J) = -ZERO  ;  ENDIF  ! Avoids writing -0.0 for -FG1(J) below
               ENDDO
               WRITE(F06,9207) ACHAR(Udd), ( -FG1(J),J=1,6)
            ENDIF
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,9208) (QGr1(J),J=1,6)
            ENDIF

            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,9203) ( PG1(J),J=1,6)
               IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
                  WRITE(ANS,9204) (-PTET(J),J=1,6)
               ENDIF
               WRITE(ANS,9205) (QGs1(J),J=1,6)
               WRITE(ANS,9206) (QGm1(J),J=1,6)
               IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
                  WRITE(ANS,9207) ACHAR(Udd), ( -FG1(J),J=1,6)
               ENDIF
            ENDIF
                                                           ! Calc elem forces
            OPT(1) = 'N'                                   ! OPT(1) is for calc of ME
            OPT(2) = 'Y'                                   ! OPT(2) is for calc of PTE
            OPT(3) = 'N'                                   ! OPT(3) is for calc of SEi, STEi
            OPT(4) = 'Y'                                   ! OPT(4) is for calc of KE
            OPT(5) = 'N'                                   ! OPT(5) is for calc of PPE
            OPT(6) = 'N'                                   ! OPT(6) is for calc of KE-diff stiff
            NUM_CONN_ELEMS = GRID_ELEM_CONN_ARRAY(I,2)
            DO J=1,NUM_CONN_ELEMS
               AELEM = GRID_ELEM_CONN_ARRAY(I,2+J)
               CALL GET_ARRAY_ROW_NUM ( 'ESORT1', SUBR_NAME, NELE, ESORT1, AELEM, IELE )
               CALL GET_ELGP ( IELE )
               CALL GET_ELEM_AGRID_BGRID ( IELE, 'N' )
               DO K=1,ELGP
                  IF (AGRID(K) == GRID_NUM) THEN
                     PLY_NUM = 0                           ! 'N' in call to EMG means do not write to BUG file
                     CALL EMG ( IELE, OPT, 'N', SUBR_NAME, 'N' )

                     CALL ELMDIS                           ! Get local displ, UEL, for this element
                                                           ! Calc elem loads, PEL. 
                     CALL CALC_ELEM_NODE_FORCES            ! JTSUB will only be used if SUBLOD(INT_SC_NUM,2) > 0
                                                           ! Transform elem loads from local to global coords
                     CALL TRANSFORM_NODE_FORCES (COORD_SYS)! Now we have elem node forces in global coords for the elem that has
!                                                            internal grid K (actual grid GRID_BUM) for this elem
                     DO L=1,6
                        BEG_ROW_NUM = 6*(K-1) + 1
                        PEG1(L) = PEG(BEG_ROW_NUM + L - 1) ! PEG are elem forces at elem ends in global coords
                        IF(PEG1(L) == ZERO) THEN  ;  PEG1(L) = -ZERO  ;  ENDIF  ! Avoids writing -0.0 for -PEG1(J) below

                        TOTALS(L) = TOTALS(L) - PEG1(L)
                     ENDDO
                     WRITE(F06,9209) TYPE, EID, ( -PEG1(L),L=1,6)
                     IF (DEBUG(200) > 0) THEN
                        WRITE(ANS,9209) TYPE, EID, ( -PEG1(L),L=1,6)
                     ENDIF
                  ENDIF
               ENDDO 
            ENDDO 

            WRITE(F06,9210)
            IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
               IF (NDOFO == 0) THEN
                  WRITE(F06,9211) (TOTALS(J),J=1,6)
               ELSE
                  WRITE(F06,9310) (TOTALS(J),J=1,6), ACHAR(Udd)
               ENDIF
            ELSE
               WRITE(F06,9211) (TOTALS(J),J=1,6)     ! KEEP THIS
            ENDIF

            IF (DEBUG(192) > 0) THEN
               DO J=1,6
                  IF (DABS(TOTALS(J)) > MAX_ABS_ALL_GRDS(J)) THEN
                     MAX_ABS_ALL_GRDS(J) = DABS(TOTALS(J))
                     MAX_ABS_GRID(J) = GRID(I,1)
                  ENDIF
               ENDDO
            ENDIF
            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,9210)
               IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
                  IF (NDOFO == 0) THEN
                     WRITE(ANS,9211) (TOTALS(J),J=1,6)
                  ELSE
                     WRITE(ANS,9310) (TOTALS(J),J=1,6), CHAR(128)
                  ENDIF
               ELSE
                  WRITE(ANS,9211) (TOTALS(J),J=1,6)  ! KEEP THIS
               ENDIF
            ENDIF

         ENDIF

! For each of the 6 components (J=1,6 for components T1, T2, T3, R1, R2, R3) , calc % of grid force imbalance as a % of the largest
! force item in that component

         DO J=1,6
            IF (DABS( FG1(J)) > MAX_ABS(J)) MAX_ABS(J) = DABS( FG1(J))
            IF (DABS( PG1(J)) > MAX_ABS(J)) MAX_ABS(J) = DABS( PG1(J))
            IF (DABS(PTET(J)) > MAX_ABS(J)) MAX_ABS(J) = DABS(PTET(J))
            IF (DABS(QGs1(J)) > MAX_ABS(J)) MAX_ABS(J) = DABS(QGs1(J))
            IF (DABS(QGm1(J)) > MAX_ABS(J)) MAX_ABS(J) = DABS(QGm1(J))
            IF (DABS(PEG1(J)) > MAX_ABS(J)) MAX_ABS(J) = DABS(PEG1(J))
         ENDDO

      ENDDO
      WRITE(SC1,*) CR13

      CALL CALCULATE_GPFB_IMBALANCE(CHAR_PCT, MAX_ABS, MAX_ABS_PCT, MAX_ABS_GRID, MAX_ABS_ALL_GRDS)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9101 FORMAT(' OUTPUT FOR SUBCASE ',I8)

 9102 FORMAT(' OUTPUT FOR EIGENVECTOR ',I8)

 9103 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (boundary ',A,' for grid',I8,' component',I2,')')

 9105 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (modal acceleration for mode ',I8,')')

 9200 FORMAT(//,1X,'                                          G R I D   P O I N T   F O R C E   B A L A N C E',/,                  &
             1X,   '                                            (in global coordinate system at each grid)',/)

 8999 FORMAT(//,1X,'                                   C B   G R I D   P O I N T   F O R C E   B A L A N C E   O T M',/,           &
             1X,   '                                            (in global coordinate system at each grid)',/)

 9201 FORMAT(1X,   '                               FORCE BALANCE FOR GRID POINT ', I8, ' IN GLOBAL COORD SYSTEM ',I8,/)

 9202 FORMAT(1X,   '                              T1            T2            T3            R1            R2            R3',/)

 9203 FORMAT(1X,   'APPLIED FORCE          ',6(1ES14.6))

 9204 FORMAT(1X,   'MINUS EQUIV THERM LOAD ',6(1ES14.6))

 9205 FORMAT(1X,   'SPC FORCE              ',6(1ES14.6))

 9206 FORMAT(1X,   'MPC FORCE              ',6(1ES14.6))

 9207 FORMAT(1X,   'INERTIA FORCE (-Mgg*',A1,'g)',6(1ES14.6))

 9208 FORMAT(1X,   'SUBSTRUCTURE I/F FORCE ',6(1ES14.6))

 9209 FORMAT(1X,      A8  ,' ELEM ' ,I9     ,6(1ES14.6))

 9210 FORMAT(1X,   '                        ------------- ------------- ------------- ------------- ------------- -------------')

 9211 FORMAT(1X,   'TOTALS                :',6(1ES14.6),/,                                                                         &
             1X,   '(should all be 0)',//) 

 9310 FORMAT(1X,   'TOTALS                :',6(1ES14.6),13X,/,                                                                     &
             1X,   '(may not be zero since there were OMIT''d DOF''s which can mean that the correct inertia forces at the G-set', &
                   ' are not -Mgg*',A1,'g)',//)

 9799 FORMAT(1X,A)

12345 FORMAT(5X,'Process grid ',I8,' of ',I8,A)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE CHK_COL_ARRAYS_ZEROS 

      IMPLICIT NONE

      INTEGER(LONG)                   :: II                ! DO loop index

! **********************************************************************************************************************************

      DO II=1,NDOFG

         IF (ALLOCATED( FG_COL)) THEN
            IF ( FG_COL(II) == -ZERO) THEN   ;    FG_COL(II) = ZERO   ;   ENDIF
         ENDIF

         IF (ALLOCATED( PG_COL)) THEN
            IF ( PG_COL(II) == -ZERO) THEN   ;    PG_COL(II) = ZERO   ;   ENDIF
         ENDIF

         IF (ALLOCATED(QGs_COL)) THEN
            IF (QGs_COL(II) == -ZERO) THEN   ;   QGs_COL(II) = ZERO   ;   ENDIF
         ENDIF

         IF (ALLOCATED(QGm_COL)) THEN
            IF (QGm_COL(II) == -ZERO) THEN   ;   QGm_COL(II) = ZERO   ;   ENDIF
         ENDIF

         IF (ALLOCATED(QGr_COL)) THEN
            IF (QGr_COL(II) == -ZERO) THEN   ;   QGr_COL(II) = ZERO   ;   ENDIF
         ENDIF

         IF (ALLOCATED( UG_COL)) THEN
            IF ( UG_COL(II) == -ZERO) THEN   ;    UG_COL(II) = ZERO   ;   ENDIF
         ENDIF

      ENDDO

      END SUBROUTINE CHK_COL_ARRAYS_ZEROS


      SUBROUTINE CALCULATE_GPFB_IMBALANCE(CHAR_PCT, MAX_ABS, MAX_ABS_PCT, MAX_ABS_GRID, MAX_ABS_ALL_GRDS)
      ! For each of the 6 components (J=1,6 for components T1, T2, T3, R1, R2, R3)
      !  - calc % of grid force imbalance as a % of the largest
      !    force item in that component
      USE PENTIUM_II_KIND, ONLY       :  BYTE, SHORT, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ANS, F06
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE
      CHARACTER(13*BYTE)              :: CHAR_PCT(6)        ! Character representation of MEFFMASS sum percents of total model mass
      INTEGER(LONG)                   :: I                  ! DO loop index
                                                            
      REAL(DOUBLE), INTENT(IN)        :: MAX_ABS(6)         ! The 6 abs largest values of any force item in the 6 components
      REAL(DOUBLE), INTENT(INOUT)     :: MAX_ABS_PCT(6)     ! Modal mass as % of total mass
      
      !INTRINSIC IAND
      INTEGER(LONG), INTENT(IN)       :: max_abs_grid(6)    ! Grid where max-abs for GP force balance totals exists
      REAL(DOUBLE), INTENT(IN)        :: max_abs_all_grds(6)! The 6 max-abs from GP force balance totals

      IF (DEBUG(192) > 0) THEN
         DO I=1,6
            IF (MAX_ABS(I) > ZERO) THEN
               MAX_ABS_PCT(I) = ONE_HUNDRED*MAX_ABS_ALL_GRDS(I)/MAX_ABS(I)
               WRITE(CHAR_PCT(I),9213) MAX_ABS_PCT(I)
            ELSE                                                 ! Denominator is zero so leave % blank
               CHAR_PCT(I)(1:) = ' '
            ENDIF
         ENDDO
         WRITE(F06,9214)
         WRITE(F06,9202)
         WRITE(F06,9215) (MAX_ABS_ALL_GRDS(I),I=1,6)
         IF (DEBUG(192) > 1) THEN
            WRITE(F06,9216) (MAX_ABS(I),I=1,6)
            WRITE(F06,9217) (CHAR_PCT(I),I=1,6)
         ENDIF
         WRITE(F06,9218) (MAX_ABS_GRID(I),I=1,6)
         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,9214)
            WRITE(ANS,9202)
            WRITE(ANS,9215) (MAX_ABS_ALL_GRDS(I),I=1,6)
            IF (DEBUG(192) > 1) THEN
               WRITE(ANS,9216) (MAX_ABS(I),I=1,6)
               WRITE(ANS,9217) (CHAR_PCT(I),I=1,6)
            ENDIF
            WRITE(ANS,9218) (MAX_ABS_GRID(I),I=1,6)
         ENDIF
      ENDIF

 9202 FORMAT(1X,   '                              T1            T2            T3            R1            R2            R3',/)

 9213 FORMAT(1ES12.2,'%')

 9214 FORMAT(1X,   '                                     Max abs values of force imbalance totals from above grids',/)

 9215 FORMAT(1X,'Max abs imbal any grid:',6(1ES14.6))

 9216 FORMAT(1X,'Max abs force any grid:',6(1ES14.6))

 9217 FORMAT(1X,'as % of max abs force : ',6(A14))

 9218 FORMAT(1X,   'Occurs at grid*       :',2X,I8,5(6X,I8),/,                                                                     &
             1X,   '(*for output set)')

      END SUBROUTINE CALCULATE_GPFB_IMBALANCE
!===================================================================================================================================

      END SUBROUTINE GP_FORCE_BALANCE_PROC
