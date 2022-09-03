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
  
      SUBROUTINE LINK0
  
! LINK0:

!  - reads in the data deck
!  - checks for restart
!  - processes grid and coord data
!  - sequences grids
!  - process Case Control output requests
!  - forms the DOF tables
!  - processes concentrated masses
!  - calcs rigid body mass props (GPWG)
!  - process temperature and pressure load input data to get arrays needed for element load calcs
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, SHORT, LONG, SINGLE, DOUBLE, QUAD

      USE IOUNT1, ONLY                :  MOU4, SC1, WRT_BUG, WRT_LOG
      USE IOUNT1, ONLY                :  ANS, BUG, ERR, F06, F21, F22, F23, F24, F25, IN1, L1B, L1C, L1D, L1F, L1G, L1H, L1I, L1K, &
                                         L1L, L1N, L1O, L1P, L1Q, L1S, L1T, L1U, L1V, L1W, L1X, L1Y, OP2, OU4, SEQ

      USE IOUNT1, ONLY                :  ANSFIL, F04, F21FIL, F22FIL, F23FIL, F24FIL, F25FIL, INFILE, LINK1B, LINK1C, LINK1D,      &
                                         LINK1F, LINK1H, LINK1I, LINK1K, LINK1L, LINK1N, LINK1O, LINK1P, LINK1Q, LINK1S, LINK1T,   &
                                         LINK1U, LINK1V, LINK1W, LINK1X, LINK1Y, OP2FIL, OU4FIL, SEQFIL

      USE IOUNT1, ONLY                :  L1LSTAT, L1NSTAT, L1OSTAT, L1QSTAT, L1YSTAT, OP2STAT

      USE IOUNT1, ONLY                :  F21_MSG, F22_MSG, F23_MSG, F24_MSG, F25_MSG, L1B_MSG, L1C_MSG, L1D_MSG, L1F_MSG, L1H_MSG, &
                                         L1I_MSG, L1K_MSG, L1L_MSG, L1N_MSG, L1O_MSG, L1P_MSG, L1Q_MSG, L1S_MSG, L1T_MSG, L1U_MSG, &
                                         L1V_MSG, L1W_MSG, L1X_MSG, L1Y_MSG, OP2_MSG, OU4_MSG, SEQ_MSG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, BANDIT_ERR, CHKPNT, COMM, DEMO_GRID_LIMIT, ENFORCED, EPSIL1_SET, FATAL_ERR, &
                                         IBIT, KMAT_BW, KMAT_DEN, LGRID, LINKNO, MBUG, MELDTS, NAOCARD, NCUSERIN,                  &
                                         NDOFG, NDOFR, NDOFSE, NELE, NFORCE, NGRAV, NGRID, NMPC, NPCARD,                           &
                                         NPUSERIN, NRFORCE, NRIGEL, NSLOAD, NSPC, NSPC1, NTCARD, NTERM_KGG, NUM_PARTVEC_RECORDS,   &
                                         NUM_SUPT_CARDS, NUM_USET_RECORDS, PROG_NAME, RESTART, SOL_NAME, WARN_ERR 

      USE SCONTR, ONLY                :  ELDT_BUG_DAT1_BIT, ELDT_BUG_DAT2_BIT, ELDT_BUG_ME_BIT  , ELDT_BUG_P_T_BIT ,               &
                                         ELDT_BUG_SE_BIT  , ELDT_BUG_KE_BIT  , ELDT_BUG_SHPJ_BIT, ELDT_BUG_BMAT_BIT,               &
                                         ELDT_BUG_BCHK_BIT, ELDT_BUG_U_P_BIT,  ELDT_F21_P_T_BIT , ELDT_F22_ME_BIT  ,               &
                                         ELDT_F23_KE_BIT  , ELDT_F24_SE_BIT  , ELDT_F25_U_P_BIT
      use scontr, only                :  ndofo
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE DOF_TABLES, ONLY            :  TDOFI
      USE PARAMS, ONLY                :  CHKGRDS, EPSIL, EQCHK_OUTPUT, GRDPNT, GRDPNT_IN, GRIDSEQ, MEFMGRID, MEFMLOC, PRTCONN,     &
                                         PRTBASIC, PRTCORD, PRTDOF, PRTTSET, PRTSTIFD, PRTSTIFF, SETLKTK, SETLKTM, SUPINFO,        &
                                         SUPWARN, WTMASS
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MACHINE_PARAMS, ONLY        :  MACH_PREC
      USE MODEL_STUF, ONLY            :  ANY_GPFO_OUTPUT, EIG_METH, ELDT, ETYPE, MEFFMASS_CALC, NUM_EMG_FATAL_ERRS, PLY_NUM, OELDT
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE BANDIT_MODULE
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, TR6_CG, TR6_MEFM, TR6_0            
      USE SPARSE_MATRICES, ONLY       :  SYM_KGG, I_KGG, J_KGG, KGG
      USE OUTPUT4_MATRICES, ONLY      :  NUM_OU4_REQUESTS, OU4_FILE_UNITS
  
      USE LINK0_USE_IFs
                       
      IMPLICIT NONE

      LOGICAL                         :: FILE_OPND         ! = .TRUE. if a file is opened

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LINK0'
      CHARACTER(44*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run

      CHARACTER(1*BYTE)               :: OPEN_UNT(MOU4)    ! 'Y' if we need to open an OU4 file unit

                                                           ! Option indicator for subr EMG
      CHARACTER(1*BYTE)               :: OPT(6)    = (/'N', 'N', 'N', 'N', 'N', 'N'/)

      CHARACTER( 1*BYTE)              :: RBG_GSET_ALLOCATED! 'Y' or 'N' depending on whether array RBGLOBAL has been allocated
      CHARACTER(132*BYTE)             :: TDOF_MSG          ! Message to be printed out regarding at what point in the run the TDOF,I
!                                                            tables are printed out
     
      INTEGER(LONG)                   :: BANDIT_BW         ! Matrix bandwidth returned from subr BANDIT

                                                           ! Array used to tell subr ELDT_PROC_FOR_RESTART which ELDT to calc
      INTEGER(LONG)                   :: CHK_ELDT_BIT(0:MELDTS-1)

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I1,I2             ! Intermediate integer variable
      INTEGER(LONG)                   :: JERROR    = 0     ! Local error count
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: PRINTIT   = 0     ! 'Y'/'N' depending on whether to tell subr GET_MATRIX_DIAG_STATS to
!                                                            print output
      INTEGER(LONG)                   :: R_SET_COL         ! Col number in TDOF where the R-set exists
      INTEGER(LONG)                   :: R_SET_DOF         ! Comp number in the R-set
      INTEGER(LONG)                   :: SETLKTK_DEF       ! Default value of SETLKTK
      INTEGER(LONG)                   :: SETLKTM_DEF       ! Default value of SETLKTM
      INTEGER(LONG) :: POST

      REAL(DOUBLE)                    :: KGG_DIAG(NDOFG)   ! Diagonal of KGG (needed for equil check on RESTART)
      REAL(DOUBLE)                    :: KGG_MAX_DIAG      ! Max diag term from KGG (needed for equil check on RESTART)

      INTRINSIC                       :: IAND
   
! **********************************************************************************************************************************
      LINKNO = 0
      POST = -1

! Initialize WRT_BUG

      DO I=0,MBUG-1
         WRT_BUG(I) = 0
      ENDDO

      RBG_GSET_ALLOCATED = 'N'

! Set default values for SETLKT from values in module PARAMS

      SETLKTK_DEF = SETLKTK
      SETLKTM_DEF = SETLKTM

! Set time initializing parameters

      CALL TIME_INIT

! Get date and time, write to screen

      CALL OURDAT
      CALL OURTIM
      WRITE(SC1,152) LINKNO

! Write logo, date and copyright info to text files
  
      WRITE(F06,150) LINKNO
      IF (WRT_LOG > 0) THEN
         WRITE(F04,150) LINKNO
      ENDIF
      WRITE(ERR,150) LINKNO

! Reset units for error output (were set to SC1 in MAIN1)

      OUNT(1) = ERR
      OUNT(2) = F06

! Read input data to count sizes of arrays (no. GRID's, elems, etc.)
  
      CALL OURTIM
      MODNAM = 'DETERMINE ARRAY SIZES - CASE CONTROL        '
      WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL LOADC0
  
! Initial pass on Bulk Data when this is not a restart

res11:IF (RESTART == 'N') THEN
         CALL OURTIM
         MODNAM = 'DETERMINE ARRAY SIZES - BULK DATA           '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL LOADB0
      ENDIF res11

! Start back at beginnng of input file to read Exec, Case Control and Bulk Data decks 

      REWIND (IN1)
  
! Processes the EXEC CONTROL DECK

      CALL OURTIM
      MODNAM = 'READ EXEC CONTROL DECK                      '
      WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      WRITE(F06,*)
      CALL ALLOCATE_IN4_FILES ( 'IN4FIL', 0, 0, SUBR_NAME )
      CALL LOADE

      IF (SOL_NAME(1:8) == 'NLSTATIC') THEN
         CALL ALLOCATE_NL_PARAMS ( 'MYSTRAN' )
      ENDIF

! pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
! Code here is used to test whether user has permission to run problems.

! Code removed 10/02/15

! pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp

! If CHKPNT was in the Exec Control deck, reset file close status' to KEEP

      IF (CHKPNT == 'Y') THEN
         IF (SOL_NAME(1:8) /= 'NLSTATIC') THEN
            CALL SET_FILE_CLOSE_STAT ( 'KEEP' )
         ELSE
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,9401) SOL_NAME
            IF (SUPWARN == 'N') THEN
               WRITE(F06,9401) SOL_NAME
            ENDIF
         ENDIF
      ENDIF

! If nonlinear we need to keep files for iterations

      IF (SOL_NAME(1:8) == 'NLSTATIC') THEN
         CALL SET_FILE_CLOSE_STAT ( 'KEEP' )
      ENDIF

! Processes the Case Control deck and write check-point data to L1Z if this is NOT a restart or verify that the check-pointed data
! agrees between the original run and this restart run
 
      CALL OURTIM
      MODNAM = 'ALLOCATE MEMORY FOR SOME MODEL DATA ARRAYS  '
      WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL ALLOCATE_MODEL_STUF ( 'SETS ARRAYS', SUBR_NAME )
      CALL ALLOCATE_MODEL_STUF ( 'TITLES', SUBR_NAME )
      CALL ALLOCATE_MODEL_STUF ( 'SC_xxxx', SUBR_NAME )
      CALL ALLOCATE_MODEL_STUF ( 'SCNUM', SUBR_NAME )
      CALL ALLOCATE_MODEL_STUF ( 'SUBLOD', SUBR_NAME )
      CALL ALLOCATE_MODEL_STUF ( 'SPC_MPC_SET', SUBR_NAME )
      CALL OURTIM

      MODNAM = 'READ CASE CONTROL DECK                      '
      WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL LOADC

! If this is a restart, do the following:

res12:IF (RESTART == 'Y') THEN

         CALL READ_L1Z

         CALL ALLOCATE_MODEL_STUF ( 'CONM2, RCONM2', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'CORD, RCORD', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRID, RGRID', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRID_ID', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRID_SEQ, INV_GRID_SEQ', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ETYPE, EDAT, EPNT', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ESORT1', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ESORT2', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'EOFF', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ELEM PROPERTIES AND MATERIALS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'VVEC, OFFSETS, PLATE stuff', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'SINGLE ELEMENT ARRAYS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'PPNT, PDATA, PTYPE', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'PLOAD4_3D_DATA', SUBR_NAME )

         CALL LINK1_RESTART_DATA

         CALL LOADB_RESTART                                ! Read new Bulk Data entries allowed

         CALL SET_SPARSE_MAT_SYM                           ! Uses SPARSTOR which was read from L1A

         IF (DEBUG(1) == 1) THEN                           ! Print KIND parameters if DEBUG says to
            WRITE(F06,*)
            WRITE(F06,11) BYTE,SHORT,LONG,SINGLE,DOUBLE,QUAD
            WRITE(F06,*)
         ENDIF

         IF (DEBUG(2) == 1) THEN                           ! Print CONSTANTS_1 if DEBUG says to
            CALL PRINT_CONSTANTS_1
         ENDIF

         CALL GET_MACHINE_PARAMS
         IF (EPSIL1_SET == 'N') THEN
            EPSIL(1) = MACH_PREC
         ENDIF

         IF (DEBUG(5) == 1) THEN                           ! Print Gauss quadriture abscissas and weights if DEBUG says to
            CALL PRINT_ORDER
         ENDIF

      ENDIF res12
 
res13:IF (RESTART == 'N') THEN

! Open files needed for writing Bulk Data in LOADB.
! L1F: Rigid element data
! L1I: FORCE, MOMENT for loads requested in Case Control 
! L1K: Temperature data for temp loads requested in Case Control
! L1M: Eigen extraction data for METHOD requested in Case Control
! L1N: ASET, OMIT data
! L1O: SPC data for SPC's requested in Case Control
! L1P: GRAV data for loads requested in Case Control
! L1Q: Element pressure load data for loads requested in Case Control
! L1S: Data from B.D. MPC cards 
! L1T: 
! L1U: Data from B.D. RFORCE cards
! L1W: Data from B.D. SLOAD  cards 

         CALL FILE_OPEN ( L1F, LINK1F, OUNT, 'REPLACE', L1F_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1I, LINK1I, OUNT, 'REPLACE', L1I_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1K, LINK1K, OUNT, 'REPLACE', L1K_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1N, LINK1N, OUNT, 'REPLACE', L1N_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1O, LINK1O, OUNT, 'REPLACE', L1O_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1P, LINK1P, OUNT, 'REPLACE', L1P_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1Q, LINK1Q, OUNT, 'REPLACE', L1Q_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1S, LINK1S, OUNT, 'REPLACE', L1S_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1T, LINK1T, OUNT, 'REPLACE', L1T_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1U, LINK1U, OUNT, 'REPLACE', L1U_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1V, LINK1V, OUNT, 'REPLACE', L1V_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1W, LINK1W, OUNT, 'REPLACE', L1W_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL FILE_OPEN ( L1X, LINK1X, OUNT, 'REPLACE', L1X_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
!xx      IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
!xx         CALL FILE_OPEN ( L1M, LINK1M, OUNT, 'REPLACE', L1M_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
!xx      ENDIF
  
! Get machine parameters and set EPSIL(1). This may change later if user inputs a value on a PARAM EPSIL entry.

         CALL GET_MACHINE_PARAMS
         IF (EPSIL1_SET == 'N') THEN
            EPSIL(1) = MACH_PREC
         ENDIF

! Processes the Bulk Data deck

         CALL OURTIM
         MODNAM = 'ALLOCATE MEMORY FOR SOME MODEL DATA ARRAYS  '
!        WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ALLOCATE_MODEL_STUF ( 'SEQ1,2', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'FORMOM_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'PRESS_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRAV_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'LOAD_SIDS, LOAD_FACS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'MPC_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'MPCADD_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'SPC_SIDS, SPC1_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'SPCADD_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ETYPE, EDAT, EPNT', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ELEM PROPERTIES AND MATERIALS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'VVEC, OFFSETS, PLATE stuff', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRID, RGRID', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'CORD, RCORD', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'CONM2, RCONM2', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'CMASS, PMASS, RPMASS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'RFORCE_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'SLOAD_SIDS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'RIGID_ELEM_IDS', SUBR_NAME )

         CALL OURTIM
         MODNAM = 'READ BULK DATA DECK                         '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL LOADB

         CALL DEALLOCATE_MODEL_STUF ( 'SPC_SIDS, SPC1_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'MPC_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'FORMOM_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'GRAV_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'MPCADD_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'RFORCE_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'SLOAD_SIDS' )
         CALL DEALLOCATE_MODEL_STUF ( 'SPC_MPC_SET' )
         WRITE(F06,*)

! Write EDAT table, if requested

         IF (DEBUG(108) > 0) THEN
            CALL WRITE_EDAT
         ENDIF

! Make sure we have a finite WTMASS (need to divide by it in several places later in the code)

         IF (DABS(WTMASS) < EPSIL(1)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1804) WTMASS
            WRITE(F06,1804) WTMASS
         ENDIF

! Quit if FATAL_ERR > 0

         IF (FATAL_ERR > 0) THEN
            CALL OUTA_HERE ( 'Y' )
         ENDIF

! Reset MEFMGRID to GRDPNT if MEFMLOC was 'GRDPNT'. However, we need to make sure that GRDPNT is not the default (-1) otherwise
! we get problems in subr RB_DISP_MATRIX_PROC. Therefore reset GRDPNT if it is -1 to 0 (basic origin). This will mean that, if
! there is no GRDPNT in the Bulk data, we will still get results the same as if PARAM, GRDPNT, 0 was there.

         IF (MEFMLOC == 'GRDPNT') THEN
            IF (GRDPNT == -1) THEN                         !  We can't have GRDPNT = -1 if we want MEFMGRID = GRDPNT
               GRDPNT   = 0
            ENDIF
            MEFMGRID = GRDPNT
         ENDIF

! Allocate file names, unit numbers, etc for the USERIN elements whose matrices will be on In4 files.
! First check that there is a 1 to 1 correspondence of number of CUSERIN and PUSERIN B.D. entries

         IF (NPUSERIN /= NCUSERIN) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1802) NCUSERIN, NPUSERIN
            WRITE(F06,1802) NCUSERIN, NPUSERIN
            CALL OUTA_HERE ( 'Y' )
         ENDIF

! If ENFORCED = 'Y' then all DOF's are SE and we will read their values from ENFFIL and use that to create LINK1O

         IF (ENFORCED == 'Y') THEN
            CALL WRITE_ENF_TO_L1O
         ENDIF

! Make sure that there is at least 1 grid

         IF (NGRID < 1) THEN
            WRITE(ERR,1827) NDOFG
            WRITE(F06,1827) NDOFG
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                         ! No G-set, so quit
         ENDIF

! Make sure that there is at least 1 element

         IF (NELE < 1) THEN
            WRITE(ERR,1820) NELE
            WRITE(F06,1820) NELE
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                         ! No G-set, so quit
         ENDIF

! Close files opened for writing Bulk data info to

         IF (NRIGEL > 0) THEN 
            CALL FILE_CLOSE ( L1F, LINK1F, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1F, LINK1F, 'DELETE', 'Y' )
         ENDIF

         IF (NFORCE > 0) THEN
            CALL FILE_CLOSE ( L1I, LINK1I, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1I, LINK1I, 'DELETE', 'Y' )
         ENDIF

         IF (NTCARD > 0) THEN
            CALL FILE_CLOSE ( L1K, LINK1K, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1K, LINK1K, 'DELETE', 'Y' )
         ENDIF

         IF (NAOCARD > 0) THEN
            CALL FILE_CLOSE ( L1N, LINK1N, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1N, LINK1N, 'DELETE', 'Y' )
         ENDIF

         IF ((NSPC > 0) .OR. (NSPC1 > 0)) THEN
            CALL FILE_CLOSE ( L1O, LINK1O, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1O, LINK1O, 'DELETE', 'Y' )
         ENDIF
  
         IF (NGRAV > 0) THEN
            CALL FILE_CLOSE ( L1P, LINK1P, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1P, LINK1P, 'DELETE', 'Y' )
         ENDIF
  
         IF (NPCARD > 0) THEN
            CALL FILE_CLOSE ( L1Q, LINK1O, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1Q, LINK1O, 'DELETE', 'Y' )
         ENDIF
  
         IF (NMPC > 0) THEN
            CALL FILE_CLOSE ( L1S, LINK1S, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1S, LINK1S, 'DELETE', 'Y' )
         ENDIF

         IF (NUM_SUPT_CARDS > 0) THEN
            CALL FILE_CLOSE ( L1T, LINK1T, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1T, LINK1T, 'DELETE', 'Y' )
         ENDIF

         IF (NRFORCE > 0) THEN
            CALL FILE_CLOSE ( L1U, LINK1U, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1U, LINK1U, 'DELETE', 'Y' )
         ENDIF
  
         IF (NSLOAD > 0) THEN
            CALL FILE_CLOSE ( L1W, LINK1W, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1W, LINK1W, 'DELETE', 'Y' )
         ENDIF
  
         IF (NUM_PARTVEC_RECORDS > 0) THEN
            CALL FILE_CLOSE ( L1V, LINK1V, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1V, LINK1V, 'DELETE', 'Y' )
         ENDIF

         IF (NUM_USET_RECORDS > 0) THEN
            CALL FILE_CLOSE ( L1X, LINK1X, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( L1X, LINK1X, 'DELETE', 'Y' )
         ENDIF

         IF (DEBUG(200) == 0) THEN                         ! ANS was opened in subr MYSTRAN_FILES. We only need it if DEBUG(200) > 0
            CALL FILE_CLOSE ( ANS, ANSFIL, 'DELETE', 'Y' )
         ENDIF
 
         CALL SET_SPARSE_MAT_SYM                           ! Set sparse matrix sym

      ENDIF res13

! Check error flag; if > 0 quit.
  
      IF (FATAL_ERR > 0)  THEN
          WRITE(ERR,9998) FATAL_ERR
          WRITE(F06,9998) FATAL_ERR
          CALL OUTA_HERE ( 'Y' )                           ! Finished processing data deck and there are input errors
      ENDIF
  
res14:IF (RESTART == 'N') THEN

         CALL OURTIM
         MODNAM = 'ALLOCATE MEMORY FOR SOME MODEL DATA ARRAYS  '
!        WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ALLOCATE_MODEL_STUF ( 'ESORT1', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'ESORT2', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'EOFF', SUBR_NAME )

         CALL OURTIM
         MODNAM = 'SORT AND CHECK ELEMENTS                     '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ELESORT
         CALL DEALLOCATE_MODEL_STUF ( 'RIGID_ELEM_IDS' )
  
! Open L1B, OP2 for writing grid and coord data to.

         CALL FILE_OPEN ( L1B, LINK1B, OUNT, 'REPLACE', L1B_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

         CALL FILE_OPEN ( OP2, OP2FIL, OUNT, 'REPLACE', OP2_MSG, 'NEITHER', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         OP2STAT = 'KEEP    '
         CALL WRITE_OP2_HEADER(POST)
         

! Element processing to convert external PID's to internal.
  
         CALL OURTIM
         MODNAM = 'ELEM PROCESSOR                              '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ELEM_PROP_MATL_IIDS
  
! Grid and coordinate system processing
  
         CALL OURTIM
         MODNAM = 'ALLOCATE MEMORY FOR SOME MODEL DATA ARRAYS  '
!        WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ALLOCATE_MODEL_STUF ( 'SINGLE ELEMENT ARRAYS', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRID_ID', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'GRID_SEQ, INV_GRID_SEQ', SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'TN', SUBR_NAME )

         CALL OURTIM
         MODNAM = 'GRID PROCESSOR                              '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL GRID_PROC
         CALL DEALLOCATE_MODEL_STUF ( 'TN' )
  
! Run through elements and make sure all grids on elem connection entries are defined. Need this so that if any are missing we
! won't send wrong NGRID value to Bandit. We don't need BGRID, but GET_ELEM_AGRID_BGRID checks that all grids on connection
! entries are defined

         IF (CHKGRDS == 'Y') THEN
            CALL OURTIM
            MODNAM = 'CHECK THAT ALL GRIDS FOR ELEMS EXIST        '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            DO I=1,NELE
               WRITE(SC1,12345,ADVANCE='NO') '       Process element', I, NELE, CR13
               CALL GET_ELEM_AGRID_BGRID ( I, 'Y' )
            ENDDO
         ENDIF
         WRITE(SC1,*) CR13

! Run Bandit, if requested

         REWIND (IN1)
         IF (GRIDSEQ(1:6) == 'BANDIT') THEN
            CALL OURTIM
            MODNAM = 'BANDIT - RESEQUENCE GRIDS                   '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL FILE_OPEN ( SEQ, SEQFIL, OUNT, 'REPLACE', SEQ_MSG, 'WRITE_STIME', 'FORMATTED', 'READWRITE', 'REWIND','Y','N','Y' )
            CALL BANDIT ( NGRID, BANDIT_BW, KMAT_DEN, BANDIT_ERR )
            INQUIRE ( FILE=SEQFIL, OPENED=FILE_OPND )
            IF (FILE_OPND) THEN
               CALL FILE_CLOSE ( SEQ, SEQFIL, 'KEEP', 'Y' )
            ENDIF
            WRITE(ERR,1030) BANDIT_ERR
            IF (SUPINFO == 'N') THEN
               WRITE(F06,1030) BANDIT_ERR
            ENDIF
            IF (BANDIT_ERR == 0) THEN
               KMAT_BW = 6*BANDIT_BW
            ELSE                                           ! If BANDIT had error can't use SETLKT = 1 (which depends on BANDIT_BW)
               IF (SETLKTK == 1) THEN
                  WRITE(F06,1001) 'SETLKTK', SETLKTK, SETLKTK_DEF
                  SETLKTK = SETLKTK_DEF
               ENDIF
               IF (SETLKTM == 1) THEN
                  WRITE(F06,1001) 'SETLKTM', SETLKTM, SETLKTM_DEF
                  SETLKTM = SETLKTM_DEF
               ENDIF
            ENDIF
         ENDIF
         CALL FILE_CLOSE ( IN1, INFILE, 'KEEP', 'Y' )

! Grid point sequencing

         CALL OURTIM
         MODNAM = 'GRID SEQUENCE PROCESSOR                     '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL SEQ_PROC
         CALL FILE_CLOSE ( L1B, LINK1B, 'KEEP', 'Y' )
         CALL DEALLOCATE_MODEL_STUF ( 'SEQ1,2' )

      ENDIF res14

! Subcase processing

      CALL ALLOCATE_MODEL_STUF ( 'GROUT, ELOUT', SUBR_NAME )
      CALL ALLOCATE_MODEL_STUF ( 'ELDT', SUBR_NAME )
      CALL FILE_OPEN ( L1D, LINK1D, OUNT, 'REPLACE', L1D_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      CALL OURTIM
      MODNAM = 'SUBCASE PROCESSOR                           '
      WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      CALL SUBCASE_PROC
      CALL DEALLOCATE_MODEL_STUF ( 'GROUT, ELOUT' )
      CALL DEALLOCATE_MODEL_STUF ( 'SETS ARRAYS' )
      CALL DEALLOCATE_MODEL_STUF ( 'TITLES' )
      CALL DEALLOCATE_MODEL_STUF ( 'SC_xxxx' )
      CALL FILE_CLOSE ( L1D, LINK1D, 'KEEP', 'Y' )
                                                           ! If we will be writing data to the F2i disk files open them now
      IF ( IAND(OELDT,IBIT(ELDT_F21_P_T_BIT) ) > 0) THEN
         CALL FILE_OPEN ( F21, F21FIL, OUNT, 'REPLACE', F21_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

      IF ( IAND(OELDT,IBIT(ELDT_F22_ME_BIT)  ) > 0) THEN
         CALL FILE_OPEN ( F22, F22FIL, OUNT, 'REPLACE', F22_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF
         
      IF ( IAND(OELDT,IBIT(ELDT_F23_KE_BIT)  ) > 0) THEN
         CALL FILE_OPEN ( F23, F23FIL, OUNT, 'REPLACE', F23_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

      IF ( IAND(OELDT,IBIT(ELDT_F24_SE_BIT)  ) > 0) THEN
         CALL FILE_OPEN ( F24, F24FIL, OUNT, 'REPLACE', F24_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

      IF ( IAND(OELDT,IBIT(ELDT_F25_U_P_BIT) ) > 0) THEN
         CALL FILE_OPEN ( F25, F25FIL, OUNT, 'REPLACE', F25_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      ENDIF

! Print table showing the elements connected to each grid if requested  

      IF (RESTART == 'N') THEN
         IF ((ANY_GPFO_OUTPUT > 0) .OR. (PRTCONN > 0)) THEN
            CALL OURTIM
            MODNAM = 'GRID/ELEMENT CONNECTION TABLE               '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL GRID_ELEM_CONN_TABLE
            IF (ANY_GPFO_OUTPUT == 0) THEN
               CALL DEALLOCATE_MODEL_STUF ( 'GRID_ELEM_CONN_ARRAY' )
            ENDIF
         ENDIF
      ENDIF

res15:IF (RESTART == 'Y') THEN

         IF (ANY_GPFO_OUTPUT > 0) THEN
            CALL OURTIM
            MODNAM = 'GRID/ELEMENT CONNECTION TABLE               '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL GRID_ELEM_CONN_TABLE
         ENDIF

         DO I=0,MELDTS-1
            CHK_ELDT_BIT(I) = 0
         ENDDO                                             ! Note below elem mass not done here since it would be done in subr GPWG
         IF ( IAND(OELDT,IBIT(ELDT_BUG_DAT1_BIT) ) > 0) THEN   ;   CHK_ELDT_BIT( 0) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_DAT2_BIT) ) > 0) THEN   ;   CHK_ELDT_BIT( 1) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_P_T_BIT)  ) > 0) THEN   ;   CHK_ELDT_BIT( 2) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_ME_BIT)   ) > 0) THEN   ;   CHK_ELDT_BIT( 3) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_KE_BIT)   ) > 0) THEN   ;   CHK_ELDT_BIT( 4) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_SE_BIT)   ) > 0) THEN   ;   CHK_ELDT_BIT( 5) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_U_P_BIT)  ) > 0) THEN   ;   CHK_ELDT_BIT( 6) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_SHPJ_BIT) ) > 0) THEN   ;   CHK_ELDT_BIT( 7) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_BMAT_BIT) ) > 0) THEN   ;   CHK_ELDT_BIT( 8) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_BUG_BCHK_BIT) ) > 0) THEN   ;   CHK_ELDT_BIT( 9) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_F21_P_T_BIT)  ) > 0) THEN   ;   CHK_ELDT_BIT(10) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_F22_ME_BIT)   ) > 0) THEN   ;   CHK_ELDT_BIT(11) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_F23_KE_BIT)   ) > 0) THEN   ;   CHK_ELDT_BIT(12) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_F24_SE_BIT)   ) > 0) THEN   ;   CHK_ELDT_BIT(13) = 1   ;   ENDIF
         IF ( IAND(OELDT,IBIT(ELDT_F25_U_P_BIT)  ) > 0) THEN   ;   CHK_ELDT_BIT(14) = 1   ;   ENDIF
         CALL ELDT_PROC_FOR_RESTART ( CHK_ELDT_BIT )
         CALL DEALLOCATE_MODEL_STUF ( 'SINGLE ELEMENT ARRAYS' )

         IF (PRTCORD > 0) THEN
            CALL ALLOCATE_MODEL_STUF ( 'TN', SUBR_NAME )
            CALL CORD_PROC
            CALL DEALLOCATE_MODEL_STUF ( 'TN' )
         ENDIF

         IF (PRTBASIC > 0) THEN
            CALL WRITE_GRID_COORDS
         ENDIF

         CALL ALLOCATE_DOF_TABLES ( 'TSET' , SUBR_NAME )
         CALL ALLOCATE_DOF_TABLES ( 'TDOF' , SUBR_NAME )
         CALL ALLOCATE_DOF_TABLES ( 'TDOFI', SUBR_NAME )
         CALL ALLOCATE_DOF_TABLES ( 'TDOF_ROW_START' , SUBR_NAME )
         CALL READ_DOF_TABLES
         CALL CALC_TDOF_ROW_START ( 'N' )
!xx      CALL ALLOCATE_MODEL_STUF ( 'GRID_SEQ, INV_GRID_SEQ', SUBR_NAME )
         IF (PRTTSET > 0) THEN
            CALL WRITE_TSET
         ENDIF
         IF (PRTDOF > 0) THEN
            CALL WRITE_TDOF ( 'After all AUTOSPC' )
         ENDIF
         CALL DEALLOCATE_MODEL_STUF ( 'SCNUM' )
         CALL DEALLOCATE_MODEL_STUF ( 'SUBLOD' )
         CALL DEALLOCATE_MODEL_STUF ( 'ETYPE, EDAT, EPNT' )
         CALL DEALLOCATE_MODEL_STUF ( 'VVEC, OFFSETS, PLATE stuff' )
         CALL DEALLOCATE_MODEL_STUF ( 'ESORT1' )
         CALL DEALLOCATE_MODEL_STUF ( 'ESORT2' )
         CALL DEALLOCATE_MODEL_STUF ( 'EOFF' )
         CALL DEALLOCATE_MODEL_STUF ( 'GROUT, ELOUT' )
         CALL DEALLOCATE_MODEL_STUF ( 'ELDT' )
         CALL DEALLOCATE_MODEL_STUF ( 'GTEMP' )
         CALL DEALLOCATE_MODEL_STUF ( 'TPNT, TDATA' )
         CALL DEALLOCATE_MODEL_STUF ( 'PPNT, PDATA, PTYPE' )
         CALL DEALLOCATE_MODEL_STUF ( 'PLOAD4_3D_DATA' )

      ENDIF res15

res16:IF (RESTART == 'N') THEN

         NUM_EMG_FATAL_ERRS = 0                            ! Process ELDATA printed output requests for ELDATA item 0, 1
         JERROR  = 0
         DO I=1,NELE
            DO J=1,5                                       ! 05/27/07: Initialization of OPT added
               OPT(J) = 'N'
            ENDDO
            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_DAT1_BIT))
            I2 = IAND(ELDT(I),IBIT(ELDT_BUG_DAT2_BIT))
            IF ((I1 > 0) .OR. (I2 > 0)) THEN
               IF (I1 > 0) THEN
                  WRT_BUG(0) = 1
               ENDIF
               IF (I2 > 0) THEN
                  WRT_BUG(1) = 1
               ENDIF
               WRITE(BUG,10)
               PLY_NUM = 0
               CALL EMG ( I   , OPT, 'N', SUBR_NAME, 'Y' ) ! Calc no matrices (all OPT = 'N'). 'Y' means write to BUG file
               IF (NUM_EMG_FATAL_ERRS /=0) THEN
                  JERROR = JERROR + NUM_EMG_FATAL_ERRS
                  CYCLE
               ENDIF
            ENDIF
         ENDDO
         WRT_BUG(0) = 0
         WRT_BUG(1) = 0

         IF (JERROR > 0) THEN
            WRITE(ERR,9876) JERROR
            WRITE(F06,9876) JERROR
            CALL OUTA_HERE ( 'Y' )
         ENDIF

! Degree of Freedom processing. Also, enforced displs written to L1H.
!   Open L1F for reading rigid element data
!   Open L1S for reading MPC DOF's if NMPC > 0
!   Open L1O for reading SPC DOF's if NSPC or NSPC1 > 0
!   Open L1N for reading ASET, ASET1, OMIT, OMIT1 data if NAOCARD > 0
!   Open L1C for writing DOF tables to
!   Open L1H for writing enforced displ SPC's. Keep L1H for later processing in YS_ARRAY if SOL = STATICS
  
         IF (NRIGEL > 0) THEN
            CALL FILE_OPEN ( L1F, LINK1F, OUNT, 'OLD', L1F_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
         ENDIF
  
         IF (NMPC > 0) THEN
            CALL FILE_OPEN ( L1S, LINK1S, OUNT, 'OLD', L1S_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
         ENDIF

         IF ((NSPC > 0) .OR. (NSPC1 > 0)) THEN
            CALL FILE_OPEN ( L1O, LINK1O, OUNT, 'OLD', L1O_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
         ENDIF

         IF (NAOCARD > 0) THEN
            CALL FILE_OPEN ( L1N, LINK1N, OUNT, 'OLD', L1N_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
         ENDIF

         IF (NUM_SUPT_CARDS > 0) THEN
            CALL FILE_OPEN ( L1T, LINK1T, OUNT, 'OLD', L1T_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
         ENDIF

         CALL FILE_OPEN ( L1C, LINK1C, OUNT, 'REPLACE', L1C_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

         CALL FILE_OPEN ( L1H, LINK1H, OUNT, 'REPLACE', L1H_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
  
         CALL OURTIM
         MODNAM = 'ALLOCATE MEMORY FOR SOME MODEL DATA ARRAYS  '
!        WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ALLOCATE_DOF_TABLES ( 'TSET', SUBR_NAME )
         CALL ALLOCATE_DOF_TABLES ( 'TDOF', SUBR_NAME )
         CALL ALLOCATE_DOF_TABLES ( 'TDOFI', SUBR_NAME )
         CALL ALLOCATE_DOF_TABLES ( 'TDOF_ROW_START' , SUBR_NAME )
         CALL ALLOCATE_MODEL_STUF ( 'MPC_IND_GRIDS', SUBR_NAME )

         CALL OURTIM
         MODNAM = 'DOF PROCESSOR                               '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         TDOF_MSG(1:)  = ' '
         TDOF_MSG(56:) = '(Before any AUTOSPC)'
         CALL DOF_PROC ( TDOF_MSG )

! Call subr to write any possible USETSTR output requests

         CALL WRITE_USETSTR

         CALL DEALLOCATE_MODEL_STUF ( 'SPCADD_SIDS' )

         IF (NMPC > 0) THEN
            CALL FILE_CLOSE ( L1S, LINK1S, 'KEEP', 'Y' )
         ENDIF

         IF (NRIGEL > 0) THEN
            CALL FILE_CLOSE ( L1F, LINK1F, 'KEEP', 'Y' )
         ENDIF

         IF ((NSPC > 0) .OR. (NSPC1 > 0)) THEN
            CALL FILE_CLOSE ( L1O, LINK1O, L1OSTAT, 'Y' )
         ENDIF

         IF (NDOFSE > 0) THEN
            IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                               &
               ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
               CALL FILE_CLOSE ( L1H, LINK1H, 'KEEP', 'Y' )
            ELSE
               CALL FILE_CLOSE ( L1H, LINK1H, 'DELETE', 'Y' )
            ENDIF
         ENDIF

         IF (NAOCARD > 0) THEN
            CALL FILE_CLOSE ( L1N, LINK1N, L1NSTAT, 'Y' )
         ENDIF

! CONM2 processing to get CONM2 data in basic coords at the mass
  
         CALL FILE_OPEN ( L1Y, LINK1Y, OUNT, 'REPLACE', L1Y_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL OURTIM
         MODNAM = 'CONM2 PROCESSOR #1                          '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL CONM2_PROC_1
         CALL FILE_CLOSE ( L1Y, LINK1Y, L1YSTAT, 'Y' )
  
! Grid point weight generator (model weight, c.g., etc.)

         CALL OURTIM
         MODNAM = 'ALLOCATE MEMORY FOR RBGLOBAL ARRAY          '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ALLOCATE_RBGLOBAL ( 'G ', SUBR_NAME )
         RBG_GSET_ALLOCATED = 'Y'

         IF ((GRDPNT_IN >= 0) .OR. (MEFFMASS_CALC == 'Y')) THEN

            CALL OURTIM
            MODNAM = 'GRID POINT WEIGHT GENERATOR              '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

            IF (NCUSERIN > 0) THEN
               DO I=1,NELE
                  IF (ETYPE(I) == 'USERIN  ') THEN
                     CALL GPWG_USERIN ( I )
                  ENDIF
               ENDDO
            ENDIF

            IF (NCUSERIN > 0) THEN
               CALL GPWG ( 'RESIDUAL STR' )
            ENDIF

            CALL GPWG ( 'OA MODEL    ' )

            WRITE(SC1,*) CR13

         ENDIF

      ENDIF res16

res17:IF (RESTART == 'Y') THEN

         CALL DEALLOCATE_MODEL_STUF ( 'CMASS, PMASS, RPMASS' )
         CALL DEALLOCATE_MODEL_STUF ( 'ELEM PROPERTIES AND MATERIALS' )
         CLOSE ( L1B, STATUS='KEEP')
         CLOSE ( L1G, STATUS='KEEP')
         CLOSE ( L1K, STATUS='KEEP')
         CLOSE ( L1Y, STATUS='KEEP')

         CALL OURTIM
         MODNAM = 'CHECKING FOR MATRICES TO PRINT ON RESTART'
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         IF ((PRTSTIFF(1) >= 1) .OR. (PRTSTIFD(1) >= 1) .OR. (EQCHK_OUTPUT(1) > 0)) THEN
            CALL ALLOCATE_SPARSE_MAT ( 'KGG', NDOFG, NTERM_KGG, SUBR_NAME )
            CALL READ_MATRIX_1 ( LINK1L, L1L, 'N', 'Y', L1LSTAT, L1L_MSG,'KGG', NTERM_KGG, 'Y', NDOFG, I_KGG, J_KGG, KGG)
         ENDIF
         CALL PRT_MATS_ON_RESTART
         WRITE(SC1, * ) '       Deallocate KGG          '
         CALL DEALLOCATE_SPARSE_MAT ( 'KGG' )

         IF (EQCHK_OUTPUT(1) > 0) THEN                     ! Do equilibrium check on the G-set stiffness matrix, if requested

            IF (RBG_GSET_ALLOCATED == 'N') THEN
               CALL ALLOCATE_RBGLOBAL ( 'G ', SUBR_NAME )
            ENDIF
            CALL RB_DISP_MATRIX_PROC ('EQCHK REF GRID', 0 )! Need rigid body displ matrix for the G-set if equil check requested
                                                           ! Get matrix diagonal and stats and print, if requested.
            IF ((PRTSTIFF(1) >= 1) .OR. (PRTSTIFD(1) >= 1)) THEN
               PRINTIT = 0                                 ! Diag stats were already printed in subr PRT_MATS_ON_RESTART
            ELSE
               PRINTIT = 1
            ENDIF
            CALL GET_MATRIX_DIAG_STATS ( 'KGG', 'G ', NDOFG, NTERM_KGG, I_KGG, J_KGG, KGG, PRINTIT, KGG_DIAG, KGG_MAX_DIAG  )

            CALL OURTIM
            MODNAM = 'EQUILIBRIUM CHECK ON KGG                  '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL STIFF_MAT_EQUIL_CHK ( EQCHK_OUTPUT(1), 'G ', SYM_KGG, NDOFG, NTERM_KGG, I_KGG, J_KGG, KGG, KGG_DIAG, KGG_MAX_DIAG,&
                                       RBGLOBAL_GSET )

         ENDIF

      ENDIF res17

res18:IF (RESTART == 'N') THEN

! CONM2 processing to get CONM2 data in global coords at the grid
  
         CALL OURTIM
         MODNAM = 'CONM2 PROCESSOR #2                          '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL CONM2_PROC_2
  
! Temperature data processing

         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            CALL OURTIM
            MODNAM = 'ALLOCATE MEM FOR ELEM AND GRID TEMP ARRAYS    '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_MODEL_STUF ( 'GTEMP', SUBR_NAME )
            CALL ALLOCATE_MODEL_STUF ( 'CGTEMP', SUBR_NAME )
            CALL ALLOCATE_MODEL_STUF ( 'ETEMP', SUBR_NAME )
            CALL ALLOCATE_MODEL_STUF ( 'CETEMP', SUBR_NAME )
            CALL ALLOCATE_MODEL_STUF ( 'TPNT, TDATA', SUBR_NAME )

            IF (NTCARD > 0) THEN
               OUNT(1) = ERR
               OUNT(2) = F06
               CALL FILE_OPEN ( L1K, LINK1K, OUNT, 'OLD', L1K_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'GRID & ELEM TEMPERATURE DATA PROCESSOR      '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL TEMPERATURE_DATA_PROC
               CALL FILE_CLOSE ( L1K, LINK1K, 'KEEP', 'Y' )
            ELSE
               CALL FILE_CLOSE ( L1K, LINK1K, 'DELETE', 'Y' )
            ENDIF
         ENDIF
         CALL DEALLOCATE_MODEL_STUF ( 'ETEMP' )
         CALL DEALLOCATE_MODEL_STUF ( 'CGTEMP' )
         CALL DEALLOCATE_MODEL_STUF ( 'CETEMP' )
  
! Element pressure data processing. Open L1Q which contains element pressure Bulk Data
        
         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            CALL OURTIM
            MODNAM = 'ALLOCATE MEMORY FOR ELEM PRESSURE DATA ARRAYS '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_MODEL_STUF ( 'PPNT, PDATA, PTYPE', SUBR_NAME )
            CALL ALLOCATE_MODEL_STUF ( 'PLOAD4_3D_DATA', SUBR_NAME )

            IF (NPCARD > 0) THEN
  
               CALL FILE_OPEN ( L1Q, LINK1Q, OUNT, 'OLD', L1Q_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
  
               CALL OURTIM
               MODNAM = 'ELEMENT PRESSURE DATA PROCESSOR             '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL PRESSURE_DATA_PROC
  
               CALL FILE_CLOSE ( L1Q, LINK1Q, 'KEEP', 'Y' )
            ENDIF
         ENDIF
         CALL DEALLOCATE_MODEL_STUF ( 'PRESS_SIDS' )
  
! Generate TR6_CG rigid body displ matrix for the R-set if this run has R-set DOF's. TR6_CG not needed until LINK6 but, by calc'ing
! it here we can avoid keeping CORD, RCORD, GRID, RGRID arrays from having to be allocated until then

         IF (NDOFR > 0) THEN

            CALL OURTIM
            MODNAM = 'GENERATE RIGID BODY DISPL MATRIX            '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL RB_DISP_MATRIX_PROC ( 'CG', 0 )
            CALL ALLOCATE_RBGLOBAL ( 'R ', SUBR_NAME )
            CALL TDOF_COL_NUM ( 'R ', R_SET_COL )

            DO I=1,NDOFG
               R_SET_DOF = TDOFI(I,R_SET_COL)
               IF (R_SET_DOF > 0) THEN
                  DO J=1,6
                     TR6_CG(R_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                  ENDDO
               ENDIF
            ENDDO

            IF (MEFMLOC == 'CG  ') THEN
               DO I=1,NDOFG
                  R_SET_DOF = TDOFI(I,R_SET_COL)
                  IF (R_SET_DOF > 0) THEN
                     DO J=1,6
                        TR6_MEFM(R_SET_DOF,J) = TR6_CG(R_SET_DOF,J)
                     ENDDO
                  ENDIF
               ENDDO
            ELSE
               IF (MEFMGRID == 0) THEN
                  CALL RB_DISP_MATRIX_PROC ( 'BASIC ORIGIN', 0 )
               ELSE
                  CALL RB_DISP_MATRIX_PROC ( 'GRID', MEFMGRID )
               ENDIF
               DO I=1,NDOFG
                  R_SET_DOF = TDOFI(I,R_SET_COL)
                  IF (R_SET_DOF > 0) THEN
                     DO J=1,6
                        TR6_MEFM(R_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF

            CALL RB_DISP_MATRIX_PROC ( 'BASIC ORIGIN', 0 )
            DO I=1,NDOFG
               R_SET_DOF = TDOFI(I,R_SET_COL)
               IF (R_SET_DOF > 0) THEN
                  DO J=1,6
                     TR6_0(R_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                  ENDDO
               ENDIF
            ENDDO

         ENDIF

! Generate rigid body displ matrix for the G-set if requested by user (if it has been, some EQCHK_OUTPUT(i) will be > 0)

         IF((EQCHK_OUTPUT(1) > 0) .OR. (EQCHK_OUTPUT(2) > 0) .OR. (EQCHK_OUTPUT(3) > 0) .OR. (EQCHK_OUTPUT(4) > 0) .OR.            &
            (EQCHK_OUTPUT(5) > 0)) THEN
            IF (RBG_GSET_ALLOCATED == 'N') THEN
               CALL OURTIM
               MODNAM = 'ALLOCATE MEMORY FOR RBGLOBAL ARRAY          '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL ALLOCATE_RBGLOBAL ( 'G ', SUBR_NAME )
            ENDIF
            CALL OURTIM                                 ! already calculated above
            MODNAM = 'GENERATE RIGID BODY DISPL MATRIX            '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL RB_DISP_MATRIX_PROC ( 'EQCHK REF GRID', 0 )
         ENDIF

! Calculate YSe enforced displ array

         IF (NDOFSE > 0) THEN

            IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                               &
               ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN

               CALL OURTIM
               MODNAM = 'ALLOCATE MEMORY FOR Yse ARRAY               '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL ALLOCATE_COL_VEC ( 'YSe', NDOFSE, SUBR_NAME )

               OUNT(1) = ERR
               OUNT(2) = F06
               CALL FILE_OPEN ( L1H, LINK1H, OUNT, 'OLD', L1H_MSG, 'READ_STIME', 'UNFORMATTED', 'READWRITE', 'REWIND',             &
                               'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'CALCULATE YS ENFORCED DISPL ARRAY           '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL YS_ARRAY
               CALL DEALLOCATE_COL_VEC ( 'YSe' )
               CALL FILE_CLOSE ( L1H, LINK1H, 'KEEP', 'Y' )

            ENDIF

         ENDIF

! Write problem size parameters

         WRITE(ERR,144) NGRID
         WRITE(ERR,145) NDOFG
         IF (SUPINFO == 'N') THEN
            WRITE(F06,144) NGRID
            WRITE(F06,145) NDOFG
         ENDIF
         IF (DEBUG(187) > 0) THEN
            CALL WRITE_ELEM_SUMMARY
         ENDIF
         WRITE(ERR,146) NELE
         IF (SUPINFO == 'N') THEN
            WRITE(F06,146) NELE
         ENDIF

! Write data to L1Z for restart (only need it if CHKPNT was in CC but write it anyway in case user changes their mind)

         CALL WRITE_L1Z

      ENDIF res18

! Deallocate several items and we will re-allocate them at the beginning of LINK1. This is needed for BUCKLING and NLSTATIC
! since LINK0 does not get re-run at each step but LINK1 does (and we need these arrays allocated each time LINK1 starts.
! Earlier, we had 'SINGLE ELEMENT ARRAYS' allocated here (but not in LINK1).

      CALL DEALLOCATE_MODEL_STUF ( 'SINGLE ELEMENT ARRAYS' )

! Write messages on what OUTPUT4 matrices were requested and open the OU4 files

      IF (NUM_OU4_REQUESTS > 0) THEN

         CALL OUTPUT4_MATRIX_MSGS ( OUNT )                 ! Write the messages on what OUTPUT4 matrices were requested

         DO I=1,MOU4                                       ! Determine which OU4 units will be needed
            OPEN_UNT(I) = 'N'
            DO J=1,NUM_OU4_REQUESTS
               IF (OU4_FILE_UNITS(J) == OU4(I)) THEN
                  OPEN_UNT(I) = 'Y'
               ENDIF
            ENDDO
         ENDDO

         DO I=1,MOU4                                       ! Open the OU4 units
            IF (OPEN_UNT(I) == 'Y') THEN
               CALL FILE_OPEN (OU4(I), OU4FIL(I), OUNT, 'REPLACE', OU4_MSG(I),'NEITHER','UNFORMATTED','WRITE','REWIND','Y','N','Y')
            ENDIF
         ENDDO

      ENDIF

! Write data to L1A (this rewrites L1A if this a restart - needed to tell LINK9 that this is a restart)

      COMM(LINKNO) = 'C'
res20:IF (RESTART == 'N') THEN
         CALL WRITE_L1A ( 'KEEP', 'Y', 'Y' )
      ENDIF res20

! Check allocation status of allocatable arrays, if requested

      IF (DEBUG(100) > 0) THEN
         CALL CHK_ARRAY_ALLOC_STAT
         IF (DEBUG(100) > 1) THEN
            CALL WRITE_ALLOC_MEM_TABLE ( 'at the end of '//SUBR_NAME )
         ENDIF
      ENDIF

! Write LINK0 end to F04, F06

      CALL OURTIM
      IF (WRT_LOG > 0) THEN
         WRITE(F04,151) LINKNO
      ENDIF
      WRITE(F06,151) LINKNO

      IF (( DEBUG(193) == 1) .OR. (DEBUG(193) == 999)) THEN
         CALL FILE_INQUIRE ( 'Near end of LINK0' )
      ENDIF

! Write LINK0 end to screen

      WRITE(SC1,154) LINKNO

! **********************************************************************************************************************************
   10 FORMAT(' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>',&
             '>>>>>>>>>>>>>>>>>')

   11 FORMAT(/,' *INFORMATION: KIND PARAMETERS ARE: CHARACTER BYTE                                  = ',I4,' BYTES',/ &
               '                                    SHORT INTEGER                                   = ',I4,' BYTES',/ &
               '                                    LONG  INTEGER                                   = ',I4,' BYTES',/ &
               '                                    SINGLE PREC REAL                                = ',I4,' BYTES',/ &
               '                                    DOUBLE PREC REAL                                = ',I4,' BYTES',/ &
               '                                    QUAD   PREC REAL                                = ',I4,' BYTES')  

  144 FORMAT(' *INFORMATION: NUMBER OF GRID POINTS                                                  = ',I12)

  145 FORMAT(' *INFORMATION: NUMBER OF G SET DEGREES OF FREEDOM (NDOFG)                             = ',I12,/)

  146 FORMAT(' *INFORMATION: TOTAL NUMBER OF ELEMENTS (NELE)                                        = ',I12,/)

  150 FORMAT(/,' >> LINK',I3,' BEGIN',/)

  151 FORMAT(/,' >> LINK',I3,' END',/)

  152 FORMAT(/,' >> LINK',I3,' BEGIN')

  154 FORMAT(  ' >> LINK',I3,' END')

 1001 FORMAT(' *INFORMATION: BANDIT DID NOT RUN SUCCESSFULLY SO USER REQUEST FOR ',A,' = ',I2,' CANNOT BE USED.' ,                 &
                           ' IT IS RESET TO DEFAULT VALUE = ',I2)

 1030 FORMAT(' *INFORMATION: BANDIT WAS CALLED TO RESEQUENCE THE GRIDS AND HAS RETURNED WITH ERROR  = ',I8,/)

 1092 FORMAT(1X,I2,'/',A44,18X,2X,I2,':',I2,':',I2,'.',I3)

 1802 FORMAT(' *ERROR  1802: THERE MUST BE THE SAME NUMBER OF CUSERIN ELEM CONN ENTRIES, (NCUSERIN = ',I8,')'                      &
                    ,/,14X,'                    AND NUMBER OF PUSERIN PROPERTY  ENTRIES, (NPUSERIN = ',I8,')')

 1804 FORMAT(' *ERROR  1804: ABSOLUTE VALUE OF BULK DATA PARAM WTMASS MUST BE > 0 BUT IS = ',1ES14.6)

 1820 FORMAT(' *ERROR  1820: THERE MUST BE AT LEAST 1 ELEMENT IN THE BULK DATA. HOWEVER, NELE = ',I8)

 1827 FORMAT(' *ERROR  1827: THERE MUST BE AT LEAST 1 GRID IN THE BULK DATA. HOWEVER, NGRID = ',I8)

 9401 FORMAT(' *WARNING    : REQUEST FOR CHKPNT IS NOT ALLOWED IN SOLUTION ',A)

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')

 9998 FORMAT(/,' PROCESSING TERMINATED DUE TO ',I8,' INPUT ERRORS. CHECK OUTPUT FILE FOR ERROR MESSAGES')

12345 FORMAT(A, ', I = ', I8, ' of ', I8, A)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE ELDT_PROC_FOR_RESTART ( CHK_ELDT_BIT )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_FIJ
      USE SCONTR, ONLY                :  ELDT_BUG_DAT1_BIT, ELDT_BUG_DAT1_BIT, ELDT_BUG_ME_BIT  , ELDT_BUG_P_T_BIT ,               &
                                         ELDT_BUG_SE_BIT  , ELDT_BUG_KE_BIT  , ELDT_BUG_SHPJ_BIT, ELDT_BUG_BMAT_BIT,               &
                                         ELDT_BUG_BCHK_BIT, ELDT_F21_P_T_BIT , ELDT_F22_ME_BIT  , ELDT_F23_KE_BIT  ,               &
                                         ELDT_F24_SE_BIT  , MBUG, MFIJ, MELDTS

      IMPLICIT NONE

      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr EMG (to tell it what to calc)

                                                           ! Array used to tell subr ELDT_PROC_FOR_RESTART which ELDT to calc
      INTEGER(LONG), INTENT(IN)       :: CHK_ELDT_BIT(0:MELDTS-1)

      INTEGER(LONG)                   :: II,jj             ! DO loop indices
      INTEGER(LONG)                   :: I00               ! Indicex
      INTEGER(LONG)                   :: IIERR             ! Error indicator

! **********************************************************************************************************************************
      DO II=1,6
         OPT(II) = 'N'
      ENDDO

      DO II=0,MBUG-1
         WRT_BUG(II) = 0
      ENDDO

      IF (CHK_ELDT_BIT( 0) > 0) THEN
         WRT_BUG(0) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 1) > 0) THEN
         WRT_BUG(1) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 2) > 0) THEN
         OPT(1)     = 'Y'
         WRT_BUG(2) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 3) > 0) THEN
         OPT(2)     = 'Y'
         OPT(5)     = 'Y'
         WRT_BUG(3) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 4) > 0) THEN
         OPT(3)     = 'Y'
         WRT_BUG(4) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 5) > 0) THEN
         OPT(4)     = 'Y'
         WRT_BUG(5) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 6) > 0) THEN
         OPT(5)     = 'Y'
         WRT_BUG(6) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 7) > 0) THEN
         OPT(5)     = 'Y'
         WRT_BUG(7) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 8) > 0) THEN
         OPT(5)     = 'Y'
         WRT_BUG(8) = 1
      ENDIF

      IF (CHK_ELDT_BIT( 9) > 0) THEN
         OPT(5)     = 'Y'
         WRT_BUG(9) = 1
      ENDIF

      NUM_EMG_FATAL_ERRS = 0
      IIERR = 0
      I00   = 0
      DO II=1,NELE

         I00 = 0

         DO JJ=0,MBUG-1
            WRT_BUG(JJ) = 0
         ENDDO

         DO JJ=1,MFIJ
            WRT_FIJ(JJ) = 0
         ENDDO

         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_DAT1_BIT) ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(0) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_DAT2_BIT) ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(1) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_P_T_BIT)  ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(2) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_ME_BIT)   ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(3) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_KE_BIT)   ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(4) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_SE_BIT)   ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(5) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_U_P_BIT)  ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(6) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_SHPJ_BIT) ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(7) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_BMAT_BIT) ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(8) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_BUG_BCHK_BIT) ) > 0) THEN   ;   I00 = 1   ;   WRT_BUG(9) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_F21_P_T_BIT)  ) > 0) THEN   ;   I00 = 1   ;   WRT_FIJ(1) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_F22_ME_BIT)   ) > 0) THEN   ;   I00 = 1   ;   WRT_FIJ(2) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_F23_KE_BIT)   ) > 0) THEN   ;   I00 = 1   ;   WRT_FIJ(3) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_F24_SE_BIT)   ) > 0) THEN   ;   I00 = 1   ;   WRT_FIJ(4) = 1   ;   ENDIF
         IF ( IAND(ELDT(II),IBIT(ELDT_F25_U_P_BIT)  ) > 0) THEN   ;   I00 = 1   ;   WRT_FIJ(5) = 1   ;   ENDIF

         IF (I00 > 0) THEN

            WRITE(BUG,10)
            PLY_NUM = 0
            CALL EMG ( II  , OPT, 'N', SUBR_NAME, 'Y' )    ! 'Y' means write to BUG file

            IF (NUM_EMG_FATAL_ERRS /=0) THEN
               IIERR = IIERR + NUM_EMG_FATAL_ERRS
               CYCLE
            ENDIF

            IF ((WRT_FIJ(1) > 0) .OR. (WRT_FIJ(2) > 0) .OR. (WRT_FIJ(3) > 0) .OR. (WRT_FIJ(4) > 0) .OR. (WRT_FIJ(5) > 0)) THEN
               CALL WRITE_FIJFIL ( 9999, 0 )
            ENDIF

         ENDIF

      ENDDO

      IF (IIERR > 0) THEN
         WRITE(ERR,9876) IIERR
         WRITE(F06,9876) IIERR
         CALL OUTA_HERE ( 'Y' )                         ! Errors from subr EMG, so quit
      ENDIF

! **********************************************************************************************************************************
   10 FORMAT(' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>',&
             '>>>>>>>>>>>>>>>>>')

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')

! **********************************************************************************************************************************

      END SUBROUTINE ELDT_PROC_FOR_RESTART

! ##################################################################################################################################

      SUBROUTINE WRITE_ELEM_SUMMARY

      USE SCONTR, ONLY                :  NCBAR, NCBEAM, NCELAS1, NCELAS2, NCELAS3, NCELAS4, NCHEXA8, NCHEXA20, NCPENTA6, NCPENTA15,&
                                         NCQUAD4, NCQUAD4K, NCROD, NCTETRA4, NCTETRA10, NCTRIA3, NCTRIA3K, NCUSER1, NCUSERIN

      INTEGER(LONG)                   :: TOTAL = 0         ! Sum of number of elements written

! **********************************************************************************************************************************
      WRITE(F06,7776)

      IF (NCBAR     > 0) THEN
         TOTAL = TOTAL + NCBAR
         WRITE(F06,7777) 'BAR    ', NCBAR 
      ENDIF

      IF (NCBEAM    > 0) THEN
         TOTAL = TOTAL + NCBEAM
         WRITE(F06,7777) 'BEAM   ', NCBEAM 
      ENDIF

      IF (NCELAS1   > 0) THEN
         TOTAL = TOTAL + NCELAS1
         WRITE(F06,7777) 'CELAS1 ', NCELAS1 
      ENDIF

      IF (NCELAS2   > 0) THEN
         TOTAL = TOTAL + NCELAS2
         WRITE(F06,7777) 'CELAS2 ', NCELAS2
      ENDIF

      IF (NCELAS3   > 0) THEN
         TOTAL = TOTAL + NCELAS3
         WRITE(F06,7777) 'CELAS3 ', NCELAS3 
      ENDIF

      IF (NCELAS4   > 0) THEN
         TOTAL = TOTAL + NCELAS4
         WRITE(F06,7777) 'CELAS4 ', NCELAS4 
      ENDIF

      IF (NCHEXA8   > 0) THEN
         TOTAL = TOTAL + NCHEXA8
         WRITE(F06,7777) 'HEXA8  ', NCHEXA8 
      ENDIF

      IF (NCHEXA20  > 0) THEN
         TOTAL = TOTAL + NCHEXA20
         WRITE(F06,7777) 'HEXA20 ', NCHEXA20 
      ENDIF

      IF (NCPENTA6  > 0) THEN
         TOTAL = TOTAL + NCPENTA6
         WRITE(F06,7777) 'PENTA6 ', NCPENTA6 
      ENDIF

      IF (NCPENTA15 > 0) THEN
         TOTAL = TOTAL + NCPENTA15
         WRITE(F06,7777) 'PENTA15', NCPENTA15 
      ENDIF

      IF (NCQUAD4   > 0) THEN
         TOTAL = TOTAL + NCQUAD4
         WRITE(F06,7777) 'QUAD4  ', NCQUAD4 
      ENDIF

      IF (NCQUAD4K  > 0) THEN
         TOTAL = TOTAL + NCQUAD4K
         WRITE(F06,7777) 'QUAD4K ', NCQUAD4K 
      ENDIF

      IF (NCROD     > 0) THEN
         TOTAL = TOTAL + NCROD
         WRITE(F06,7777) 'ROD    ', NCROD 
      ENDIF

      IF (NCTETRA4  > 0) THEN
         TOTAL = TOTAL + NCTETRA4
         WRITE(F06,7777) 'TETRA4 ', NCTETRA4 
      ENDIF

      IF (NCTETRA10 > 0) THEN
         TOTAL = TOTAL + NCTETRA10
         WRITE(F06,7777) 'TETRA10', NCTETRA10 
      ENDIF

      IF (NCTRIA3   > 0) THEN
         TOTAL = TOTAL + NCTRIA3
         WRITE(F06,7777) 'TRIA3  ', NCTRIA3 
      ENDIF

      IF (NCTRIA3K  > 0) THEN
         TOTAL = TOTAL + NCTRIA3K
         WRITE(F06,7777) 'TRIA3K ', NCTRIA3K 
      ENDIF

      IF (NCUSER1   > 0) THEN
         TOTAL = TOTAL + NCUSER1
         WRITE(F06,7777) 'USER1  ', NCUSER1 
      ENDIF

      IF (NCUSERIN  > 0) THEN
         TOTAL = TOTAL + NCUSERIN
         WRITE(F06,7777) 'USERIN ', NCUSERIN 
      ENDIF

      IF (TOTAL /= NELE) THEN
         WRITE(F06,7779) TOTAL - NELE
      ENDIF

      WRITE(F06,7778)

! **********************************************************************************************************************************
 7776 FORMAT(' *INFORMATION: FOLLOWING IS A LIST OF ALL ELEMENTS IN THE INPUT DATA:')

 7777 FORMAT(15X,'Number of ',A,' elements                                             = ',I12)

 7778 FORMAT(15X,'                                                                         ------------')

 7779 FORMAT(15X,'Other                                                                  = ',I12)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_ELEM_SUMMARY

      END SUBROUTINE LINK0
