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
  
      SUBROUTINE LINK1
  
! LINK1 processes:

!  - MPC's and rigid elements resulting in a sparse RMG array of constraint coefficients
!  - applied forces (incl grid forces and moments, gravity, pressure, thermal, centrifugal, scalar). Result is a sparse load array
!  - G-set mass, stiffness and load matrices. Results are sparse stiffness, mass and load arrays
!  - process differential stiffness resulting in a sparse array of differential stiffness values
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG

      USE IOUNT1, ONLY                :  WRT_LOG

      USE IOUNT1, ONLY                :  ERR, F04, F06, F21, F22, F23, F24, L1C, L1F, L1I, L1G, L1J, L1P, L1S, L1U, L1W, SC1
                                         

      USE IOUNT1, ONLY                :  F21FIL, F22FIL, F23FIL, F24FIL, LINK1C, LINK1F, LINK1I, LINK1G, LINK1J, LINK1P, LINK1S,   &
                                         LINK1U, LINK1W

      USE IOUNT1, ONLY                :  L1FSTAT, L1ISTAT, L1PSTAT, L1SSTAT, L1USTAT, L1WSTAT

      USE IOUNT1, ONLY                :  F21_MSG, F22_MSG, F23_MSG, F24_MSG, L1F_MSG, L1G_MSG, L1I_MSG, L1J_MSG, L1P_MSG, L1S_MSG, &
                                         L1U_MSG, L1W_MSG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, COMM, ELDT_F21_P_T_BIT, ELDT_F22_ME_BIT, ELDT_F23_KE_BIT, ELDT_F24_SE_BIT,  &
                                         FATAL_ERR, IBIT, LINKNO, LTERM_KGG, LTERM_KGGD, LTERM_MGGE, NDOFM, NFORCE,                &
                                         NGRAV, NMPC, NPLOAD, NRFORCE, NRIGEL, NSLOAD, NTERM_RMG, NTSUB, RESTART, SOL_NAME

      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE DOF_TABLES, ONLY            :  TDOFI

      USE PARAMS, ONLY                :  EMP0_PAUSE, ESP0_PAUSE, SETLKTK, SKIPMGG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  OELDT
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
  
      USE LINK1_USE_IFs
                       
      IMPLICIT NONE

      LOGICAL                         :: LEXIST            ! .TRUE. if a file exists
      LOGICAL                         :: LOPEN             ! .TRUE. if a file is opened

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LINK1'
      CHARACTER(10*BYTE)              :: LTERM_NAME        ! Name for an LTERM value
      CHARACTER(44*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run
      CHARACTER( 1*BYTE)              :: RESPONSE          ! User response ('Y' or 'N') to a screen prompt
     
      INTEGER(LONG)                   :: BUCKLING_STEP     ! If SOL is BUCKLING then this is step 1 or 2 in the process, otherwise 0
      INTEGER(LONG)                   :: I1                ! Intermediate integer variable
      INTEGER(LONG)                   :: I2                ! Intermediate integer variable
      INTEGER(LONG)                   :: I3                ! Intermediate integer variable
      INTEGER(LONG)                   :: I4                ! Intermediate integer variable
      INTEGER(LONG)                   :: LTERM             ! Value for LTERM_KGGD or LTERM_KGG 
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), PARAMETER        :: P_LINKNO    = 0   ! Prior LINK no's that should have run before this LINK can execute

      INTRINSIC                       :: IAND
   
! **********************************************************************************************************************************
      LINKNO = 1

! Set time initializing parameters

      CALL TIME_INIT

! Get date and time, write to screen

      CALL OURDAT
      CALL OURTIM
      WRITE(SC1,152) LINKNO

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
 
!xx   CALL READ_L1A ( 'KEEP', 'Y' )
      CALL INIT_COUNTERS

! Check COMM for successful completion of prior LINKs

      IF (COMM(P_LINKNO) /= 'C') THEN
         WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
         WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Prior LINK's didn't complete, so quit
      ENDIF

! Allocate DOF tables

      CALL OURTIM
      MODNAM = 'ALLOCATE DOF TABLES'
      WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

! Set BUCKLING_STEP based on LOAD_ISTEP (see subr MYSTRAN.FOR)

      BUCKLING_STEP = 0
      IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         BUCKLING_STEP = LOAD_ISTEP
      ENDIF

! Allocate several arrays since they are needed every time LINK1 gets re-run (for BUCKLING and NLSTATIC). They were used in
! LINK0 but deallocated there at the end of LINK0 so that we could allocate them again here

      CALL ALLOCATE_MODEL_STUF ( 'SINGLE ELEMENT ARRAYS', SUBR_NAME )

res19:IF (RESTART == 'N') THEN

         CALL ALLOCATE_MODEL_STUF ( 'SYS_LOAD', SUBR_NAME )

! Process MPC's and rigid elements.

         IF ((NMPC > 0) .OR. (NRIGEL > 0)) THEN

            CALL FILE_OPEN ( L1J, LINK1J, OUNT, 'REPLACE', L1J_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

            IF (NMPC > 0) THEN 
               CALL FILE_OPEN ( L1S, LINK1S, OUNT, 'OLD', L1S_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'MPC PROCESSOR                               '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL MPC_PROC
               CALL FILE_CLOSE ( L1S, LINK1S, 'KEEP', 'Y' )
            ENDIF

            IF (NRIGEL > 0) THEN                           ! Process rigid elements.
               CALL FILE_OPEN ( L1F, LINK1F, OUNT, 'OLD', L1F_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'RIGID ELEMENT PROCESSOR                     '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL RIGID_ELEM_PROC
               CALL FILE_CLOSE ( L1F, LINK1F, 'KEEP', 'Y' )
            ENDIF

            CALL FILE_CLOSE ( L1J, LINK1J, 'KEEP', 'Y' )   ! Subr SPARSE_RMG will reopen LINK1S

            CALL OURTIM
            MODNAM = 'ALLOCATE MEMORY FOR RMG ARRAY               '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL OURTIM                                    ! Generate sparse RMG (constraint) matrix.
            MODNAM = 'SPARSE RMG PROCESSOR                        '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL SPARSE_RMG
            CALL FILE_CLOSE ( L1J, LINK1J, 'KEEP', 'Y' )

         ENDIF

! FORCE/MOMENT processing. Open L1I which contains FORCE/MOMENT Bulk Data.
  
            IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                               &
               ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            CALL OURTIM
            MODNAM = 'ALLOCATE MEMORY FOR SYS_LOAD ARRAY          '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            IF (NFORCE > 0) THEN
               CALL FILE_OPEN ( L1I, LINK1I, OUNT, 'OLD', L1I_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'FORCE/MOMENT PROCESSOR                      '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL FORCE_MOM_PROC
               CALL FILE_CLOSE ( L1I, LINK1I, L1ISTAT, 'Y' )
            ENDIF
         ENDIF
     
! Element thermal and pressure load processing
  
         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR. (SOL_NAME(1:8) == 'BUCKLING')) THEN
!xx         ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            CALL OURTIM
            MODNAM = 'ALLOCATE MEMORY FOR THERMAL LOAD ARRAYS       '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            IF ((NPLOAD > 0) .OR. (NTSUB > 0)) THEN
               CALL OURTIM
               MODNAM = 'ELEMENT THERMAL AND PRESSURE LOAD PROCESSOR '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL EPTL
            ENDIF
         ENDIF
         INQUIRE ( FILE=F21FIL, EXIST=LEXIST, OPENED=LOPEN )
         IF (LOPEN) THEN
            CALL FILE_CLOSE ( F21, F21FIL, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( F21, F21FIL, 'DELETE', 'Y' )
         ENDIF
! Generate G-set mass matrix, MGG
         IF (SKIPMGG == 'N') THEN

            CALL OURTIM
            MODNAM = 'CALCULATE ESTIMATE OF MASS MATRIX SIZE      '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL EMP0                                        ! Calcs estimate of LTERM_MGGE
            IF (EMP0_PAUSE == 'Y') THEN
               WRITE(SC1,'(A,I12)') 'From EMP0: LTERM_MGGE  = ',LTERM_MGGE
               WRITE(SC1,*) 'Do you want to change LTERM_MGGE estimate? (Y/N)'
               READ(*,*) RESPONSE
               IF ((RESPONSE == 'Y') .OR. (RESPONSE == 'y')) THEN
                  WRITE(SC1,*) 'Enter new LTERM_MGGE'
                  WRITE(SC1,*)
                  READ (*,*) LTERM_MGGE
                  WRITE(SC1,'(A,I12)') 'New LTERM_MGGE will be = ',LTERM_MGGE
               ENDIF
            ENDIF
      
            CALL OURTIM
            MODNAM = 'ALLOCATE MEM FOR EMSKEY, EMSCOL, EMSPNT, EMS'
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_EMS_ARRAYS ( SUBR_NAME )
      
            CALL OURTIM
            MODNAM = 'ELEMENT MASS MATRIX PROCESSOR               '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL EMP
            INQUIRE ( FILE=F22FIL, EXIST=LEXIST, OPENED=LOPEN )
            IF (LOPEN) THEN
               CALL FILE_CLOSE ( F22, F22FIL, 'KEEP', 'Y' )
            ELSE
               CALL FILE_CLOSE ( F22, F22FIL, 'DELETE', 'Y' )
            ENDIF

! Formulate MGGC mass matrix for concentrated masses

            CALL OURTIM
            MODNAM = 'CONCENTRATED MASS MATRIX PROCESSOR          '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL MGGC_MASS_MATRIX

            CALL OURTIM
            MODNAM = 'ALLOCATE MEMORY FOR ELEM MASS ARRAYS        '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_L1_MGG ( 'MGGE', SUBR_NAME )

! Convert system mass matrix from linked list format to sparse format

            CALL OURTIM
            MODNAM = 'SPARSE MGG PROCESSOR                        '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL SPARSE_MGG

            CALL DEALLOCATE_EMS_ARRAYS
            CALL DEALLOCATE_L1_MGG ( 'MGGE'   )
            CALL DEALLOCATE_L1_MGG ( 'MGGC'   )
            CALL DEALLOCATE_L1_MGG ( 'MGGS'   )
            IF ((SOL_NAME(1:8) /= 'BUCKLING') .AND. (SOL_NAME(1:8) /= 'NLSTATIC')) THEN
               CALL DEALLOCATE_MODEL_STUF ( 'CONM2, RCONM2' )
               CALL DEALLOCATE_MODEL_STUF ( 'CMASS, PMASS, RPMASS' )
            ENDIF

         ELSE

!           IF (OLD_NTERM_MGG > 0) THEN
!           ELSE
!           ENDIF

         ENDIF

! Gravity load processing. Open L1P which contains GRAV Bulk Data.
  
         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            IF (NGRAV > 0) THEN
               OUNT(1) = ERR
               OUNT(2) = F06
               CALL FILE_OPEN ( L1P, LINK1P, OUNT, 'OLD', L1P_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'GRAV LOAD PROCESSOR                         '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL GRAV_PROC
               CALL FILE_CLOSE ( L1P, LINK1P, L1PSTAT, 'Y' )
            ENDIF
         ENDIF
  
! RFORCE load processing. Open L1U which contains RFORCE Bulk Data.
  
         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            IF (NRFORCE > 0) THEN
               OUNT(1) = ERR
               OUNT(2) = F06
               CALL FILE_OPEN ( L1U, LINK1U, OUNT, 'OLD', L1U_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'RFORCE LOAD PROCESSOR                       '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL RFORCE_PROC
               CALL FILE_CLOSE ( L1U, LINK1U, L1USTAT, 'Y' )
            ENDIF
         ENDIF
         CALL DEALLOCATE_L1_MGG ( 'I2_MGG' )

! SLOAD load processing. Open L1W which contains SLOAD Bulk Data.
  
         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            IF (NSLOAD > 0) THEN
               OUNT(1) = ERR
               OUNT(2) = F06
               CALL FILE_OPEN ( L1W, LINK1W, OUNT, 'OLD', L1W_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
               CALL OURTIM
               MODNAM = 'SLOAD LOAD PROCESSOR                        '
               WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL SLOAD_PROC
               CALL FILE_CLOSE ( L1W, LINK1W, L1WSTAT, 'Y' )
            ENDIF
         ENDIF
  
! Deallocate

         CALL DEALLOCATE_MODEL_STUF ( 'LOAD_SIDS, LOAD_FACS' )

! Convert loads to sparse form and write to file

         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            CALL OURTIM
            MODNAM = 'CONVERT LOADS TO SPARSE MATRIX FORM         '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL SPARSE_PG
            CALL DEALLOCATE_MODEL_STUF ( 'SYS_LOAD' )
         ENDIF

! Estimate LTERM so arrays can be allocated for G-set stiffness matrix
  
         CALL OURTIM
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ESP0
         IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
            MODNAM     = 'CALCULATE ESTIMATE OF KGGD MATRIX SIZE        '
            LTERM_NAME = 'LTERM_KGGD'
            LTERM      =  LTERM_KGGD
         ELSE
            MODNAM     = 'CALCULATE ESTIMATE OF KGG MATRIX SIZE         '
            LTERM_NAME = 'LTERM_KGG'
            LTERM      =  LTERM_KGG
         ENDIF
         
         IF (ESP0_PAUSE == 'Y') THEN
            WRITE(SC1,'(A,A,A,I12)') ' From ESP0: ', LTERM_NAME,' = ',LTERM
            WRITE(SC1,'(A,A,A)') ' Do you want to change ',LTERM_NAME,' estimate? (Y/N)'
            READ(*,*) RESPONSE
            IF ((RESPONSE == 'Y') .OR. (RESPONSE == 'y')) THEN
               WRITE(SC1,'(A,A)') 'Enter new ', LTERM_NAME
               WRITE(SC1,*)
               READ (*,*) LTERM
               IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
                  LTERM_KGGD = LTERM
               ELSE
                  LTERM_KGG  = LTERM
               ENDIF
               WRITE(SC1,'(A,A,A,I12)') 'New ', LTERM_NAME,' will be = ',LTERM
            ENDIF
         ENDIF
   
         if (setlktk /= 3) then                            ! Subr ESP0 estimated LTERM conservatively. Now allocate this amount
            CALL OURTIM
            MODNAM = 'ALLOCATE MEM FOR STFKEY, STFCOL, STFPNT, STF'
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_STF_ARRAYS ( 'STFKEY', SUBR_NAME )
            CALL ALLOCATE_STF_ARRAYS ( 'STF3', SUBR_NAME )
         else
            Write(err,*) '*ERROR     : PROGRAMMING ERROR IN SUBR ',SUBR_NAME,' SETLKTK CANNOT = 3'
            Write(f06,*) '*ERROR     : PROGRAMMING ERROR IN SUBR ',SUBR_NAME,' SETLKTK CANNOT = 3'
         endif
     
! Compute element stiffness and merge into system stiffness matrix.
  
         CALL OURTIM
         MODNAM = 'G-SET STIFFNESS MATRIX PROCESSOR            '
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL ESP

         IF ((SOL_NAME(1:8) /= 'BUCKLING') .AND. (SOL_NAME(1:8) /= 'NLSTATIC') .AND. (SOL_NAME(1:8) /= 'DIFFEREN')) THEN
            CALL DEALLOCATE_MODEL_STUF ( 'SCNUM' )
            CALL DEALLOCATE_MODEL_STUF ( 'ELDT' )
            CALL DEALLOCATE_MODEL_STUF ( 'TPNT, TDATA' )
            CALL DEALLOCATE_MODEL_STUF ( 'PPNT, PDATA, PTYPE' )
            CALL DEALLOCATE_MODEL_STUF ( 'PLOAD4_3D_DATA' )
            CALL DEALLOCATE_MODEL_STUF ( 'GTEMP' )
         ENDIF

         INQUIRE ( FILE=F23FIL, EXIST=LEXIST, OPENED=LOPEN )
         IF (LOPEN) THEN
            CALL FILE_CLOSE ( F23, F23FIL, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( F23, F23FIL, 'DELETE', 'Y' )
         ENDIF

         CALL DEALLOCATE_IN4_FILES ( 'IN4FIL' )

         INQUIRE ( FILE=F24FIL, EXIST=LEXIST, OPENED=LOPEN )
         IF (LOPEN) THEN
            CALL FILE_CLOSE ( F24, F24FIL, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( F24, F24FIL, 'DELETE', 'Y' )
         ENDIF

! Convert system stiff matrix from linked list format to sparse format (SPARSE_KGG calls grid singularity check subr)

         IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
            CALL OURTIM
            MODNAM = 'SPARSE KGGD PROCESSOR                       '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL SPARSE_KGGD
            CALL DEALLOCATE_MODEL_STUF ( 'MPC_IND_GRIDS' )
            CALL DEALLOCATE_STF_ARRAYS ( 'STFKEY' )
            CALL DEALLOCATE_STF_ARRAYS ( 'STF3' )
         ELSE
            CALL OURTIM
            MODNAM = 'SPARSE KGG PROCESSOR                        '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL SPARSE_KGG
            CALL DEALLOCATE_MODEL_STUF ( 'MPC_IND_GRIDS' )
            CALL DEALLOCATE_STF_ARRAYS ( 'STFKEY' )
            CALL DEALLOCATE_STF_ARRAYS ( 'STF3' )
         ENDIF

! Write DOF tables and deallocate

         CALL OURTIM
         MODNAM = 'WRITE DOF TABLES TO FILE AND DEALLOCATE     '
         WRITE(SC1,*) CR13
         WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL WRITE_DOF_TABLES
         CALL FILE_CLOSE ( L1C, LINK1C, 'KEEP', 'Y' )

! Write element data to L1G. Save L1G for use in LINK9.
         IF (LOAD_ISTEP == 1) THEN

            CALL FILE_OPEN ( L1G, LINK1G, OUNT, 'REPLACE', L1G_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

            CALL OURTIM
            MODNAM = 'WRITE ELEMENT DATA TO FILE                  '
            WRITE(SC1,1092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL ELSAVE

            IF ((SOL_NAME(1:8) /= 'BUCKLING') .AND. (SOL_NAME(1:8) /= 'NLSTATIC') .AND. (SOL_NAME(1:8) /= 'DIFFEREN')) THEN
               CALL DEALLOCATE_MODEL_STUF ( 'ETYPE, EDAT, EPNT' )
               CALL DEALLOCATE_MODEL_STUF ( 'VVEC, OFFSETS, PLATE stuff' )
               CALL DEALLOCATE_MODEL_STUF ( 'ELEM PROPERTIES AND MATERIALS' )
               CALL DEALLOCATE_MODEL_STUF ( 'EOFF' )
               CALL DEALLOCATE_MODEL_STUF ( 'ESORT1' )
               CALL DEALLOCATE_MODEL_STUF ( 'ESORT2' )
            ENDIF

            CALL FILE_CLOSE ( L1G, LINK1G, 'KEEP', 'Y' )
  
         ENDIF

      ENDIF res19
  
! Deallocate

      CALL DEALLOCATE_MODEL_STUF ( 'SINGLE ELEMENT ARRAYS' )
      IF ((SOL_NAME(1:8) /= 'NLSTATIC') .AND. (SOL_NAME(1:8) /= 'DIFFEREN')) THEN
      	CALL DEALLOCATE_MODEL_STUF ( 'SUBLOD' )
      ENDIF

! Check allocation status of allocatable arrays, if requested

      IF (DEBUG(100) > 0) THEN
         CALL CHK_ARRAY_ALLOC_STAT
         IF (DEBUG(100) > 1) THEN
            CALL WRITE_ALLOC_MEM_TABLE ( 'at the end of '//SUBR_NAME )
         ENDIF
      ENDIF

! Write data to L1A (this rewrites L1A if this a restart - needed to tell LINK9 that this is a restart)

      COMM(LINKNO) = 'C'
res20:IF (RESTART == 'N') THEN
         CALL WRITE_L1A ( 'KEEP', 'Y', 'Y' )
      ENDIF res20

! Write LINK1 end to F04, F06

      CALL OURTIM
      IF (WRT_LOG > 0) THEN
         WRITE(F04,151) LINKNO
      ENDIF
      WRITE(F06,151) LINKNO

      IF (( DEBUG(193) == 1) .OR. (DEBUG(193) == 999)) THEN
         CALL FILE_INQUIRE ( 'near end of LINK1' )
      ENDIF

! Write LINK1 end to screen
      WRITE(SC1,154) LINKNO

! **********************************************************************************************************************************
  150 FORMAT(/,' >> LINK',I3,' BEGIN',/)

  151 FORMAT(/,' >> LINK',I3,' END',/)

  152 FORMAT(/,' >> LINK',I3,' BEGIN')

  154 FORMAT(  ' >> LINK',I3,' END')

 1092 FORMAT(1X,I2,'/',A44,18X,2X,I2,':',I2,':',I2,'.',I3)

 9998 FORMAT(/,' PROCESSING TERMINATED DUE TO ',I8,' INPUT ERRORS. CHECK OUTPUT FILE FOR ERROR MESSAGES')

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE INIT_COUNTERS

      USE PENTIUM_II_KIND
      USE SCONTR

      IMPLICIT NONE

! **********************************************************************************************************************************
      NTERM_KGG  = 0   ;   LTERM_KGG  = 0
      NTERM_KGGD = 0   ;   LTERM_KGGD = 0
      NTERM_MGG  = 0
      NTERM_MGGC = 0
      NTERM_MGGE = 0   ;   LTERM_MGGE = 0
      NTERM_MGGS = 0
      NTERM_PG   = 0
!     NTERM_RMG  = 0
!     NTERM_RMM  = 0
!     NTERM_RMN  = 0

! **********************************************************************************************************************************

      END SUBROUTINE INIT_COUNTERS

      END SUBROUTINE LINK1
