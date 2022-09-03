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
 
      SUBROUTINE ESP
 
! Element stiffness processor
 
! ESP generates the G-set stiffness matrix and puts it into the 1D array STF of nonzero stiffness terms above the
! diagonal.
 
! ESP processes the elements sequentially to generate element KE matrix using the EMG set of routines. The element
! stiffness are transformed from local to basic to global coords for each grid and then merged into the system
! stiffness, STF, array. See explanation, with an example, in module STF_ARRAYS


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, F23, F23FIL, F23_MSG, F24, F24FIL, F24_MSG, FILE_NAM_MAXLEN, SC1, SCR,     &
                                         WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_KE_BIT, ELDT_BUG_SE_BIT,                                           &
                                         ELDT_F23_KE_BIT, ELDT_F24_SE_BIT, ELDT_BUG_BCHK_BIT, ELDT_BUG_BMAT_BIT, ELDT_BUG_SHPJ_BIT,&
                                         FATAL_ERR, IBIT, LINKNO, LTERM_KGG, LTERM_KGGD, MBUG, MELDOF, NDOFG, NELE, NGRID,         &
                                         NTERM_KGG, NTERM_KGGD, NSUB, SOL_NAME
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  AGRID, ELDT, ELDOF, ELGP, GRID_ID, NUM_EMG_FATAL_ERRS, PLY_NUM, OELDT, KE, KED, TYPE
      USE STF_ARRAYS, ONLY            :  STFKEY, STF3
      USE STF_TEMPLATE_ARRAYS, ONLY   :  CROW, TEMPLATE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE ESP_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP'
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr EMG (to tell it what to calc)
      CHARACTER(24*BYTE)              :: NAME              ! Name for output error purposes
      CHARACTER(FILE_NAM_MAXLEN*BYTE) :: SCRFIL            ! File name
 
      INTEGER(LONG), PARAMETER        :: DEB_NUM   = 46    ! Debug number for output error message
      INTEGER(LONG)                   :: EDOF(MELDOF)      ! A list of the G-set DOF's for an elem
      INTEGER(LONG)                   :: EDOF_ROW_NUM      ! Row number in array EDOF
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no. in array TDOF where G-set DOF's are kept 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: I1                ! Intermediate variable resulting from an IAND operation
      INTEGER(LONG)                   :: IDUM              ! Dummy variable used when flipping DOF's
      INTEGER(LONG)                   :: IERROR            ! Local error indicator
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: IS                ! A pointer into arrays STFKEY and STFPNT
      INTEGER(LONG)                   :: ISS               ! A particular value of IS
      INTEGER(LONG)                   :: KGG_ROW           ! A row no. in KGG or KGGD
      INTEGER(LONG)                   :: KGG_ROWJ          ! Another row no. in KGG or KGGD
      INTEGER(LONG)                   :: KGG_COL           ! A col no. in KGG or KGGD
      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem stiffness terms or only
!                                                            the ones on and above the diagonal (controlled by param SPARSTOR)
      INTEGER(LONG)                   :: MAX_NUM           ! MAX of NTERM_KGG/NDOFG (used for DEBUG printout)
      INTEGER(LONG)                   :: NTERM             ! Either NTERM_KGGD (BUCKLING) or NTERM_KGG otherwise
      INTEGER(LONG)                   :: NUM_COMPS         ! 6 if GRID is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: PKTERM            ! Count of the terms in TEMPLATE for nonzero stiffness terms
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: TDOF_ROW_NUM      ! Row number in array TDOF
                                                           ! Indicator for output of elem data to BUG file
      INTEGER(LONG)                   :: LTERM             ! Either LTERM_KGGD (BUCKLING) or LTERM_KGG otherwise
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP_BEGEND

      REAL(DOUBLE)                    :: DQE(MELDOF,NSUB)  ! Dummy array in call to ELEM_TRANSFORM_LBG
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
 
      INTRINSIC                       :: DABS
      INTRINSIC                       :: IAND
      INTRINSIC                       :: MAX

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1  = EPSIL(1)
      NTERM = 0

! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! Null dummy array DQE used in call to ELEM_TRANSFORM_LBG

      DO I=1,MELDOF
         DO J=1,NSUB
            DQE(I,J) = ZERO
         ENDDO
      ENDDO

! LTERM is used in this subr to make sure we do not try to use more array dimension than was allocated. So here, set LTERM to be
! either LTERM_KGG or LTERM_KGGD depending on BUCKLING

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         LTERM = LTERM_KGGD
      ELSE
         LTERM = LTERM_KGG
      ENDIF


! DEBUG(10) = 13 or 33 requests that array TEMPLATE be printed

      IF ((DEBUG(10) == 13) .OR. (DEBUG(10) == 33)) THEN
         CALL ALLOCATE_TEMPLATE ( SUBR_NAME )
      ENDIF

! Set up the option flags for EMG:

      OPT(1) = 'N'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'N'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'N'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
 
      IF      ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         OPT(4) = 'N'                                      ! OPT(4) is for calc of KE-linear
         OPT(6) = 'Y'                                      ! OPT(6) is for calc of KE-nonlinear
      ELSE IF ((SOL_NAME(1:8) == 'DIFFEREN') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
         OPT(4) = 'Y'                                      ! OPT(4) is for calc of KE-linear
         OPT(6) = 'Y'                                      ! OPT(6) is for calc of KE-nonlinear
      ELSE
         OPT(4) = 'Y'                                      ! OPT(4) is for calc of KE-linear
         OPT(6) = 'N'                                      ! OPT(6) is for calc of KE-nonlinear
      ENDIF

! Process the elements:
 
      IS  = 0
      ISS = IS
      IF ((DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
         CALL DUMPSTF ( '0', 0, 0, 0, 0, 0, 0 )
      ENDIF
 
      IERROR = 0
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
elems:DO I=1,NELE

         WRITE(SC1,12345,ADVANCE='NO') I, NELE, CR13

         IF ((DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
            WRITE(F06,14001)
         ENDIF

         DO J=0,MBUG-1
            WRT_BUG(J) = 0
         ENDDO

         IF (LINKNO == 1) THEN                             ! Only want element stiff matrix, KE, written to BUG file in LINK 1

            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_KE_BIT))       ! WRT_BUG(4): printed output of KE
            IF (I1 > 0) THEN
               WRT_BUG(4) = 1
            ENDIF

            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_SE_BIT))       ! WRT_BUG(5): printed output of SEi, STEi
            IF (I1 > 0) THEN
               WRT_BUG(5) = 1
            ENDIF

            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_SHPJ_BIT))     ! WRT_BUG(7): printed output of shape fcns and Jacobians for some elems
            IF (I1 > 0) THEN
               WRT_BUG(7) = 1
            ENDIF

            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_BMAT_BIT))     ! WRT_BUG(8): printed output of strain-displ matrices for some elements
            IF (I1 > 0) THEN
               WRT_BUG(8) = 1
            ENDIF

            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_BCHK_BIT))     ! WRT_BUG(9): printed output of R.B., const strain checks for some elems
            IF (I1 > 0) THEN
               WRT_BUG(9) = 1
            ENDIF

         ENDIF

         OPT(3) = 'N'                                      ! OPT(3) is for calc of SEi, STEi
         IF ((WRT_BUG(4) == 1) .OR. (WRT_BUG(5) == 1)) THEN
            OPT(3) = 'Y'
         ENDIF
 
         PLY_NUM = 0
         CALL EMG ( I   , OPT, 'Y', SUBR_NAME, 'Y' )       ! 'N' means do not write to BUG file

         IF (NUM_EMG_FATAL_ERRS /=0) THEN
            IERROR = IERROR + NUM_EMG_FATAL_ERRS
            CYCLE elems
         ENDIF 

         I1 = IAND(OELDT,IBIT(ELDT_F23_KE_BIT))           ! Do we need to write elem stiff matrices to F23 files?
         IF (I1 > 0) THEN
            CALL WRITE_FIJFIL ( 3, 0 )
         ENDIF

         I1 = IAND(OELDT,IBIT(ELDT_F24_SE_BIT))           ! Do we need to write elem stress recovery matrices to F24 files?
         IF (I1 > 0) THEN
            CALL WRITE_FIJFIL ( 4, 0 )
         ENDIF

         EDOF_ROW_NUM = 0                                  ! Generate element DOF'S
         DO J = 1,ELGP
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(J), IGRID )
            ROW_NUM_START = TDOF_ROW_START(IGRID)
            CALL GET_GRID_NUM_COMPS ( AGRID(J), NUM_COMPS, SUBR_NAME )
            DO K = 1,NUM_COMPS
               CALL TDOF_COL_NUM ( 'G ',  G_SET_COL_NUM )
               TDOF_ROW_NUM       = ROW_NUM_START + K - 1
               EDOF_ROW_NUM       = EDOF_ROW_NUM + 1
               EDOF(EDOF_ROW_NUM) = TDOF(TDOF_ROW_NUM, G_SET_COL_NUM)
            ENDDO 
         ENDDO 
 
! Write diagonostics on negative diag stiffness before transformation to global

         IF ((DEBUG(189) == 1) .OR. (DEBUG(189) == 3)) THEN
            CALL WRITE_NEG_DIAG_STIFFNESS ( 1 )
         ENDIF

! Transform KE from local at the elem ends to basic at elem ends to global at elem ends to global at grids.
                                                           ! Transform PTE from local-basic-global
         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  ')) THEN
            IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
               CALL ELEM_TRANSFORM_LBG ( 'KED', KED, DQE )
            ELSE
               CALL ELEM_TRANSFORM_LBG ( 'KE' , KE , DQE )
            ENDIF
         ENDIF 

! Write diagonostics on negative diag stiffness after  transformation to global

         IF ((DEBUG(189) == 2) .OR. (DEBUG(189) == 3)) THEN
            CALL WRITE_NEG_DIAG_STIFFNESS ( 2 )
         ENDIF

! Put the elem stiff matrix, KE, or KED (now in global coords), into STF array. J ranges over rows, K over cols of elem stiff mat 
 
kgg_rows:DO J = 1,ELDOF
            KGG_ROWJ  = EDOF(J)
            IF ((DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
               WRITE(F06,*)
            ENDIF

            IF (SPARSTOR == 'SYM') THEN                    ! Set KSTART depending on SPARSTOR
               KSTART = J                                  ! Process only upper right portion of ME
            ELSE
               KSTART = 1                                  ! Process all of ME
            ENDIF

kgg_cols:   DO K = KSTART,ELDOF
               KGG_ROW  = KGG_ROWJ                         ! Make sure we have correct row num. It may have been flipped w/ col
               KGG_COL  = EDOF(K)
               IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
                  IF (DABS(KED(J,K)) < EPS1) THEN
                     CYCLE kgg_cols
                  ENDIF
               ELSE
                  IF (DABS(KE(J,K)) < EPS1) THEN
                     CYCLE kgg_cols
                  ENDIF
               ENDIF
 
               IF (SPARSTOR == 'SYM') THEN                 ! If 'SYM', Flip KGG_COL,KGG_ROW if KGG_COL < KGG_ROW
                  IF (KGG_COL < KGG_ROW) THEN
                     IDUM    = KGG_ROW
                     KGG_ROW = KGG_COL
                     KGG_COL = IDUM
                  ENDIF
               ENDIF

               IS = STFKEY(KGG_ROW)                        ! Get pointer to first term in row KGG_ROW of global stiff matrix

               IF (IS == 0) THEN                           ! STFKEY(KGG_ROW)=0 means no current terms in global stiff matrix at row
                                                           ! KGG_ROW so update NTERM & reset STF arrays
                  NTERM = NTERM + 1

                  IF ((DEBUG(10) == 13) .OR. (DEBUG(10) == 33)) THEN
                     IF (ALLOCATED(TEMPLATE)) THEN
                        TEMPLATE(KGG_ROW,KGG_COL) = .TRUE.
                     ELSE
                        NAME = 'TEMPLATE                '
                        WRITE(ERR,1628) SUBR_NAME,DEB_NUM,NAME
                        WRITE(F06,1628) SUBR_NAME,DEB_NUM,NAME
                        FATAL_ERR = FATAL_ERR + 1
                        CALL OUTA_HERE ( 'Y' )             ! Coding error (TEMPLATE should be allocated), so quit
                     ENDIF
                  ENDIF

                  IF (NTERM > LTERM) THEN
                     WRITE(ERR,1624) SUBR_NAME, 'STIFFNESS','LTERM', LTERM
                     WRITE(F06,1624) SUBR_NAME, 'STIFFNESS','LTERM', LTERM
                     FATAL_ERR = FATAL_ERR + 1
                     CALL OUTA_HERE ( 'Y' )                ! MYSTRAN limitation, so quit
                  ENDIF

                  STFKEY(KGG_ROW)   = NTERM
                  STF3(NTERM)%Col_1 = KGG_COL
                  STF3(NTERM)%Col_2 = 0
                  IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
                     STF3(NTERM)%Col_3 = KED(J,K)
                  ELSE
                     STF3(NTERM)%Col_3 = KE(J,K)
                  ENDIF

                  IF ((DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
                     CALL DUMPSTF ( 'A', J, K, KGG_ROW, KGG_COL, IS, ISS )
                  ENDIF
 
               ELSE                                        ! STFKEY(KGG_ROW) /= 0 means there are already some terms in row KGG_ROW

stfpnt0:          DO                                       ! so, run this loop until we find a place to put stiff(J,K). If there is
                                                           ! already a term in this row w/ same DOF's as stiff(J,K), loop runs once.
                                                           ! If not, then this loop runs until it finds STFPNT=0, and inserts term.
 
                     IF (KGG_COL == STF3(IS)%Col_1) THEN   ! There is a term that exists with same DOF'S as stiff(J,K) so add terms 

                        IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
                           STF3(IS)%Col_3 = STF3(IS)%Col_3 + KED(J,K)
                        ELSE
                           STF3(IS)%Col_3 = STF3(IS)%Col_3 + KE(J,K)
                        ENDIF
                        IF ((DEBUG(10) == 13) .OR. (DEBUG(10) == 33)) THEN
                           IF (ALLOCATED(TEMPLATE)) THEN
                              TEMPLATE(KGG_ROW,KGG_COL) = .TRUE.
                           ELSE
                              NAME = 'TEMPLATE                '
                              WRITE(ERR,1628) SUBR_NAME,DEB_NUM,NAME
                              WRITE(F06,1628) SUBR_NAME,DEB_NUM,NAME
                              FATAL_ERR = FATAL_ERR + 1
                              CALL OUTA_HERE ( 'Y' )       ! Coding error (TEMPLATE should be allocated), so quit
                           ENDIF
                        ENDIF
                        IF ((DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
                           CALL DUMPSTF ( 'B', J, K, KGG_ROW, KGG_COL, IS, ISS )
                        ENDIF

                        CYCLE kgg_cols                     ! We have added a term to STF so exit this loop and go to next col of KGG
 
                     ELSE                                  ! This is a new term for row J. Need to cycle until we find STFPNT = 0.
                                                           ! Then we can put KE(J,K), or KED(J,K) in STF
                        ISS = IS
                        IS  = STF3(IS)%Col_2
                        IF (IS == 0) THEN                  ! We are at end of where terms are in this row, so stiff(J,K) goes here 
                           IF ((DEBUG(10) == 13) .OR. (DEBUG(10) == 33)) THEN
                              IF (ALLOCATED(TEMPLATE)) THEN
                                 TEMPLATE(KGG_ROW,KGG_COL) = .TRUE.
                              ELSE
                                 NAME = 'TEMPLATE                '
                                 WRITE(ERR,1628) SUBR_NAME,DEB_NUM,NAME
                                 WRITE(F06,1628) SUBR_NAME,DEB_NUM,NAME
                                 FATAL_ERR = FATAL_ERR + 1
                                 CALL OUTA_HERE ( 'Y' )    ! Coding error (TEMPLATE should be allocated), so quit
                              ENDIF
                           ENDIF
                           IF (NTERM+1 > LTERM) THEN
                              WRITE(ERR,1624) SUBR_NAME, 'STIFFNESS','LTERM', LTERM
                              WRITE(F06,1624) SUBR_NAME, 'STIFFNESS','LTERM', LTERM
                              FATAL_ERR = FATAL_ERR + 1
                              CALL OUTA_HERE ( 'Y' )       ! MYSTRAN limitation, so quit
                           ENDIF
                           NTERM = NTERM+1                 ! Increment NTERM
                           STF3(ISS)%Col_2   = NTERM       ! STFPNT for the current stiff(J,K) term
                           STF3(NTERM)%Col_2 = 0           ! Latest STFPNT is set to 0 so we will know when to insert next term
                           STF3(NTERM)%Col_1 = KGG_COL     ! STFCOL always is KGG_COL
                           IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
                              STF3(NTERM)%Col_3 = KED(J,K)
                           ELSE
                              STF3(NTERM)%Col_3 = KE(J,K)
                           ENDIF
                           IF ((DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
                              CALL DUMPSTF ( 'C', J, K, KGG_ROW, KGG_COL, IS, ISS )
                           ENDIF

                           CYCLE kgg_cols                  ! We put stiff(J,K) into STF so exit this loop and go to next col of KGG 
                        ELSE                               ! STFPNT /= 0 so cycle this loop until we get it = 0
                           CYCLE stfpnt0
                        ENDIF
 
                     ENDIF

                  ENDDO stfpnt0 
 
               ENDIF
 
            ENDDO kgg_cols 

         ENDDO kgg_rows 
 
      ENDDO elems 
      WRITE(SC1,*) CR13
 
! Reset subr EMG option flags:
 
      OPT(3) = 'N'
      OPT(4) = 'N'
      OPT(6) = 'N'
 
! Quit if IERROR > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,9876) IERROR
         WRITE(F06,9876) IERROR
         CALL OUTA_HERE ( 'Y' )                            ! IERROR is count of all subr EMG errors, so quit
      ENDIF

! Print out TEMPLATE which shows where the nonzero values are in the upper triangle of the stiffness matrix

      IF ((DEBUG(10) == 13) .OR. (DEBUG(10) == 33)) THEN 

         IF (ALLOCATED(TEMPLATE)) THEN

            PKTERM = 0                                     ! Count nonzero terms in K based on TEMPLATE array.
            DO I=1,NDOFG                                   ! Call this PKTERM and it should be same as LTERM
               DO J=I,NDOFG
                  IF (TEMPLATE(I,J)) THEN
                     PKTERM = PKTERM + 1
                  ENDIF
               ENDDO 
            ENDDO

            WRITE(F06,14002) PKTERM 
            WRITE(F06,*)
            DO I=1,NDOFG
               DO J=1,NDOFG
                  CROW(J) = ' '
               ENDDO
               DO J=I,NDOFG
                  IF (TEMPLATE(I,J)) THEN
                     CROW(J) = 'K'
                  ELSE
                     CROW(J) = '_'
                  ENDIF
               ENDDO 
               WRITE(F06,*) (CROW(J),J=1,NDOFG)
            ENDDO
            WRITE(F06,*)

         ELSE

            WRITE(ERR,1628) SUBR_NAME,DEB_NUM,NAME
            WRITE(F06,1628) SUBR_NAME,DEB_NUM,NAME
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                         ! Coding error (TEMPLATE should be allocated), so quit
            ENDIF

      ENDIF
   
! Deallocate TEMPLATE and CROW arrays  

      IF ((ALLOCATED(TEMPLATE)) .OR. (ALLOCATED(CROW))) THEN
         CALL DEALLOCATE_TEMPLATE
      ENDIF 

! Open a scratch file that will be used to write array STF3 so that we can deallocate them and then reallocate them with the exact
! amount of memory they need (so we do have wasted memory going into subr SPARSE_KGG)
  
      SCRFIL(1:)  = ' '
      SCRFIL(1:9) = 'SCRATCH-991'
      OPEN (SCR(1),STATUS='SCRATCH',POSITION='REWIND',FORM='UNFORMATTED',ACTION='READWRITE',IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN
         CALL OPNERR ( IOCHK, SCRFIL, OUNT, 'Y' )
         CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )
         CALL OUTA_HERE ( 'Y' )
      ENDIF
      REWIND (SCR(1))

      DO I=1,NTERM
         WRITE(SCR(1)) STF3(I)
      ENDDO
      CALL DEALLOCATE_STF_ARRAYS ( 'STF3' )

      CALL ALLOCATE_STF_ARRAYS ( 'STF3', SUBR_NAME )

      REWIND (SCR(1))
      DO I=1,NTERM
         READ(SCR(1),IOSTAT=IOCHK) STF3(I)
         IF (IOCHK /= 0) THEN
            REC_NO = J
            CALL READERR ( IOCHK, SCRFIL, 'SCR FILE WITH STF3', REC_NO, OUNT, 'Y' )
            CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )
            CALL OUTA_HERE ( 'Y' )                         ! Error reading scratch file, so quit
         ENDIF
      ENDDO
      CALL FILE_CLOSE (SCR(1), SCRFIL, 'DELETE', 'Y' )

! Reset LTERM and NTERM to appropriate values

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         NTERM_KGGD = NTERM
         LTERM_KGGD = NTERM_KGGD                           ! reset LTERM now that we have det. actual number of terms in KGGD
      ELSE
         NTERM_KGG  = NTERM
         LTERM_KGG  = NTERM_KGG                            ! reset LTERM now that we have det. actual number of terms in KGG
      ENDIF


! **********************************************************************************************************************************
! Debug output:
  
      IF((DEBUG(10) == 11) .OR. (DEBUG(10) == 12) .OR. (DEBUG(10) == 13) .OR.                                                     &
         (DEBUG(10) == 31) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
         WRITE(F06,1260)
         MAX_NUM = MAX(NTERM,NDOFG) 
         DO I=1,MAX_NUM
            IF      (MAX_NUM == NTERM) THEN
               IF (NDOFG >= I) THEN
                  WRITE(F06,1261) I,STFKEY(I),STF3(I)
               ELSE
                  WRITE(F06,1262) I, STF3(I)
               ENDIF
            ELSE IF (MAX_NUM == NDOFG) THEN
               IF (NTERM >= I) THEN
                  WRITE(F06,1261) I,STFKEY(I),STF3(I)
               ELSE
                  WRITE(F06,1263) I,STFKEY(I)
               ENDIF
            ENDIF
         ENDDO 
         WRITE(F06,*)
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1260 FORMAT(/,'            I   STFKEY(I)   STFCOL(I)   STFPNT(I)           STF(I)')      

 1261 FORMAT(1X,I12,I12,I12,I12,3X,1ES21.14)

 1262 FORMAT(1X,I12,12X,I12,I12,3X,1ES21.14)

 1263 FORMAT(1X,I12,I12)

 1624 FORMAT(' *ERROR  1624: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY NON-ZERO TERMS IN THE ',A,' MATRIX. LIMIT IS ',A,'    = ',I12)

 1628 FORMAT(' *ERROR  1628: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' BASED ON DEBUG ',I3,' VALUE, ARRAY ',A,' SHOULD BE ALLOCATED BUT IT IS NOT')

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')

12345 FORMAT(5X,'Calculating stiff matrix. Process elem  ',I8,' of ',I8, A)

14001 FORMAT(' ********************************************************************************************************************&
&******')

14002 FORMAT('From subr ESP: TEMPLATE array showing ',I12,' actual nonzero terms in upper triangle of K')

35791 format(' In ESP: I, EID, J, K, BGRID(J), TDOF_ROW_NUM, 6*(BGRID(J)-1)+K, Diff = ',8i8)

88770 format(' In ESP:                         KGG_MAX_DIAG_TERM  = ',47X,1ES15.6)

88771 format(' In ESP #1: J, K, IS, NTERM_KGG, KGG(diagonal term) = ',4i8,1es15.6)

88772 format(' In ESP #2: J, K, IS, NTERM_KGG, KGG(diagonal term) = ',4i8,1es15.6)

88773 format(' In ESP #3: J, K, IS, NTERM_KGG, KGG(diagonal term) = ',4i8,1es15.6)

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DUMPSTF ( WHAT, J, K, KGG_ROW, KGG_COL, IS, ISS )

! Prints out info on the formulation of stiffness arrays for subr ESP, which generates the arrays

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, ERR, F04, F06
      USE MODEL_STUF, ONLY            :  EID
      USE STF_ARRAYS, ONLY            :  STF3

      IMPLICIT NONE

      CHARACTER(1*BYTE), INTENT(IN)   :: WHAT              ! Indicator of where this subr was called from in subr ESP

      INTEGER(LONG)                   :: IS                ! A pointer into arrays STFKEY and STFPNT
      INTEGER(LONG)                   :: ISS               ! A particular value of IS
      INTEGER(LONG)    , INTENT(IN)   :: J                 ! Row number of elem stiff matrix term, KE(J,K), or KED(J,K)
      INTEGER(LONG)    , INTENT(IN)   :: K                 ! Col number of elem stiff matrix term, KE(J,K), or KED(J,K)
      INTEGER(LONG)    , INTENT(IN)   :: KGG_COL           ! Row number of KGG matrix where KE(J,K), or KED(J,K), goes 
      INTEGER(LONG)    , INTENT(IN)   :: KGG_ROW           ! Col number of KGG matrix where KE(J,K), or KED(J,K), goes 

! **********************************************************************************************************************************
      IF      (WHAT == '0') THEN

         WRITE(F06,8910)

      ELSE IF (WHAT == 'A') THEN

         WRITE(F06,8930) EID,J,K,KGG_ROW,KGG_COL,STFKEY(KGG_ROW),NTERM,STF3(NTERM)%Col_1,STF3(NTERM)%Col_2,                        &
                         STF3(NTERM)%Col_3,IS,ISS

      ELSE IF (WHAT == 'B') THEN

         WRITE(F06,8940) EID,J,K,KGG_ROW,KGG_COL,STFKEY(KGG_ROW),NTERM,STF3(NTERM)%Col_1,STF3(NTERM)%Col_2,                        &
                         STF3(NTERM)%Col_3,IS ,ISS,STF3(ISS)%Col_2,STF3(IS)%Col_3

      ELSE IF (WHAT == 'C') THEN

         WRITE(F06,8950) EID,J,K,KGG_ROW,KGG_COL,STFKEY(KGG_ROW),NTERM,STF3(NTERM)%Col_1,STF3(NTERM)%Col_2,                        &
                         STF3(NTERM)%Col_3,IS,ISS

      ENDIF

      RETURN

! **********************************************************************************************************************************
 8910 FORMAT(4X,'ELEM       J       K KGG_ROW KGG_COL  STFKEY  NKTERM  STFCOL  STFPNT      STF         IS     ISS  STFPNT     STF'&
          ,/,4X,' ID                                 (KGG_ROW)       (NKTERM) (NKTERM)   (NKTERM)                   (ISS)     (IS)')

 8920 FORMAT(1X,'---------------------------------------------------------------------------------------------------------------', &
'--------')

 8930 FORMAT(1X,'A',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8)

 8940 FORMAT(1X,'B',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8,I8,1ES12.3)

 8950 FORMAT(1X,'C',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8)

! **********************************************************************************************************************************
      END SUBROUTINE DUMPSTF

! ##################################################################################################################################

      SUBROUTINE WRITE_NEG_DIAG_STIFFNESS ( WHAT )

      USE CONSTANTS_1,ONLY            :  ZERO
      USE MODEL_STUF, ONLY            :  AGRID, EID, ELGP, ELDOF, TYPE

      IMPLICIT NONE

      INTEGER(LONG)                   :: II,JJ,KK,LL,MM    ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_DIAG_NEGS     ! Number of neg values on the diag of the quad element stiffness matrix
      INTEGER(LONG)                   :: WHAT              ! What header to write out

      REAL(DOUBLE)                    :: MAX_ABS_DIAG      ! Max absolute diagonal term
      REAL(DOUBLE)                    :: RATIO             ! Ratio of diagonal term to MAX_ABS_DIAG
      REAL(DOUBLE)                    :: ZE(ELDOF,ELDOF)   ! Either KE or KED (KED for BUCKLING)

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         DO II=1,ELDOF
            DO JJ=1,ELDOF
               ZE(II,JJ) = KED(II,JJ)
            ENDDO
         ENDDO
      ELSE
         DO II=1,ELDOF
            DO JJ=1,ELDOF
               ZE(II,JJ) = KE(II,JJ)
            ENDDO
         ENDDO
      ENDIF


      MAX_ABS_DIAG = ZERO
      NUM_DIAG_NEGS = 0
      DO II=1,ELDOF
         IF (DABS(ZE(II,II)) > MAX_ABS_DIAG) THEN
            MAX_ABS_DIAG = ZE(II,II)
         ENDIF
         IF (ZE(II,II) < 0.D0) THEN
            NUM_DIAG_NEGS = NUM_DIAG_NEGS + 1               
         ENDIF
      ENDDO

      IF (NUM_DIAG_NEGS > 0) THEN

         IF (WHAT == 1) THEN
            WRITE(F06,97531) TYPE, EID, NUM_DIAG_NEGS
         ELSE IF (WHAT == 2) THEN
            WRITE(F06,97532) TYPE, EID, NUM_DIAG_NEGS
         ENDIF

         WRITE(F06,97533)
         KK=0
         DO LL=1,ELGP
            CALL GET_GRID_NUM_COMPS ( AGRID(LL), NUM_COMPS, SUBR_NAME )
            DO MM=1,NUM_COMPS
               KK = KK + 1
               RATIO = ZERO
               IF (MAX_ABS_DIAG > ZERO) THEN
                  RATIO = ZE(KK,KK)/MAX_ABS_DIAG
               ENDIF
               IF (ZE(KK,KK) >= ZERO) THEN
                  IF (MM == 1) THEN
                     WRITE(F06,90869) AGRID(LL), MM, ZE(KK,KK), RATIO
                  ELSE
                     WRITE(F06,90870) MM, ZE(KK,KK), RATIO
                  ENDIF
               ELSE
                  IF (MM == 1) THEN
                     WRITE(F06,90871) AGRID(LL), MM, ZE(KK,KK), RATIO
                  ELSE
                     WRITE(F06,90872) MM, ZE(KK,KK), RATIO
                  ENDIF
               ENDIF
            ENDDO
            WRITE(F06,*)
         ENDDO
         WRITE(F06,*)
      ENDIF

97531 FORMAT(' Diagonal stiffnesses for ',A,' element ',I8, ' in local elem coords. Element has ', I3,' negative diagonal terms')

97532 FORMAT(' Diagonal stiffnesses for ',A,' element ',I8, ' in global     coords. Element has ', I3,' negative diagonal terms')

97533 FORMAT( '    Grid  Comp  Diagonal stiffness      Ratio')

90869 FORMAT( I8,I6,1ES17.6,1ES17.6)

90870 FORMAT( 8X,I6,1ES17.6,1ES17.6)

90871 FORMAT( I8,I6,1ES17.6,1ES17.6,' *****NEGATIVE STIFFNESS*****')

90872 FORMAT( 8X,I6,1ES17.6,1ES17.6,' *****NEGATIVE STIFFNESS*****')

      END SUBROUTINE WRITE_NEG_DIAG_STIFFNESS

      END SUBROUTINE ESP
