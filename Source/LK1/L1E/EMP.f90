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
 
      SUBROUTINE EMP
 
! Element mass processor
 
! EMP generates the portion of the G-set mass matrix due to element mass and puts it into the 1D array EMS of nonzero mass terms
! above the diagonal. Integer arrays EMSKEY, EMSPNT and EMSCOL are generated to form a linked list for the mass terms. 
 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, F22, F22FIL, F22_MSG, SC1, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_ME_BIT, ELDT_F22_ME_BIT, FATAL_ERR, IBIT, LINKNO, LTERM_MGGE,   &
                                         MBUG, MELDOF, NDOFG, NELE, NGRID, NTERM_MGGE, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR
      USE SUBR_BEGEND_LEVELS, ONLY    :  EMP_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  AGRID, ELDT, ELDOF, ELGP, GRID_ID, NUM_EMG_FATAL_ERRS, ME, OELDT, PLY_NUM, TYPE
      USE EMS_ARRAYS, ONLY            :  EMS, EMSCOL, EMSKEY, EMSPNT
 
      USE EMP_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EMP'
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr EMG (to tell it what to calc)
 
      INTEGER(LONG)                   :: EDOF(MELDOF)      ! A list of the G-set DOF's for an elem
      INTEGER(LONG)                   :: EDOF_ROW_NUM      ! Row number in array EDOF
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no. in array TDOF where G-set DOF's are kept 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: I1                ! Intermediate variable resulting from an IAND operation
      INTEGER(LONG)                   :: IDUM              ! Dummy variable used when flipping DOF's
      INTEGER(LONG)                   :: IERROR            ! Local error indicator
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IS                ! A pointer into arrays EMSKEY and EMSPNT
      INTEGER(LONG)                   :: ISS               ! A particular value of IS
      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem mass terms or only
!                                                            the ones on and above the diagonal (controlled by param SPARSTOR)
      INTEGER(LONG)                   :: MAX_NUM           ! MAX of NTERM_MGGE/NDOFG (used for DEBUG printout)
      INTEGER(LONG)                   :: MGG_ROW           ! A row no. in MGG
      INTEGER(LONG)                   :: MGG_ROWJ          ! Another row no. in EMS
      INTEGER(LONG)                   :: MGG_COL           ! A col no. in MGG
      INTEGER(LONG)                   :: NUM_COMPS         ! 6 if GRID is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr READERR  
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: TDOF_ROW_NUM      ! Row number in array TDOF
                                                           ! Indicator for output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EMP_BEGEND
 
      REAL(DOUBLE)                    :: DQE(MELDOF,NSUB)  ! Dummy array in call to ELEM_TRANSFORM_LBG
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
 
      INTRINSIC                       :: DABS, IAND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Make units for writing errors the error file and output file

      OUNT(1) = ERR
      OUNT(2) = F06

!! Null dummy array DQE used in call to ELEM_TRANSFORM_LBG

      DO I=1,MELDOF
         DO J=1,NSUB
            DQE(I,J) = ZERO
         ENDDO
      ENDDO

! Set up the option flags for EMG:
 
      OPT(1) = 'Y'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'N'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'N'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'N'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff
 
! Process the elements:
 
      IS  = 0
      ISS = IS
      IF ((DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
         CALL DUMPEMS ( '0', 0, 0, 0, 0, 0, 0 )
      ENDIF
 
      IERROR = 0
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
elems:DO I=1,NELE

         WRITE(SC1,12345,ADVANCE='NO') I, NELE, CR13

         DO J=0,MBUG-1
            WRT_BUG(J) = 0
         ENDDO

         IF (LINKNO == 1) THEN                             ! Only want element mass matrix, ME, written to BUG file in LINK 1
            I1 = IAND(ELDT(I),IBIT(ELDT_BUG_ME_BIT))       ! WRT_BUG(3): printed output of ME
            IF (I1 > 0) THEN
               WRT_BUG(3) = 1
            ENDIF
         ENDIF

         IF ((DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
            WRITE(F06,14001)
         ENDIF

         NUM_EMG_FATAL_ERRS = 0
         PLY_NUM = 0
         CALL EMG ( I   , OPT, 'N', SUBR_NAME, 'Y' )       ! 'Y' means write to BUG file
         IF (NUM_EMG_FATAL_ERRS /=0) THEN
            IERROR = IERROR + NUM_EMG_FATAL_ERRS
            CYCLE elems
         ENDIF 

         I1 = IAND(OELDT,IBIT(ELDT_F22_ME_BIT))            ! Do we need to write elem mass matrices to F22 files
         IF (I1 > 0) THEN
            CALL WRITE_FIJFIL ( 2, 0 )
         ENDIF

         EDOF_ROW_NUM = 0                                  ! Generate element DOF'S
         DO J = 1,ELGP
!           CALL CALC_TDOF_ROW_NUM ( AGRID(J), ROW_NUM_START, 'N' )
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
 
! Transform ME from local at the elem ends to basic at elem ends to global at elem ends to global at grids.

                                                           ! Transform PTE from local-basic-global
         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  '))THEN

            CALL ELEM_TRANSFORM_LBG ( 'ME', ME, DQE )
24357 format(6(1es14.6))

         ENDIF 

! Put the element mass matrix, ME, into EMS array. J ranges over rows, K over cols of elem mass matrix, ME 
 
mgg_rows:DO J = 1,ELDOF
            MGG_ROWJ  = EDOF(J)
            IF ((DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
               WRITE(F06,*)
            ENDIF

            IF (SPARSTOR == 'SYM') THEN                    ! Set KSTART depending on SPARSTOR
               KSTART = J                                  ! Process only upper right portion of ME
            ELSE
               KSTART = 1                                  ! Process all of ME
            ENDIF

mgg_cols:   DO K = KSTART,ELDOF
               MGG_ROW  = MGG_ROWJ                         ! Make sure we have correct row num. It may have been flipped w/ col
               MGG_COL  = EDOF(K)
               IF (DABS(ME(J,K)) < EPS1) THEN
                  CYCLE mgg_cols
               ENDIF
 
               IF (SPARSTOR == 'SYM') THEN                 ! If 'SYM', Flip MGG_COL,MGG_ROW if MGG_COL < MGG_ROW
                  IF (MGG_COL < MGG_ROW) THEN
                     IDUM    = MGG_ROW
                     MGG_ROW = MGG_COL
                     MGG_COL = IDUM
                  ENDIF
               ENDIF

               IS = EMSKEY(MGG_ROW)                        ! Get pointer to first term in row MGG_ROW of global mass matrix
               IF (IS == 0) THEN                           ! EMSKEY(MGG_ROW)=0 means no current terms in global mass matrix at row
                                                           ! MGG_ROW update NTERM_MGGE and reset EMSKEY, EMSCOL, EMSPNT, EMS arrays
                  NTERM_MGGE = NTERM_MGGE + 1

                  IF (NTERM_MGGE > LTERM_MGGE) THEN
                     WRITE(ERR,1624) SUBR_NAME, 'MASS    ', 'LTERM_MGGE', LTERM_MGGE
                     WRITE(F06,1624) SUBR_NAME, 'MASS    ', 'LTERM_MGGE', LTERM_MGGE
                     CALL OUTA_HERE ( 'Y' )                        ! MYSTRAN limitation, so quit
                  ENDIF

                  EMSKEY(MGG_ROW) = NTERM_MGGE
                  EMSCOL(NTERM_MGGE) = MGG_COL
                  EMSPNT(NTERM_MGGE) = 0
                  EMS(NTERM_MGGE) = ME(J,K)

                  IF ((DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
                     CALL DUMPEMS ( 'A', J, K, MGG_ROW, MGG_COL, IS, ISS )
                  ENDIF

               ELSE                                        ! EMSKEY(MGG_ROW) /= 0 means there are already some terms in row MGG_ROW

emspnt0:          DO                                       ! so, run this loop until we find a place to put ME(J,K). If there is
                                                           ! already a term in this row w/ same DOF's as ME(J,K), loop runs once.
                                                           ! If not, then this loop runs until it finds EMSPNT=0, and insetrs term.
 
                     IF (MGG_COL == EMSCOL(IS)) THEN       ! There is a term that exists with same DOF'S as ME(J,K) so add terms 

                        EMS(IS) = EMS(IS) + ME(J,K)
                        IF ((DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
                           CALL DUMPEMS ( 'B', J, K, MGG_ROW, MGG_COL, IS, ISS )
                        ENDIF

                        CYCLE mgg_cols                     ! We have added a term to EMS so exit this loop and do next col of MGG
 
                     ELSE                                  ! This is a new term for row J. Need to cycle until we find EMSPNT = 0.
                                                           ! Then we can put ME(J,K) in EMS
                        ISS = IS
                        IS  = EMSPNT(IS)
                        IF (IS == 0) THEN                  ! We are at end of where terms are in this row, so ME(J,K) goes here 
                           IF (NTERM_MGGE+1 > LTERM_MGGE) THEN
                              WRITE(ERR,1624) SUBR_NAME, 'MASS', 'LTERM_MGGE', LTERM_MGGE
                              WRITE(F06,1624) SUBR_NAME, 'MASS', 'LTERM_MGGE', LTERM_MGGE
                              CALL OUTA_HERE ( 'Y' )       ! MYSTRAN limitation, so quit
                           ENDIF
                           NTERM_MGGE        = NTERM_MGGE+1! Increment NTERM_MGGE
                           EMSPNT(ISS)       = NTERM_MGGE  ! EMSPNT for the current ME(J,K) term
                           EMSPNT(NTERM_MGGE) = 0          ! Latest EMSPNT is set to 0 so we will know when to insert next ME(J,K) 
                           EMSCOL(NTERM_MGGE) = MGG_COL    ! EMSCOL always is MGG_COL
                           EMS   (NTERM_MGGE) = ME(J,K)
                           IF ((DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
                              CALL DUMPEMS ( 'C', J, K, MGG_ROW, MGG_COL, IS, ISS )
                           ENDIF

                           CYCLE mgg_cols                  ! We put ME(J,K) into EMS so exit this loop and do next col of MGG 
                        ELSE                               ! EMSPNT /= 0 so cycle this loop until we get it = 0
                           CYCLE emspnt0
                        ENDIF
 
                     ENDIF

                  ENDDO emspnt0 
 
               ENDIF
 
            ENDDO mgg_cols 

         ENDDO mgg_rows 
 
      ENDDO elems 
 
      WRITE(SC1,*) CR13

! Debug output:
  
      IF((DEBUG(10) == 21) .OR. (DEBUG(10) == 22) .OR. (DEBUG(10) == 23) .OR.                                                     &
         (DEBUG(10) == 31) .OR. (DEBUG(10) == 32) .OR. (DEBUG(10) == 33)) THEN
         WRITE(F06,1260)
         MAX_NUM = MAX(NTERM_MGGE,NDOFG) 
         DO I=1,MAX_NUM
            IF      (MAX_NUM == NTERM_MGGE) THEN
               IF (NDOFG >= I) THEN
                  WRITE(F06,1261) I,EMSKEY(I),EMSCOL(I),EMSPNT(I),EMS(I)
               ELSE
                  WRITE(F06,1262) I,          EMSCOL(I),EMSPNT(I),EMS(I)
               ENDIF
            ELSE IF (MAX_NUM == NDOFG) THEN
               IF (NTERM_MGGE >= I) THEN
                  WRITE(F06,1261) I,EMSKEY(I),EMSCOL(I),EMSPNT(I),EMS(I)
               ELSE
                  WRITE(F06,1263) I,EMSKEY(I)
               ENDIF
            ENDIF
         ENDDO 
         WRITE(F06,*)
      ENDIF
 
! Reset subr EMG option flags:
 
      OPT(3) = 'N'
      OPT(4) = 'N'
 
! Quit if IERROR > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,9876) IERROR
         WRITE(F06,9876) IERROR
         CALL OUTA_HERE ( 'Y' )                                    ! IERROR is count of all subr EMG errors, so quit
      ENDIF
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1260 FORMAT(/,'            I   EMSKEY(I)   EMSCOL(I)   EMSPNT(I)           EMS(I)')      

 1261 FORMAT(1X,I12,I12,I12,I12,3X,1ES21.14)

 1262 FORMAT(1X,I12,12X,I12,I12,3X,1ES21.14)

 1263 FORMAT(1X,I12,I12)

 1624 FORMAT(' *ERROR  1624: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY NON-ZERO TERMS IN THE ',A,' MATRIX. LIMIT IS ',A,' = ',I12)

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')

12345 FORMAT(5X,'Calculating mass matrix. Process elem   ',I8,' of ',I8, A)

14001 FORMAT(' ******************************************************************************************************************&
&******')

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DUMPEMS ( WHAT, J, K, MGG_ROW, MGG_COL, IS, ISS )

! Prints out info on the formulation of stiffness arrays for subr ESP, which generates the arrays

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NTERM_MGGE
      USE MODEL_STUF, ONLY            :  EID
      USE EMS_ARRAYS, ONLY            :  EMS, EMSCOL, EMSKEY, EMSPNT

      IMPLICIT NONE

      CHARACTER(1*BYTE), INTENT(IN)   :: WHAT              ! Indicator of where this subr was called from in subr EMP

      INTEGER(LONG)                   :: IS                ! A pointer into arrays EMSKEY and EMSPNT
      INTEGER(LONG)                   :: ISS               ! A particular value of IS
      INTEGER(LONG)    , INTENT(IN)   :: J                 ! Row number of elem mass matrix term, ME(J,K)
      INTEGER(LONG)    , INTENT(IN)   :: K                 ! Col number of elem mass matrix term, ME(J,K)
      INTEGER(LONG)    , INTENT(IN)   :: MGG_COL           ! Row number of MGG matrix where ME(J,K) goes 
      INTEGER(LONG)    , INTENT(IN)   :: MGG_ROW           ! Col number of MGG matrix where ME(J,K) goes 

! **********************************************************************************************************************************
      IF      (WHAT == '0') THEN

         WRITE(F06,8910)

      ELSE IF (WHAT == 'A') THEN

         WRITE(F06,8930) EID, J, K, MGG_ROW, MGG_COL, EMSKEY(MGG_ROW), NTERM_MGGE, EMSCOL(NTERM_MGGE), EMSPNT(NTERM_MGGE),         &
                         EMS(NTERM_MGGE), IS, ISS

      ELSE IF (WHAT == 'B') THEN

         IF (ISS /= 0) THEN
         WRITE(F06,8940) EID, J, K, MGG_ROW, MGG_COL, EMSKEY(MGG_ROW), NTERM_MGGE, EMSCOL(NTERM_MGGE), EMSPNT(NTERM_MGGE),        &
                         EMS(NTERM_MGGE), IS, ISS, EMSPNT(ISS), EMS(IS)

         ELSE
         WRITE(F06,8941) EID, J, K, MGG_ROW, MGG_COL, EMSKEY(MGG_ROW), NTERM_MGGE, EMSCOL(NTERM_MGGE), EMSPNT(NTERM_MGGE),        &
                         EMS(NTERM_MGGE), IS, ISS,              EMS(IS)
         ENDIF

      ELSE IF (WHAT == 'C') THEN

         WRITE(F06,8950) EID, J, K, MGG_ROW, MGG_COL, EMSKEY(MGG_ROW), NTERM_MGGE, EMSCOL(NTERM_MGGE), EMSPNT(NTERM_MGGE),         &
                         EMS(NTERM_MGGE), IS, ISS

      ENDIF

      RETURN

! **********************************************************************************************************************************
 8910 FORMAT(4X,'ELEM       J       K MGG_ROW MGG_COL  EMSKEY  NKTERM  EMSCOL  EMSPNT      EMS         IS     ISS  EMSPNT     EMS'&
          ,/,4X,' ID                                 (MGG_ROW)       (NKTERM) (NKTERM)   (NKTERM)                   (ISS)     (IS)')

 8920 FORMAT(1X,'---------------------------------------------------------------------------------------------------------------', &
'--------')

 8930 FORMAT(1X,'A',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8)

 8940 FORMAT(1X,'B',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8,I8,1ES12.3)

 8941 FORMAT(1X,'B',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8,' -------',1ES12.3)

 8950 FORMAT(1X,'C',I6,I8,I8,I8,I8,I8,I8,I8,I8,1ES12.3,I8,I8)

! **********************************************************************************************************************************
      END SUBROUTINE DUMPEMS

      END SUBROUTINE EMP
