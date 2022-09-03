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
 
      SUBROUTINE ESP0
 
! Provides an estimate of the size that is required for array KGG or KGGD. The estimate is needed so that the G-set stiffness
! matrix can be allocated. There are 4 possible means of providing the estimate of this size, and they all use Bulk Data
! PARAM SETLKTK. The variable used here to estimate the size of either the KGG or KGGD stiffness matrix is LTERM. After all
! processing in subr ESP0, LTERM will be set to be either LTERM_KGG or LTERM_LGGD, depending on which matrix is being processed

! Bulk Data PARAM card SETLKTK is used to control how LTERM is estimated. Field 3 of the B.D. PARAM SETLKTK card can
! be either 0, 1, 2 or 3:

! If field 3 of PARAM SETLKTK is 0: then the estimate of LTERM is based
!    on full elem KE matrices unconnected (i.e. connected DOF's recounted). If the value in field 3 of the PARAM SETLKTK
!    card is PAUSE then LINK1 is PAUSE'd after subr ESP0 so user can change the estimate of LTERM

! If field 3 of PARAM SETLKTK is 1: then the estimate of LTERM is based
!    on Bandit bandwidth of KGG times NDOFG. If the value in field 4 of the B.D. PARAM SETLKTK card is PAUSE then LINK1 is
!    PAUSE'd after subr ESP0 so user can change the estimate of LTERM

! If field 3 of PARAM SETLKTK is 2: then the estimate of LTERM is based on actual elem KE matrices unconnected.

! If field 3 of PARAM SETLKTK is 3: then the value in field 4 is used as the estimate for LTERM.


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, KMAT_BW, KMAT_DEN, LTERM_KGG, LTERM_KGGD, SOL_NAME
      USE PARAMS, ONLY                :  GRIDSEQ, SETLKTK, SUPINFO, USR_LTERM_KGG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP0_BEGEND

      USE ESP0_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP0'

      INTEGER(LONG)                   :: LTERM             ! Count of number of estimated terms in KGG or KGGD
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP0_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF      (SETLKTK == 0) THEN                          ! LTERM based on full elem stiffness matrices unconnected

          CALL ESP0_0 ( LTERM )

      ELSE IF (SETLKTK == 1) THEN                          ! LTERM based on BW returned from subr BANDIT (if BANDIT was run).

         IF (KMAT_BW > 0) THEN
            CALL ESP0_1 ( LTERM )
         ELSE
            CALL ESP0_0 ( LTERM )
         ENDIF

      ELSE IF (SETLKTK == 2) THEN                          ! LTERM based on matrix density returned from subr BANDIT

         IF (KMAT_DEN > ZERO) THEN
            CALL ESP0_2 ( LTERM )
         ELSE
            CALL ESP0_0 ( LTERM )
         ENDIF

      ELSE IF (SETLKTK == 3) THEN                          ! LTERM based on actual elem KE matrices unconnected

         CALL ESP0_3 ( LTERM )

      ELSE IF (SETLKTK == 4) THEN                          ! Use estimate from user

         LTERM = USR_LTERM_KGG

      ENDIF
 
! Now use LTERM to be the estimate for the appropriate G-set stiffness:

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         LTERM_KGGD = LTERM
      ELSE
         LTERM_KGG  = LTERM
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################
 
      SUBROUTINE ESP0_0 ( LTERM )
 
! Estimates LTERM based on full elem stiffness matrices in an unassembled state (not connected)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NELE, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SPARSTOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP0_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, EPNT, ETYPE, ELGP, TYPE
      use model_stuf, only            :  eid
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP0_0'

      INTEGER(LONG), INTENT(OUT)      :: LTERM             ! Count of number of estimated terms in KGG or KGGD
      INTEGER(LONG)                   :: DELTA_LTERM       ! Increment of LTERM for one element
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP0_BEGEND + 1
 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process the elements: Asume each is has a stiffness matrix that is completely full
 

      LTERM = 0
      DO I = 1,NELE
 
         CALL GET_ELGP ( I )

         IF (SPARSTOR == 'SYM   ') THEN
            DELTA_LTERM = 3*ELGP*(6*ELGP + 1)
            LTERM = LTERM + DELTA_LTERM        ! SYM has terms only on diag and above
         ELSE
            DELTA_LTERM = (6*ELGP)*(6*ELGP)
            LTERM = LTERM + DELTA_LTERM        ! NONSYM can have full matrix
         ENDIF

    
      ENDDO   
 
      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         WRITE(ERR,4321) LTERM, SETLKTK
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4321) LTERM, SETLKTK
         ENDIF
      ELSE
         WRITE(ERR,4322) LTERM, SETLKTK
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4322) LTERM, SETLKTK
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: IN ESPO_0: ESTIMATE OF NUMBER OF NONZEROS IN STIFF MATRIX KGGD IS      = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3)

 4322 FORMAT(' *INFORMATION: IN ESPO_0: ESTIMATE OF NUMBER OF NONZEROS IN STIFF MATRIX KGG IS       = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3)

! **********************************************************************************************************************************
 
      END SUBROUTINE ESP0_0

! ##################################################################################################################################
 
      SUBROUTINE ESP0_1 ( LTERM )
 
! Estimates LTERM based on number of rows in the stiff matrix times the stiffness matrix bandwidth from BANDIT.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  KMAT_BW, KMAT_DEN, NDOFG, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP0_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP0_1'

      INTEGER(LONG), INTENT(OUT)      :: LTERM             ! Count of number of estimated terms in KGG or KGGD
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP0_BEGEND + 1
 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Estimate number of nonzero terms as the number of rows in the stiff matrix times the stiff matrix bandwidth:
 
      LTERM = NDOFG*KMAT_BW

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         WRITE(ERR,4321) LTERM, SETLKTK, NDOFG, KMAT_BW
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4321) LTERM, SETLKTK, NDOFG, KMAT_BW
         ENDIF
      ELSE
         WRITE(ERR,4322) LTERM, SETLKTK, NDOFG, KMAT_BW
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4322) LTERM, SETLKTK, NDOFG, KMAT_BW
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: IN ESP0_1: ESTIMATE OF NUMBER OF NONZEROS IN STIFF MATRIX KGGD IS      = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3                                                                         &
                   ,/,100X,' NDOFG         = ',I12,/,100X,' BANDIT BW     = ',I12)

 4322 FORMAT(' *INFORMATION: IN ESP0_1: ESTIMATE OF NUMBER OF NONZEROS IN STIFF MATRIX KGG IS       = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3                                                                         &
                   ,/,100X,' NDOFG         = ',I12,/,100X,' BANDIT BW     = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE ESP0_1

! ##################################################################################################################################
 
      SUBROUTINE ESP0_2 ( LTERM )
 
! Estimates LTERM based on the full size of the stiffness matrix times the density returned from subr BANDIT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, KMAT_BW, KMAT_DEN, NDOFG, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE_HUNDRED
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP0_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP0_2'

      INTEGER(LONG), INTENT(OUT)      :: LTERM             ! Count of number of estimated terms in KGG or KGGD
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP0_BEGEND + 1
 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Estimate number of nonzero terms as the number of rows in the stiff matrix times the stiff matrix bandwidth:
 
      LTERM = NINT((KMAT_DEN/ONE_HUNDRED)*NDOFG*NDOFG)  ! KMAT_DEN is in %

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         WRITE(ERR,4321) LTERM, SETLKTK, NDOFG, KMAT_DEN
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4321) LTERM, SETLKTK, NDOFG, KMAT_DEN
         ENDIF
      ELSE
         WRITE(ERR,4322) LTERM, SETLKTK, NDOFG, KMAT_DEN
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4322) LTERM, SETLKTK, NDOFG, KMAT_DEN
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: IN ESP0_2: ESTIMATE OF NUMBER OF NONZEROS IN STIFF MATRIX KGGD IS      = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3                                                                         &
                   ,/,100X,' NDOFG         = ',I12,/,100X,' MATRIX DENSITY= ',1ES11.3,'%')

 4322 FORMAT(' *INFORMATION: IN ESP0_2: ESTIMATE OF NUMBER OF NONZEROS IN STIFF MATRIX KGG IS       = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3                                                                         &
                   ,/,100X,' NDOFG         = ',I12,/,100X,' MATRIX DENSITY= ',1ES11.3,'%')

! **********************************************************************************************************************************
 
      END SUBROUTINE ESP0_2

! ##################################################################################################################################
 
      SUBROUTINE ESP0_3 ( LTERM )
 
! Estimates LTERM based on actual element stiffness matrices unconnected.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELDOF, NELE, NSUB, SOL_NAME
      USE PARAMS, ONLY                :  EPSIL, SETLKTK, SPARSTOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP0_BEGEND
      USE MODEL_STUF, ONLY            :  ELDOF, NUM_EMG_FATAL_ERRS, PLY_NUM, KE, TYPE
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP0_3'
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr EMG (to tell it what to calc)
 
      INTEGER(LONG), INTENT(OUT)      :: LTERM             ! Count of number of estimated terms in KGG or KGGD
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Local error indicator
      INTEGER(LONG)                   :: KSTART            ! Index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP0_BEGEND

      REAL(DOUBLE)                    :: DQE(MELDOF,NSUB)  ! Dummy array in call to ELEM_TRANSFORM_LBG
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         

      EPS1 = EPSIL(1)

! Null dummy array DQE used in call to ELEM_TRANSFORM_LBG

      DO I=1,MELDOF
         DO J=1,NSUB
            DQE(I,J) = ZERO
         ENDDO
      ENDDO

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
 
      IERROR = 0
      LTERM  = 0
elems:DO I=1,NELE

         WRITE(SC1,12345,ADVANCE='NO') I, NELE, CR13

         PLY_NUM = 0
         CALL EMG ( I   , OPT, 'N', SUBR_NAME, 'N' )       ! 'N' means do not write to BUG file

         IF (NUM_EMG_FATAL_ERRS /=0) THEN
            IERROR = IERROR + NUM_EMG_FATAL_ERRS
            CYCLE elems
         ENDIF 

! Transform KE from local at the elem ends to basic at elem ends to global at elem ends to global at grids.
                                                           ! Transform PTE from local-basic-global
         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  '))THEN
            CALL ELEM_TRANSFORM_LBG ( 'KE', KE, DQE )
         ENDIF 

! Count nonzero terms in transformed KE

kgg_rows:DO J=1,ELDOF
            IF (SPARSTOR == 'SYM') THEN
               KSTART = J
            ELSE
               KSTART = 1
            ENDIF
kgg_cols:   DO K=KSTART,ELDOF
               IF (DABS(KE(J,K)) < EPS1) THEN
                  CYCLE kgg_cols
               ELSE
                  LTERM = LTERM + 1
               ENDIF
            ENDDO kgg_cols
         ENDDO kgg_rows

      ENDDO elems
      WRITE(SC1,*) CR13

! Quit if IERROR > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,9876) IERROR
         WRITE(F06,9876) IERROR
         CALL OUTA_HERE ( 'Y' )                            ! IERROR is count of all subr EMG errors, so quit
      ENDIF

      IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
         WRITE(ERR,4321) LTERM, SETLKTK
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4321) LTERM, SETLKTK
         ENDIF
      ELSE
         WRITE(ERR,4322) LTERM, SETLKTK
         IF (SUPINFO == 'N') THEN
            WRITE(F06,4322) LTERM, SETLKTK
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: IN ESP0_3: INITIAL EST. OF NUMBER OF NONZEROS IN STIFF MATRIX KGGD IS  = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3)

 4322 FORMAT(' *INFORMATION: IN ESP0_3: INITIAL EST. OF NUMBER OF NONZEROS IN STIFF MATRIX KGG IS   = ',I12,                       &
                           ' BASED ON PARAM SETLKTK = ',I3)

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')


12345 FORMAT(5X,'Estimate size of KGG: process elem  ',I8,' of ',I8, A)

! **********************************************************************************************************************************
 
      END SUBROUTINE ESP0_3

! ##################################################################################################################################

      SUBROUTINE DUMPSTF0 ( WHAT, J, K, KGG_ROW, KGG_COL )

! Prints out info on the formulation of stiffness arrays for subr ESP0_3 which estimates LTERM for subr ESP

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE MODEL_STUF, ONLY            :  EID

      IMPLICIT NONE

      CHARACTER(1*BYTE), INTENT(IN)   :: WHAT              ! Indicator of where this subr was called from in subr ESP0_3

      INTEGER(LONG)    , INTENT(IN)   :: J                 ! Row number of elem stiff matrix term, KE(J,K)
      INTEGER(LONG)    , INTENT(IN)   :: K                 ! Col number of elem stiff matrix term, KE(J,K)
      INTEGER(LONG)    , INTENT(IN)   :: KGG_COL           ! Row number of KGG matrix where KE(J,K) goes 
      INTEGER(LONG)    , INTENT(IN)   :: KGG_ROW           ! Col number of KGG matrix where KE(J,K) goes 

! **********************************************************************************************************************************
      IF      (WHAT == '0') THEN

         WRITE(F06,8910)

      ELSE IF (WHAT == 'A') THEN

         WRITE(F06,8930) EID, J, K, KGG_ROW, KGG_COL

      ENDIF

      RETURN

! **********************************************************************************************************************************
 8910 FORMAT(1X,'     ELEM        J        K  KGG_ROW  KGG_COL') 

 8930 FORMAT(1X,'A',I8,I8,I8,I8,I8)

! **********************************************************************************************************************************

      END SUBROUTINE DUMPSTF0

      END SUBROUTINE ESP0
