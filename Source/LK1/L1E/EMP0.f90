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
 
      SUBROUTINE EMP0
 
! Provides an estimate of LTERM_MGGE, which is the size that is required for the G-set mass array for element (not grid point) mass,
! MGGE. The estimate is needed so that MGGE can be allocated.

! If field 3 of PARAM SETLKTK is 0: then the estimate of LTERM_MGGE is based
!    on full elem ME matrices unconnected (i.e. connected DOF's recounted). If the value in field 3 of the PARAM SETLKTM
!    card is PAUSE then LINK1 is PAUSE'd after subr EMP0 so user can change the estimate of LTERM_MGGE

! If field 3 of PARAM SETLKTK is 1: then the estimate of LTERM_MGGE is based
!    on Bandit bandwidth of MGG times NDOFG. If the value in field 4 of the B.D. PARAM SETLKTM card is PAUSE then LINK1 is
!    PAUSE'd after subr EMP0 so user can change the estimate of LTERM_MGGE

! If field 3 of PARAM SETLKTK is 2: then the estimate of LTERM_MGGE is based on actual elem ME matrices unconnected.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  LTERM_MGGE, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  GRIDSEQ, SETLKTM, USR_LTERM_MGG
      USE SUBR_BEGEND_LEVELS, ONLY    :  EMP0_BEGEND

      USE EMP0_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EMP0'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EMP0_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF      (SETLKTM == 0) THEN                          ! LTERM_MGG based on full elem mass matrices not connected

          CALL EMP0_0

      ELSE IF (SETLKTM == 3) THEN                          ! LTERM_MGG based on actual elem ME matrices unconnected

         CALL EMP0_3

      ELSE IF (SETLKTM == 4) THEN                          ! Use estimate from user

         LTERM_MGGE = USR_LTERM_MGG

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
 
      SUBROUTINE EMP0_0
 
! Estimates LTERM_MGGE based on full elem mass matrices in an unassembled state (not connected)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  LTERM_MGGE, NELE, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SPARSTOR, SUPINFO
      USE SUBR_BEGEND_LEVELS, ONLY    :  EMP0_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, EID, EPNT, ETYPE, ELGP, TYPE
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EMP0_0'

      INTEGER(LONG)                   :: DELTA_LTERM_MGGE  ! Increment of LTERM_MGGE for one element
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EMP0_BEGEND + 1
 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process the elements: Assume mass has no coupling from one grid to another
 

      LTERM_MGGE = 0
      DO I = 1,NELE
 
         CALL GET_ELGP ( I )

         IF (SPARSTOR == 'SYM   ') THEN
            DELTA_LTERM_MGGE = 3*ELGP*(6*ELGP + 1)
            LTERM_MGGE = LTERM_MGGE + DELTA_LTERM_MGGE     ! SYM has terms only on diag and above
         ELSE
            DELTA_LTERM_MGGE = (6*ELGP)*(6*ELGP)
            LTERM_MGGE = LTERM_MGGE + DELTA_LTERM_MGGE     ! NONSYM can have full matrix
         ENDIF
    

      ENDDO
 
      WRITE(ERR,4321) LTERM_MGGE, SETLKTM
      IF (SUPINFO == 'N') THEN
         WRITE(F06,4321) LTERM_MGGE, SETLKTM
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: IN EMPO_0: ESTIMATE OF NUMBER OF NONZEROS IN MASS MATRIX MGGE IS       = ',I12,                       &
                           ' BASED ON PARAM SETLKTM = ',I3)

! **********************************************************************************************************************************
 
      END SUBROUTINE EMP0_0

! ##################################################################################################################################
 
      SUBROUTINE EMP0_3
 
! Estimates LTERM_MGG based on actual element mass matrices unconnected.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LTERM_MGGE, MELDOF, NELE, NSUB
      USE PARAMS, ONLY                :  EPSIL, SETLKTM, SPARSTOR, SUPINFO
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  EMP0_BEGEND
      USE MODEL_STUF, ONLY            :  ELDOF, NUM_EMG_FATAL_ERRS, ME, PLY_NUM, TYPE
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EMP0_3'
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr EMG (to tell it what to calc)
 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Local error indicator
      INTEGER(LONG)                   :: KSTART            ! Index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EMP0_BEGEND

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
 
      OPT(1) = 'Y'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'N'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'N'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'N'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff
 
! Process the elements:
 
      IERROR = 0
      LTERM_MGGE = 0
elems:DO I=1,NELE

         WRITE(SC1,12345,ADVANCE='NO') I, NELE, CR13

         PLY_NUM = 0
         CALL EMG ( I   , OPT, 'N', SUBR_NAME, 'N' )       ! 'N' means do not write to BUG file

         IF (NUM_EMG_FATAL_ERRS /=0) THEN
            IERROR = IERROR + NUM_EMG_FATAL_ERRS
            CYCLE elems
         ENDIF 

! Transform ME from local at the elem ends to basic at elem ends to global at elem ends to global at grids.
                                                           ! Transform PTE from local-basic-global
         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  '))THEN
            CALL ELEM_TRANSFORM_LBG ( 'ME', ME, DQE )
         ENDIF 

! Count nonzero terms in transformed ME

         DO J=1,ELDOF
            IF (SPARSTOR == 'SYM') THEN
               KSTART = J
            ELSE
               KSTART = 1
            ENDIF
            DO K=KSTART,ELDOF
               IF (DABS(ME(J,K)) < EPS1) THEN
                  CYCLE
               ELSE
                  LTERM_MGGE = LTERM_MGGE + 2
               ENDIF
            ENDDO
         ENDDO

      ENDDO elems

      WRITE(SC1,*) CR13

! Quit if IERROR > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,9876) IERROR
         WRITE(F06,9876) IERROR
         CALL OUTA_HERE ( 'Y' )                            ! IERROR is count of all subr EMG errors, so quit
      ENDIF

      WRITE(ERR,4321) LTERM_MGGE, SETLKTM
      IF (SUPINFO == 'N') THEN
         WRITE(F06,4321) LTERM_MGGE, SETLKTM
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: IN EMPO_3: ESTIMATE OF NUMBER OF NONZEROS IN MASS MATRIX MGGE IS       = ',I12,                       &
                           ' BASED ON PARAM SETLKTM = ',I3)

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')


12345 FORMAT(5X,'Estimate size of MGG: process elem  ',I8,' of ',I8, A)

! **********************************************************************************************************************************
 
      END SUBROUTINE EMP0_3

      END SUBROUTINE EMP0
