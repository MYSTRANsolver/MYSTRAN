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
 
      SUBROUTINE ESP0_FINAL
 
! Estimate number of terms in stiffness matrix by going through the complete process of generating it except that terms are not put
! into array STF. This way, we only need to allocate arrays STFCOL, STFPNT with the conservative estimate of LTERM_KGG. Once this
! subr finishes, we will hae an exact value of LTERM_KGG at which time we deallocate STFCOL, STFPNT and allocate them plus STF with
! the exact LTERM_KGG and then calculate them in subr ESP.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IBIT, LTERM_KGG, MELDOF, NELE, NGRID, NTERM_KGG, NSUB
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR, SUPINFO
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP0_FINAL_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  AGRID, ELDT, ELDOF, ELGP, GRID_ID, NUM_EMG_FATAL_ERRS, PLY_NUM, KE, TYPE
      USE STF_ARRAYS, ONLY            :  STFKEY, STF3
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE ESP0_FINAL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ESP0_FINAL'
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option flags for subr EMG (to tell it what to calc)
 
      INTEGER(LONG)                   :: EDOF(MELDOF)      ! A list of the G-set DOF's for an elem
      INTEGER(LONG)                   :: EDOF_ROW_NUM      ! Row number in array EDOF
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no. in array TDOF where G-set DOF's are kept 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IDUM              ! Dummy variable used when flipping DOF's
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IS                ! A pointer into array STF3
      INTEGER(LONG)                   :: ISS               ! A particular value of IS
      INTEGER(LONG)                   :: KGG_ROW           ! A row no. in KGG
      INTEGER(LONG)                   :: KGG_ROWJ          ! Another row no. in KGG
      INTEGER(LONG)                   :: KGG_COL           ! A col no. in KGG
      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem stiffness terms or only
!                                                            the ones on and above the diagonal (controlled by param SPARSTOR)
      INTEGER(LONG)                   :: NUM_COMPS         ! 6 if GRID is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: TDOF_ROW_NUM      ! Row number in array TDOF
                                                           ! Indicator for output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP0_FINAL_BEGEND

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
      OPT(4) = 'Y'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff
 
! Process the elements:
 
      IS  = 0
      ISS = IS
      LTERM_KGG = 0
 
elems:DO I=1,NELE

         WRITE(SC1,12345,ADVANCE='NO') I, NELE, CR13

         PLY_NUM = 0
         CALL EMG ( I   , OPT, 'N', SUBR_NAME, 'N' )       ! 'N' means do not write to BUG file

         EDOF_ROW_NUM = 0                                  ! Generate element DOF'S
         DO J = 1,ELGP
!xx         CALL CALC_TDOF_ROW_NUM ( AGRID(J), ROW_NUM_START, 'N' )
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
 
! Transform KE from local at the elem ends to basic at elem ends to global at elem ends to global at grids.
                                                           ! Transform PTE from local-basic-global
         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  '))THEN
            CALL ELEM_TRANSFORM_LBG ( 'KE', KE, DQE )
         ENDIF 

! Put the element stiff matrix, KE (now in global coords), into STF array. J ranges over rows, K over cols of elem stiff matrix, KE 
 
kgg_rows:DO J = 1,ELDOF
            KGG_ROWJ  = EDOF(J)

            IF (SPARSTOR == 'SYM') THEN                    ! Set KSTART depending on SPARSTOR
               KSTART = J                                  ! Process only upper right portion of ME
            ELSE
               KSTART = 1                                  ! Process all of ME
            ENDIF

kgg_cols:   DO K = KSTART,ELDOF
               KGG_ROW  = KGG_ROWJ                         ! Make sure we have correct row num. It may have been flipped w/ col
               KGG_COL  = EDOF(K)
               IF (DABS(KE(J,K)) < EPS1) THEN
                  CYCLE kgg_cols
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
                                                           ! KGG_ROW update NTERM_KGG and reset STFKEY, STFCOL, STFPNT, STF arrays
                  LTERM_KGG = LTERM_KGG + 1

                  STFKEY(KGG_ROW)       = LTERM_KGG
                  STF3(LTERM_KGG)%Col_1 = KGG_COL
                  STF3(LTERM_KGG)%Col_2 = 0

               ELSE                                        ! STFKEY(KGG_ROW) /= 0 means there are already some terms in row KGG_ROW

stfpnt0:          DO                                       ! so, run this loop until we find a place to put KE(J,K). If there is
                                                           ! already a term in this row w/ same DOF's as KE(J,K), loop runs once.
                                                           ! If not, then this loop runs until it finds STFPNT=0, and inserts term.
 
                     IF (KGG_COL == STF3(IS)%Col_1) THEN   ! There is a term that exists with same DOF's as KE(J,K) so add terms 

                        CYCLE kgg_cols                     ! We have added a term to STF so exit this loop and go to next col of KGG
 
                     ELSE                                  ! This is a new term for row J. Need to cycle until we find STFPNT = 0.
                                                           ! Then we can put KE(J,K) in STF
                        ISS = IS
                        IS  = STF3(IS)%Col_2
                        IF (IS == 0) THEN                  ! We are at end of where terms are in this row, so KE(J,K) goes here 
                           LTERM_KGG         = LTERM_KGG+1 ! Increment LTERM_KGG
                           STF3(ISS)%Col_2       = LTERM_KGG! STFPNT for the current KE(J,K) term
                           STF3(LTERM_KGG)%Col_2 = 0       ! Latest STFPNT is set to 0 so we will know when to insert next KE(J,K) 
                           STF3(LTERM_KGG)%Col_1 = KGG_COL ! STFCOL always is KGG_COL

                           CYCLE kgg_cols                  ! We put KE(J,K) into STF so exit this loop and go to next col of KGG 
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
 
      WRITE(ERR,4321) LTERM_KGG
      IF (SUPINFO == 'N') THEN
         WRITE(F06,4321) LTERM_KGG
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 4321 FORMAT(' *INFORMATION: FINAL   LTERM_KGG EST OF THE NUMBER OF NONZEROS IN STIFF MATRIX KGG IS = ',I12)

12345 FORMAT(5X,'Estimate size of KGG: process elem  ',I8,' of ',I8, A)

! **********************************************************************************************************************************
 
      END SUBROUTINE ESP0_FINAL

