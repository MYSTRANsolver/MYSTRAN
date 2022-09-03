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
 
      SUBROUTINE MGGC_MASS_MATRIX
 
! Forms the mass matrix, MGGC, for concentrated masses by calling subr MGG_CONM2_PROC to process the concentrated masses
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  NGRID, NTERM_MGGC, BLNK_SUB_NAM
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR, WTMASS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MGGC_MASS_MATRIX_BEGEND
      USE MODEL_STUF, ONLY            :  AGRID, GRID_ID, INV_GRID_SEQ
      USE SPARSE_MATRICES, ONLY       :  I_MGGC, J_MGGC, MGGC
 
      USE MGGC_MASS_MATRIX_USE_IFs

      IMPLICIT NONE
  
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MGGC_MASS_MATRIX'
      CHARACTER( 1*BYTE)              :: MGG_CONM2_NONZERO ! 'Y'/'N' indicator if a nonzero MGG_CONM2 6 x 6 matrix was created

      INTEGER(LONG)                   :: GRID_NUM          ! The actual grid number for which we create a 6 x 6 mass matrix for
!                                                            one CONM2 or 1 element (if one exists for this grid)
      INTEGER(LONG)                   :: DELTA_KTERM_MGGC  ! Coumt of nonzero terms in MGGC array for 1 grid
      INTEGER(LONG)                   :: KTERM_MGGC        ! Coumt of nonzero terms in MGGC array
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters   
      INTEGER(LONG)                   :: IJ                ! Index
      INTEGER(LONG)                   :: IROW_START        ! Row number in TDOF where data begins for IGRID
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem mass terms or only
!                                                            the ones on and above the diagonal (controlled by param SPARSTOR)
      INTEGER(LONG)                   :: MGGC_COL_NUM      ! A calculated col number for a nonzero term in MGG arrays
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MGGC_MASS_MATRIX_BEGEND

      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: MGG_CONM2(6,6)    ! 6 X 6 mass matrix in global coords for one CONM2

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Initialize

      NTERM_MGGC = 36*NGRID                                ! Max possible NTERM_MGGC. Will be set to actual later by count nonzeros
      CALL ALLOCATE_L1_MGG ( 'MGGC', SUBR_NAME )

      KTERM_MGGC = 0
      I_MGGC(1)  = 1
      IROW_START = 1
i_do1:DO I=1,NGRID

!xx      GRID_NUM = GRID_ID(INV_GRID_SEQ(I))               ! GRID_NUM's are in TDOFI order (internal DOF order)
         GRID_NUM = GRID_ID(I)
         CALL MGG_CONM2_PROC ( I, GRID_NUM, MGG_CONM2, MGG_CONM2_NONZERO )
         CALL GET_GRID_NUM_COMPS ( GRID_NUM, NUM_COMPS, SUBR_NAME )

         IF (MGG_CONM2_NONZERO == 'Y') THEN 
            DO J=1,NUM_COMPS
               DELTA_KTERM_MGGC = 0

               IF (SPARSTOR == 'SYM') THEN                 ! Set KSTART depending on SPARSTOR
                  KSTART = J                               ! Process only upper right portion of MGG_CONM2
               ELSE
                  KSTART = 1                               ! Process all of MGG_CONM2
               ENDIF
               DO K=KSTART,NUM_COMPS
                  IF (DABS(MGG_CONM2(J,K)) >= EPS1) THEN
                     MGGC_COL_NUM       = IROW_START + K - 1
                     KTERM_MGGC         = KTERM_MGGC + 1
                     DELTA_KTERM_MGGC   = DELTA_KTERM_MGGC + 1
                     IF (KTERM_MGGC > NTERM_MGGC) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, KTERM_MGGC, 'MGGC' )
                      J_MGGC(KTERM_MGGC) = MGGC_COL_NUM
                        MGGC(KTERM_MGGC) = MGG_CONM2(J,K)
                  ENDIF
               ENDDO
!xx            IJ = 6*(I-1) + J 
               IJ = NUM_COMPS*(I-1) + J 
               I_MGGC(IJ+1) = I_MGGC(IJ) + DELTA_KTERM_MGGC 
            ENDDO

         ELSE

            DO J=1,NUM_COMPS
               IJ = IROW_START + J - 1
               I_MGGC(IJ+1) = I_MGGC(IJ)
            ENDDO

         ENDIF

         IROW_START = IROW_START + NUM_COMPS

      ENDDO i_do1

      NTERM_MGGC = KTERM_MGGC

! Do not deallocate MGGC arrays - they are needed later in subr SPARSE_MGG

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
 
      SUBROUTINE MGG_CONM2_PROC ( INT_GRID_ID, GRID_NUM, MGG_CONM2, MGG_CONM2_NONZERO )
 
! Generates 6 x 6 mass matrix, MGG_CONM2, for one CONM2 for grid GRID_NUM (if there is any CONM2 connected to this grid)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NCONM2, NGRID, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MGGC_MASS_MATRIX_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  CONM2, RCONM2
      USE PARAMS, ONLY                :  ART_MASS, ART_ROT_MASS, ART_TRAN_MASS
 
      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MGG_CONM2_PROC'
      CHARACTER( 1*BYTE), INTENT(OUT) :: MGG_CONM2_NONZERO ! 'Y'/'N' indicator if a nonzero MGG_CONM2 6 x 6 matrix was created

      INTEGER(LONG), INTENT(IN)       :: INT_GRID_ID       ! The internal grid number for which we create a 6 x 6 mass matrix for
!                                                            one CONM2 (if one exists for this grid)
      INTEGER(LONG), INTENT(IN)       :: GRID_NUM          ! The actual grid number for internal grid ID INT_GRID_ID
      INTEGER(LONG)                   :: I,J,L             ! DO loop indices or counters   
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MGGC_MASS_MATRIX_BEGEND + 1

      REAL(DOUBLE) , INTENT(OUT)      :: MGG_CONM2(6,6)    ! 6 X 6 mass matrix in global coords for one CONM2

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      MGG_CONM2_NONZERO = 'N'

      DO I=1,6
         DO J=1,6
            MGG_CONM2(I,J) = ZERO
         ENDDO 
      ENDDO

! Add artificial mass terms to the grid, if requested

      IF (ART_MASS == 'Y') THEN

         DO I=1,3
            MGG_CONM2(I,I) = ART_TRAN_MASS
         ENDDO
         MGG_CONM2_NONZERO = 'Y'

         DO I=4,6
            MGG_CONM2(I,I) = ART_ROT_MASS
         ENDDO
         MGG_CONM2_NONZERO = 'Y'

      ENDIF

! Process CONM2's for this grid

      DO L=1,NCONM2

         IF (CONM2(L,2) == GRID_NUM) THEN

            WRITE(SC1,12345,ADVANCE='NO') CONM2(L,1), INT_GRID_ID, NGRID, CR13
            WRITE(SC1,*) CR13

            MGG_CONM2_NONZERO = 'Y'

            MGG_CONM2(1,1) =  MGG_CONM2(1,1) + RCONM2(L, 1)
            MGG_CONM2(2,2) =  MGG_CONM2(2,2) + RCONM2(L, 1)
            MGG_CONM2(3,3) =  MGG_CONM2(3,3) + RCONM2(L, 1)

            MGG_CONM2(1,5) =  MGG_CONM2(1,5) + RCONM2(L, 1)*RCONM2(L,4)  
            MGG_CONM2(1,6) =  MGG_CONM2(1,6) - RCONM2(L, 1)*RCONM2(L,3)

            MGG_CONM2(2,4) =  MGG_CONM2(2,4) - RCONM2(L, 1)*RCONM2(L,4)
            MGG_CONM2(2,6) =  MGG_CONM2(2,6) + RCONM2(L, 1)*RCONM2(L,2)

            MGG_CONM2(3,4) =  MGG_CONM2(3,4) + RCONM2(L, 1)*RCONM2(L,3)
            MGG_CONM2(3,5) =  MGG_CONM2(3,5) - RCONM2(L, 1)*RCONM2(L,2)

            MGG_CONM2(4,4) =  MGG_CONM2(4,4) + RCONM2(L, 5)
            MGG_CONM2(4,5) =  MGG_CONM2(4,5) - RCONM2(L, 6)
            MGG_CONM2(4,6) =  MGG_CONM2(4,6) - RCONM2(L, 8)

            MGG_CONM2(5,5) =  MGG_CONM2(5,5) + RCONM2(L, 7)
            MGG_CONM2(5,6) =  MGG_CONM2(5,6) - RCONM2(L, 9)

            MGG_CONM2(6,6) =  MGG_CONM2(6,6) + RCONM2(L,10)

            DO I=1,6
               DO J=1,I-1
                  MGG_CONM2(I,J) = MGG_CONM2(J,I)
               ENDDO
            ENDDO

         ENDIF

      ENDDO 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
12345 format(5X,'Process mass for CONM2 ',I8,' int G.P. ',I8,' of ',I8,'           ', A)

! **********************************************************************************************************************************

      END SUBROUTINE MGG_CONM2_PROC

      END SUBROUTINE MGGC_MASS_MATRIX
