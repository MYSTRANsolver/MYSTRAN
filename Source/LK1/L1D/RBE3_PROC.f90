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

      SUBROUTINE RBE3_PROC ( RTYPE, REC_NO, IERR )

! Processes a single RBE3 "rigid" element, per call, to get terms for the RMG constraint matrix. When the Bulk data was read, the
! RBE3 input data was written to file LINK1F. In this subr, file LINK1F is read and RBE3 terms for array RMG are calculated and
! written to file LINK1J.  Later, in subr SPARSE_RMG, LINK1J will be read to create the sparse array RMG (of all rigid element and
! MPC coefficients) which will be used in LINK2 to reduce the G-set mass, stiffness and load matrices to the N-set. 

! The derivation of the equations for the RBE3 are shown in Appendix E to the MYSTRAN User's Reference Manual
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F, LINK1F, L1F_MSG, L1J
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MRBE3, NCORD, NGRID, NTERM_RMG
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  CORD, GRID_ID, GRID, RCORD, RGRID
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
 
      USE RBE3_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RBE3_PROC'
      CHARACTER( 8*BYTE), INTENT(IN)  :: RTYPE             ! The type of rigid element being processed (RBE2)
      CHARACTER( 1*BYTE)              :: CDOF_D(6)         ! An output from subr RDOF (= 1 if a displ comp is in COMPS_D)
      CHARACTER( 1*BYTE)              :: CDOF_I(6)         ! An output from subr RDOF (= 1 if a displ comp 1-6 is in COMPS_I)

      INTEGER(LONG), INTENT(INOUT)    :: IERR              ! Count of errors in RIGID_ELEM_PROC
      INTEGER(LONG), INTENT(INOUT)    :: REC_NO            ! Record number when reading file L1F
      INTEGER(LONG)                   :: AGRID_D           ! Dep   grid ID (actual) read from a record of file LINK1F
      INTEGER(LONG)                   :: AGRID_I(MRBE3)    ! Indep grid ID (actual) read from a record of file LINK1F
      INTEGER(LONG)                   :: COMPS_D           ! Dipsl components associated with dep   grid, AGRID_D
      INTEGER(LONG)                   :: COMPS_I(MRBE3)    ! Dipsl components associated with indep grid, AGRID_I
      INTEGER(LONG)                   :: ECORD_D           ! Global coord ID (actual) for grid AGRID_D
      INTEGER(LONG)                   :: ECORD_I           ! Global coord ID (actual) for grid AGRID_I
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_D ! Row number in array GRID_ID where AGRID_D is found
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_I ! Row number in array GRID_ID where AGRID_I is found
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no., in TDOF array, of the G-set DOF list
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices
      INTEGER(LONG)                   :: ICORD_D           ! Internal coord ID corresponding to ECORD_D
      INTEGER(LONG)                   :: ICORD_I           ! Internal coord ID corresponding to ECORD_I
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: IRBE3             ! Number of triplets of grid/comp/weight on L1F for RBE3 elems
      INTEGER(LONG)                   :: IROW              ! A row number in matrix RDI_GLOBAL
      INTEGER(LONG)                   :: JERR              ! Local error count
      INTEGER(LONG)                   :: ITERM_RMG         ! Countof number of records written to L1J (should be NTERM_RMG at end)
      INTEGER(LONG)                   :: M_SET_COL_NUM     ! Col no., in TDOF array, of the M-set DOF list
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components for a grid
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REID              ! RBE2 elem ID read from file LINK1F
      INTEGER(LONG)                   :: RMG_COL_NUM_D(6)  ! Col no's. in RMG for 6 components of dep DOF at ref pt (if they exist)
      INTEGER(LONG)                   :: RMG_ROW_NUM       ! Row no. of a term in array RMG
      INTEGER(LONG)                   :: ROW_NUM           ! A row number in array TDOF
      INTEGER(LONG)                   :: ROW_NUM_START_D   ! DOF number where TDOF data begins for the ref grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND

      REAL(DOUBLE)                    :: EPS1              ! Small number
      REAL(DOUBLE)                    :: DX_BAR            ! Wgt'd avg diff in x dist from indep pt i to ref pt A (in ref pt global)
      REAL(DOUBLE)                    :: DY_BAR            ! Wgt'd avg diff in y dist from indep pt i to ref pt A (in ref pt global)
      REAL(DOUBLE)                    :: DZ_BAR            ! Wgt'd avg diff in z dist from indep pt i to ref pt A (in ref pt global)
      REAL(DOUBLE)                    :: DX0(3)            ! Differences in coords of one indep pt and ref pt in basic coord system
      REAL(DOUBLE)                    :: DXI(MRBE3)        ! Differences in X coords of indep pt and ref pt in ref pt global system
      REAL(DOUBLE)                    :: DYI(MRBE3)        ! Differences in Y coords of indep pt and ref pt in ref pt global system
      REAL(DOUBLE)                    :: DZI(MRBE3)        ! Differences in Z coords of indep pt and ref pt in ref pt global system
      REAL(DOUBLE)                    :: PHID, THETAD      ! Angles output from subr GEN_T0L, called herein but not needed here
      REAL(DOUBLE)                    :: DUM3(3)           ! Intermediate result in a calc
      REAL(DOUBLE)                    :: EBAR_YZ           ! Sum of weights times radii squared divided by WT for rotation about x
      REAL(DOUBLE)                    :: EBAR_ZX           ! Sum of weights times radii squared divided by WT for rotation about y
      REAL(DOUBLE)                    :: EBAR_XY           ! Sum of weights times radii squared divided by WT for rotation about z
      REAL(DOUBLE)                    :: T0D(3,3)          ! Transform a vector to basic coords from one in global coords at AGRID_D
      REAL(DOUBLE)                    :: TDI(3,3)          ! TOD'*T0I
      REAL(DOUBLE)                    :: T0I(3,3)          ! Transform a vector to basic coords from one in global coords at AGRID_I
      REAL(DOUBLE)                    :: X0_D(3)           ! Basic coords of AGRID_D reference point
      REAL(DOUBLE)                    :: X0_I(3)           ! Basic coords of AGRID_D reference point
      REAL(DOUBLE)                    :: WTi(MRBE3)        ! Weight value for an indep grid
      REAL(DOUBLE)                    :: WT                ! Sum of weights on this RBE3
      REAL(DOUBLE)                    :: WT6(6)            ! WT6(i) = Sum of weights in comp i of an indep grid NB *** new 10/03/21

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! File LINK1F contains data from the logical RBE3 cards in the input B.D. deck. For each logical RBE3 card, LINK1F has:
!     1st record          :'RBE3' the element type. This record was read in subr RIGID_ELEM_PROC before calling this subr

!     2nd record          : REID   : elem ID
!                           AGRID_D: reference (or dependent) grid
!                           COMPS_D: dependent displ comps
!                           IRBE3  : number of independent sets of grid/components/weight in the element
!                           WT     : total of all of the WTi weights on the RBE3 entry (calc'd when RBE3 bdf entry read in BD_RBE3

!     3rd record          : GRID(1), COMP(1), WTi(1): 1st independent grid, the independent components and the weight for this grid

!     4th record          : GRID(2), COMP(2), WTi(2): 2nd independent grid, the independent components and the weight for this grid

!     5th record, and on, : GRID(3), COMP(3), WTi(3): 3rd independent grid, the independent components and the weight for this grid

! The above record structure is repeated for each RBE3 logical card in the data deck (in the order in which they were read from the 
! B.D. deck).

! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
      EPS1 = EPSIL(1)

      JERR = 0

! Init weight totals in each of the 6 components           ! NB *** new 10/03/21

      DO I=1,6                                             ! NB *** new 10/03/21
         WT6(I) = ZERO                                     ! NB *** new 10/03/21
      ENDDO                                                ! NB *** new 10/03/21

! Start reading at the 2nd record of L1F for this RBE3 (first record, RYPE, was read above in calling subr, RIGID_ELEM_PROC):
                                                           ! Read 2nd record from L1F for this RBE3
      READ(L1F,IOSTAT=IOCHK) REID, AGRID_D, COMPS_D, IRBE3, WT
      REC_NO = REC_NO + 1
      IF (IOCHK == 0) THEN
         CALL GET_GRID_NUM_COMPS ( AGRID_D, NUM_COMPS, SUBR_NAME )
         IF (NUM_COMPS /= 6) THEN
            IERR  = IERR + 1
            JERR = JERR + 1
            WRITE(ERR,1951) 'RBE3', REID, NUM_COMPS
            WRITE(F06,1951) 'RBE3', REID, NUM_COMPS
         ENDIF
      ELSE
         CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
         IERR = IERR + 1
         JERR = JERR + 1
      ENDIF

      DO I=1,IRBE3                                         ! Read remaining records from L1F for this RBE3
         READ(L1F,IOSTAT=IOCHK) AGRID_I(I), COMPS_I(I), WTi(I)
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
            IERR = IERR + 1
            JERR = JERR + 1
         ENDIF
         CALL RDOF ( COMPS_I(I), CDOF_I )                  ! NB *** new 10/03/21 + next 6 lines
         IF (CDOF_I(1) == '1') THEN   ;   WT6(1) = WT6(1) + WTi(I)   ;   ENDIF
         IF (CDOF_I(2) == '1') THEN   ;   WT6(2) = WT6(2) + WTi(I)   ;   ENDIF
         IF (CDOF_I(3) == '1') THEN   ;   WT6(3) = WT6(3) + WTi(I)   ;   ENDIF
         IF (CDOF_I(4) == '1') THEN   ;   WT6(4) = WT6(4) + WTi(I)   ;   ENDIF
         IF (CDOF_I(5) == '1') THEN   ;   WT6(5) = WT6(5) + WTi(I)   ;   ENDIF
         IF (CDOF_I(6) == '1') THEN   ;   WT6(6) = WT6(6) + WTi(I)   ;   ENDIF
         write(f06,*)
      ENDDO

! Return if error

      IF (JERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF      

! Get T0D (transforms global vector at AGRID_D to basic)

      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, GRID_ID_ROW_NUM_D )
      ECORD_D = GRID(GRID_ID_ROW_NUM_D,3)
      IF (ECORD_D /= 0) THEN
         DO I=1,NCORD
            IF (ECORD_D == CORD(I,2)) THEN
               ICORD_D = I
               EXIT
            ENDIF
         ENDDO   
         CALL GEN_T0L ( GRID_ID_ROW_NUM_D, ICORD_D, THETAD, PHID, T0D )
      ELSE
         DO I=1,3
            DO J=1,3
               T0D(I,J) = ZERO
            ENDDO   
            T0D(I,I) = ONE
         ENDDO   
      ENDIF

! Get coords of the reference grid (AGRID_D) in basic coord system

      DO I=1,3
         X0_D(I) = RGRID(GRID_ID_ROW_NUM_D,I)
      ENDDO

! Calc DXI, DYI, DZI, DX_BAR, DY_BAR, DZ_BAR

      DX_BAR = ZERO
      DY_BAR = ZERO
      DZ_BAR = ZERO

      DO J=1,IRBE3

         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I(J), GRID_ID_ROW_NUM_I )
         DO K=1,3
            X0_I(K) = RGRID(GRID_ID_ROW_NUM_I,K)
            DX0(K)  = X0_I(K) - X0_D(K)
         ENDDO
                                                           ! Transform rel coords from basic to the coord sys at the ref pt
         CALL MATMULT_FFF_T ( T0D, DX0, 3, 3, 1, DUM3 )
                                                           ! Calc radius and in-plane angle from ref pt to indep points
         DXI(J) = DUM3(1)
         DYI(J) = DUM3(2)
         DZI(J) = DUM3(3)

         DX_BAR = DX_BAR + WTi(J)*DXI(J)/WT6(1)            ! NB *** new 10/03/21. Change WT to WT6(1)
         DY_BAR = DY_BAR + WTi(J)*DYI(J)/WT6(2)            ! NB *** new 10/03/21. Change WT to WT6(2)
         DZ_BAR = DZ_BAR + WTi(J)*DZI(J)/WT6(3)            ! NB *** new 10/03/21. Change WT to WT6(3)
      ENDDO


! Calc the EBAR's

      EBAR_YZ = ZERO
      EBAR_ZX = ZERO
      EBAR_XY = ZERO

      DO J=1,IRBE3

         EBAR_YZ = EBAR_YZ + WTi(J)*( DYI(J)*DYI(J) + DZI(J)*DZI(J) )/WT
         EBAR_ZX = EBAR_ZX + WTi(J)*( DZI(J)*DZI(J) + DXI(J)*DXI(J) )/WT
         EBAR_XY = EBAR_XY + WTi(J)*( DXI(J)*DXI(J) + DYI(J)*DYI(J) )/WT

      ENDDO

   
      CALL TDOF_COL_NUM ( 'G ', G_SET_COL_NUM )
      CALL TDOF_COL_NUM ( 'M ', M_SET_COL_NUM )
      CALL RDOF ( COMPS_D, CDOF_D )
! Calc RMG_COL_NUM_D's no's for up to 6 DOF's for ref pt

!xx   CALL CALC_TDOF_ROW_NUM ( AGRID_D, ROW_NUM_START_D, 'N' )
      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, IGRID )
      ROW_NUM_START_D = TDOF_ROW_START(IGRID)
      DO I=1,6
         RMG_COL_NUM_D(I) = 0
         IF (CDOF_D(I) == '1') THEN
            IROW = I
            RMG_COL_NUM_D(I) = TDOF(ROW_NUM_START_D+I-1, G_SET_COL_NUM)
         ENDIF
      ENDDO

! Write terms to L1J for the constraint equations. The outer loop is for each of the RBE3 equations and the inner loop cycles over
! the IRBE3 grids in the "independent" set (the i points). There are up to 6 constraint eqns per RBE3 (1 each for T1, T2, T3,
! R1, R2 R3 comps in the indep set for the RBE3)

      ITERM_RMG = 0
do_i1:DO I=1,6
cdof_dep:IF (CDOF_D(I) == '1') THEN                        ! The I-th component is in DDOF so write this row to RMG
            IROW = I
!xx         CALL CALC_TDOF_ROW_NUM ( AGRID_D, ROW_NUM_START_D, 'N' )
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, IGRID )
            ROW_NUM_START_D = TDOF_ROW_START(IGRID)
            ROW_NUM = ROW_NUM_START_D + I - 1
            RMG_ROW_NUM = TDOF(ROW_NUM, M_SET_COL_NUM)

            IF ((RMG_ROW_NUM > 0) .AND. (RMG_COL_NUM_D(I) > 0)) THEN
                                                          ! Write coeff for the T1, T2 or T3 component at the ref pt
               IF ((I == 1) .OR. (I == 2) .OR. (I == 3)) THEN
                  IF (DABS(WT) > EPS1) THEN 
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), ONE
                     ITERM_RMG = ITERM_RMG + 1
                  ELSE
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), ONE
                     ITERM_RMG = ITERM_RMG + 1
                     CYCLE do_i1
                  ENDIF
               ENDIF

               IF      (I == 1) THEN                       ! Write coeffs for the R2, R3 comps at the ref pt for the 1st eqn

                  IF (CDOF_D(5) /= '0') THEN
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(5), +DZ_BAR 
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF

                  IF (CDOF_D(6) /= '0') THEN
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(6), -DY_BAR 
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF

               ELSE IF (I == 2) THEN                       ! Write coeffs for the R1, R3 comps at the ref pt for the 2nd eqn

                  IF (CDOF_D(4) /= '0') THEN
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(4), -DZ_BAR 
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF

                  IF (CDOF_D(6) /= '0') THEN
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(6), +DX_BAR 
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF

               ELSE IF (I == 3) THEN                       ! Write coeffs for the R1, R2 comps at the ref pt for the 3rd eqn

                  IF (CDOF_D(4) /= '0') THEN
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(4), +DY_BAR 
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF

                  IF (CDOF_D(5) /= '0') THEN
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(5), -DX_BAR 
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF

               ENDIF
                                                           ! Write coeffs for the R1, R2 and R3 comps at the ref pt for eqns 4,5,6
               IF (I == 4) THEN 
                  IF (DABS(EBAR_YZ) > EPS1) THEN 
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), EBAR_YZ
                     ITERM_RMG = ITERM_RMG + 1
                  ELSE
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), ONE
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF
               ENDIF

               IF (I == 5) THEN 
                  IF (DABS(EBAR_ZX) > EPS1) THEN 
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), EBAR_ZX
                     ITERM_RMG = ITERM_RMG + 1
                  ELSE
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), ONE
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF
               ENDIF

               IF (I == 6) THEN 
                  IF (DABS(EBAR_XY) > EPS1) THEN 
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), EBAR_XY
                     ITERM_RMG = ITERM_RMG + 1
                  ELSE
                     WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(I), ONE
                     ITERM_RMG = ITERM_RMG + 1
                  ENDIF
               ENDIF

               IF (I == 4) THEN
                  WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(2), -DZ_BAR
                  ITERM_RMG = ITERM_RMG + 1
                  WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(3), +DY_BAR
                  ITERM_RMG = ITERM_RMG + 1
               ENDIF

               IF (I == 5) THEN
                  WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(1), +DZ_BAR
                  ITERM_RMG = ITERM_RMG + 1
                  WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(3), -DX_BAR
                  ITERM_RMG = ITERM_RMG + 1
               ENDIF

               IF (I == 6) THEN
                  WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(1), -DY_BAR
                  ITERM_RMG = ITERM_RMG + 1
                  WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_D(2), +DX_BAR
                  ITERM_RMG = ITERM_RMG + 1
               ENDIF

            ELSE
               IF (RMG_ROW_NUM  == 0) THEN
                  WRITE(ERR,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  WRITE(F06,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  FATAL_ERR = FATAL_ERR + 1
                  IERR = IERR + 1
                  JERR = JERR + 1
               ENDIF
               IF (RMG_COL_NUM_D(I) == 0) THEN
                  WRITE(ERR,1510) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  WRITE(F06,1510) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  FATAL_ERR = FATAL_ERR + 1
                  IERR = IERR + 1
                  JERR = JERR + 1
               ENDIF
            ENDIF

do_j1:      DO J=1,IRBE3                                   ! Cycle over "indep" terms (some here may actually be dep elsewhere)
                                                           ! Get T0I (transforms global vector at AGRID_I to basic)
               CALL RDOF ( COMPS_I(J), CDOF_I )
               
               CALL GET_GRID_NUM_COMPS ( AGRID_I(J), NUM_COMPS, SUBR_NAME )
               IF (NUM_COMPS /= 6) THEN
                  IERR  = IERR + 1
                  JERR  = JERR + 1
                  WRITE(ERR,1951) 'RBE3', REID, NUM_COMPS
                  WRITE(F06,1951) 'RBE3', REID, NUM_COMPS
                  FATAL_ERR = FATAL_ERR + 1
                  RETURN
               ENDIF

               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I(J), GRID_ID_ROW_NUM_I )
               ECORD_I= GRID(GRID_ID_ROW_NUM_I,3)
               IF (ECORD_I /= 0) THEN
                  DO K=1,NCORD
                     IF (ECORD_I == CORD(K,2)) THEN
                        ICORD_I = K
                        EXIT
                     ENDIF
                  ENDDO   
                  CALL GEN_T0L ( GRID_ID_ROW_NUM_I, ICORD_I, THETAD, PHID, T0I )
               ELSE
                  DO K=1,3
                     DO L=1,3
                        T0I(K,L) = ZERO
                     ENDDO   
                     T0I(K,K) = ONE
                  ENDDO   
               ENDIF

               CALL MATMULT_FFF_T ( T0D, T0I, 3, 3, 3, TDI )

               IF ((I == 1) .OR. (I == 2) .OR. (I == 3)) THEN
                  CALL WRITE_L1J_123 ( I, J, ITERM_RMG, G_SET_COL_NUM, RMG_ROW_NUM, WTi, AGRID_I, CDOF_I, COMPS_I, TDI )
               ELSE
                  CALL WRITE_L1J_456 ( I, J, ITERM_RMG, G_SET_COL_NUM, RMG_ROW_NUM, WTi, AGRID_I, CDOF_I, DXI, DYI, DZI )
               ENDIF

            ENDDO do_j1

         ENDIF cdof_dep

      ENDDO do_i1

      NTERM_RMG = NTERM_RMG + ITERM_RMG

! Return if JERR > 0

      IF (JERR > 0) THEN
         RETURN
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1503 FORMAT(' *ERROR  1503: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' FOR RBE3 ',I8,', THE COUNT OF RECORDS WRITTEN TO FILE'                                                &
                    ,/,15X,A                                                                                                       &
                    ,/,14X,' IS ITERM_RMG = ',I8,' BUT SHOULD BE NTERM_RMG = ',I8,' DETERMINED IN ANOTHER SUBR')

 1509 FORMAT(' *ERROR  1509: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A8,' RIGID ELEMENT NUMBER ',I8,', DEPENDENT GRID NUMBER ',I8,', COMPONENT ',I2                          &
                    ,/,14X,' IS NOT A M-SET DOF IN TABLE TDOFI')

 1510 FORMAT(' *ERROR  1510: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A8,' RIGID ELEMENT NUMBER ',I8,', INDEPENDENT GRID NUMBER ',I8,', COMPONENT ',I2                        &
                    ,/,14X,' IS NOT A G-SET DOF IN TABLE TDOFI')

 1951 FORMAT(' *ERROR  1951: ',A,I8,' USES GRID ',I8,' WHICH IS A SCALAR POINT. SCALAR POINTS NOT ALLOWED FOR THIS ELEM TYPE')







! **********************************************************************************************************************************

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE WRITE_L1J_123 ( I, J, ITERM_RMG, G_SET_COL_NUM, RMG_ROW_NUM, WTi, AGRID_I, CDOF_I, COMPS_I, TDI )

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  L1J
      USE SCONTR, ONLY                :  FATAL_ERR, MRBE3, NGRID
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  GRID_ID

      CHARACTER( 1*BYTE), INTENT(IN)  :: CDOF_I(6)         ! An output from subr RDOF (= 1 if a displ comp 1-6 is in COMPS_I)

      INTEGER(LONG), INTENT(IN)       :: I,J               ! DO loop indices
      INTEGER(LONG), INTENT(IN)       :: AGRID_I(MRBE3)    ! Indep grid ID (actual) read from a record of file LINK1F
      INTEGER(LONG), INTENT(IN)       :: COMPS_I(MRBE3)    ! Dipsl components associated with indep grid, AGRID_I
      INTEGER(LONG), INTENT(IN)       :: G_SET_COL_NUM     ! Col no., in TDOF array, of the G-set DOF list
      INTEGER(LONG), INTENT(IN)       :: RMG_ROW_NUM       ! Row no. of a term in array RMG
      INTEGER(LONG), INTENT(INOUT)    :: ITERM_RMG         ! Count of number of records written to L1J (should be NTERM_RMG at end)
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: K                 ! DO loop index
      INTEGER(LONG)                   :: RMG_COL_NUM_I     ! Col no. of a term in array RMG
      INTEGER(LONG)                   :: ROW_NUM           ! A row number in array TDOF
      INTEGER(LONG)                   :: ROW_NUM_START_I   ! DOF number where TDOF data begins for a grid

      REAL(DOUBLE) , INTENT(IN)       :: TDI(3,3)          ! TOD'*T0I
      REAL(DOUBLE) , INTENT(IN)       :: WTi(MRBE3)        ! Weight value for an indep grid

! **********************************************************************************************************************************

      DO K=1,3

         IF (CDOF_I(K) == '1') THEN
!xx         CALL CALC_TDOF_ROW_NUM ( AGRID_I(J), ROW_NUM_START_I, 'N' )
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I(J), IGRID )
            ROW_NUM_START_I = TDOF_ROW_START(IGRID)
            ROW_NUM = ROW_NUM_START_I + K - 1
            RMG_COL_NUM_I = TDOF(ROW_NUM,G_SET_COL_NUM)

            IF (RMG_COL_NUM_I > 0) THEN                    ! NB *** new 10/03/21 Change WT (below) to WT6(K)
               WRITE(L1J) RMG_ROW_NUM, RMG_COL_NUM_I, -WTi(J)*TDI(I,K)/WT6(K)
               ITERM_RMG = ITERM_RMG + 1
            ELSE
               WRITE(ERR,1513) 'RBE3_PROC', AGRID_I(J) ,COMPS_I(J), RMG_COL_NUM_I
               WRITE(F06,1513) 'RBE3_PROC', AGRID_I(J) ,COMPS_I(J), RMG_COL_NUM_I
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF

         ENDIF

      ENDDO

! **********************************************************************************************************************************
 1513 FORMAT(' *ERROR  1513: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' COL NUMBER IN ARRAY RMG CALCULATED FOR GRID ',I8,', COMPONENT ',I2,' MUST BE > 0 BUT IS = ',I8)


! **********************************************************************************************************************************

      END SUBROUTINE WRITE_L1J_123

! ##################################################################################################################################

      SUBROUTINE WRITE_L1J_456 ( I, J, ITERM_RMG, G_SET_COL_NUM, RMG_ROW_NUM, WTi, AGRID_I, CDOF_I, DXI, DYI, DZI )

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  L1J
      USE SCONTR, ONLY                :  FATAL_ERR, NGRID, MRBE3
      USE CONSTANTS_1, ONLY           :  ONE
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  GRID_ID

      CHARACTER( 1*BYTE), INTENT(IN)  :: CDOF_I(6)         ! An output from subr RDOF (= 1 if a displ comp 1-6 is in COMPS_I)

      INTEGER(LONG), INTENT(IN)       :: I,J               ! DO loop indices
      INTEGER(LONG), INTENT(IN)       :: AGRID_I(MRBE3)    ! Indep grid ID (actual) read from a record of file LINK1F
      INTEGER(LONG), INTENT(IN)       :: G_SET_COL_NUM     ! Col no., in TDOF array, of the G-set DOF list
      INTEGER(LONG), INTENT(IN)       :: RMG_ROW_NUM       ! Row no. of a term in array RMG
      INTEGER(LONG), INTENT(INOUT)    :: ITERM_RMG         ! Count of number of records written to L1J (should be NTERM_RMG at end)
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: RMG_COL_NUM_START ! Col no. of a term in array RMG
      INTEGER(LONG)                   :: ROW_NUM_START_I   ! DOF number where TDOF data begins for a grid

      REAL(DOUBLE) , INTENT(IN)       :: DXI(MRBE3)        ! Distances from ref pt to pt i in X global directions at ref pt 
      REAL(DOUBLE) , INTENT(IN)       :: DYI(MRBE3)        ! Distances from ref pt to pt i in Y global directions at ref pt 
      REAL(DOUBLE) , INTENT(IN)       :: DZI(MRBE3)        ! Distances from ref pt to pt i in Z global directions at ref pt 
      REAL(DOUBLE) , INTENT(IN)       :: WTi(MRBE3)        ! Weight value for an indep grid

! **********************************************************************************************************************************
!xx   CALL CALC_TDOF_ROW_NUM ( AGRID_I(J), ROW_NUM_START_I, 'N' )
      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I(J), IGRID )
      ROW_NUM_START_I = TDOF_ROW_START(IGRID)
      RMG_COL_NUM_START = TDOF(ROW_NUM_START_I,G_SET_COL_NUM)

      IF (RMG_COL_NUM_START <= 0) THEN
         WRITE(ERR,1513) 'RBE3_PROC', AGRID_I(J) ,'1', RMG_COL_NUM_START
         WRITE(F06,1513) 'RBE3_PROC', AGRID_I(J) ,'1', RMG_COL_NUM_START
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      IF      (I == 4) THEN                                ! Rotation about x, i.e. in yz (23) plane

         IF (CDOF_I(2) == '1') THEN
            WRITE(L1J) RMG_ROW_NUM, (RMG_COL_NUM_START-1)+2, +WTi(J)*DZI(J)/WT
            ITERM_RMG = ITERM_RMG +1
         ENDIF

         IF (CDOF_I(3) == '1') THEN
            WRITE(L1J) RMG_ROW_NUM, (RMG_COL_NUM_START-1)+3, -WTi(J)*DYI(J)/WT
            ITERM_RMG = ITERM_RMG +1
         ENDIF

      ELSE IF (I == 5) THEN                                ! Rotation about y, i.e. in zx (31) plane      

         IF (CDOF_I(1) == '1') THEN
            WRITE(L1J) RMG_ROW_NUM, (RMG_COL_NUM_START-1)+1, -WTi(J)*DZI(J)/WT
            ITERM_RMG = ITERM_RMG +1
         ENDIF

         IF (CDOF_I(3) == '1') THEN
            WRITE(L1J) RMG_ROW_NUM, (RMG_COL_NUM_START-1)+3, +WTi(J)*DXI(J)/WT
            ITERM_RMG = ITERM_RMG +1
         ENDIF

      ELSE IF (I == 6) THEN                                ! Rotation about z, i.e. in xy (12) plane

         IF (CDOF_I(1) == '1') THEN
            WRITE(L1J) RMG_ROW_NUM, (RMG_COL_NUM_START-1)+1, +WTi(J)*DYI(J)/WT
            ITERM_RMG = ITERM_RMG +1
         ENDIF

         IF (CDOF_I(2) == '1') THEN
            WRITE(L1J) RMG_ROW_NUM, (RMG_COL_NUM_START-1)+2, -WTi(J)*DXI(J)/WT
            ITERM_RMG = ITERM_RMG +1
         ENDIF

      ENDIF      

! **********************************************************************************************************************************
 1513 FORMAT(' *ERROR  1513: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' COL NUMBER IN ARRAY RMG CALCULATED FOR GRID ',I8,', COMPONENT ',A,' MUST BE > 0 BUT IS = ',I8)


! **********************************************************************************************************************************

      END SUBROUTINE WRITE_L1J_456

      END SUBROUTINE RBE3_PROC
