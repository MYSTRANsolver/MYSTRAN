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

! End MIT license text

      SUBROUTINE GPWG ( WHICH )
 
! Generates rigid body mass properties for the finite element model
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, OP2, SC1, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_ME_BIT, IBIT, MBUG, NCONM2, NCORD, NELE, NGRID, SOL_NAME, WARN_ERR
      USE PARAMS, ONLY                :  EPSIL, GRDPNT, MEFMGRID, MEFMLOC, SUPWARN, WTMASS
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GPWG_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  AGRID, BGRID, CONM2, CORD, CAN_ELEM_TYPE_OFFSET, ELDT, ELGP, NUM_EMG_FATAL_ERRS,          &
                                         GRID, GRID_ID, MCG, ME, MEFFMASS_CALC, MEFM_RB_MASS,                                      &
                                         MODEL_MASS, MODEL_IXX, MODEL_IYY, MODEL_IZZ, MODEL_XCG, MODEL_YCG, MODEL_ZCG,             &
                                         OFFDIS, OFFSET, PLY_NUM, RCONM2, RGRID, TYPE, USERIN_RBM0
 
      USE GPWG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GPWG'
      CHARACTER(12*BYTE), INTENT(IN)  :: WHICH             ! Whether to get mass props for
!                                                            (1) OA model
!                                                            (2) residual str, or
!                                                            (3) USERIN elems

      CHARACTER(1*BYTE)               :: OPT(6)            ! Option flags for what to calculate when subr EMG is called

      INTEGER(LONG)                   :: ACID_G            ! Actual global coord sys ID for a grid
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: I1                ! Intermediate variable resulting from an IAND operation
      INTEGER(LONG)                   :: ICID_G            ! Internal coord sys ID corresponding to actual coord sys ACID_G
      INTEGER(LONG)                   :: IERROR            ! Local error indicator
      INTEGER(LONG)                   :: JDOF              ! Array index used in getting mass terms from the elem mass matrix, ME
      INTEGER(LONG)                   :: GRID_NUM          ! An actual grid ID
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where an actual grid ID is found
      INTEGER(LONG)                   :: INFO        = 0   ! An output from subr GPWG_PMOI, called herein
      INTEGER(LONG)                   :: NUM_COMPS         ! Either 6 or 1 depending on whether grid is a physical grid or a SPOINT
      INTEGER(LONG)                   :: REFPNT            ! Reference point for GPWG calc (either GRDPNT of MEFMGRID)
      INTEGER(LONG)                   :: REFPNT_DEF        ! Default value of GRDPNT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GPWG_BEGEND

      REAL(DOUBLE)                    :: BASIC_OFF(3)      ! Offsets of an element at a grid in basic coords
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: DX                ! X offset of a mass from its' grid
      REAL(DOUBLE)                    :: DY                ! Y offset of a mass from its' grid
      REAL(DOUBLE)                    :: DZ                ! Z offset of a mass from its' grid
      REAL(DOUBLE)                    :: GLOBAL_OFF(3)     ! Offsets of an element at a grid in global coords
      REAL(DOUBLE)                    :: MASS              ! Total mass
      REAL(DOUBLE)                    :: M0                ! An intermediate variable used in calc model mass props
      REAL(DOUBLE)                    :: MOI1(3,3)         ! Moments of inertia (several diff interps during exec of this subr)
      REAL(DOUBLE)                    :: S(3,3)            ! Tranformation matrix from basic to principal axes of inertia
      REAL(DOUBLE)                    :: IS(3,3)           ! Moments of Inertia relative to the CG
      REAL(DOUBLE)                    :: MX                ! First moment of MASS about X axis
      REAL(DOUBLE)                    :: MY                ! First moment of MASS about Y axis
      REAL(DOUBLE)                    :: MZ                ! First moment of MASS about Z axis
      REAL(DOUBLE)                    :: PHID, THETAD      ! Outputs from subr GEN_T0L
      REAL(DOUBLE)                    :: Q(3,3)            ! Transformation between S and Q axes
                                                           ! Output from subr GPWG_PMOI, called herein (transform to princ dir's)
      REAL(DOUBLE)                    :: RB_MASS_BASIC(6,6)! MO: 6x6 rigid body mass matrix about ref point in basic coords
      REAL(DOUBLE)                    :: T0G(3,3)          ! Coord transformation matrix from basic to global for a grid
      REAL(DOUBLE)                    :: TRANS(3,3)        ! Transfer terms when MOI's about c.g. ard calc'd from MOI's about XREF
      REAL(DOUBLE)                    :: XB(3)             ! Basic coord diffs bet c.g. and XREF in X, Y, Z directions
      REAL(DOUBLE)                    :: XD(3)             ! Basic coord diffs bet a mass (at it's c.m.) and XREF in X, Y, Z dirs
      REAL(DOUBLE)                    :: XREF(3)           ! GRDPNT basic coords (or origin of basic sys if GRDPNT doesn't exist)
      REAL(DOUBLE)                    :: X2                ! XD(1)*XD(1)
      REAL(DOUBLE)                    :: Y2                ! XD(2)*XD(2)
      REAL(DOUBLE)                    :: Z2                ! XD(3)*XD(3)
      REAL(DOUBLE)                    :: XY                ! XD(1)*XD(2)
      REAL(DOUBLE)                    :: XZ                ! XD(1)*XD(3)
      REAL(DOUBLE)                    :: YZ                ! XD(2)*XD(3)

      ! op2
      INTEGER(LONG)                   :: ITABLE            ! the op2 subtable counter
      INTEGER(LONG)                   :: ANALYSIS_CODE     ! the result type
      INTEGER(LONG), PARAMETER        :: TABLE_CODE = 0    ! is this right?
      CHARACTER(LEN=8)                :: TABLE_NAME         ! Name of the op2 table that we're writing
 
      INTRINSIC                       :: DABS
      INTRINSIC                       :: IAND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         

      EPS1 = EPSIL(1)

      ! Set defaults in case GRDPNT grid cannot be found or is input as 0 (basic origin)
      REFPNT_DEF = 0
      XREF(1)    = ZERO
      XREF(2)    = ZERO
      XREF(3)    = ZERO

      IF ((SOL_NAME(1:5) == 'MODES') .AND. (MEFFMASS_CALC == 'Y')) THEN
         REFPNT = MEFMGRID
      ELSE
         REFPNT = GRDPNT
      ENDIF 

      ! Get reference point coordinates in basic system for the reference point
      IF (REFPNT /= -1) THEN
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, REFPNT, GRID_ID_ROW_NUM )
         IF (GRID_ID_ROW_NUM /= -1) THEN                   ! REFPNT is a grid point in the model, so get its basic coords
            XREF(1) = RGRID(GRID_ID_ROW_NUM,1)
            XREF(2) = RGRID(GRID_ID_ROW_NUM,2)
            XREF(3) = RGRID(GRID_ID_ROW_NUM,3)
         ELSE                                              ! REFPNT was not 0 and is not a grid number in the model
            IF (REFPNT /= 0) THEN
               WARN_ERR  = WARN_ERR + 1
               WRITE(ERR,1402) REFPNT
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,1402) REFPNT
               ENDIF
            ENDIF
            REFPNT = REFPNT_DEF
         ENDIF
      ENDIF
 
! Generate total mass, first and second moments by summing up mass terms. XD(i) are components of vector from
! ref point to a mass point. At this time, mass units are input units without PARAM WTMASS which is what we want for
! the grid point weight generator. Later the mass will be converted by multiplying by WTMASS.
 
      MASS  = ZERO
      MX    = ZERO
      MY    = ZERO
      MZ    = ZERO   
      DO I=1,3
         DO J=1,3
            MOI1(I,J) = ZERO
         ENDDO
      ENDDO
      XB(1) = ZERO
      XB(2) = ZERO
      XB(3) = ZERO
 
      ! First process element mass terms
      OPT(1) = 'Y'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'N'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'N'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'N'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff

      IERROR = 0
elems:DO I = 1,NELE

         WRITE(SC1,12345,ADVANCE='NO') I, NELE, CR13

         PLY_NUM = 0
         CALL EMG ( I   , OPT, 'N', SUBR_NAME, 'N' )       ! 'N' means do not write to BUG file

         IF (NUM_EMG_FATAL_ERRS == 0) THEN

            IF (TYPE /= 'USERIN  ') THEN

res_or_oa:     IF ((WHICH(1:8) == 'OA MODEL') .OR. (WHICH == 'RESIDUAL STR')) THEN

elgp_do:          DO J=1,ELGP

                     BASIC_OFF(1) = ZERO
                     BASIC_OFF(2) = ZERO
                     BASIC_OFF(3) = ZERO
                     IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
                        IF (OFFSET(J) == 'Y') THEN               ! Calc (or set to 0) elem offset in basic (BASIC_OFF) at this grid
                           GLOBAL_OFF(1) = OFFDIS(J,1)
                           GLOBAL_OFF(2) = OFFDIS(J,2)
                           GLOBAL_OFF(3) = OFFDIS(J,3)
                           CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(J), GRID_ID_ROW_NUM )
                           ACID_G = GRID(GRID_ID_ROW_NUM,3)
                           IF (ACID_G /= 0) THEN
k_do111:                      DO K=1,NCORD
                                 IF (ACID_G == CORD(K,2)) THEN
                                    ICID_G = K
                                    EXIT k_do111
                                 ENDIF
                              ENDDO k_do111
                              CALL GEN_T0L ( GRID_ID_ROW_NUM, ICID_G, THETAD, PHID, T0G )
                              CALL MATMULT_FFF ( T0G, GLOBAL_OFF, 3, 3, 1, BASIC_OFF )
                           ELSE
                              BASIC_OFF(1) = GLOBAL_OFF(1)
                              BASIC_OFF(2) = GLOBAL_OFF(2)
                              BASIC_OFF(3) = GLOBAL_OFF(3)
                           ENDIF
                        ENDIF
                     ENDIF

                     CALL GET_GRID_NUM_COMPS ( AGRID(J), NUM_COMPS, SUBR_NAME )
                     JDOF  = NUM_COMPS*(J - 1) + 1
                     M0    = ME(JDOF,JDOF)
                     MASS  = MASS + M0
                     XD(1) = RGRID(BGRID(J),1) + BASIC_OFF(1) - XREF(1)
                     XD(2) = RGRID(BGRID(J),2) + BASIC_OFF(2) - XREF(2)
                     XD(3) = RGRID(BGRID(J),3) + BASIC_OFF(3) - XREF(3)
                     MX    = MX + M0*XD(1)
                     MY    = MY + M0*XD(2)
                     MZ    = MZ + M0*XD(3)
                     X2    = XD(1)*XD(1)
                     Y2    = XD(2)*XD(2)
                     Z2    = XD(3)*XD(3)
                     XY    = XD(1)*XD(2)
                     XZ    = XD(1)*XD(3)
                     YZ    = XD(2)*XD(3)
                     MOI1(1,1) = MOI1(1,1) + M0*(Y2 + Z2)
                     MOI1(2,2) = MOI1(2,2) + M0*(X2 + Z2)
                     MOI1(3,3) = MOI1(3,3) + M0*(X2 + Y2)
                     MOI1(2,1) = MOI1(2,1) - M0*XY
                     MOI1(3,1) = MOI1(3,1) - M0*XZ
                     MOI1(3,2) = MOI1(3,2) - M0*YZ

                  ENDDO elgp_do

               ENDIF res_or_oa

            ELSE                                           ! TYPE is USERIN.

userin:        IF ((WHICH(1:8) == 'OA MODEL') .OR. (WHICH(1:6) == 'USERIN')) THEN

                  M0   = USERIN_RBM0(1,1)
                  MASS = MASS + M0
                  IF (DABS(M0) > EPS1) THEN
                     XD(1) = USERIN_RBM0(2,6)/M0 !- XREF(1)
                     XD(2) = USERIN_RBM0(3,4)/M0 !- XREF(2)
                     XD(3) = USERIN_RBM0(1,5)/M0 !- XREF(3)
                     MX    = MX + M0*XD(1)
                     MY    = MY + M0*XD(2)
                     MZ    = MZ + M0*XD(3)

                     ! NOTE: USERIN_RBM0 was calc'd rel to GRDPNT grid point
                     MOI1(1,1) = MOI1(1,1) + USERIN_RBM0(4,4)
                     MOI1(2,2) = MOI1(2,2) + USERIN_RBM0(5,5)
                     MOI1(3,3) = MOI1(3,3) + USERIN_RBM0(6,6)
                     MOI1(2,1) = MOI1(2,1) + USERIN_RBM0(5,4)
                     MOI1(3,1) = MOI1(3,1) + USERIN_RBM0(6,4)
                     MOI1(3,2) = MOI1(3,2) + USERIN_RBM0(6,5)
                  ELSE
                     XD(1) = ZERO
                     XD(2) = ZERO
                     XD(3) = ZERO
                  ENDIF

               ENDIF userin

            ENDIF

         ELSE 

            IERROR = IERROR + NUM_EMG_FATAL_ERRS
            CYCLE

         ENDIF

      ENDDO elems

      WRITE(SC1,*) CR13

      OPT(1) = 'N'                                         ! Reset subr EMG option value:

      IF (IERROR > 0) THEN
         WRITE(ERR,9876) IERROR
         WRITE(F06,9876) IERROR
         CALL OUTA_HERE ( 'Y' )                            ! Errors from subr EMG, so quit
      ENDIF

! Now process mass in CONM's and calc MOI's about XREF in basic coords. The CONM2 data must be values at the mass in basic coords
! so that this subr must be run after subr CONM2_PROC_1 and before subr CONM2_PROC_2.
! We process all CONM2's, and add values for repeated grids (i.e. if model has 2 diff CONM2's at the same grid the mass props
! for that grid will be the sum of the mass props for each grid)
 
      DO I=1,NCONM2

         GRID_NUM = CONM2(I,2)
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_NUM, GRID_ID_ROW_NUM )
         MASS  = MASS + RCONM2(I,1)                        ! Add all masses

         ! DX, DY, DZ are offsets from the grid to the i-th mass in basic coords
         DX        = RCONM2(I,2)
         DY        = RCONM2(I,3)
         DZ        = RCONM2(I,4)

         ! XD(i) are distances from XREF to the i-th mass 
         XD(1)     = RGRID(GRID_ID_ROW_NUM,1) + DX - XREF(1)
         XD(2)     = RGRID(GRID_ID_ROW_NUM,2) + DY - XREF(2)
         XD(3)     = RGRID(GRID_ID_ROW_NUM,3) + DZ - XREF(3)

         M0        = RCONM2(I,1)

         ! First moments about XREF in basic coords
         MX        = MX + M0*XD(1)
         MY        = MY + M0*XD(2)
         MZ        = MZ + M0*XD(3)

         ! Terms needed below for MOI calc
         X2        = XD(1)*XD(1)
         Y2        = XD(2)*XD(2)
         Z2        = XD(3)*XD(3)
         XY        = XD(1)*XD(2)
         XZ        = XD(1)*XD(3)
         YZ        = XD(2)*XD(3)

         ! Following are MOI's about XREF in basic coords
         MOI1(1,1) = MOI1(1,1) + RCONM2(I, 5) + M0*(Y2 + Z2)
         MOI1(2,2) = MOI1(2,2) + RCONM2(I, 7) + M0*(X2 + Z2)
         MOI1(3,3) = MOI1(3,3) + RCONM2(I,10) + M0*(X2 + Y2)
         MOI1(2,1) = MOI1(2,1) + RCONM2(I, 6) - M0*XY
         MOI1(3,1) = MOI1(3,1) + RCONM2(I, 8) - M0*XZ
         MOI1(3,2) = MOI1(3,2) + RCONM2(I, 9) - M0*YZ

      ENDDO   

      ! Set other 3 products of inertia based on symmetry
      MOI1(1,2) = MOI1(2,1)
      MOI1(1,3) = MOI1(3,1)
      MOI1(2,3) = MOI1(3,2)

      ! backup MOI1 to create MO
      !MO(1,1) = MOI1(1,1)
      !MO(2,2) = MOI1(2,2)
      !MO(3,3) = MOI1(3,3)
      !
      !MO(1,2) = MOI1(1,2)
      !MO(1,3) = MOI1(1,3)
      !MO(2,3) = MOI1(2,3)
      !
      !MO(2,1) = MOI1(1,2)
      !MO(3,1) = MOI1(1,3)
      !MO(3,2) = MOI1(2,3)

      ! XB(I) are components of distance from reference point, XREF, to c.g.
      IF (DABS(MASS) > EPS1) THEN
         XB(1) = MX/MASS
         XB(2) = MY/MASS
         XB(3) = MZ/MASS
      ENDIF
 
      ! M0 - RB_MASS_BASIC: is 6x6 rigid body mass matrix about ref point in basic coords
      DO I=1,6                                             ! Init
         DO J=1,6
            RB_MASS_BASIC(I,J) = ZERO
         ENDDO
      ENDDO

      DO I=1,3
         RB_MASS_BASIC(I,I) = MASS
      ENDDO

      RB_MASS_BASIC(1,4) =  ZERO         ;   RB_MASS_BASIC(1,5) =  MASS*XB(3)   ;   RB_MASS_BASIC(1,6) = -MASS*XB(2)
      RB_MASS_BASIC(2,4) = -MASS*XB(3)   ;   RB_MASS_BASIC(2,5) =  ZERO         ;   RB_MASS_BASIC(2,6) =  MASS*XB(1)
      RB_MASS_BASIC(3,4) =  MASS*XB(2)   ;   RB_MASS_BASIC(3,5) = -MASS*XB(1)   ;   RB_MASS_BASIC(3,6) =  ZERO

      DO I=4,6
         DO J=4,6
            RB_MASS_BASIC(I,J) = MOI1(I-3,J-3)
         ENDDO
      ENDDO

      ! Calc MEFM_RB_MASS here for MODES. It is calc'd in CALC_MRRcb for CB
      IF ((SOL_NAME(1:5) == 'MODES') .AND. (MEFFMASS_CALC == 'Y')) THEN
         DO I=1,6
            DO J=1,6
               MEFM_RB_MASS(I,J) = RB_MASS_BASIC(I,J)
            ENDDO
         ENDDO
      ENDIF

      ! Output results so far
      IF (REFPNT >= 0) THEN
         IF      (WHICH(1:12) == 'RESIDUAL STR' ) THEN
            WRITE(F06,902)
         ELSE IF (WHICH(1: 8) == 'OA MODEL'     ) THEN
            WRITE(F06,903)
         ENDIF

         IF (REFPNT == 0) THEN
            WRITE(F06,1001)
         ELSE IF (REFPNT > 0) THEN
            WRITE(F06,1002) REFPNT
            IF ((SOL_NAME(1:5) == 'MODES') .AND. (MEFFMASS_CALC == 'Y')) THEN
               IF (GRDPNT /= MEFMGRID) THEN
                  WRITE(F06,1013) MEFMGRID
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF (REFPNT >= 0) THEN
         WRITE(F06,1004) MASS
         WRITE(F06,1005) (XB(I),I=1,3)

         WRITE(F06,1021)
         WRITE(F06,1901)
         DO I=1,3
            WRITE(F06,1022) (RB_MASS_BASIC(I,J),J=1,6)
         ENDDO
         WRITE(F06,1902)
         DO I=4,6
            WRITE(F06,1022) (RB_MASS_BASIC(I,J),J=1,6)
         ENDDO
         WRITE(F06,1901)
         WRITE(F06,*)
         WRITE(F06,*)

         WRITE(F06,1006)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (MOI1(I,J),J=1,3)              ! MOI1 now are MOI's about ref pt in basic
         ENDDO
         WRITE(F06,1900)
         WRITE(F06,*)
         WRITE(F06,*)

      ENDIF
 
      ! S, TRANS: Generate moments of inertia about c.g. in basic coord. system
      TRANS(1,1) =  MASS*(XB(2)*XB(2) + XB(3)*XB(3))
      TRANS(2,2) =  MASS*(XB(1)*XB(1) + XB(3)*XB(3))
      TRANS(3,3) =  MASS*(XB(1)*XB(1) + XB(2)*XB(2))
      TRANS(1,2) = -MASS*XB(1)*XB(2)
      TRANS(1,3) = -MASS*XB(1)*XB(3)
      TRANS(2,3) = -MASS*XB(2)*XB(3)
      TRANS(2,1) =  TRANS(1,2)
      TRANS(3,1) =  TRANS(1,3)
      TRANS(3,2) =  TRANS(2,3)
      DO I=1,3
         DO J=1,3
            MOI1(I,J) = MOI1(I,J) - TRANS(I,J)
         ENDDO
      ENDDO
 
      ! backup MOI1 to create IS
      IS(1,1) = MOI1(1,1)
      IS(2,2) = MOI1(2,2)
      IS(3,3) = MOI1(3,3)
      
      IS(1,2) = MOI1(1,2)
      IS(1,3) = MOI1(1,3)
      IS(2,3) = MOI1(2,3)
      
      IS(2,1) = MOI1(1,2)
      IS(3,1) = MOI1(1,3)
      IS(3,2) = MOI1(2,3)

      ! Set model total mass parameters about c.g.
      MODEL_MASS = WTMASS*MASS
      MODEL_IXX  = WTMASS*MOI1(1,1)
      MODEL_IYY  = WTMASS*MOI1(2,2)
      MODEL_IZZ  = WTMASS*MOI1(3,3)
      MODEL_XCG  = XREF(1) + XB(1)
      MODEL_YCG  = XREF(2) + XB(2)
      MODEL_ZCG  = XREF(3) + XB(3)
      DO I=1,6
         DO J=1,6
            MCG(I,J) = ZERO
         ENDDO
      ENDDO
      MCG(1,1) = MODEL_MASS
      MCG(2,2) = MODEL_MASS
      MCG(3,3) = MODEL_MASS
      MCG(4,4) = MODEL_IXX
      MCG(5,5) = MODEL_IYY
      MCG(6,6) = MODEL_IZZ

      ! Output MOI's about c.g.
      IF (REFPNT >= 0) THEN
         WRITE(F06,1007)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (MOI1(I,J),J=1,3)              ! MOI1 now are MOI's about cg in basic
         ENDDO
         WRITE(F06,1900)
         WRITE(F06,*)
         WRITE(F06,*)
      ENDIF
 
      ! Q - Get principal MOI's and S transformation matrix (eigenvectors of MOI1)
      CALL GPWG_PMOI ( MOI1, Q, INFO )
 
      ! Write out princ MOI's and coord transf. Otherwise errors were written in subr GPWG_PMOI
      IF ((INFO == 0) .AND. (REFPNT >= 0)) THEN  

         IF(REFPNT > -1) THEN
 1         FORMAT("WRITE OGPWG OP2: ",A)
 2         FORMAT("* DEBUG OGPWG ITABLE=",i4)
           WRITE(ERR,1) "START"
           WRITE(ERR,2) ITABLE
           
           TABLE_NAME = "OGPWG   "
           CALL WRITE_TABLE_HEADER(TABLE_NAME)
           ITABLE = -3
           ANALYSIS_CODE = 1   ! TODO: this is probably wrong, but is weird for this table

           WRITE(ERR,2) ITABLE
           CALL WRITE_ITABLE(ITABLE)
           WRITE(ERR,2) ITABLE

           WRITE(ERR,1) "WRITE_OPGWG_TABLE3"
           CALL WRITE_OPGWG_TABLE3(ITABLE, ANALYSIS_CODE, TABLE_CODE, REFPNT)
           WRITE(ERR,2) ITABLE
           CALL WRITE_ITABLE(ITABLE)
           ITABLE = ITABLE - 1

           WRITE(ERR,1) "WRITE_OPGWG_TABLE4"
           WRITE(ERR,2) ITABLE

           !data = (self.MO.ravel().tolist() + self.S.ravel().tolist() +
           !        mcg.ravel().tolist() + self.IS.ravel().tolist() + self.IQ.ravel().tolist() +
           !        self.Q.ravel().tolist())
           
           ! not verified
           ! mass shouldn't be a scalar (it's a vector), 
           ! but for physical structure they're all the same
           WRITE(OP2) 78   ! the number of values we're going to write
           WRITE(OP2) ((REAL(RB_MASS_BASIC(I,J), 4), J=1,6), I=1,6),              & ! (6,6) MO - 36
                      ((REAL(TRANS(I,J), 4), J=1,3), I=1,3),                      & ! (3,3) S - 9
                      ! mass should be (3,1) instead of (1,)
                      ! CG should be (3,3) instead of (3,1)
                      ! faking...
                      REAL(MASS, 4), (REAL(XB(I), 4),I=1,3),                      & ! mass - cg
                      REAL(MASS, 4), (REAL(XB(I), 4),I=1,3),                      & ! mass - cg
                      REAL(MASS, 4), (REAL(XB(I), 4),I=1,3),                      & ! mass - cg - 12

                      ((REAL(IS(I,J), 4), J=1,3), I=1,3),                         & ! (3,3) IS
                      REAL(MODEL_IXX, 4), REAL(MODEL_IYY, 4), REAL(MODEL_IZZ, 4), & ! (3,)  IQ
                      ((REAL(Q(I,J), 4), J=1,3), I=1,3)                             ! (3,3) Q - 21
           !CALL WRITE_ITABLE(ITABLE)
           !ITABLE = ITABLE - 1
           CALL END_OP2_TABLE(ITABLE)
           WRITE(ERR,2) ITABLE
           WRITE(ERR,1) "WRITE_OPGWG_TABLE END"
         ENDIF


         WRITE(F06,1008)
         WRITE(F06,1900)
         
         ! I(Q) - MOI1 is now principal MOI matrix
         DO I=1,3
            WRITE(F06,1101) (MOI1(I,J),J=1,3)
         ENDDO
         WRITE(F06,1900)
         WRITE(F06,*)
         WRITE(F06,*)
 
         WRITE(F06,1009)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (Q(I,J),J=1,3)
         ENDDO
         WRITE(F06,1900)
         WRITE(F06,*)
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
  902 FORMAT(/,'  O U T P U T   F R O M   T H E   G R I D   P O I N T   W E I G H T   G E N E R A T O R   F O R   R E S I D U A L' &
             ,'   S T R U C T U R E')

  903 FORMAT(/,7X,'O U T P U T   F R O M   T H E   G R I D   P O I N T   W E I G H T   G E N E R A T O R   F O R   O V E R A L L', &
               '   M O D E L')

 1001 FORMAT(26X,'                   (reference point is basic coord system origin)'                                               &
            ,/)

 1002 FORMAT(26X,'                      (reference point is grid point '    ,I8,')'                                                &
            ,/)

 1004 FORMAT(  26X,'                            Total mass = '    ,1ES13.6                                                         &
            ,/)

 1005 FORMAT(61X,                                    'X             Y             Z'                                               &
          ,/,26X,'             C.G. location :',3(1ES14.6),                                                                        &
           /,26X,'              (relative to reference point in basic coordinate system)'                                          &
          ,//)

 1006 FORMAT(36X,'M.O.I. matrix - about reference point in basic coordinate system')

 1007 FORMAT(34X,'M.O.I. matrix - about above c.g. location in basic coordinate system')

 1008 FORMAT(37X,'M.O.I. matrix - about above c.g. location in principal directions')

 1009 FORMAT(37X,'Transformation from basic coordinates to principal directions')

 1013 FORMAT(  20X,' N O T E: Since user requested modal effective masses to be calculated the reference point',/,                 &
               20X,'          for this mass calc has been changed to the PARAM MEFMASS grid, ',I8,/)

 1021 FORMAT(29X,'6x6 Rigid body mass matrix - about reference point in basic coordinate system')

 1022 FORMAT(22X,'*',3(1ES14.6),'  *',3(1ES14.6),'  *')

 1901 FORMAT('                      ***                                                                                     ***')

 1902 FORMAT('                      *  ************  ************  ************  *  ************  ************  ************  *')

 1101 FORMAT(45X,'*',3(1ES14.6),'  *')

 1900 FORMAT(45X,'***',40X,'***')

 1402 FORMAT(' *WARNING    : PARAM GRDPNT (OR PARAM MEFMGRID) REFERENCES NONEXISTENT GRID POINT ',I8,'. BASIC ORIGIN WILL BE USED')

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')

12345 FORMAT('    Working on element ',I8,' of ',I8,A)


! **********************************************************************************************************************************
 
      END SUBROUTINE GPWG

!-------------------------------------------------------------------------------------------------------------
      SUBROUTINE WRITE_OPGWG_TABLE3(ITABLE, ANALYSIS_CODE, TABLE_CODE, REFERENCE_POINT)

     USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY               :  ERR, F06, OP2
 
      USE WRITE_ROD_USE_IFs

      IMPLICIT NONE
 
      !INTEGER(LONG), INTENT(IN)       :: ISUBCASE          ! the current subcase
      INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      CHARACTER(LEN=128)              :: TITLE             ! the model TITLE
      CHARACTER(LEN=128)              :: SUBTITLE          ! the subcase SUBTITLE
      CHARACTER(LEN=128)              :: LABEL             ! the subcase LABEL
      INTEGER(LONG), INTENT(IN) :: ANALYSIS_CODE, TABLE_CODE, REFERENCE_POINT

      INTEGER(LONG) :: DEVICE_CODE, APPROACH_CODE
      INTEGER(LONG), PARAMETER    :: ISUBCASE = 0     ! the current subcase
      INTEGER(LONG), PARAMETER    :: NUM_WIDE = 79     ! the number of "words" for an element

      ! dummy
      TITLE = ""
      SUBTITLE = ""
      LABEL = ""
      DEVICE_CODE = 1  ! plot
      
      APPROACH_CODE = ANALYSIS_CODE*10 + DEVICE_CODE

      ! 584 bytes
      WRITE(OP2) 146
      WRITE(OP2) APPROACH_CODE, TABLE_CODE, REFERENCE_POINT, ISUBCASE, 0,   &
            0, 0, 0, 0, NUM_WIDE, &
            0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, &
            TITLE, SUBTITLE, LABEL

      ITABLE = ITABLE - 1        ! flip it to -4, -6, ... so we don't have to do this later
      END SUBROUTINE WRITE_OPGWG_TABLE3


