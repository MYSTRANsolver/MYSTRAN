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
 
      SUBROUTINE ELMOUT ( INT_ELEM_ID, DUM_BUG, CASE_NUM, OPT )
 
! Prints elem related data (controlled by Case Control ELDATA requests and situational variable WRT_BUG(i) ).
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, BUG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_DAT1_BIT, ELDT_BUG_DAT2_BIT, ELDT_BUG_ME_BIT, ELDT_BUG_P_T_BIT,    &
                                         ELDT_BUG_SE_BIT, ELDT_BUG_KE_BIT, ELDT_BUG_U_P_BIT, MBUG, MDT, MELGP, METYPE,             &
                                         MEMATR, MEMATC, MEPROP, MPRESS, NSUB, NTSUB, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMOUT_BEGEND
      USE CONSTANTS_1, ONLY           :  CONV_RAD_DEG, ZERO
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, ELFORCEN, QUADAXIS, QUAD4TYP
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  AGRID, BGRID, BE1, BE2, BE3, BENSUM, BMEANT, CAN_ELEM_TYPE_OFFSET, DOFPIN, DT, ELAS_COMP, &
                                         EID, EB, EM, ES, ET, ELEM_LEN_AB, ELDOF, ELMTYP, ELGP, EMAT, EPROP, FCONV, HBAR, KE, KED, &
                                         ME, MXWARP, NUM_PLIES, NUM_SEi, OFFDIS, OFFSET, PCOMP_PROPS, PEB, PEG, PEL, PHI_SQ,       &
                                         PPE, PRESS, PSI_HAT, PTE, QUAD_DELTA, QUAD_GAMMA, QUAD_THETA, SE1, SE2, SE3,              &
                                         SHELL_T, SHRSUM, STE1, STE2, STE3, THETAM, TE, TYPE, UEB, UEG, UEL, XEB, XEL, SCNUM,      &
                                         SUBLOD, ULT_STRE, ULT_STRN
      USE ELMOUT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMOUT'
      CHARACTER(LEN=*), INTENT(IN)    :: OPT(6)              ! Array of EMG option indicators explained above
      CHARACTER( 1*BYTE)              :: FOUND               ! Used in determining if we found something we were looking for
      CHARACTER(12*BYTE)              :: GRID_TYPE(ELGP)     ! Type of grid: scalar point or act grid point
      CHARACTER(60*BYTE)              :: NAME1               ! Text used for output print purposes
      CHARACTER(21*BYTE)              :: NAME2               ! Text used for output print purposes
      CHARACTER(12*BYTE)              :: NAME3               ! Text used for output print purposes
  
      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID         ! Internal element ID for which
      INTEGER(LONG), INTENT(IN)       :: CASE_NUM            ! Can be subcase number (e.g. for UEL, PEL output)
      INTEGER(LONG), INTENT(IN)       :: DUM_BUG(0:MBUG-1)   ! Indicator for output of elem data to BUG file
      INTEGER(LONG)                   :: I2                  ! Counter
      INTEGER(LONG)                   :: I,J,K               ! DO loop indices
      INTEGER(LONG)                   :: NUM_COMPS           ! No. displ components (1 for SPOINT, 6 for actual grid)
      INTEGER(LONG)                   :: NUM_STRESS_MATS     ! Number of SEi/BEi matrices for this element
      INTEGER(LONG)                   :: TCASE2(NSUB)        ! TCASE2(I) gives the internal subcase no. for internal thermal case I
!                                                              If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                              thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                              TCASE2(1-5) = 3, 5, 0, 0, 0 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMOUT_BEGEND

      REAL(DOUBLE)                    :: OEL(6)              ! Temp array for holding elem displ, node loads
      REAL(DOUBLE)                    :: SHELL_T_avg         ! Average of the diag terms from transverse shear matrix SHELL_T

      INTRINSIC                       :: ABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, DUM_BUG
 9001    FORMAT(1X,A,' BEGN ',F10.3, 5X, 10I4)
      ENDIF

! **********************************************************************************************************************************
! Set GRID_TYPE

      DO I=1,ELGP
         GRID_TYPE(I) = 'undefined   '
      ENDDO
      DO I=1,ELGP
         CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
         IF      (NUM_COMPS == 1) THEN
            GRID_TYPE(I) = 'scalar point'
         ELSE IF (NUM_COMPS == 6) THEN
            GRID_TYPE(I) = 'grid   point'
         ENDIF
      ENDDO

! Write output from ELMDAT subroutine.

      IF (DUM_BUG(0) > 0) THEN
  
         WRITE(BUG,1000)

         WRITE(BUG,1001) ELDT_BUG_DAT1_BIT, TYPE, EID
         WRITE(BUG,*)

         IF ((TYPE == 'QDPLT2   ') .OR. (TYPE == 'QUAD4   ')) THEN
            WRITE(BUG,*) '  Bending portion of QUAD4 is based on QUAD4TYP formulation = ',QUAD4TYP
            WRITE(BUG,*)
         ENDIF

         WRITE(BUG,*) '  Internal element number,       INT_ELEM_ID  = ' ,INT_ELEM_ID
         WRITE(BUG,*) '  Number of grids elem is connected to, ELGP  = ' ,ELGP
         WRITE(BUG,*) '  Number of DOFs  elem is connected to, ELDOF = ' ,ELDOF
         WRITE(BUG,*)

         WRITE(BUG,*) '  Actual & Internal G.P.s and basic coordinates (AGRID, BGRID, I, XEB)'
         WRITE(BUG,*) '  --------------------------------------------------------------------'
         WRITE(BUG,*) '         Grid Number                   Basic System Coordinates'
         WRITE(BUG,*) '  Actual  Internal  Element        X              Y              Z'
         DO I=1,ELGP
            WRITE(BUG,1004)  AGRID(I), BGRID(I),I,(XEB(I,J),J=1,3)
         ENDDO 
         IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'USER1   ')) THEN
            WRITE(BUG,1113) (XEB(ELGP+1,J),J=1,3)
         ENDIF
         WRITE(BUG,*)

         IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'USER1   ')) THEN
            WRITE(BUG,*) '  Basic coordinate directions of v vector'
            WRITE(BUG,*) '  ---------------------------------------'
            WRITE(BUG,3003) (XEB(ELGP+1,J),J=1,3)
            WRITE(BUG,*)
         ENDIF

      ENDIF

      IF (DUM_BUG(1) > 0) THEN

!zzzz    IF (PCOMP_PROPS == 'N') THEN

            WRITE(BUG,1000)

            WRITE(BUG,1001) ELDT_BUG_DAT2_BIT, TYPE, EID

            WRITE(BUG,*) '  EPROP, Array of element property data'
            WRITE(BUG,*) '  -------------------------------------'
            WRITE(BUG,3005) (EPROP(I),I=1,MEPROP)
            WRITE(BUG,*)

            WRITE(BUG,*) '  EMAT, Array of element matl data'
            WRITE(BUG,*) '  --------------------------------'
            WRITE(BUG,*) '    Col 1         Col 2         Col 3         Col 4'
            DO I=1,MEMATR
               WRITE(BUG,3004) (EMAT(I,J),J=1,MEMATC)
            ENDDO 
            WRITE(BUG,*)

            WRITE(BUG,*) '  ULT_STRE, material stress allowables'
            WRITE(BUG,*) '  ------------------------------------'
            WRITE(BUG,*) '    Col 1         Col 2         Col 3         Col 4'
            WRITE(BUG,3024) (ULT_STRE(1,J),J=1,MEMATC),'   Tension ultimate'
            WRITE(BUG,3024) (ULT_STRE(2,J),J=1,MEMATC),'   Compr   ultimate'
            WRITE(BUG,3024) (ULT_STRE(3,J),J=1,MEMATC),'   Shear   ultimate'
            WRITE(BUG,*)

            WRITE(BUG,*) '  ULT_STRN, material strain allowables'
            WRITE(BUG,*) '  ------------------------------------'
            WRITE(BUG,*) '    Col 1         Col 2         Col 3         Col 4'
            WRITE(BUG,3024) (ULT_STRN(1,J),J=1,MEMATC),'   Tension ultimate'
            WRITE(BUG,3024) (ULT_STRN(2,J),J=1,MEMATC),'   Compr   ultimate'
            WRITE(BUG,3024) (ULT_STRN(3,J),J=1,MEMATC),'   Shear   ultimate'
            WRITE(BUG,*)

            FOUND = 'N'
            DO I=1,ELDOF
               IF (DOFPIN(I) /= 0) THEN
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO
            IF (FOUND == 'Y') THEN
               WRITE(BUG,*) '  DOFPIN, Array of element DOFs pinned'
               WRITE(BUG,*) '  ------------------------------------'
               WRITE(BUG,2024) (DOFPIN(I),I=1,ELDOF)
               WRITE(BUG,*)
            ENDIF

            IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
               WRITE(BUG,*) '  OFFDIS, Array of element offsets'
               WRITE(BUG,*) '  --------------------------------'
               DO I=1,ELGP
                  WRITE(BUG,3003) (OFFDIS(I,J),J=1,3)
               ENDDO 
               WRITE(BUG,*)
            ENDIF

            IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
               WRITE(BUG,5001) ELEM_LEN_AB
            ENDIF

            IF(TYPE(1:4) == 'ELAS') THEN                   ! For ELAS, write displ comps, at elem grids, that elem connects to
               WRITE(BUG,4001) (ELAS_COMP(I), GRID_TYPE(I), AGRID(I), I=1,2) 
            ENDIF

! Write output from ELMGMi

            IF     ((TYPE == 'ROD     ') .OR. (TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR.                                  &
                    (TYPE == 'TRMEM   ') .OR. (TYPE == 'TRPLT1  ') .OR. (TYPE == 'TRPLT2  ') .OR.                                  &
                    (TYPE == 'TRIA3K  ') .OR. (TYPE == 'TRIA3   ')) THEN
                  WRITE(BUG,*) '  TE coord transformation matrix from subr ELMGM1'

            ELSE IF ((TYPE == 'QDMEM   ') .OR. (TYPE == 'QDPLT1  ') .OR. (TYPE == 'QDPLT2   ') .OR.                                &
                     (TYPE == 'QUAD4K  ') .OR. (TYPE == 'QUAD4   ')) THEN
                  WRITE(BUG,*) '  TE coord transformation matrix from subr ELMGM2 with QUADAXIS = ',QUADAXIS

            ELSE IF ((TYPE == 'HEXA8   ') .OR. (TYPE == 'HEXA20  ') .OR.                                                           &
                     (TYPE == 'PENTA6  ') .OR. (TYPE == 'PENTA15 ') .OR.                                                           &
                     (TYPE == 'TETRA4  ') .OR. (TYPE == 'TETRA10 ')) THEN
                  WRITE(BUG,*) '  TE coord transformation matrix from subr ELMGM3'

            ENDIF
            WRITE(BUG,*) '  ----------------------------------------------------------------------'

            IF ((TYPE(1:1) == 'Q') .OR. (TYPE(1:1) == 'H')) THEN
               IF (QUADAXIS == 'SPLITD') THEN
                  WRITE(BUG,*) '  (UEL = TE x UEB and local x axis splits angle between 2 diagonals)'
               ELSE
                  WRITE(BUG,*) '  (UEL = TE x UEB and local x axis is along side 1-2 of the element)'
               ENDIF
            ELSE
               WRITE(BUG,*) '  (UEL = TE x UEB and local x axis is along side 1-2 of the element)'
            ENDIF
            DO I=1,3
               WRITE(BUG,3003) (TE(I,J),J=1,3)
            ENDDO 
            WRITE(BUG,*)

            WRITE(BUG,*) '  Actual & Internal G.P.s and local coordinates (AGRID, BGRID, I, XEL)'
            WRITE(BUG,*) '  --------------------------------------------------------------------'
            WRITE(BUG,*) '    Grid Number     Element          Local Element System Coords'
            WRITE(BUG,*) '  Actual Internal     No.          X              Y              Z'
            DO I=1,ELGP
               WRITE(BUG,1004)  AGRID(I), BGRID(I),I,(XEL(I,J),J=1,3)
            ENDDO 
            WRITE(BUG,*)

            IF ((TYPE == 'QDMEM   ') .OR. (TYPE == 'QDPLT1  ') .OR. (TYPE == 'QDPLT2   ') .OR.                                     &
                (TYPE == 'QUAD4K  ') .OR. (TYPE == 'QUAD4   ')) THEN
               WRITE(BUG,5002) HBAR, MXWARP
               WRITE(BUG,5003) CONV_RAD_DEG*QUAD_THETA
               WRITE(BUG,5004) CONV_RAD_DEG*QUAD_GAMMA
               WRITE(BUG,5005) CONV_RAD_DEG*QUAD_DELTA
               WRITE(BUG,*)
               IF (ABS(HBAR) > MXWARP) THEN
                  WRITE(BUG,*) '  Matrix BMEAN correction to strain displ matrix used for this elem since HBAR > MXWARP'
                  WRITE(BUG,*) '  Rows of BMEAN (transpose of BMEANT):'
                  WRITE(BUG,*) '  -----------------------------------'
                  DO J=1,4
                     DO K=1,3
                        WRITE(BUG,1005) (BMEANT(I,3*(J-1)+K),I=1,8)
                     ENDDO
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDIF

            ENDIF

            WRITE(BUG,5006) CONV_RAD_DEG*THETAM               ! Write material orientation axes angle from elem x-axis

! Write material matrices for all but 1D elements

            IF((TYPE == 'TRMEM   ') .OR. (TYPE == 'TRPLT1  ') .OR. (TYPE == 'TRPLT2  ') .OR.                                       &
               (TYPE == 'TRIA3K  ') .OR. (TYPE == 'TRIA3   ') .OR.                                                                 &
               (TYPE == 'QDMEM   ') .OR. (TYPE == 'QDPLT1  ') .OR. (TYPE == 'QDPLT2   ') .OR.                                      &
               (TYPE == 'QUAD4K  ') .OR. (TYPE == 'QUAD4   ')) THEN

               WRITE(BUG,*) '  EM material matrix, in local element coordinate system, for membrane stresses'
               WRITE(BUG,*) '  -----------------------------------------------------------------------------'
               DO I=1,3
                  WRITE(BUG,3003) (EM(I,J),J=1,3)
               ENDDO 
               WRITE(BUG,*)

               WRITE(BUG,*) '  EB material matrix, in local element coordinate system, for bending stresses'
               WRITE(BUG,*) '  ----------------------------------------------------------------------------'
               DO I=1,3
                  WRITE(BUG,3003) (EB(I,J),J=1,3)
               ENDDO 
               WRITE(BUG,*)

               WRITE(BUG,*) '  ET material matrix, in local element coordinate system, for transverse shear stresses'
               WRITE(BUG,*) '  -------------------------------------------------------------------------------------'
               DO I=1,2
                  WRITE(BUG,3002) (ET(I,J),J=1,2)
               ENDDO 
               WRITE(BUG,*)

            ELSE IF ((TYPE == 'HEXA8   ') .OR. (TYPE == 'HEXA20  ') .OR.                                                           &
                     (TYPE == 'PENTA6  ') .OR. (TYPE == 'PENTA15 ') .OR.                                                           &
                     (TYPE == 'TETRA4  ') .OR. (TYPE == 'TETRA10 ')) THEN

               WRITE(BUG,*) '  ES material matrix, in local element coordinate system, for 3D solid elements'
               WRITE(BUG,*) '  -----------------------------------------------------------------------------'
               DO I=1,6
                  WRITE(BUG,3006) (ES(I,J),J=1,6)
               ENDDO 
               WRITE(BUG,*)

            ENDIF

!zzzz    ENDIF

      ENDIF

! **********************************************************************************************************************************
! Element thermal and pressure load matrices

      IF (DUM_BUG(2) > 0) THEN 

         WRITE(BUG,1000)

         WRITE(BUG,1001) ELDT_BUG_P_T_BIT, TYPE, EID

         IF (NTSUB > 0) THEN

            DO I = 1,NSUB
               TCASE2(I) = 0
            ENDDO 
            J = 0
            DO I = 1,NSUB
               IF (SUBLOD(I,2) /= 0) THEN
                  J = J + 1
                  TCASE2(J) = I
               ENDIF
            ENDDO   

            WRITE(BUG,*) '  DT, Array of element temperature data (one row for each thermal subcase):'
            WRITE(BUG,*) '  ------------------------------------------------------------------------'
            DO J=1,NTSUB
               WRITE(BUG,1006) J, NTSUB
               WRITE(BUG,3026) (DT(I,J),I=1,MDT)
               WRITE(BUG,*)
            ENDDO 
            WRITE(BUG,*)

            DO J=1,NTSUB
               WRITE(BUG,2001) SCNUM(TCASE2(J))
               IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
                  WRITE(BUG,1007)
               ELSE
                  IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
                     WRITE(BUG,1003) (OFFSET(I),I=1,ELGP)
                  ENDIF
               ENDIF
               WRITE(BUG,3006) (PTE(I,J),I=1,ELDOF)
               WRITE(BUG,*)
            ENDDO 
         ELSE
            WRITE(BUG,*) '  There is no thermal load data'
         ENDIF
         WRITE(BUG,*)

         WRITE(BUG,*) '  PRESS, Array of element pressure data (one row for each subcase):'
         WRITE(BUG,*) '  ----------------------------------------------------------------'
         DO J=1,NSUB
            WRITE(BUG,1006) J, NSUB
            WRITE(BUG,3026) (PRESS(I,J),I=1,MPRESS)
            WRITE(BUG,*)
         ENDDO 
         WRITE(BUG,*)

         DO J=1,NSUB
            WRITE(BUG,2003) SCNUM(J)
            IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
               WRITE(BUG,1007)
            ELSE
               IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
                  WRITE(BUG,1003) (OFFSET(I),I=1,ELGP)
               ENDIF
            ENDIF
            WRITE(BUG,3006) (PPE(I,J),I=1,ELDOF)
            WRITE(BUG,*)
         ENDDO 
         WRITE(BUG,*)

      ENDIF

! **********************************************************************************************************************************
! Element mass

      IF (DUM_BUG(3) > 0) THEN

         WRITE(BUG,1000)

         WRITE(BUG,1001) ELDT_BUG_ME_BIT, TYPE, EID

         WRITE(BUG,*) '  ME element mass matrix in local element coordinate system'
         WRITE(BUG,*) '  ---------------------------------------------------------'
         IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
            WRITE(BUG,1007)
         ELSE
            IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
               WRITE(BUG,1003) (OFFSET(I),I=1,ELGP)
            ENDIF
         ENDIF
         DO I=1,ELDOF
            WRITE(BUG,*) '  Row',I
            WRITE(BUG,3006) (ME(I,J),J=1,ELDOF)
            WRITE(BUG,*)
         ENDDO 

      ENDIF

! **********************************************************************************************************************************
! Element stiffness matrix

      IF (DUM_BUG(4) > 0) THEN

         WRITE(BUG,1000)

         WRITE(BUG,1001) ELDT_BUG_KE_BIT, TYPE, EID

         IF (TYPE(1:5) == 'TRIA3') THEN
            SHELL_T_avg = 0.5*( SHELL_T(1,1) + SHELL_T(2,2) )
            WRITE(BUG,*) '  TRIA3 plate element parameters used in calculating transverse shear stiffness'
            WRITE(BUG,*) '  -----------------------------------------------------------------------------'
            NAME1 = '      CBMIN3                                             = ' ; WRITE(BUG,5007) NAME1, CBMIN3
            NAME1 = '      SHELL_T_avg = 0.5*[(SHELL_T(1,1) + SHELL_T(2,2)]   = ' ; WRITE(BUG,5007) NAME1, SHELL_T_avg
            NAME1 = '      BENSUM                                             = ' ; WRITE(BUG,5007) NAME1, BENSUM
            NAME1 = '      SHRSUM                                             = ' ; WRITE(BUG,5007) NAME1, SHRSUM
            NAME1 = '      PSI_HAT = BENSUM/SHRSUM                            = ' ; WRITE(BUG,5007) NAME1, PSI_HAT
            NAME1 = '      PHI_SQ  = CBMIN3*PSI_HAT/(1 + CBMIN3*PSI_HAT)      = ' ; WRITE(BUG,5007) NAME1, PHI_SQ
            NAME1 = '      SHELL_T_avg*PHI_SQ                                 = ' ; WRITE(BUG,5007) NAME1, SHELL_T_avg*PHI_SQ
            WRITE(BUG,*)
         ENDIF

         IF (TYPE(1:5) == 'QUAD4') THEN
            SHELL_T_avg = 0.5*( SHELL_T(1,1) + SHELL_T(2,2) )
            WRITE(BUG,*) '  QUAD4 plate element parameters used in calculating transverse shear stiffness'
            WRITE(BUG,*) '  -----------------------------------------------------------------------------'
            NAME1 = '      CBMIN4                                             = ' ; WRITE(BUG,5007) NAME1, CBMIN4
            NAME1 = '      SHELL_T_avg = 0.5*[(SHELL_T(1,1) + SHELL_T(2,2)]   = ' ; WRITE(BUG,5007) NAME1, SHELL_T_avg
            NAME1 = '      BENSUM                                             = ' ; WRITE(BUG,5007) NAME1, BENSUM
            NAME1 = '      SHRSUM                                             = ' ; WRITE(BUG,5007) NAME1, SHRSUM
            NAME1 = '      PSI_HAT = BENSUM/SHRSUM                            = ' ; WRITE(BUG,5007) NAME1, PSI_HAT
            NAME1 = '      PHI_SQ  = CBMIN4*PSI_HAT/(1 + CBMIN4*PSI_HAT)      = ' ; WRITE(BUG,5007) NAME1, PHI_SQ
            NAME1 = '      SHELL_T_avg*PHI_SQ                                 = ' ; WRITE(BUG,5007) NAME1, SHELL_T_avg*PHI_SQ
            WRITE(BUG,*)
         ENDIF

         IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
            WRITE(BUG,*) '  KED element stiffness matrix in local element coordinate system'
            WRITE(BUG,*) '  ---------------------------------------------------------------'
            IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
               WRITE(BUG,1007)
            ELSE
               IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
                  WRITE(BUG,1003) (OFFSET(I),I=1,ELGP)
               ENDIF
            ENDIF
            DO I=1,ELDOF
               WRITE(BUG,*) '  Row',I
               WRITE(BUG,3006) (KED(I,J),J=1,ELDOF)
               WRITE(BUG,*)
            ENDDO
         ELSE 
            WRITE(BUG,*) '  KE element stiffness matrix in local element coordinate system'
            WRITE(BUG,*) '  --------------------------------------------------------------'
            IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
               WRITE(BUG,1007)
            ELSE
               IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
                  WRITE(BUG,1003) (OFFSET(I),I=1,ELGP)
               ENDIF
            ENDIF
            DO I=1,ELDOF
               WRITE(BUG,*) '  Row',I
               WRITE(BUG,3006) (KE(I,J),J=1,ELDOF)
               WRITE(BUG,*)
            ENDDO
         ENDIF 

      ENDIF

! **********************************************************************************************************************************
! SEi, STEi stress recovery matrices

      IF (DUM_BUG(5) > 0) THEN

         WRITE(BUG,1000)

!zzzz    IF (PCOMP_PROPS == 'N') THEN

            WRITE(BUG,1001) ELDT_BUG_SE_BIT, TYPE, EID

!xx         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE(1:5) /= 'TRIA3') .AND. (TYPE(1:5) /= 'QUAD4') .AND                               &
!xx             (TYPE(1:4) /= 'HEXA') .AND. (TYPE(1:5) /= 'PENTA') .AND. (TYPE(1:5) /= 'TETRA')) THEN

            IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
               IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
                  WRITE(BUG,1003) (OFFSET(I),I=1,ELGP)
               ENDIF
            ELSE
               WRITE(BUG,*) '  SEi, STEi matrices do not include the effects of offsets (this is done later in the code)'
               WRITE(BUG,*)
            ENDIF

            DO I=1,METYPE
               IF (TYPE == ELMTYP(I)) THEN
                  NUM_STRESS_MATS = NUM_SEi(I)
               ENDIF
            ENDDO
            WRITE(BUG,*) '  This element has ',NUM_STRESS_MATS,' stress recovery points. The first is at the center of the element.'
            WRITE(BUG,*) '  Subsequent stress recovery points are at the element corners or at a mesh of Gauss points.'
            WRITE(BUG,*)
            WRITE(BUG,*) '  For the MIN4T QUAD4 element the 2nd-5th recovery points are values at the center of each of the'
            WRITE(BUG,*) '  4 non-overlapping TRIA elements making up the QUAD4 and the 1st recovery point matrices are'
            WRITE(BUG,*) '  the average of the ones for points 2-5'
            WRITE(BUG,*)

            DO K=1,NUM_STRESS_MATS
               WRITE(BUG,*) '  SE1 matrix for stress data recovery point ',k
               WRITE(BUG,*) '  ---------------------------------------------'
               DO I=1,3
                  WRITE(BUG,*) '  Row',I
                  WRITE(BUG,3006) (SE1(I,J,K),J=1,ELDOF)
                  WRITE(BUG,*)
               ENDDO 
               WRITE(BUG,*)
            ENDDO

            DO K=1,NUM_STRESS_MATS
               WRITE(BUG,*) '  SE2 matrix for stress data recovery point ',k
               WRITE(BUG,*) '  ---------------------------------------------'
               DO I=1,3
                  WRITE(BUG,*) '  Row',I
                  WRITE(BUG,3006) (SE2(I,J,K),J=1,ELDOF)
                  WRITE(BUG,*)
               ENDDO 
               WRITE(BUG,*)
            ENDDO

            DO K=1,NUM_STRESS_MATS
               WRITE(BUG,*) '  SE3 matrix for stress data recovery point ',k
               WRITE(BUG,*) '  ---------------------------------------------'
               DO I=1,3
                  WRITE(BUG,*) '  Row',I
                  WRITE(BUG,3006) (SE3(I,J,K),J=1,ELDOF)
                  WRITE(BUG,*)
               ENDDO 
               WRITE(BUG,*)
            ENDDO

            IF (NTSUB > 0) THEN

               DO K=1,NUM_STRESS_MATS
                  WRITE(BUG,*) '  STE1 matrix for thermal stress/strain effects for stress data recovery point ',k
                  WRITE(BUG,*) '  --------------------------------------------------------------------------------'
                  DO J=1,NTSUB
                     WRITE(BUG,*) '  Column',J
                     WRITE(BUG,3003) (STE1(I,J,K),I=1,3)
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDDO

               DO K=1,NUM_STRESS_MATS
                  WRITE(BUG,*) '  STE2 matrix for thermal stress/strain effects for stress data recovery point ',k
                  WRITE(BUG,*) '  --------------------------------------------------------------------------------'
                  DO J=1,NTSUB
                     WRITE(BUG,*) '  Column',J
                     WRITE(BUG,3003) (STE2(I,J,K),I=1,3)
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDDO

               DO K=1,NUM_STRESS_MATS
                  WRITE(BUG,*) '  STE3 matrix for thermal stress/strain effects for stress data recovery point ',k
                  WRITE(BUG,*) '  --------------------------------------------------------------------------------'
                  DO J=1,NTSUB
                     WRITE(BUG,*) '  Column',J
                     WRITE(BUG,3003) (STE3(I,J,K),I=1,3)
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDDO

            ENDIF

            WRITE(BUG,*) '  Factors that convert engineering stresses to enginering forces:'
            WRITE(BUG,*) '  --------------------------------------------------------------'
            WRITE(BUG,'(A,1ES14.6)') '  Factor for membrane     stresses, FCONV(1) = ', FCONV(1)
            WRITE(BUG,'(A,1ES14.6)') '  Factor for bending      stresses, FCONV(2) = ', FCONV(2)
            WRITE(BUG,'(A,1ES14.6)') '  Factor for transv shear stresses, FCONV(3) = ', FCONV(3)
            WRITE(BUG,*)
            WRITE(BUG,*)

            IF ((TYPE(1:4) == 'BUSH' ) .OR. (TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:6) == 'USERIN') .OR.   &
                (TYPE(1:4) == 'HEXA' ) .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN

               DO K=1,NUM_STRESS_MATS
                  WRITE(BUG,*) '  BE1 matrix for strain data recovery point ',k
                  WRITE(BUG,*) '  ---------------------------------------------'
                  DO I=1,3
                     WRITE(BUG,*) '  Row',I
                     WRITE(BUG,3006) (BE1(I,J,K),J=1,ELDOF)
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDDO

               DO K=1,NUM_STRESS_MATS
                  WRITE(BUG,*) '  BE2 matrix for strain data recovery point ',k
                  WRITE(BUG,*) '  ---------------------------------------------'
                  DO I=1,3
                     WRITE(BUG,*) '  Row',I
                     WRITE(BUG,3006) (BE2(I,J,K),J=1,ELDOF)
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDDO

               DO K=1,NUM_STRESS_MATS
                  WRITE(BUG,*) '  BE3 matrix for strain data recovery point ',k
                  WRITE(BUG,*) '  ---------------------------------------------'
                  DO I=1,3
                     WRITE(BUG,*) '  Row',I
                     WRITE(BUG,3006) (BE3(I,J,K),J=1,ELDOF)
                     WRITE(BUG,*)
                  ENDDO 
                  WRITE(BUG,*)
               ENDDO

            ENDIF

!zzzz    ENDIF

      ENDIF

! **********************************************************************************************************************************
! Write output from LINK9 calculation of element displs, nodal forces

      IF (DUM_BUG(6) > 0) THEN

         WRITE(BUG,1000)

         WRITE(BUG,1001) ELDT_BUG_U_P_BIT, TYPE, EID

!        IF     (ELFORCEN == 'LOCAL') THEN

            NAME2 = 'AGRID, UEL displs of ' ; NAME3 = ' local elem ' ; WRITE(BUG,2010) NAME2, NAME3, CASE_NUM
            I2 = 0
            DO I=1,ELGP
               DO J=1,6
                  OEL(J) = ZERO
               ENDDO
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  OEL(J) = UEL(I2)
               ENDDO
               WRITE(BUG,3007) AGRID(I), (OEL(J),J=1,6)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

            NAME2 = 'AGRID, PEL loads at  ' ; NAME3 = ' local elem ' ; WRITE(BUG,2010) NAME2, NAME3, CASE_NUM
            I2 = 0
            DO I=1,ELGP
               DO J=1,6
                  OEL(J) = ZERO
               ENDDO
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  OEL(J) = PEL(I2)
               ENDDO
               WRITE(BUG,3007) AGRID(I), (OEL(J),J=1,6)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

!        ELSE IF (ELFORCEN == 'BASIC') THEN

            NAME2 = 'AGRID, UEB displs of ' ; NAME3 = ' basic      ' ; WRITE(BUG,2010) NAME2, NAME3, CASE_NUM
            I2 = 0
            DO I=1,ELGP
               DO J=1,6
                  OEL(J) = ZERO
               ENDDO
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  OEL(J) = UEB(I2)
               ENDDO
               WRITE(BUG,3007) AGRID(I), (OEL(J),J=1,6)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

            NAME2 = 'AGRID, PEB loads at  ' ; NAME3 = ' basic      ' ; WRITE(BUG,2010) NAME2, NAME3, CASE_NUM
            I2 = 0
            DO I=1,ELGP
               DO J=1,6
                  OEL(J) = ZERO
               ENDDO
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  OEL(J) = PEB(I2)
               ENDDO
               WRITE(BUG,3007) AGRID(I), (OEL(J),J=1,6)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

!        ELSE IF (ELFORCEN == 'GLOBAL') THEN

            NAME2 = 'AGRID, UEG displs of ' ; NAME3 = ' global     ' ; WRITE(BUG,2010) NAME2, NAME3, CASE_NUM
            I2 = 0
            DO I=1,ELGP
               DO J=1,6
                  OEL(J) = ZERO
               ENDDO
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  OEL(J) = UEG(I2)
               ENDDO
               WRITE(BUG,3007) AGRID(I), (OEL(J),J=1,6)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

            NAME2 = 'AGRID, PEG loads at  ' ; NAME3 = ' global     ' ; WRITE(BUG,2010) NAME2, NAME3, CASE_NUM
            I2 = 0
            DO I=1,ELGP
               DO J=1,6
                  OEL(J) = ZERO
               ENDDO
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  OEL(J) = PEG(I2)
               ENDDO
               WRITE(BUG,3007) AGRID(I), (OEL(J),J=1,6)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

!        ENDIF

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1000 FORMAT(' ------------------------------------------------------------------------------------------------------------------',&
             '-----------------')

 1001 FORMAT(' ELDATA(',I2,',PRINT) requests for ',A,' element number ',I8,/,                                                      &
             ' ==============================================================',/)

 1003 FORMAT('   Grid point offset indicators: ',4(1X,A1))

 1004 FORMAT(1X,I8,1X,I8,1X,I6,2X,3(1ES15.6))

 1113 FORMAT(3X,'End of v vector:',3(1ES15.6))

 1005 FORMAT(8(1ES14.6))

 1006 FORMAT('   Row ',I8,' of ',I8,/,'   ------------------------')

 1007 FORMAT(' (does not include effects of offsets for BAR, BEAM or ROD)')

 2001 FORMAT('   Thermal  load matrix for subcase number ',  I8  ,' (in local element coordinates)',/,    &
             '   -------------------------------------------------------------------------------------------------------')

 2003 FORMAT('   Pressure load matrix for subcase number ',  I8  ,' (in local element coordinates)',/,    &
             '   ---------------------------------------------------------------------------------------------------------')

 2010 FORMAT(3X,A,' elem nodes in ',A,' coordinate system for subcase ',I8                                                     ,//,&
             '    Grid           T1            T2            T3            R1            R2            R3'                      ,/,&
             '    ----           --            --            --            --            --            --')
 2024 FORMAT(24I3) 

 3002 FORMAT(2(1ES14.6))

 3003 FORMAT(3(1ES14.6))

 3004 FORMAT(4(1ES14.6))

 3005 FORMAT(5(1ES14.6))

 3006 FORMAT(6(1ES14.6))

 3007 FORMAT(I8,4X,6(1ES14.6))

 3024 FORMAT(4(1ES14.6),A)

 3026 FORMAT(10(1ES14.6))

 4001 FORMAT('   ELAS_COMP - Displacement components that ELAS elem connects to: comp',I2,' at ',A,I8,' and comp',I2,' at ',A,I8,/,&
             '   ----------------------------------------------------------------------------------------------------------------',&
             '-----------------',/)

 5001 FORMAT('   Length of element between grids 1 and 2 (including effects of offsets) = ',1ES13.6,/,                             &
             '   ----------------------------------------------------------------------',/)

 5002 FORMAT('   HBAR warp of quadrilateral element = ',1ES13.6,'. BMEAN will be used if HBAR > MXWARP = ',                        &
                 1ES13.6/,                                                                                                         &
             '   ---------------------------------------------------------------------------------------------------------------',/)

 5003 FORMAT('   QUAD_THETA angle between side 1-2 and diagonal 1-3                        = ',1ES13.6,' (degrees)')

 5004 FORMAT('   QUAD_GAMMA angle between side 1-2 and diagonal 2-4                        = ',1ES13.6,' (degrees)')

 5005 FORMAT('   QUAD_DELTA angle to rotate from side 1-2 to split angle between diagonals = ',1ES13.6,' (degrees) = ',            &
                 '(QUAD_THETA - QUAD_GAMMA)/2'                                                                                  ,/,&
             '   -------------------------------------------------------------------------')

 5006 FORMAT('   THETAM angle to rotate from local x axis to material orientation axis = ',1ES13.6,' (degrees)',/,                 &
             '   ---------------------------------------------------------------------',/)

 5007 FORMAT(A,1ES14.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE ELMOUT
