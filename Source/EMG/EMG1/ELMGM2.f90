! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

      SUBROUTINE ELMGM2 ( WRITE_WARN )
 
! Calcs and checks elem geometry for quad elems and provides a transformation matrix ( TE ) to transfer the elem stiffness matrix
! in the elem system to the basic coordinate system. Calculates grid point coords in local coord system.
! To define the elem coordinate system, a mean plane is defined which lies midway between the grid points (HBAR is mean dist).
! The elem z direction is in the direction of the cross product of the diagonals (V13 x V24). Initially, the x axis is along
! side 1-2 of the elem projection onto the mean plane. For elems thet are not rectangular, the x,y axes are rotated such that x
! splits the angle between the diagonals.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEFE, MEWE, MELGP, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMGM2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, TWO
      USE PARAMS, ONLY                :  EPSIL, QUADAXIS, SUPWARN
      USE MODEL_STUF, ONLY            :  AGRID, BMEANT, EID, ELGP, EMG_IFE, EMG_IWE, EMG_RWE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS,     &
                                         HBAR, MXWARP, QUAD_DELTA, QUAD_GAMMA, QUAD_THETA, TE, TE_IDENT, TYPE, WARP_WARN, XEB, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE ELMGM2_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMGM2'
      CHARACTER(30*BYTE)              :: NAME(15)          ! Names for BUG output purposes
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: DIAG_GRID1        ! Used for error output purposes
      INTEGER(LONG)                   :: DIAG_GRID2        ! Used for error output purposes
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: QUAD_GEOM_ERR = 0 ! Local error count
      INTEGER(LONG)                   :: ID(3)             ! ID(i) is set to 1 if the i-th diagonal of TE is 1.0
      INTEGER(LONG)                   :: IPNT              ! An internal grid (1,2,3 or 4) of an elem
      INTEGER(LONG)                   :: SIDE_GRID1        ! Used for error output purposes
      INTEGER(LONG)                   :: SIDE_GRID2        ! Used for error output purposes
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMGM2_BEGEND

      REAL(DOUBLE)                    :: V12B(3)           ! Vector from G.P. 1 to G.P. 2 in basic coords
      REAL(DOUBLE)                    :: V13B(3)           ! Vector from G.P. 1 to G.P. 3 in basic coords (a diagonal)
      REAL(DOUBLE)                    :: V24B(3)           ! Vector from G.P. 2 to G.P. 4 in basic coords (a diagonal)
      REAL(DOUBLE)                    :: V13BM             ! Mag of V13B
      REAL(DOUBLE)                    :: V24BM             ! Mag of V24B
      REAL(DOUBLE)                    :: DHBAR             ! DABS(HBAR)
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare to real zero
      REAL(DOUBLE)                    :: EPS4              ! A small number to compare to real zero
      REAL(DOUBLE)                    :: IVEC(3)           ! A vector in the elem x dir
      REAL(DOUBLE)                    :: JVEC(3)           ! A vector in the elem y dir
      REAL(DOUBLE)                    :: KVEC(3)           ! A vector in the elem z dir
      REAL(DOUBLE)                    :: L12               ! Length of side 1-2 of the elem in the mean plane
      REAL(DOUBLE)                    :: L23               ! Length of side 2-3 of the elem in the mean plane
      REAL(DOUBLE)                    :: L34               ! Length of side 3-4 of the elem in the mean plane
      REAL(DOUBLE)                    :: L41               ! Length of side 4-1 of the elem in the mean plane
      REAL(DOUBLE)                    :: MAGI              ! Magnitude of vector IVEC
      REAL(DOUBLE)                    :: MAGJ              ! Magnitude of vector JVEC
      REAL(DOUBLE)                    :: MAGK              ! Magnitude of vector KVEC
      REAL(DOUBLE)                    :: CT_QD(3,3)        ! Coord transf matrix which will rotate a vector thru an angle QUAD_DELTA
      REAL(DOUBLE)                    :: TE_12(3,3)        ! TE matrix for this elem if local x is parallel to side 1-2
      REAL(DOUBLE)                    :: TE_SD(3,3)        ! TE matrix for this elem if local x splits angle between the two diags
      REAL(DOUBLE)                    :: X12               ! (X1 - X2) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: X13               ! (X1 - X3) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: X14               ! (X1 - X4) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: X23               ! (X2 - X3) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: X24               ! (X2 - X4) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: X34               ! (X3 - X4) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: X3                ! -X13
      REAL(DOUBLE)                    :: X4                ! -X14
      REAL(DOUBLE)                    :: Y3                !
      REAL(DOUBLE)                    :: Y4                !
      REAL(DOUBLE)                    :: Y34               ! (Y3 - Y4) in elem mean plane in local elem coords
      REAL(DOUBLE)                    :: VAR(15)           ! Variables for BUG output purposes
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)
      EPS4 = EPSIL(4)

! Initialize XEL to zero
 
      DO I=1,MELGP
         DO J=1,3
            XEL(I,J) = ZERO
         ENDDO 
      ENDDO 

! **********************************************************************************************************************************
! Calculate elem z direction from cross products of diagonals
 
! Generate vectors from G.P 1 to G.P 3 and from G.P. 2 to G.P. 4 (diagonals)
 
      DO I=1,3
         V13B(I) = XEB(3,I) - XEB(1,I)
         V24B(I) = XEB(4,I) - XEB(2,I)
      ENDDO 
 
      CALL CROSS ( V13B, V24B, KVEC )
      MAGK = DSQRT(KVEC(1)*KVEC(1) + KVEC(2)*KVEC(2) + KVEC(3)*KVEC(3))

! If MAGK = 0 then diagonals are parallel so write error and quit

      IF (MAGK <=  EPS1) THEN
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1911) TYPE, EID
            WRITE(F06,1911) TYPE, EID
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1911
            ENDIF
         ENDIF
         RETURN
      ENDIF

! Unit vector in elem local z direction is 3rd row of TE
 
      DO I=1,3
         KVEC(I)     = KVEC(I)/MAGK
         TE_12(3,I) = KVEC(I)
      ENDDO 
 
! **********************************************************************************************************************************
! Calc initial elem x dir along side 1-2 of the elem projection onto the mean plane.

      DO I=1,3
         V12B(I) = XEB(2,I) - XEB(1,I)
      ENDDO 
 
! HBAR is one half of the projection of V12B in z direction
 
      HBAR = HALF*((V12B(1)*KVEC(1) + V12B(2)*KVEC(2) + V12B(3)*KVEC(3)))
 
! Now calculate initial x direction along side 1-2 of the elem projection onto the mean plane.
 
      DO I=1,3
         IVEC(I) = V12B(I) - TWO*HBAR*KVEC(I)
      ENDDO 

! If initial MAGI = 0 then write error and quit.
      MAGI  = DSQRT(IVEC(1)*IVEC(1) + IVEC(2)*IVEC(2) + IVEC(3)*IVEC(3))
      IF (MAGI <= EPS1) THEN
         SIDE_GRID1 = 1
         SIDE_GRID2 = 2
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
            WRITE(F06,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1908
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = SIDE_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = SIDE_GRID2
            ENDIF
         ENDIF
         RETURN
      ENDIF

! Unit vector in initial elem x direction

      DO I=1,3
         IVEC(I)     = IVEC(I)/MAGI                        ! Unit vector along side 1-2 in the mean plane (NOT from G.P. 1-2)
         TE_12(1,I) = IVEC(I)
      ENDDO 

! **********************************************************************************************************************************
! Calculate unit vector in initial elem. y direction (from KVEC x IVEC):

      CALL CROSS ( KVEC, IVEC, JVEC )
      MAGJ = DSQRT(JVEC(1)*JVEC(1) + JVEC(2)*JVEC(2) + JVEC(3)*JVEC(3))

! If MAGJ =0 then write error and quit.

      IF (MAGJ <= EPS1) THEN
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1912) TYPE, EID
            WRITE(F06,1912) TYPE, EID
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1912
            ENDIF
         ENDIF
         RETURN
      ENDIF

      DO I=1,3
         JVEC(I)    = JVEC(I)/MAGJ
         TE_12(2,I) = JVEC(I)
      ENDDO 

! **********************************************************************************************************************************
! Perform some geometry checks on the quad element

! First, calculate XEL coords of grids in local element coord system (relative to node 1) with local x along side 1-2. 

      XEL(1,1) = ZERO
      XEL(1,2) = ZERO
      XEL(1,3) = ZERO

      XEL(2,3) = ZERO                                      ! All z coords in mean plane are zero by definition of mean plane
      XEL(3,3) = ZERO                                      ! even though using TE to calc them may not yield zero's
      XEL(4,3) = ZERO

      DO I=2,ELGP
         DO J=1,2                                          ! Only calc x znd y coords. z coords in mean plane are zero by definition
            XEL(I,J) = ZERO
            DO K=1,3
               XEL(I,J) = XEL(I,J) + (XEB(I,K) - XEB(1,K))*TE_12(J,K)
            ENDDO 
         ENDDO 
      ENDDO

      QUAD_GEOM_ERR = 0
      CALL QUAD_GEOM_CHECK
      IF (QUAD_GEOM_ERR > 0) THEN
         RETURN
      ENDIF
 
! **********************************************************************************************************************************
! Now TE_12 is for elem coord system with x along projected side 1-2. We need to rotate x-y (about z) to get x in a
! direction which splits the angle between the two diagonals. QUAD_THETA is the angle between side 1-2 and diagonal from
! point 1 to point 3. QUAD_GAMMA is the angle between side 1-2 and the diagonal from point 2 to point 4. The rotation
! about z is thru an angle of QUAD_DELTA = (QUAD_THETA - QUAD_GAMMA)/2.
 
! Find QUAD_THETA from the dot product of vector along side 1-2 and  diagonal from point 1 to point 3 (in the mean plane)
! Find QUAD_GAMMA from the dot product of vector along side 1-2 and  diagonal from point 2 to point 4 (in the mean plane).
! Use ABS to get the acute angle
 
      QUAD_THETA = DACOS(( V13B(1)*IVEC(1) + V13B(2)*IVEC(2) + V13B(3)*IVEC(3))/V13BM)
      QUAD_GAMMA = DACOS((-V24B(1)*IVEC(1) - V24B(2)*IVEC(2) - V24B(3)*IVEC(3))/V24BM)
      QUAD_DELTA = (QUAD_THETA - QUAD_GAMMA)/TWO

      CALL PLANE_COORD_TRANS_21 ( QUAD_DELTA, CT_QD, SUBR_NAME )
      CALL MATMULT_FFF ( CT_QD, TE_12, 3, 3, 3, TE_SD )

! Select how the local x axis is to be

      IF (QUADAXIS == 'SPLITD') THEN

         DO I=1,3
            DO J=1,3
               TE(I,J) = TE_SD(I,J)
            ENDDO 
         ENDDO 

      ELSE

         DO I=1,3
            DO J=1,3
               TE(I,J) = TE_12(I,J)
            ENDDO 
         ENDDO 

      ENDIF

! Now TE is final transformation from basic to elem coordinates. That is, UEL = TE*UEB        
 
      IF ((DEBUG(6) == 1) .AND. (WRT_BUG(0) == 1)) THEN

         WRITE(BUG,*) '  Coord transformation matrix that rotates a vector through angle QUAD_DELTA'
         WRITE(BUG,*) '  --------------------------------------------------------------------------'
         DO I=1,3
            WRITE(BUG,90003) (CT_QD(I,J),J=1,3)
         ENDDO 
         WRITE(BUG,*)

         WRITE(BUG,*) '  TE matrix if local x is along side 1-2'
         WRITE(BUG,*) '  --------------------------------------'
         DO I=1,3
            WRITE(BUG,90003) (TE_12(I,J),J=1,3)
         ENDDO 
         WRITE(BUG,*)

         WRITE(BUG,*) '  TE matrix if local x splits the angle between the diagonals'
         WRITE(BUG,*) '  -----------------------------------------------------------'
         DO I=1,3
            WRITE(BUG,90003) (TE_SD(I,J),J=1,3)
         ENDDO 
         WRITE(BUG,*)

         IF (QUADAXIS == 'SPLITD') THEN
            WRITE(BUG,*) '  Final TE matrix - local x splits angle between diagonals:'
            WRITE(BUG,*) '  --------------------------------------------------------'
         ELSE
            WRITE(BUG,*) '  Final TE matrix - local x parallel to side 1-2:'
            WRITE(BUG,*) '  ----------------------------------------------'
         ENDIF
         DO I=1,3
            WRITE(BUG,90003) (TE(I,J),J=1,3)
         ENDDO 
         WRITE(BUG,*)
         CALL CHECK_TE_MATRIX ( TE, 'TE' )
      ENDIF


! **********************************************************************************************************************************
! Now set TE_IDENT to be 'Y' if TE is an identity matrix. TE will be an identity matrix if the diagonal terms are unity.

      TE_IDENT = 'N'
      DO I=1,3
         IF (DABS(TE(I,I) - ONE) < EPS1) THEN
            ID(I) = 1
         ELSE
            ID(I) = 0
         ENDIF
      ENDDO 
      IF ((ID(1) == 1) .AND. (ID(2) == 1) .AND. (ID(3) == 1)) THEN
         TE_IDENT = 'Y'
      ENDIF

! **********************************************************************************************************************************
! Calculate XEL coords of grids in local element coord system (relative to node 1). 

      XEL(1,1) = ZERO
      XEL(1,2) = ZERO
      XEL(1,3) = ZERO

      XEL(2,3) = ZERO                                      ! All z coords in mean plane are zero by definition of mean plane
      XEL(3,3) = ZERO                                      ! even though using TE to calc them may not yield zero's
      XEL(4,3) = ZERO

      DO I=2,ELGP
         DO J=1,2                                          ! Only calc x znd y coords. z coords in mean plane are zero by definition
            XEL(I,J) = ZERO
            DO K=1,3
               XEL(I,J) = XEL(I,J) + (XEB(I,K) - XEB(1,K))*TE(J,K)
            ENDDO 
         ENDDO 
      ENDDO

      IF ((DEBUG(6) == 1) .AND. (WRT_BUG(0) == 1)) THEN
         WRITE(BUG,*) '  Grid coords in mean plane - using final coord system, TE:'
         WRITE(BUG,*) '  --------------------------------------------------------'
         DO I=1,4
            WRITE(BUG,90003) (XEL(I,J),J=1,3)
         ENDDO 
         WRITE(BUG,*)
      ENDIF

! **********************************************************************************************************************************
! If HBAR is nonzero, we need to calculate transformation from mean plane to the grid points. This is used only for
! the membrane element. BMEANT is the transpose of the B matrix for the QDMEM1 elem.
 
      IF (DABS(HBAR) > MXWARP) THEN
         CALL CALC_BMEANT
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1908 FORMAT(' *ERROR  1908: ',A,' ELEMENT ',I8,' HAS LENGTH = ZERO ON THE SIDE THAT HAS INTERNAL GRIDS ',I2,' AND ',I2)

 1911 FORMAT(' *ERROR  1911: ',A,' ELEMENT ',I8,' HAS ITS 2 DIAGONALS PARALLEL.')

 1912 FORMAT(' *ERROR  1912: CANNOT CALCULATE VECTOR IN ELEMENT Y DIRECTION FOR ',A,' ELEMENT ',I8)

90003 FORMAT(3(1ES14.6))




! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE QUAD_GEOM_CHECK

! Checks QUAD geometry. The coords of the 4 points have local x from side 1 to side 2

      IMPLICIT NONE

      INTEGER(LONG)                   :: CW_ERR            ! Error indicator for CW/CCW check

      REAL(DOUBLE)                    :: K13VEC(3)         ! A vector resulting from V12 cross V23
      REAL(DOUBLE)                    :: K24VEC(3)         ! A vector resulting from V23 cross V34
      REAL(DOUBLE)                    :: K31VEC(3)         ! A vector resulting from V34 cross V41
      REAL(DOUBLE)                    :: K42VEC(3)         ! A vector resulting from V41 cross V12
      REAL(DOUBLE)                    :: V12L(3)           ! Vector from G.P. 1 to G.P. 2 in basic coords
      REAL(DOUBLE)                    :: V23L(3)           ! Vector from G.P. 2 to G.P. 3 in basic coords
      REAL(DOUBLE)                    :: V34L(3)           ! Vector from G.P. 3 to G.P. 4 in basic coords
      REAL(DOUBLE)                    :: V41L(3)           ! Vector from G.P. 4 to G.P. 1 in basic coords

! **********************************************************************************************************************************
! Variables used in checking geometry

      X12 = -(V12B(1)*IVEC(1) + V12B(2)*IVEC(2) + V12B(3)*IVEC(3))
      X13 = -(V13B(1)*IVEC(1) + V13B(2)*IVEC(2) + V13B(3)*IVEC(3))
      X24 = -(V24B(1)*IVEC(1) + V24B(2)*IVEC(2) + V24B(3)*IVEC(3))
      X14 =  X12 + X24
      X23 =  X13 - X12
      X34 =  X14 - X13
      Y3  =  (V13B(1)*JVEC(1) + V13B(2)*JVEC(2) + V13B(3)*JVEC(3))
      Y4  =  (V24B(1)*JVEC(1) + V24B(2)*JVEC(2) + V24B(3)*JVEC(3))
      Y34 =  Y3 - Y4
      L12 =  DABS(X12)
      L23 =  DSQRT(X23*X23 + Y3*Y3)
      L34 =  DSQRT(X34*X34 + Y34*Y34)
      L41 =  DSQRT(X14*X14 + Y4*Y4)

      X3  = -X13
      X4  = -X14

      IF ((DEBUG(6) == 1) .AND. (WRT_BUG(0) == 1)) THEN
         WRITE(BUG,*) '  Variables used in checking quad geometry:'
         WRITE(BUG,*) '  ----------------------------------------'
         NAME( 1) = ' X12 = -V12B(t)*IVEC       =  '  ;  VAR( 1) = X12
         NAME( 2) = ' X13 = -V13B(t)*IVEC       =  '  ;  VAR( 2) = X13
         NAME( 3) = ' X24 = -V24B(t)*IVEC       =  '  ;  VAR( 3) = X24
         NAME( 4) = ' X14 =  X12 + X24          =  '  ;  VAR( 4) = X14
         NAME( 5) = ' X23 =  X13 - X12          =  '  ;  VAR( 5) = X23
         NAME( 6) = ' X34 =  X14 - X13          =  '  ;  VAR( 6) = X34
         NAME( 7) = ' X3  = -X13                =  '  ;  VAR( 7) = X3
         NAME( 8) = ' Y3  =  V13B(t)*JVEC       =  '  ;  VAR( 8) = Y3
         NAME( 9) = ' X4  = -X14                =  '  ;  VAR( 9) = X4
         NAME(10) = ' Y4  =  V24B(t)*JVEC       =  '  ;  VAR(10) = Y4
         NAME(11) = ' Y34 =  Y3 - Y4            =  '  ;  VAR(11) = Y34
         NAME(12) = ' L12 = Side 1-2 length     =  '  ;  VAR(12) = L12
         NAME(13) = ' L23 = Side 2-3 length     =  '  ;  VAR(13) = L23
         NAME(14) = ' L34 = Side 3-4 length     =  '  ;  VAR(14) = L34
         NAME(15) = ' L41 = Side 4-1 length     =  '  ;  VAR(15) = L41
         DO I=1,15
            WRITE(BUG,90001) NAME(I),VAR(I)
         ENDDO
         WRITE(BUG,*)
      ENDIF

! Make sure that all side lengths are finite

      IF (DABS(L12) < EPS1) THEN
         QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
         SIDE_GRID1 = 1
         SIDE_GRID2 = 2
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
            WRITE(F06,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1908
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = SIDE_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = SIDE_GRID2
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(L23) < EPS1) THEN
         QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
         SIDE_GRID1 = 2
         SIDE_GRID2 = 3
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
            WRITE(F06,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1908
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = SIDE_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = SIDE_GRID2
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(L34) < EPS1) THEN
         QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
         SIDE_GRID1 = 3
         SIDE_GRID2 = 4
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
            WRITE(F06,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1908
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = SIDE_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = SIDE_GRID2
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(L41) < EPS1) THEN
         QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
         SIDE_GRID1 = 4
         SIDE_GRID2 = 1
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
            WRITE(F06,1908) TYPE, EID, SIDE_GRID1, SIDE_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1908
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = SIDE_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = SIDE_GRID2
            ENDIF
         ENDIF
      ENDIF

! Check that element is numered clockwise or counter clockwise. This can be skipped if DEBUG(194) = 1 or 3

      IF ((DEBUG(194) == 1) .OR. (DEBUG(194) == 3)) THEN

         DO I=1,3
            V12L(I) = XEL(2,I) - XEL(1,I)
            V23L(I) = XEL(3,I) - XEL(2,I)
            V34L(I) = XEL(4,I) - XEL(3,I)
            V41L(I) = XEL(1,I) - XEL(4,I)
         ENDDO 
 
         CALL CROSS ( V12L, V23L, K13VEC )
         CALL CROSS ( V23L, V34L, K24VEC )
         CALL CROSS ( V34L, V41L, K31VEC )
         CALL CROSS ( V41L, V12L, K42VEC )

         CW_ERR = 0
         IF      ((K13VEC(3) > ZERO) .AND. (K24VEC(3) > ZERO) .AND. (K31VEC(3) > ZERO) .AND. (K42VEC(3) > ZERO)) THEN
            CONTINUE
         ELSE IF ((K13VEC(3) < ZERO) .AND. (K24VEC(3) < ZERO) .AND. (K31VEC(3) < ZERO) .AND. (K42VEC(3) < ZERO)) THEN
            CONTINUE
         ELSE
            CW_ERR = CW_ERR + 1
         ENDIF

         IF (CW_ERR > 0) THEN
            QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            IF (WRT_ERR > 0) THEN
               WRITE(ERR,1910) TYPE, EID
               WRITE(F06,1910) TYPE, EID
            ELSE
               IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
                  ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
                  EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1910
               ENDIF
            ENDIF
         ENDIF

      ENDIF

! Calculate and check diagonal lengths

      DO I=1,3
         V13B(I) = XEB(3,I) - XEB(1,I)
         V24B(I) = XEB(4,I) - XEB(2,I)
      ENDDO 
      V13BM = DSQRT(V13B(1)*V13B(1) + V13B(2)*V13B(2) + V13B(3)*V13B(3))
      V24BM = DSQRT(V24B(1)*V24B(1) + V24B(2)*V24B(2) + V24B(3)*V24B(3))        
      IF ((DEBUG(6) == 1) .AND. (WRT_BUG(0) == 1)) THEN
         WRITE(BUG,*) '  V13BM, V24BM diagonal lengths (in mean plane):'
         WRITE(BUG,*) '  ----------------------------------------------'
         WRITE(BUG,90003) V13BM,V24BM
         WRITE(BUG,*)
      ENDIF
 
      IF (V13BM < EPS1) THEN
         QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         DIAG_GRID1 = 1
         DIAG_GRID2 = 3
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1909) TYPE, EID, DIAG_GRID1, DIAG_GRID2
            WRITE(F06,1909) TYPE, EID, DIAG_GRID1, DIAG_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1909
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = DIAG_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = DIAG_GRID2
            ENDIF
         ENDIF
      ENDIF

      IF (V24BM < EPS1) THEN
         QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         DIAG_GRID1 = 2
         DIAG_GRID2 = 4
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1909) TYPE, EID, DIAG_GRID1, DIAG_GRID2
            WRITE(F06,1909) TYPE, EID, DIAG_GRID1, DIAG_GRID2
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1909
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = DIAG_GRID1
               EMG_IFE(NUM_EMG_FATAL_ERRS,3) = DIAG_GRID2
            ENDIF
         ENDIF
      ENDIF
 
! Print warning if all points not in a plane by more than WARP_WARN. Use average diagonal length times EPS4 as measure.

      WARP_WARN = EPS4*(V13BM + V24BM)/TWO
      DHBAR = DABS(HBAR)
      IF (DHBAR > WARP_WARN) THEN
         WARN_ERR = WARN_ERR + 1
         IF ((WRT_ERR > 0) .AND. (WRITE_WARN == 'Y')) THEN
            WRITE(ERR,1915) TYPE, EID, DHBAR, WARP_WARN
            IF (SUPWARN == 'N') THEN
               WRITE(F06,1915) TYPE, EID, DHBAR, WARP_WARN
            ENDIF
         ELSE
            IF (WARN_ERR <= MEWE) THEN
               EMG_IWE(WARN_ERR,1) = 1915
               EMG_RWE(WARN_ERR,1) = DHBAR
               EMG_RWE(WARN_ERR,2) = WARP_WARN
            ENDIF
         ENDIF
      ENDIF

! Check interior angles. This can be skipped if DEBUG(194) = 2 or 3

      IF ((DEBUG(194) == 2) .OR. (DEBUG(194) == 3)) THEN

         IPNT = 0
         IF      (Y4  < ZERO) THEN
            IPNT = 1
         ELSE IF (Y3  < ZERO) THEN
            IPNT = 2
         ELSE IF (X14 < (X12 + X23*Y4/Y3)) THEN
            IPNT = 3
         ELSE IF (X13 > (Y3/Y4)*X14) THEN
            IPNT = 4
         ENDIF
         IF (IPNT > 0) THEN
            QUAD_GEOM_ERR = QUAD_GEOM_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            IF (WRT_ERR > 0) THEN
               WRITE(ERR,1914) TYPE, EID, IPNT
               WRITE(F06,1914) TYPE, EID, IPNT
            ELSE
               IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
                  ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
                  EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1914
                  EMG_IFE(NUM_EMG_FATAL_ERRS,2) = IPNT
               ENDIF
            ENDIF
         ENDIF

      ENDIF

      RETURN

! **********************************************************************************************************************************
 1908 FORMAT(' *ERROR  1908: ',A,' ELEMENT ',I8,' HAS LENGTH = ZERO ON THE SIDE THAT HAS GRIDS ',I2,' AND ',I2)

 1909 FORMAT(' *ERROR  1909: ',A,' ELEMENT ',I8,' HAS LENGTH = ZERO FOR THE DIAGONAL BETWEEN INTERNAL GRIDS ',I2,' AND ',I2)

 1910 FORMAT(' *ERROR  1910: ',A,' ELEMENT ',I8,' IS NOT NUMBERED IN A CLOCKWISE OR COUNTER CLOCKWISE MANNER')

 1914 FORMAT(' *ERROR  1914: ',A,' ELEMENT ',I8,' HAS INTERIOR ANGLE >= 180 DEG FOR INTERNAL GRID ',I2)

 1915 FORMAT(' *WARNING    : ',A,' ELEMENT ',I8,' IS NOT PLANAR. GRIDS ARE OUT OF PLANE BY +/-',1ES9.2,'. MAX SUGGESTED IS ',1ES9.2)

90001 FORMAT(3X,A,(1ES19.11))

90003 FORMAT(3(1ES14.6))

! **********************************************************************************************************************************

      END SUBROUTINE QUAD_GEOM_CHECK

! ##################################################################################################################################

      SUBROUTINE CALC_BMEANT

      IMPLICIT NONE

      REAL(DOUBLE)                    :: BMEAN(12,8)       ! Matrix that transforms the 8 nodal forces for an assumed flat element
!                                                            to the 12 forces for the actual warped element
      REAL(DOUBLE)                    :: DELTA1            ! sin(theta2 - quad_gamma) = sin(TH1 - GAM)
      REAL(DOUBLE)                    :: DELTA2            ! sin(theta2 + quad_gamma) = sin(TH1 + GAM)
      REAL(DOUBLE)                    :: SIN_TH1           ! sin(theta1), TH1 = interior angle of quad at node 1 
      REAL(DOUBLE)                    :: COS_TH1           ! cos(theta1) 
      REAL(DOUBLE)                    :: SIN_TH2           ! sin(theta2). TH2 = interior angle of quad at node 2 
      REAL(DOUBLE)                    :: COS_TH2           ! cos(theta2) 
      REAL(DOUBLE)                    :: SIN_GAM           ! sin(quad_gamma ). GAM = angle of side 3-4 from side 1-2 
      REAL(DOUBLE)                    :: COS_GAM           ! cos(quad_gamma ) 
      REAL(DOUBLE)                    :: CTN_TH1           ! cot(theta1) 
      REAL(DOUBLE)                    :: CTN_TH2           ! cot(theta2) 

! **********************************************************************************************************************************
      DO I=1,12
         DO J=1,8
            BMEAN(I,J) = ZERO
         ENDDO 
      ENDDO 

      SIN_TH1 =  Y4/L41
      COS_TH1 = -X14/L41

      SIN_TH2 =  Y3/L23
      COS_TH2 =  X23/L23

      SIN_GAM = (Y4 - Y3)/L34
      COS_GAM = (X3 - X4)/L34

      CTN_TH1 =  COS_TH1/SIN_TH1
      CTN_TH2 =  COS_TH2/SIN_TH2

      DELTA1 = SIN_TH2*COS_GAM - COS_TH2*SIN_GAM
      DELTA2 = SIN_TH1*COS_GAM + COS_TH1*SIN_GAM

      IF ((DEBUG(6) == 1) .AND. (WRT_BUG(0) == 1)) THEN
         NAME( 1) = 'SIN(THETA1)                     =  '  ;  VAR( 1) = SIN_TH1
         NAME( 2) = 'COS(THETA1)                     =  '  ;  VAR( 2) = COS_TH1
         NAME( 3) = 'SIN(THETA2)                     =  '  ;  VAR( 3) = SIN_TH2
         NAME( 4) = 'COS(THETA2)                     =  '  ;  VAR( 4) = COS_TH2
         NAME( 5) = 'SIN(QUAD_GAMMA)                 =  '  ;  VAR( 5) = SIN_GAM
         NAME( 6) = 'COS(QUAD_GAMMA)                 =  '  ;  VAR( 6) = COS_GAM
         NAME( 7) = 'CTN(THETA1)                     =  '  ;  VAR( 7) = CTN_TH1
         NAME( 8) = 'CTN(THETA2)                     =  '  ;  VAR( 8) = CTN_TH2
         NAME( 9) = 'DELTA1 = SIN(THETA2-QUAD_GAMMA) =  '  ;  VAR( 9) = DELTA1
         NAME(10) = 'DELTA2 = SIN(THETA1+QUAD_GAMMA) =  '  ;  VAR(10) = DELTA2
         WRITE(BUG,*) '  Variables used in creating BMEAN matrix:'
         WRITE(BUG,*) '  ---------------------------------------'
         DO I=1,10
            WRITE(BUG,90001) NAME(I),VAR(I)
         ENDDO
         WRITE(BUG,*)
      ENDIF
 
      BMEAN( 1,1) =  ONE
      BMEAN( 2,2) =  ONE
      BMEAN( 4,3) =  ONE
      BMEAN( 5,4) =  ONE
      BMEAN( 7,5) =  ONE
      BMEAN( 8,6) =  ONE
      BMEAN(10,7) =  ONE
      BMEAN(11,8) =  ONE
      BMEAN( 3,1) =  HBAR/L12
      BMEAN( 3,2) = -HBAR*(CTN_TH1/L12 - ONE/(L41*SIN_TH1))
      BMEAN( 3,3) = -BMEAN( 3,1)
      BMEAN( 3,4) = -HBAR*CTN_TH2/L12
      BMEAN( 3,7) = -HBAR*SIN_GAM/(L41*DELTA2)
      BMEAN( 3,8) = -HBAR*COS_GAM/(L41*DELTA2)
      
      BMEAN( 6,1) = -BMEAN( 3,1)
      BMEAN( 6,2) =  HBAR*CTN_TH1/L12
      BMEAN( 6,3) =  BMEAN( 3,1)
      BMEAN( 6,4) =  HBAR*(CTN_TH2/L12 - ONE/(L23*SIN_TH2))
      BMEAN( 6,5) =  HBAR*SIN_GAM/(L23*DELTA1)
      BMEAN( 6,6) =  HBAR*COS_GAM/(L23*DELTA1)
      
      BMEAN( 9,4) =  HBAR/(L23*SIN_TH2)
      BMEAN( 9,5) = -HBAR*(SIN_GAM/L23 + SIN_TH2/L34)/DELTA1
      BMEAN( 9,6) = -HBAR*(COS_GAM/L23 + COS_TH2/L34)/DELTA1
      BMEAN( 9,7) =  HBAR*SIN_TH1/(L34*DELTA2)
      BMEAN( 9,8) = -HBAR*COS_TH1/(L34*DELTA2)
      
      BMEAN(12,2) = -HBAR/(L41*SIN_TH1)
      BMEAN(12,5) =  HBAR*SIN_TH2/(L34*DELTA1)
      BMEAN(12,6) =  HBAR*COS_TH2/(L34*DELTA1)
      BMEAN(12,7) = -HBAR*(SIN_TH1/L34 - SIN_GAM/L41)/DELTA2
      BMEAN(12,8) =  HBAR*(COS_TH1/L34 + COS_GAM/L41)/DELTA2

      DO I=1,8
         DO J=1,12
            BMEANT(I,J) = BMEAN(J,I)
         ENDDO
      ENDDO

      RETURN

! **********************************************************************************************************************************
90001 FORMAT(3X,A,(1ES14.6))

91827 FORMAT(6(1ES15.6))

! **********************************************************************************************************************************

      END SUBROUTINE CALC_BMEANT

! **********************************************************************************************************************************

      END SUBROUTINE ELMGM2
