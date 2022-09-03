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

      SUBROUTINE ELMGM3 ( WRITE_WARN )
 
! Calculates and checks elem geometry for 3D elems and provides a transformation matrix ( TE ) to transfer the elem stiffness matrix
! in the elem system to the basic coordinate system. Calculates grid point coords in local coord system.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEFE, MELGP
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMGM3_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, TWO  
      USE PARAMS, ONLY                :  EPSIL, HEXAXIS
      USE MODEL_STUF, ONLY            :  EID, ELGP, EMG_IFE, ERR_SUB_NAM, HEXA_DELTA, HEXA_GAMMA, HEXA_THETA,                      &
                                         NUM_EMG_FATAL_ERRS, TE, TE_IDENT, TYPE, XEB, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE ELMGM3_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMGM3'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: SIDE_GRID1        ! Used for error output purposes
      INTEGER(LONG)                   :: SIDE_GRID2        ! Used for error output purposes
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: ID(3)             ! ID(i) is set to 1 if the i-th diagonal of TE is 1.0
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMGM3_BEGEND

      REAL(DOUBLE)                    :: EPS1              ! A small number to compare to real zero
      REAL(DOUBLE)                    :: IVEC(3)           ! A vector in the elem x dir
      REAL(DOUBLE)                    :: JVEC(3)           ! A vector in the elem y dir
      REAL(DOUBLE)                    :: HEXA_HBAR         ! Warp of HEXA element (only used in calc initial x direction along
!                                                            side 1-2 of the elem projection onto the mean plane)
      REAL(DOUBLE)                    :: KVEC(3)           ! A vector in the elem z dir
      REAL(DOUBLE)                    :: MAGI              ! Magnitude of vector IVEC
      REAL(DOUBLE)                    :: MAGJ              ! Magnitude of vector JVEC
      REAL(DOUBLE)                    :: MAGK              ! Magnitude of vector KVEC
      REAL(DOUBLE)                    :: CT_QD(3,3)        ! Coord transf matrix which will rotate a vector in a coord sys that has
!                                                            x axis along side 1-2 thru angle HEXA_DELTA to get a vector in the
!                                                            element local coord sys
      REAL(DOUBLE)                    :: TE_12(3,3)        ! TE matrix for this elem if local x is parallel to side 1-2
      REAL(DOUBLE)                    :: TE_SD(3,3)        ! TE matrix for this elem if local x splits angle between the two diags
      REAL(DOUBLE)                    :: XEBM(4,3)         ! Basic coordinates of 4 points that lie midway between the 4 grids
!                                                            1-4 and the 4 grids 5-8 of the element. Used to construct a mean plane.
!                                                            These will be referred to as points AM, BM, CM, DM
      REAL(DOUBLE)                    :: V12B(3)           ! Vector from G.P. 1 to G.P. 2 in basic coords
      REAL(DOUBLE)                    :: V13B(3)           ! Vector from G.P. 1 to G.P. 3 in basic coords (a diagonal)
      REAL(DOUBLE)                    :: V24B(3)           ! Vector from G.P. 2 to G.P. 4 in basic coords (a diagonal)
      REAL(DOUBLE)                    :: V13BM             ! Mag of V13B
      REAL(DOUBLE)                    :: V24BM             ! Mag of V24B
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Initialize XEL to zero
 
      DO I=1,MELGP
         DO J=1,3
            XEL(I,J) = ZERO
         ENDDO 
      ENDDO 

! Calc coords of a mean plane between grids 1-4 and grids 5-8. These will be referred to as points AM, BM, CM, DM:

      DO J=1,3
         XEBM(1,J) = (XEB(1,J) + XEB(5,J))/TWO
         XEBM(2,J) = (XEB(2,J) + XEB(6,J))/TWO
         XEBM(3,J) = (XEB(3,J) + XEB(7,J))/TWO
         XEBM(4,J) = (XEB(4,J) + XEB(8,J))/TWO
      ENDDO 

! **********************************************************************************************************************************
! Calculate elem z direction from cross products of diagonals
 
! Generate vectors from G.P AM to G.P CM and from G.P. BM to G.P. DM (diagonals in the mean (M) plane)
 
      DO I=1,3
         V13B(I) = XEBM(3,I) - XEBM(1,I)
         V24B(I) = XEBM(4,I) - XEBM(2,I)
      ENDDO 
 
      V13BM = DSQRT(V13B(1)*V13B(1) + V13B(2)*V13B(2) + V13B(3)*V13B(3))
      V24BM = DSQRT(V24B(1)*V24B(1) + V24B(2)*V24B(2) + V24B(3)*V24B(3))        

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
         KVEC(I)    = KVEC(I)/MAGK
         TE_12(3,I) = KVEC(I)
      ENDDO 
 
! **********************************************************************************************************************************
! Calc initial elem x dir along side 1-2 of the element. First, get vector from pt 1 to 2:

      DO I=1,3
         V12B(I) = XEB(2,I) - XEB(1,I)
      ENDDO 
 
! HEXA_HBAR is one half of the projection of V12B in z direction
 
      HEXA_HBAR = HALF*((V12B(1)*KVEC(1) + V12B(2)*KVEC(2) + V12B(3)*KVEC(3)))
 
! Now calculate initial x direction along side 1-2 of the elem projection onto the mean plane.
 
      DO I=1,3
         IVEC(I) = V12B(I) - TWO*HEXA_HBAR*KVEC(I)
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
! Calculate unit vector in initial elem. y dir. (from VZ (cross) VX):

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
         JVEC(I)     = JVEC(I)/MAGJ
         TE_12(2,I) = JVEC(I)
      ENDDO 

! **********************************************************************************************************************************
! Now TE_12 is for elem coord system with x along projected side 1-2. We need to rotate x-y (about z) to get x in a
! direction which splits the angle between the two diagonals. HEXA_THETA is the angle between side 1-2 and diagonal from
! point 1 to point 3. HEXA_GAMMA is the angle between side 1-2 and the diagonal from point 2 to point 4. The rotation
! about z is thru an angle of HEXA_DELTA = (HEXA_THETA - HEXA_GAMMA)/2.
 
! Find HEXA_THETA from the dot product of vector along side 1-2 and  diagonal from point 1 to point 3 (in the mean plane)
! Find HEXA_GAMMA from the dot product of vector along side 1-2 and  diagonal from point 2 to point 4 (in the mean plane).
! Use ABS to get the acute angle
 
      HEXA_THETA = DACOS(( V13B(1)*IVEC(1) + V13B(2)*IVEC(2) + V13B(3)*IVEC(3))/V13BM)
      HEXA_GAMMA = DACOS((-V24B(1)*IVEC(1) - V24B(2)*IVEC(2) - V24B(3)*IVEC(3))/V24BM)
      HEXA_DELTA = (HEXA_THETA - HEXA_GAMMA)/TWO

      CALL PLANE_COORD_TRANS_21 ( HEXA_DELTA, CT_QD, SUBR_NAME )
      CALL MATMULT_FFF ( CT_QD, TE_12, 3, 3, 3, TE_SD )

! Select how the local x axis is to be

      IF (HEXAXIS == 'SPLITD') THEN

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
 
      IF ((DEBUG(6) == 2) .AND. (WRT_BUG(0) == 1)) THEN

         WRITE(BUG,*) '  Coord transformation matrix that rotates a vector through angle HEXA_DELTA'
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

         IF (HEXAXIS == 'SPLITD') THEN
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

      DO I=2,ELGP
         DO J=1,3
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
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1908 FORMAT(' *ERROR  1908: ',A,' ELEMENT ',I8,' HAS LENGTH = ZERO ON THE SIDE THAT HAS INTERNAL GRIDS ',I2,' AND ',I2)

 1911 FORMAT(' *ERROR  1911: ELEMENT ',I8,' TYPE ',A,' HAS DIAG FROM INTERNAL GRIDS ',I2,' TO ',I2,                                &
                           ' PARALLEL TO DIAG FROM INTERNAL GRIDS ',I2,' TO ',I2)

 1912 FORMAT(' *ERROR  1912: CANNOT CALCULATE VECTOR IN ELEMENT Y DIRECTION FOR ELEMENT ',I8,' TYPE ',A)

 1913 FORMAT(' *ERROR  1913: BAD GEOMETRY FOR ELEMENT ',I8,' TYPE ',A,'. ELEMENT EITHER NOT NUMBERED CLOCKWISE/COUNTER CLOCKWISE.' &
                          ,' OR AN INTERNAL ANGLE IS > 180 DEG')

 1939 FORMAT(' *ERROR  1939: INTERNAL GRIDS 1 AND 2 ON ELEMENT ',I8,' TYPE ',A,' ARE COINCIDENT')

90003 FORMAT(3(1ES14.6))

! **********************************************************************************************************************************

      END SUBROUTINE ELMGM3
