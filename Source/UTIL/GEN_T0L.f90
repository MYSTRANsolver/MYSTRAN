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
 
      SUBROUTINE GEN_T0L (RGRID_ROW, ICORD, THETAD, PHID, T0L )
 
! Generates 3x3 transformation matrix, T0L, to basic (0) coordinate system from another coordinate system (L) with internal coord
! system number ICORD at the grid whose RGRID data is at row RGRID_ROW in array RGRID. Thus, if U0 is a basic system vector
! and UL is a vector in coord system L, then:
 
!                       U0 = T0L*UL                      (1)
 
! In the RCORD database, we have transformations, T0P, that transform a vector to basic (0) from the principal
! directions (P) of some other coordinate system. If the local (L) system is rectangular then T0P is T0L and all we
! have to do is get T0P from RCORD. However, if the local system is cylindrical or spherical, then we have to also
! transform from the point at R, THETA, Z (cylindrical) or R, THETA, PHI (spherical) to the respective principal axes
! of that cylindrical or spherical system. Thus, we can write eqn (1) as:
 
!                       U0 = T0P*UP                      (2)            
! and
!                       UP = TPL*UL                      (3)
 
! Substituting (3) into (2) and comparing with (1) it is seen that:
 
!                      T0L = T0P*TPL 
  
! For a rectangular system, TPL is the identity matrix.
 
! For a cylindrical system TPL is the matrix in the relationship:
 
!              |UPx|   |cos(THETA) -sin(THETA) 0 ||Ur    |
!              |UPy| = |sin(THETA)  cos(THETA) 0 ||Utheta|
!              |UPz|   |     0           0     1 ||Uz    |
 
! where UPx, UPy, UPz are the displacements in the principal directiona of the cylindrical system and Ur, Utheta, Uz
! are the cylindrical system displacements at some cordinate location R, THETA, Z in the cylindrical system.
 
! For a spherical system TPL is the matrix in the relationship:
 
!  |UPx|   |sin(THETA)*cos(PHI) cos(THETA)*cos(PHI) -sin(PHI) ||Ur    |
!  |UPy| = |sin(THETA)*sin(PHI) cos(THETA)*sin(PHI)  cos(PHI) ||Utheta|
!  |UPz|   |    cos(THETA)          -sin(THETA)         0     ||Uphi  |
 
! where UPx, UPy, UPz are the displacements in the principal directions of the spherical system and Ur, Utheta, Uphi
! are the spherical system displacements at some coordinate location R, THETA, PHI in the spherical system.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONE80, PI
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE PARAMS, ONLY                :  EPSIL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GEN_T0L_BEGEND
      USE MODEL_STUF, ONLY            :  RGRID, CORD, RCORD
 
      USE GEN_T0L_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GEN_T0L'

      INTEGER(LONG), INTENT(IN)       :: RGRID_ROW         ! Row number in array RGRID where the RGRID data is stored for the grid
!                                                            point whose coord transformation we seek. Since RGRID (at this point)
!                                                            is sorted in grid numerical order, RGRID_ROW will be the same as what
!                                                            we would get from subr GET_ARRAY_ROW_NUM with actual grid equal to
!                                                            the grid whose transformation we seek.
      INTEGER(LONG), INTENT(IN)       :: ICORD             ! Internal coord ID for coord sys L
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GEN_T0L_BEGEND
 
      REAL(DOUBLE),  INTENT(OUT)      :: THETAD,PHID       ! Azimuth and elevation angles (deg) for cylindrical/spherical coord sys 
      REAL(DOUBLE),  INTENT(OUT)      :: T0L(3,3)          ! 3 x 3 coord transformation matrix described above
      REAL(DOUBLE)                    :: CT,ST,CP,SP       ! SIN or COS of THETA or PHI
      REAL(DOUBLE)                    :: EPS1              ! EPSIL(1), a small number for comparison to zero
      REAL(DOUBLE)                    :: RAD               ! Rad from origin of sph or cyl coord. sys. ICORD to grid point RGRID_ROW
      REAL(DOUBLE)                    :: RADXY             ! Radius for calculating azimuth angle 
      REAL(DOUBLE)                    :: RDEG              ! Number of degrees in a radian 
      REAL(DOUBLE)                    :: THETA,PHI         ! Radian values for THETAD, PHID
      REAL(DOUBLE)                    :: T0P(3,3)          ! 3 x 3 transformation matrix described above
      REAL(DOUBLE)                    :: TPL(3,3)          ! 3 x 3 transformation matrix described above
      REAL(DOUBLE)                    :: XR0(3)            ! Array of relative coords of RGRID_ROW and origin of coord system ICORD
!                                                            in basic coords.
      REAL(DOUBLE)                    :: XRP(3)            ! Array of relative coords of RGRID_ROW and origin of coord system ICORD
!                                                            in local coord system L.

      INTRINSIC                       :: DASIN, DATAN2, DSIN, DCOS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      THETAD = ZERO
      PHID   = ZERO
 
      DO I=1,3
         DO J=1,3
            T0L(I,J) = ZERO
         ENDDO
      ENDDO

! If ICORD is rectangular, then no more transformation is needed. If it is cylindrical or spherical, more transformation is needed
! and depends on the location of the grid at RGRID_ROW and the location of the origin of system ICORD (both locations in basic sys).
! The location of the grid at RGRID_ROW in basic is in RGRID(RGRID_ROW,1-3). The location of the origin of system ICORD in the basic
! system is in RCORD (first 3 words of the row ICORD).
 
! Set elements of T0P to the coord. sys. transf. matrix found.
! If rectangular, we are finished.
 
      EPS1   = EPSIL(1)
      RDEG   = ONE80/PI

      DO I=1,3
         DO J=1,3
            K = 3 + 3*(I-1) + J
            T0P(I,J) = RCORD(ICORD,K)
         ENDDO
      ENDDO   
                                                           ! If 11 or 21 then rectangular
      IF ((CORD(ICORD,1) == 11) .OR. (CORD(ICORD,1) == 21)) THEN
         DO I=1,3
            DO J=1,3
               T0L(I,J) = T0P(I,J)
            ENDDO
         ENDDO
 
      ELSE                                                 ! Not Rectangular
 
! Get relative coordinates of RGRID_ROW and origin of ICORD in basic coordinate system.
 
         XR0(1) = RGRID(RGRID_ROW,1) - RCORD(ICORD,1)
         XR0(2) = RGRID(RGRID_ROW,2) - RCORD(ICORD,2)
         XR0(3) = RGRID(RGRID_ROW,3) - RCORD(ICORD,3)
 
! Transform relative coords from basic to the local coordinate system. Relative coords (3 of them) in local system are in
! XRP. Relative coords in basic system are in XR0. The transformation is: XRP = T0P(transpose)*XR0. (Note: XR0 = T0P*XRP)  
 
         CALL MATMULT_FFF_T ( T0P, XR0, 3, 3, 1, XRP )
 
! Initialize TPL
 
         DO I=1,3
            DO J=1,3
               TPL(I,J) = ZERO
            ENDDO
         ENDDO
 
! **********************************************************************************************************************************
! Calculate T0L = T0P*TPL for cylindrical and spherical systems. Note that if the radius to the grid point (from the
! coord system) is zero, we define THETA = 0
 
         IF (CORD(ICORD,1) == 22) THEN                     ! Cylindrical
            RAD = DSQRT(XRP(1)*XRP(1) + XRP(2)*XRP(2))
            IF (DABS(RAD) < EPS1) THEN
               DO I=1,3
                  DO J=1,3
                     T0L(I,J) = T0P(I,J)
                     tpl(i,j) = zero
                     tpl(i,i) = one
                  ENDDO
               ENDDO
            ELSE
               THETA    = DATAN2(XRP(2),XRP(1))
               THETAD   = RDEG*THETA
               CT       = DCOS(THETA)
               ST       = DSIN(THETA)
               TPL(1,1) =  CT
               TPL(1,2) = -ST
               TPL(1,3) =  ZERO
               TPL(2,1) =  ST
               TPL(2,2) =  CT
               TPL(2,3) =  ZERO
               TPL(3,1) =  ZERO
               TPL(3,2) =  ZERO
               TPL(3,3) =  ONE
               CALL MATMULT_FFF ( T0P, TPL, 3, 3, 3, T0L )
            ENDIF   
         ELSE IF (CORD(ICORD,1) == 23) THEN                ! Spherical
            RAD   = DSQRT(XRP(1)*XRP(1) + XRP(2)*XRP(2) + XRP(3)*XRP(3))
            RADXY = DSQRT(XRP(1)*XRP(1) + XRP(2)*XRP(2))
            IF ((DABS(RAD) < EPS1) .OR. (DABS(RADXY) < EPS1)) THEN
               THETA  = ZERO
               PHI    = ZERO
               CT     = ONE
               ST     = ZERO
               CP     = ONE
               SP     = ZERO
               DO I=1,3
                  DO J=1,3
                     T0L(I,J) = T0P(I,J)
                  ENDDO
               ENDDO
            ELSE
               THETA    =  DASIN(RADXY/RAD)
               PHI      =  DATAN2(XRP(2),XRP(1))
               CT       =  DCOS(THETA)
               ST       =  DSIN(THETA)
               CP       =  DCOS(PHI)
               SP       =  DSIN(PHI)
               PHID     =  RDEG*PHI
               THETAD   =  RDEG*THETA
               TPL(1,1) =  ST*CP
               TPL(1,2) =  CT*CP
               TPL(1,3) = -SP
               TPL(2,1) =  ST*SP
               TPL(2,2) =  CT*SP
               TPL(2,3) =  CP
               TPL(3,1) =  CT
               TPL(3,2) = -ST
               TPL(3,3) =  ZERO
               CALL MATMULT_FFF ( T0P, TPL, 3, 3, 3, T0L )
            ENDIF  
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

! **********************************************************************************************************************************
 
      END SUBROUTINE GEN_T0L
