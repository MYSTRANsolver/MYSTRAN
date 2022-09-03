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

      SUBROUTINE SHELL_ABD_MATRICES ( INT_ELEM_ID, WRITE_WARN )

! Generates shell force resultant vs strain matrices to be used in generating ME, KE, PTE, PPE, SEi for shell elements (plate
! elements whose properties are specified on a Bulk Data PSHELL or PCOMP entry). This subroutine is run under two circumstances:

!   1) To get integrated effect of all plies in a shell element (QUAD4 or TRIA3) used in development of the overall stiffness and
!      mass matrices for the element (NOTE: an element using PSHELL properties is considered as a 1 "ply" element)

!   2) To get the individual matrices for a single ply of the element used for stress/strain calcs for that ply

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC, MRMATLC, MPCOMP_PLIES, MPCOMP0, MRPCOMP_PLIES, MRPCOMP0, &
                                         WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, THIRD, HALF, THREE, TWELVE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL, IORQ1M, QUAD4TYP, PCOMPEQ, PCMPTSTM, SHRFXFAC, SUPWARN, TSTM_DEF

      USE MODEL_STUF, ONLY            :  ALPVEC, EB, EBM, EM, ET, EDAT, EID, EMAT, EPNT, EPROP, ETYPE, FAILURE_THEORY, FCONV,      &
                                         FCONV_SHEAR_THICK, INTL_MID, INTL_PID, MASS_PER_UNIT_AREA, MATL, MEPROP, MTRL_TYPE,       &
                                         NUM_EMG_FATAL_ERRS, NUM_PLIES, PLY_NUM, PCOMP, PCOMP_LAM, PCOMP_PROPS, RPCOMP, PSHEL,     &
                                         RPSHEL, RHO, RMATL, SHELL_A, SHELL_B, SHELL_D, SHELL_T, SHELL_AALP, SHELL_BALP,           &
                                         SHELL_DALP, SHELL_TALP, SHELL_T_MOD, THETA_PLY, TPLY, TYPE, ULT_STRE, ULT_STRN, ZPLY, ZS

      USE SUBR_BEGEND_LEVELS, ONLY    :  SHELL_ABD_MATRICES_BEGEND

      USE SHELL_ABD_MATRICES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SHELL_ABD_MATRICES'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

! Variables common to homogeneous and composite shell elements

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      INTEGER(LONG)                   :: I,J,K              ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHELL_ABD_MATRICES_BEGEND

      REAL(DOUBLE)                    :: DET_SHELL_T        ! Determinant of SHELL_T
      REAL(DOUBLE)                    :: EPS1               ! Small number with which to comapre zero
      REAL(DOUBLE)                    :: NSM                ! Nonstructural mass

! Variables for homogeneous shell elements

      REAL(DOUBLE)                    :: IB                 ! Bending moment of inertia
      REAL(DOUBLE)                    :: TM                 ! Membrane thickness
      REAL(DOUBLE)                    :: TS                 ! Shear thickness

! Variables for composite elements

      INTEGER(LONG)                   :: FT                 ! Failure theory (1=HILL, 2=HOFF, 3=TSAI, 4=STRN)
      INTEGER(LONG)                   :: JPLY               ! = either PLY_NUM or K in the DO loop over K=1,NUM_PLIES_TO_PROC
      INTEGER(LONG)                   :: MTRL_ACT_ID(4)     ! Material ID from MATi B.D. entries, in MATL(INTL_MID(I),1)
      INTEGER(LONG)                   :: NUM_PLIES_TO_PROC  ! = 1 if we are processing only 1 ply or NUM_PLIES if processing all
      INTEGER(LONG)                   :: PLY_PCOMP_INDEX    ! Index in array  PCOMP where data for ply K begins
      INTEGER(LONG)                   :: PLY_RPCOMP_INDEX   ! Index in array RPCOMP where data for ply K begins
      INTEGER(LONG)                   :: SOUTK              ! Stress or strain output request (1=YES or 0=NO) for ply K

      REAL(DOUBLE)                    :: ALPB(3)            ! The 3 rows of ALPVEC for mem/bend  strains
      REAL(DOUBLE)                    :: ALPD(3)            ! The 3 rows of ALPVEC for bending   strains
      REAL(DOUBLE)                    :: ALPM(3)            ! The 3 rows of ALPVEC for membrane  strains
      REAL(DOUBLE)                    :: ALPT(3)            ! The 3 rows of ALPVEC for trans shr strains
      REAL(DOUBLE)                    :: BALP(3)            ! Intermediate matrix
      REAL(DOUBLE)                    :: DALP(3)            ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM(3)             ! Intermediate matrix
      REAL(DOUBLE)                    :: AALP(3)            ! Intermediate matrix
      REAL(DOUBLE)                    :: TALP(3)            ! Intermediate matrix
      REAL(DOUBLE)                    :: PCOMP_TM           ! Membrane thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE)                    :: PCOMP_IB           ! Bending MOI of PCOMP for equivalent PSHELL
      REAL(DOUBLE)                    :: PCOMP_TS           ! Transverse shear thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE)                    :: PLY_A(3,3)         ! Transformed material matrix A for a ply 
      REAL(DOUBLE)                    :: PLY_B(3,3)         ! Transformed material matrix B for a ply 
      REAL(DOUBLE)                    :: PLY_D(3,3)         ! Transformed material matrix D for a ply 
      REAL(DOUBLE)                    :: PLY_T(2,2)         ! Transformed material matrix T for a ply 
      REAL(DOUBLE)                    :: SB                 ! Allowable interlaminar shear stress. Required if FT is specified
      REAL(DOUBLE)                    :: TREFK              ! Ref temperature for ply K
      REAL(DOUBLE)                    :: Z0                 ! Coord from ref plane to bottom surface of element
      REAL(DOUBLE)                    :: ZBK,ZTK            ! Coord from ref plane to bot and top of ply K
      REAL(DOUBLE)                    :: ZBK2,ZTK2          ! ZBK^2, ZTK^2
      REAL(DOUBLE)                    :: ZBK3,ZTK3          ! ZBK^3, ZTK^3

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

      TYPE  = ETYPE(INT_ELEM_ID)

      IF ((TYPE(1:5) /= 'TRIA3') .AND. (TYPE(1:5) /= 'QUAD4') .AND. (TYPE(1:5) /= 'SHEAR')) THEN
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1946) TYPE, SUBR_NAME
         WRITE(F06,1946) TYPE, SUBR_NAME
         RETURN
      ENDIF
      
      CALL IS_ELEM_PCOMP_PROPS ( INT_ELEM_ID )

      SHELL_T_MOD = 'N'                                    ! Reset this to N every time this subr is called

! ---------------------------------------------------------------------------------------------------------------------------------
pcom0:IF (PCOMP_PROPS == 'N') THEN                         ! Element is not a composite - uses PSHELL
!                                                            NOTE: MATERIAL_PROPS_2D is called in procedure EMG for these
         IF (TYPE == 'SHEAR   ') THEN
            TM       = EPROP(1)
            IB       = ZERO
            TS       = TM
            NSM      = EPROP(2)
            ZS(1)    = ZERO
            ZS(2)    = ZERO
            FCONV(1) = TM
            FCONV(2) = ZERO
            FCONV(3) = ZERO
         ELSE
            TM                =  EPROP(1)
            IB                =  EPROP(2)*TM*TM*TM/TWELVE
            TS                =  EPROP(3)*TM
            NSM               =  EPROP(4)
            ZS(1)             =  EPROP(5)
            ZS(2)             =  EPROP(6)
            FCONV(1)          =  TM
            FCONV(2)          = -IB                        ! Note neq sign on FCONV(2): due to sign convention on positive bending
            FCONV_SHEAR_THICK =  TS
         ENDIF

         IF ((TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'TRIA3')) THEN 
            MASS_PER_UNIT_AREA = (RHO(1)*TM + NSM)
         ENDIF

         DO I=1,3
            DO J=1,3
               SHELL_A(I,J) = ZERO
               SHELL_D(I,J) = ZERO
               SHELL_B(I,J) = ZERO
            ENDDO
         ENDDO
         IF (TYPE == 'SHEAR   ') THEN
            SHELL_A(3,3) = TM*EM(3,3)                      ! For SHEAR, all but SHELL_A(3,3) are 0.
         ELSE
            DO I=1,3
               DO J=1,3
                  SHELL_A(I,J) = TM*EM(I,J)                ! Units are force/distance
                  SHELL_D(I,J) = IB*EB(I,J)                ! Units are force*distance^2
                  SHELL_B(I,J) = TM*TM*EBM(I,J)            ! Units are force
               ENDDO
            ENDDO
         ENDIF

         DO I=1,2
            DO J=1,2
               SHELL_T(I,J) = TS*ET(I,J)                   ! Units are force/distance
            ENDDO
         ENDDO

         DO I=1,3
            ALPM(I) = ALPVEC(I,1)
            ALPB(I) = ALPVEC(I,2)
         ENDDO
 
         CALL MATMULT_FFF ( EB, ALPB, 3, 3, 1, DUM )
         DO I=1,3
            SHELL_DALP(I) = IB*DUM(I)
         ENDDO
 
         CALL MATMULT_FFF ( EM, ALPM, 3, 3, 1, DUM )
         DO I=1,3
            SHELL_AALP(I) = TM*DUM(I)
         ENDDO

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE pcom0                                           ! This element is a composite with properties defined on PCOMP
!                                                            NOTE: subr MATERIAL_PROPS_2D called here for these
         DO I=1,MEPROP
            EPROP(I) = ZERO
         ENDDO

         DO I=1,3
            ALPB(I) = ZERO
            ALPD(I) = ZERO
            ALPM(I) = ZERO
            ALPT(I) = ZERO
            AALP(I) = ZERO
            BALP(I) = ZERO
            DALP(I) = ZERO
            TALP(I) = ZERO
         ENDDO

         EPROP(4) =  RPCOMP(INTL_PID,1)    ;    Z0    = EPROP(4)
         EPROP(5) =  RPCOMP(INTL_PID,2)    ;    NSM   = EPROP(5)
         EPROP(6) =  RPCOMP(INTL_PID,3)    ;    SB    = EPROP(6)
         EPROP(7) =  RPCOMP(INTL_PID,4)    ;    TREFK = EPROP(7)
         EPROP(8) =  Z0                    ;    ZS(1) = EPROP(8) 
         EPROP(9) = -Z0                    ;    ZS(2) = EPROP(9)

         FT = PCOMP(INTL_PID,3)
         IF      (FT == 0) THEN
            FAILURE_THEORY = 'NONE'
         ELSE IF (FT == 1) THEN
            FAILURE_THEORY = 'HILL'
         ELSE IF (FT == 2) THEN
            FAILURE_THEORY = 'HOFF'
         ELSE IF (FT == 3) THEN
            FAILURE_THEORY = 'TSAI'
         ELSE IF (FT == 4) THEN
            FAILURE_THEORY = 'STRE'
         ELSE IF (FT == 5) THEN
            FAILURE_THEORY = 'STRN'
         ENDIF

         IF (PCOMP(INTL_PID,4) == 1) THEN                  ! Check whether elem is sym or nonsym layuo
            PCOMP_LAM = 'SYM'
         ELSE
            PCOMP_LAM = 'NON'                              ! If nonsym layup, make sure int order = 2 (BIG_BB, BIG_BM for QUAD)
            IF ((TYPE(1:6) == 'QUAD4 ') .AND. (QUAD4TYP == 'MIN4T')) THEN
               IF (IORQ1M /= 2) THEN
                  WARN_ERR = WARN_ERR + 1
                  WRITE(ERR, 1948) 'IORQ1M', IORQ1M
                  IF (SUPWARN == 'N') THEN
                     WRITE(F06, 1948) 'IORQ1M', IORQ1M
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         DO I=1,MEMATC
            INTL_MID(I) = 0
         ENDDO

         CALL GET_ELEM_NUM_PLIES ( INT_ELEM_ID )

         IF (WRT_BUG(1) > 0) THEN
            CALL BUG_SHELL_ABD_MATRICES ( 0, 20 )
         ENDIF

         DO I=1,3
            DO J=1,3
               SHELL_A(I,J) = ZERO
               SHELL_B(I,J) = ZERO
               SHELL_D(I,J) = ZERO
            ENDDO
         ENDDO

         DO I=1,2
            DO J=1,2
               SHELL_T(I,J) = ZERO
            ENDDO
         ENDDO

         DO I=1,3
            SHELL_AALP(I) = ZERO
            SHELL_BALP(I) = ZERO
            SHELL_DALP(I) = ZERO
         ENDDO

         DO I=1,2
            SHELL_TALP(I) = ZERO
         ENDDO

         MASS_PER_UNIT_AREA = ZERO
         MASS_PER_UNIT_AREA = NSM
         ZBK = Z0
         PCOMP_TM = ZERO
         PCOMP_IB = ZERO
         PCOMP_TS = ZERO

         IF (PLY_NUM == 0) THEN                            ! PLY_NUM = 0 means we want integrated effect of all plies
            NUM_PLIES_TO_PROC = NUM_PLIES
         ELSE                                              ! PLY_NUM > 0 means process only ply number PLY_NUM
            NUM_PLIES_TO_PROC = 1
         ENDIF

ply_do:  DO K=1,NUM_PLIES_TO_PROC

            IF (PLY_NUM == 0) THEN
               JPLY = K
            ELSE
               JPLY = PLY_NUM
            ENDIF
                                                           ! Indices in PCOMP, RPCOMP arrays where ply K data begins
            PLY_PCOMP_INDEX  = MPCOMP0  + MPCOMP_PLIES*(JPLY - 1)  + 1
            PLY_RPCOMP_INDEX = MRPCOMP0 + MRPCOMP_PLIES*(JPLY - 1) + 1

            INTL_MID(1) = PCOMP(INTL_PID,PLY_PCOMP_INDEX)  ! Shell A, D, T use membrane material props

            INTL_MID(2) = INTL_MID(1)                      ! Need to have INTL_MID(2) nonzero so that subr QDEL1,TREL1
!                                                            will call subr to calc bending stiffness. Also SEi depends on EB 

            INTL_MID(3) = INTL_MID(1)                      ! Shell T uses transverse shear material props unless material matrix
!                                                            calcuated below has zero transverse shear modulus in which case
!                                                            INTL_MID(3) will be reset to 0

            INTL_MID(4) = INTL_MID(1)                      ! Bending/membrane coupling

            SOUTK     =  PCOMP(INTL_PID,PLY_PCOMP_INDEX+1)
            TPLY      = RPCOMP(INTL_PID,PLY_RPCOMP_INDEX)  ! Ply thickness
            THETA_PLY = RPCOMP(INTL_PID,PLY_RPCOMP_INDEX+1)! Ply angle from elem material axis to ply longitudinal axis
            ZPLY      = RPCOMP(INTL_PID,PLY_RPCOMP_INDEX+2)! Coord of mid plane of ply relative to mid plane of elem

            IF      ((TYPE == 'QDMEM   ') .OR. (TYPE == 'QUAD4K  ') .OR. (TYPE == 'QUAD4   ') .OR.                                 &
                     (TYPE == 'TRMEM   ') .OR. (TYPE == 'TRIA3K  ') .OR. (TYPE == 'TRIA3   ')) THEN 
               MASS_PER_UNIT_AREA = MASS_PER_UNIT_AREA + (RHO(1)*TPLY)
            ELSE IF ((TYPE == 'QDPLT1  ') .OR. (TYPE == 'QDPLT2  ') .OR.                                                           &
                     (TYPE == 'TRPLT1  ') .OR. (TYPE == 'TRPLT2  ')) THEN
               MASS_PER_UNIT_AREA = MASS_PER_UNIT_AREA
            ENDIF

            IF (INTL_MID(1) /= 0) THEN
               MTRL_ACT_ID(1) = MATL(INTL_MID(1),1)
               MTRL_TYPE(1)   = MATL(INTL_MID(1),2)
            ENDIF

            IF (INTL_MID(2) /= 0) THEN
               MTRL_ACT_ID(2) = MATL(INTL_MID(2),1)
               MTRL_TYPE(2)   = MATL(INTL_MID(2),2)
            ENDIF

            IF (INTL_MID(3) /= 0) THEN
               MTRL_ACT_ID(3) = MATL(INTL_MID(1),1)
               MTRL_TYPE(3)   = MATL(INTL_MID(3),2)
            ENDIF

            DO I=1,MRMATLC
               DO J=1,MEMATC
                  IF (INTL_MID(J) /= 0) THEN
                     EMAT(I,J) = RMATL(INTL_MID(J),I)
                  ENDIF
               ENDDO 
            ENDDO

            IF (PCOMP(INTL_PID,2) == 0) THEN               ! Props were defined on a PCOMP entry so we need to modify TREF to be
               DO J=1,MEMATC                               ! from array PCOMP rather than from RMATL. OK if defined on PCOMP1
                  IF      (MTRL_TYPE(J) == 1) THEN
                     EMAT( 6,J) = RPCOMP(INTL_PID,4)
                  ELSE IF (MTRL_TYPE(J) == 2) THEN
                     EMAT(11,J) = RPCOMP(INTL_PID,4)
                  ELSE IF (MTRL_TYPE(J) == 8) THEN
                     EMAT(10,J) = RPCOMP(INTL_PID,4)
                  ENDIF
               ENDDO
            ENDIF
 
            EMAT(MRMATLC+1,3) = SB                         ! SB is the allowable interlaminar shear stress (on PCOMP B.D. entry)
            EMAT(MRMATLC+2,3) = SB
            CALL MATERIAL_PROPS_2D ( WRITE_WARN )
                                                           ! Reset INTL_MID(3) if either transverse shear modulii are zero
            IF ((DABS(ET(1,1)) < EPS1) .OR. (DABS(ET(2,2)) < EPS1)) THEN
               INTL_MID(3) = 0
            ENDIF


            IF (WRT_BUG(1) > 0) THEN
               CALL BUG_SHELL_ABD_MATRICES ( K, 21 )
            ENDIF

            CALL ROT_COMP_ELEM_AXES ( K, THETA_PLY, '1-2' )

            ZTK = ZBK + TPLY    ;    ZTK2 = ZTK*ZTK    ;    ZTK3 = ZTK2*ZTK    ;    ZBK2 = ZBK*ZBK    ;    ZBK3 = ZBK2*ZBK

            PCOMP_TM = PCOMP_TM +       (ZTK  - ZBK )
            PCOMP_IB = PCOMP_IB + THIRD*(ZTK3 - ZBK3)
                                                           ! PCMPTSTM is a factor (< 1) for shear to total plate thickness for PCOMP
            PCOMP_TS = PCOMP_TS +       (ZTK  - ZBK )*PCMPTSTM
 
            IF (TYPE == 'SHEAR   ') THEN                   ! For SHEAR elem there is only SHELL_A and only its 3,3 term is nonzreo
               DO I=1,3
                  DO J=1,3
                     SHELL_A(I,J) = ZERO
                     SHELL_B(I,J) = ZERO
                     SHELL_D(I,J) = ZERO
                  ENDDO
               ENDDO
               SHELL_A(3,3) = SHELL_A(3,3) + PLY_A(3,3)
            ELSE
               DO I=1,3
                  DO J=1,3
                     IF (PLY_NUM == 0) THEN                ! Use the following when all plies are to be integrated
                        PLY_A(I,J)   =       (ZTK  - ZBK )*EM(I,J)
                        PLY_B(I,J)   =  HALF*(ZTK2 - ZBK2)*EM(I,J)
                        PLY_D(I,J)   = THIRD*(ZTK3 - ZBK3)*EM(I,J)
                     ELSE                                  ! Use the following when only individual plies are evaluated separately
                        PLY_A(I,J)   = TPLY*EM(I,J)
                        PLY_B(I,J)   = ZERO
                        PLY_D(I,J)   = (TPLY*TPLY*TPLY)*EM(I,J)/TWELVE
                     ENDIF
                     SHELL_A(I,J) = SHELL_A(I,J) + PLY_A(I,J)
                     SHELL_B(I,J) = SHELL_B(I,J) + PLY_B(I,J)
                     SHELL_D(I,J) = SHELL_D(I,J) + PLY_D(I,J)
                  ENDDO
               ENDDO
            ENDIF

            DO I=1,2
               DO J=1,2
                  PLY_T(I,J)   = (ZTK - ZBK )*ET(I,J)*PCMPTSTM
                  SHELL_T(I,J) = SHELL_T(I,J) + PLY_T(I,J)
               ENDDO
            ENDDO

            DO I=1,3
               ALPM(I) = ALPVEC(I,1)
               ALPD(I) = ALPVEC(I,2)
               ALPB(I) = ALPVEC(I,4)
            ENDDO

            DO I=1,2
               ALPT(I) = ALPVEC(I,3)
            ENDDO

            CALL MATMULT_FFF ( PLY_A, ALPM, 3, 3, 1, AALP )
            CALL MATMULT_FFF ( PLY_B, ALPB, 3, 3, 1, BALP )
            CALL MATMULT_FFF ( PLY_D, ALPD, 3, 3, 1, DALP )
            CALL MATMULT_FFF ( PLY_T, ALPT, 2, 2, 1, TALP )
            DO I=1,3
               SHELL_AALP(I) = SHELL_AALP(I) + AALP(I)
               SHELL_BALP(I) = SHELL_BALP(I) + BALP(I)
               SHELL_DALP(I) = SHELL_DALP(I) + DALP(I)
            ENDDO
            DO I=1,2
               SHELL_TALP(I) = SHELL_TALP(I) + TALP(I)
            ENDDO

            IF (WRT_BUG(1) > 0) THEN
               CALL BUG_SHELL_ABD_MATRICES ( K, 22 )
            ENDIF

            ZBK = ZTK

         ENDDO ply_do

! Now put PCOMP_TM, PCOMP_IB and PCOMP_TS into array EPROP, but put them in similar to the EPROP for PSHELL properties

         EPROP(1) = PCOMP_TM                               ! PCOMP_TM should be > 0 based on checks in subr BD_PCOMP
         EPROP(2) = TWELVE*PCOMP_IB/(PCOMP_TM*PCOMP_TM*PCOMP_TM)
         EPROP(3) = PCOMP_TS/PCOMP_TM

         IF (WRT_BUG(1) > 0) THEN  
            CALL BUG_SHELL_ABD_MATRICES ( 0, 23 )
         ENDIF

         FCONV(1)  =  PCOMP_TM
         FCONV(2)  = -PCOMP_IB                             ! Note neq sign on FCONV(2): due to sign convention on positive bending
         FCONV_SHEAR_THICK = PCOMP_TS
                                                           ! Write equiv PSHELL, MAT2, if not already written for this PCOMP
         IF ((PCOMPEQ > 0) .AND. (PCOMP(INTL_PID,6) == 0)) THEN
            IF ((PCOMP_TM > EPS1) .AND. (PCOMP_IB > EPS1) .AND. (PCOMP_TS > EPS1)) THEN
               CALL WRITE_PCOMP_EQUIV ( PCOMP_TM, PCOMP_IB, PCOMP_TS )
            ELSE
               WRITE(ERR,9800) PCOMP(INTL_PID,1), PCOMP_TM, PCOMP_IB, PCOMP_TS 
               WRITE(F06,9800) PCOMP(INTL_PID,1), PCOMP_TM, PCOMP_IB, PCOMP_TS 
            ENDIF
         ENDIF  

      ENDIF pcom0

! Reset SHELL_T if singular so that we can calc finite KS shear stiffness in subrs QPLT2, TPLT2.

      DET_SHELL_T = SHELL_T(1,1)*SHELL_T(2,2) - SHELL_T(1,2)*SHELL_T(2,1)
      IF (DABS(DET_SHELL_T) < EPS1) THEN
         SHELL_T(1,1) = HALF*SHRFXFAC*( SHELL_A(1,1) + SHELL_A(2,2) )
         SHELL_T(2,2) = SHELL_T(1,1)
         SHELL_T(1,2) = ZERO
         SHELL_T(2,1) = ZERO
         WRITE(ERR,9801) EID, SHELL_T(1,1), SHELL_T(2,2)
!        WRITE(F06,9801) EID, SHELL_T(1,1), SHELL_T(2,2)
         SHELL_T_MOD = 'Y' 
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1946 FORMAT(' *ERROR  1946: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ELEMENT TYPE ',A,' IS NOT ONE OF THE ELEMENT TYPES PROCESSED IN SUBR ',A)

 1948 FORMAT(' *ERROR  1948: ',A,I8,' MUST HAVE GAUSSIAN INTEGRATION ORDERS = 2 FOR PCOMP ELEMENTS WITH SYM LAYUP.'                &
                      ,/,14X,' HOWEVER, BULK DATA PARAM ',A,' = ',I3)
 9800 FORMAT(' *INFORMATION: CANNOT OUTPUT EQUIVALENT PSHELL AND MAT2 ENTRIES FOR PCOMP ',I8,' SINCE ONE OR MORE OF THE EQUIV',    &
                           ' PROPERTIES IS ZERO:'                                                                                  &
                    ,/,14X,'    (1) EQUIVALENT MEMBRANE THICKNESS = ',1ES10.3                                                      &
                    ,/,14X,'    (2) EQUIVALENT BENDING INERTIA    = ',1ES10.3                                                      &
                    ,/,14X,'    (3) EQUIVALENT SHEAR THICKNESS    = ',1ES10.3,/)

 9801 FORMAT(' *INFORMATION: ARRAY SHELL_T USED IN FORMULATING TRANSVERSE SHEAR STIFFNESS FOR ELEMENT ', I8,' HAS BEEN RESET DUE', &
             ' TO SINGULARITY.',/, 14X,' THIS RESET COULD HAVE BEEN AVOIDED BY SPECIFYING TRANSVERSE SHEAR MATERIAL PROPERTIES',   &
             ' FOR THIS ELEMENT.',/,14X,' THE RESET VALUES FOR THE DIAGONALS OF THE 2 BY 2 SHELL_T',     &
             ' MATRIX FOR THIS ELEMENT ARE:', 2(1ES14.6),/,14X,' IN AN ATTEMPT TO SIMULATE ZERO TRANSVERSE SHEAR FLEXIBILITY'/)
             


49832 format(' In SHELL_ABD_MATRICES: TM, IB, TS, FCONV(1), FCONV(2) = ',5(1es14.6))













91304 FORMAT('   SHELL_D row',I2,' = ',3(1ES14.6),'  bending              ')



! ##################################################################################################################################
 
      CONTAINS

! ##################################################################################################################################

      SUBROUTINE BUG_SHELL_ABD_MATRICES ( JPLY, WHAT )

      USE MODEL_STUF, ONLY            :  T1M, T1T

      IMPLICIT NONE

      CHARACTER( 8*BYTE)              :: LAM = '        '  ! Field 9 of parent entry
      CHARACTER(4*BYTE)               :: MTRL_NAME         ! Material name (MAT1, etc)

      INTEGER(LONG), INTENT(IN)       :: JPLY              ! Integer ply number
      INTEGER(LONG)                   :: IIROW,IICOL       ! 
      INTEGER(LONG)                   :: J1,J2             ! Counters
      INTEGER(LONG)                   :: PCOMP_PLIES       ! Number of plies in 1 PCOMP entry incl sym plies not explicitly defined
      INTEGER(LONG), INTENT(IN)       :: WHAT              ! Which block of code to write

! **********************************************************************************************************************************
      IF      (WHAT == 13) THEN

         WRITE(BUG,91301)
 
         DO I=1,3                                         ! Write final result, SHELL_A
            WRITE(BUG,91302) I, (SHELL_A(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         DO I=1,3                                         ! Write final result, SHELL_B
            WRITE(BUG,91303) I, (SHELL_B(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         DO I=1,3                                         ! Write final result, SHELL_D
            WRITE(BUG,91304) I, (SHELL_D(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         DO I=1,2                                         ! Write final result, SHELL_T
            WRITE(BUG,91305) I, (SHELL_T(I,J),J=1,2)
         ENDDO
         WRITE(BUG,*)
 
      ELSE IF (WHAT == 20) THEN

         PCOMP_PLIES = PCOMP(INTL_PID,5)
         WRITE(BUG,92001) EID, PCOMP(INTL_PID,1), PCOMP_PLIES, PCOMP_PLIES, PCOMP(INTL_PID,1)

         WRITE(BUG,92003) 'ZO        = ', RPCOMP(INTL_PID,1),'  z coord of bottom of ply 1'
         WRITE(BUG,92003) 'NSM       = ', RPCOMP(INTL_PID,2),'  nonstructural mass'
         WRITE(BUG,92003) 'SB        = ', RPCOMP(INTL_PID,3),'  interlaminar allowable shear stress'

         IF      (FAILURE_THEORY == 'NONE') THEN
            WRITE(BUG,92002) 'FT        = ', FT, '  no failure theory specified'
         ELSE IF (FAILURE_THEORY == 'HILL') THEN
            WRITE(BUG,92002) 'FT        = ', FT, '  use HILL failure theory'
         ELSE IF (FAILURE_THEORY == 'HOFF') THEN
            WRITE(BUG,92002) 'FT        = ', FT, '  use HOFF failure theory'
         ELSE IF (FAILURE_THEORY == 'TSAI') THEN
            WRITE(BUG,92002) 'FT        = ', FT, '  use TSAI failure theory'
         ELSE IF (FAILURE_THEORY == 'STRN') THEN
            WRITE(BUG,92002) 'FT        = ', FT, '  use STRN (max strain) failure theory'
         ELSE IF (FAILURE_THEORY == 'STRE') THEN
            WRITE(BUG,92002) 'FT        = ', FT, '  use STRE (max stress) failure theory'
         ENDIF

         WRITE(BUG,92003) 'TREF      = ', RPCOMP(INTL_PID,4),'  reference temperature'
         WRITE(BUG,92003) 'GE        = ', RPCOMP(INTL_PID,5),'  damping coefficient'

         IF      (PCOMP(INTL_PID,4) == 0) THEN
            LAM(1:3) = 'NON'
            WRITE(BUG,92002) 'LAM       = ', PCOMP(INTL_PID,4),'  laminate is not a symmetric layup'
         ELSE IF (PCOMP(INTL_PID,4) == 1) THEN
            LAM(1:3) = 'SYM'
            WRITE(BUG,92002) 'LAM       = ', PCOMP(INTL_PID,4),'  laminate is a symmetric layup'
         ENDIF

         WRITE(BUG,*)
         WRITE(BUG,*)

         IF (LAM(1:3) == 'NON') THEN

            WRITE(BUG,92004)
            WRITE(BUG,92006)
            DO J=1,PCOMP_PLIES
               IICOL = MPCOMP0  + MPCOMP_PLIES*(J - 1)  + 1
               IIROW = PCOMP(INTL_PID,IICOL)
               J1 = MPCOMP0 + MPCOMP_PLIES*(J-1) + 1
               J2 = MRPCOMP0 + MRPCOMP_PLIES*(J-1) + 1
               IF (PCOMP(INTL_PID,J1+1) == 0) THEN
                  WRITE(BUG,92008) J, MATL(IIROW,1), RPCOMP(INTL_PID,J2), RPCOMP(INTL_PID,J2+1), '0 (NO )', RPCOMP(INTL_PID,J2+2) 
               ELSE
                  WRITE(BUG,92008) J, MATL(IIROW,1), RPCOMP(INTL_PID,J2), RPCOMP(INTL_PID,J2+1), '0 (YES)', RPCOMP(INTL_PID,J2+2) 
               ENDIF
            ENDDO
            WRITE(BUG,92007) RPCOMP(INTL_PID,6) 
            WRITE(BUG,*)

         ELSE

            WRITE(BUG,92005)
            WRITE(BUG,92006)
            DO J=1,PCOMP_PLIES/2
               IICOL = MPCOMP0  + MPCOMP_PLIES*(J - 1)  + 1
               IIROW = PCOMP(INTL_PID,IICOL)
               J1 = MPCOMP0 + MPCOMP_PLIES*(J-1) + 1
               J2 = MRPCOMP0 + MRPCOMP_PLIES*(J-1) + 1
               IF (PCOMP(INTL_PID,J1+1) == 0) THEN
                  WRITE(BUG,92008) J, MATL(IIROW,1), RPCOMP(INTL_PID,J2), RPCOMP(INTL_PID,J2+1), '0 (NO )', RPCOMP(INTL_PID,J2+2) 
               ELSE
                  WRITE(BUG,92008) J, MATL(IIROW,1), RPCOMP(INTL_PID,J2), RPCOMP(INTL_PID,J2+1), '0 (YES)', RPCOMP(INTL_PID,J2+2) 
               ENDIF
            ENDDO
            WRITE(BUG,*)

            DO J=PCOMP_PLIES/2 + 1,PCOMP_PLIES
               IICOL = MPCOMP0  + MPCOMP_PLIES*(J - 1)  + 1
               IIROW = PCOMP(INTL_PID,IICOL)
               J1 = MPCOMP0 + MPCOMP_PLIES*(J-1) + 1
               J2 = MRPCOMP0 + MRPCOMP_PLIES*(J-1) + 1
               IF (PCOMP(INTL_PID,J1+1) == 0) THEN
                  WRITE(BUG,92008) J, MATL(IIROW,1), RPCOMP(INTL_PID,J2), RPCOMP(INTL_PID,J2+1), '0 (NO )', RPCOMP(INTL_PID,J2+2) 
               ELSE
                  WRITE(BUG,92008) J, MATL(IIROW,1), RPCOMP(INTL_PID,J2), RPCOMP(INTL_PID,J2+1), '0 (YES)', RPCOMP(INTL_PID,J2+2) 
               ENDIF
            ENDDO
            WRITE(BUG,92007) RPCOMP(INTL_PID,6) 
            WRITE(BUG,*)

         ENDIF

      ELSE IF (WHAT == 21) THEN

         MTRL_NAME = '****'
         IF      (MTRL_TYPE(1) == 1) THEN
            MTRL_NAME = 'MAT1'
         ELSE IF (MTRL_TYPE(1) == 2) THEN
            MTRL_NAME = 'MAT2'
         ELSE IF (MTRL_TYPE(1) == 8) THEN
            MTRL_NAME = 'MAT8'
         ENDIF

         WRITE(BUG,92101) JPLY, TPLY, THETA_PLY, MTRL_NAME, MTRL_ACT_ID(1), INTL_MID(1)
         WRITE(BUG,92102)
         DO I=1,2                                          ! Write EM, ET ORIG material matrices
            WRITE(BUG,92103) I, (EM(I,J),J=1,3), I, (ET(I,J),J=1,2)
         ENDDO
         DO I=3,3
            WRITE(BUG,92104) I, (EM(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         WRITE(BUG,92105)                                  ! Write stress/strain allowables
         WRITE(BUG,92106) ULT_STRE(1,1), ULT_STRE(2,1), ULT_STRE(7,1), ULT_STRE(8,3), ULT_STRE(9,3)
         WRITE(BUG,92107) ULT_STRE(3,1), ULT_STRE(4,1), ULT_STRE(7,1)
         WRITE(BUG,*)
         WRITE(BUG,92108) ULT_STRN(1,1), ULT_STRN(2,1), ULT_STRN(7,1), ULT_STRN(8,3), ULT_STRN(9,3)
         WRITE(BUG,92109) ULT_STRN(3,1), ULT_STRN(4,1), ULT_STRN(7,1)
         WRITE(BUG,*)

      ELSE IF (WHAT == 22) THEN

         WRITE(BUG,92201)
         DO I=1,2                                          ! Write T1M and T1T
            WRITE(BUG,92202) I, (T1M(I,J),J=1,3), I, (T1T(I,J),J=1,2)
         ENDDO
         DO I=3,3
            WRITE(BUG,92203) I, (T1M(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         WRITE(BUG,92204)
         DO I=1,2                                          ! Write transformed material matrices, EM, ET
            WRITE(BUG,92205) I, (EM(I,J),J=1,3), I, (ET(I,J),J=1,2)
         ENDDO
         DO I=3,3 
            WRITE(BUG,92206) I, (EM(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,92207) JPLY, ZBK, ZTK
         DO I=1,3                                         ! Write ply A matrix
            WRITE(BUG,92208) I, (PLY_A(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         DO I=1,3                                          ! Write ply B matrix
            WRITE(BUG,92209) I, (PLY_B(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         DO I=1,3                                          ! Write ply D matrix
            WRITE(BUG,92210) I, (PLY_D(I,J),J=1,3)
         ENDDO
         WRITE(BUG,*)

         DO I=1,2                                          ! Write ply T matrix
            WRITE(BUG,92211) I, (PLY_T(I,J),J=1,2)
         ENDDO
         WRITE(BUG,*)

      ELSE IF (WHAT == 23) THEN

         WRITE(BUG,92301) PCOMP(INTL_PID,1), PCOMP_PLIES

         DO I=1,3                                         ! Write final result, SHELL_A
            WRITE(BUG,92302) I, (SHELL_A(I,J),J=1,3)
         ENDDO
         WRITE(BUG,99991)

         DO I=1,3                                         ! Write final result, SHELL_B
            WRITE(BUG,92303) I, (SHELL_B(I,J),J=1,3)
         ENDDO
         WRITE(BUG,99991)

         DO I=1,3                                         ! Write final result, SHELL_D
            WRITE(BUG,92304) I, (SHELL_D(I,J),J=1,3)
         ENDDO
         WRITE(BUG,99991)

         DO I=1,2                                         ! Write final result, SHELL_T
            WRITE(BUG,92305) I, (SHELL_T(I,J),J=1,2)
         ENDDO
         WRITE(BUG,99991)
         WRITE(BUG,99992)

      ENDIF

! **********************************************************************************************************************************
91301 FORMAT(/,'   Shell A, B, D and T matrices',/,                                                                                &
               '   ----------------------------')
91302 FORMAT('   SHELL_A row',I2,' = ',3(1ES14.6),'  membrane             ')

91303 FORMAT('   SHELL_B row',I2,' = ',3(1ES14.6),'  bend/mem coupling    ')

91304 FORMAT('   SHELL_D row',I2,' = ',3(1ES14.6),'  bending              ')

91305 FORMAT('   SHELL_T row',I2,' = ',2(1ES14.6),'                transverse shear     ')

92001 FORMAT('   Element number',I8,' uses PCOMP ',I8,' with ',I3,' plies:',//,                                                    &
             27X,'D A T A   A P P L I C A B L E   T O   A L L  ',I3,'   P L I E S   F O R   P C O M P  ',I8,/)

92002 FORMAT(39X,A,I3,11X,A)

92003 FORMAT(39X,A,1ES14.6,A)

92004 FORMAT('                                                   I N D I V I D U A L   P L Y   D A T A',/)

92005 FORMAT('                                                   I N D I V I D U A L   P L Y   D A T A',/,                         &
             '                                    (* indicates symmetric ply implicitly defined by the PCOMP entry)',/)

92006 format(29x,'Ply no.         Actual      Thickness     Matl angle      SOUTi           ZI      ',/,                           &
             29x,'               Matl  ID       TPLY          THETAi                  (Coord of mid)',/,                           &
             29x,'-------      ------------  ------------  ------------  ------------  ------------') 

92007 FORMAT(56x,'------------',/,54X,1ES14.6,'  total thickness')

92008 FORMAT(30X,I3,' ',9X,I8,3X,2(1ES14.6),4X,A7,3X,1ES14.6)

92101 FORMAT(//,5X,'  Ply number ',I2,' with thickness TPLY =',1ES13.6,' and THETA = ',0PF9.3,' deg uses ',A,1X,I8,                &
                     ' (internal matl ID',I7,')',/,                                                                                &
                5X,'  =======================================================================================================',    &
                '================',/)

92102 FORMAT(43X,'Membrane and transverse shear material matrices in input ply coords')

92103 FORMAT('             EM row',I2,' = ',3(1ES14.6),'                      ET row',I2,' = ',2(1ES14.6))

92104 FORMAT('             EM row',I2,' = ',3(1ES14.6))

92105 FORMAT(33x,'Membrane and transverse shear material stress and strain allowables in input ply coords'                      ,/,&
             29x,'Tension     Compression In-plane shear                                    Transv 13     Transv 23')

92106 FORMAT('        Stress (long) = ',3(1ES14.6),7X,'        Stress (transv) = ',2(1ES14.6))

92107 FORMAT('        Stress (lat ) = ',3(1ES14.6))

92108 FORMAT('        Strain (long) = ',3(1ES14.6),7X,'        Strain (transv) = ',2(1ES14.6))

92109 FORMAT('        Strain (lat ) = ',3(1ES14.6))

92201 FORMAT(45X,'Membrane and transverse shear material transformation matrices')

92202 FORMAT('            TEM row',I2,' = ',3(1ES14.6),'                     TET row',I2,' = ',2(1ES14.6))

92203 FORMAT('            TEM row',I2,' = ',3(1ES14.6))

92204 FORMAT(35X,'Membrane and transverse shear material matrices transformed to elem material coords')

92205 FORMAT('             EM row',I2,' = ',3(1ES14.6),'                      ET row',I2,' = ',2(1ES14.6))

92206 FORMAT('             EM row',I2,' = ',3(1ES14.6))

92207 FORMAT(52X,'A, B, D, and T matrices for ply number',I3,/,44X,'(coords of bot/top of ply = ',2(1ES14.6),')',/)

92208 FORMAT(41x,'Ply A row',I2,' = ',3(1ES14.6),'  membrane')

92209 FORMAT(41X,'Ply B row',I2,' = ',3(1ES14.6),'  bend/mem coupling')

92210 FORMAT(41X,'Ply D row',I2,' = ',3(1ES14.6),'  bending')

92211 FORMAT(41X,'Ply T row',I2,' = ',2(1ES14.6),'                transverse shear')

92301 FORMAT(//,36X,'+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +'                     ,/,&
                36X,'+                                                                                   +'                     ,/,&
                36X,'+  C O M P O S I T E   A, B, D, T   M A T R I C E S   F O R   P C O M P  ',I8,'   +'                       ,/,&
                36X,'+                             (sum over all ',I3,' plies)                              +'                  ,/,&
                36X,'+                                                                                   +')

92302 FORMAT(36X,'+   SHELL_A row',I2,' = ',3(1ES14.6),'  membrane            +')

92303 FORMAT(36X,'+   SHELL_B row',I2,' = ',3(1ES14.6),'  bend/mem coupling   +')

92304 FORMAT(36X,'+   SHELL_D row',I2,' = ',3(1ES14.6),'  bending             +')

92305 FORMAT(36X,'+   SHELL_T row',I2,' = ',2(1ES14.6),'                transverse shear    +')

99991 FORMAT(36X,'+                                                                                   +')

99992 FORMAT(36X,'+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +'/)

! **********************************************************************************************************************************

      END SUBROUTINE BUG_SHELL_ABD_MATRICES

      END SUBROUTINE SHELL_ABD_MATRICES
