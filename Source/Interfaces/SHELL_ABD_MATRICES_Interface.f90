! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

   MODULE SHELL_ABD_MATRICES_Interface

   INTERFACE

      SUBROUTINE SHELL_ABD_MATRICES ( INT_ELEM_ID, WRITE_WARN )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC, MRMATLC, MPCOMP_PLIES, MPCOMP0, MRPCOMP_PLIES, MRPCOMP0, &
                                         WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, THIRD, HALF, THREE, TWELVE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL, IORQ1M, QUAD4TYP, PCOMPEQ, PCMPTSTM, SHRFXFAC, SUPWARN, TSTM_DEF

      USE MODEL_STUF, ONLY            :  ALPVEC, EB, EBM, EM, ET, EDAT, EID, EMAT, EPNT, EPROP, ETYPE, FAILURE_THEORY, FCONV,      &
                                         INTL_MID, INTL_PID, MASS_PER_UNIT_AREA, MATL, MEPROP, MTRL_TYPE,                          &
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

      END SUBROUTINE SHELL_ABD_MATRICES

   END INTERFACE

   END MODULE SHELL_ABD_MATRICES_Interface

