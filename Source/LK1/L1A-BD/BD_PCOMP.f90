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
  
      SUBROUTINE BD_PCOMP ( CARD, LARGE_FLD_INP )
  
! Processes PCOMP Bulk Data Cards
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPCOMP, MPCOMP0, MRPCOMP0, MPCOMP_PLIES,  &
                                         MRPCOMP_PLIES, NPCOMP
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, TWO
      USE MODEL_STUF, ONLY            :  PCOMP, RPCOMP
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PCOMP_BEGEND

      USE BD_PCOMP_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PCOMP'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD               ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)          ! The 10 fields of characters making up CARD
      CHARACTER( 1*BYTE)              :: CALC_Z0     = 'N'  ! If 'Y' then calculate ZO from ply thicknesses
      CHARACTER( 1*BYTE)              :: CRD_ERR     = 'N'  ! If 'Y' then this B.D. entry has error
      CHARACTER(LEN(JCARD))           :: FT                 ! Field 6 of parent entry
      CHARACTER(LEN(JCARD))           :: LAM                ! Field 9 of parent entry
      CHARACTER(LEN(JCARD))           :: SOUT               ! Field 5 or 9 of cont entry
 
      INTEGER(LONG)                   :: CONT_NUM    = 0    ! Count of continuation entries for this PCOMP B.D. entry
      INTEGER(LONG)                   :: ICONT       = 0    ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR        = 0    ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: I4INP              ! Integer read from field on PCOMP entry
      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: I1,I2              ! Counters
      INTEGER(LONG)                   :: J1,J2              ! Counters
      INTEGER(LONG)                   :: PCOMP_PLIES0       ! Count of number of plies on PCOMP entry
      INTEGER(LONG)                   :: PCOMP_PLIES        ! No. of plies in 1 PCOMP entry incl sym plies not explicitly defined
      INTEGER(LONG)                   :: PROPERTY_ID = 0    ! Property ID (field 2 of this parent property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PCOMP_BEGEND
 
      REAL(DOUBLE)                    :: EPS1               ! A small number
      REAL(DOUBLE)                    :: GE                 ! Damping coeff
      REAL(DOUBLE)                    :: NSM                ! Non structural mass
      REAL(DOUBLE)                    :: R8INP              ! Real value resd from a PCOMP1 field
      REAL(DOUBLE)                    :: SB          = ZERO ! Allowable shear stress
      REAL(DOUBLE)                    :: TT          = ZERO ! Total plate thickness
      REAL(DOUBLE)                    :: TREF        = ZERO ! Ref temp
      REAL(DOUBLE)                    :: Z0          = ZERO ! Dist (+/-) from ref plane to bottom surface
      REAL(DOUBLE)                    :: ZI          = ZERO ! Dist (+/-) from ref plane to middle of ply i

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PCOMP Bulk Data Card:
 
!   FIELD   ITEM     ARRAY ELEMENT          EXPLANATION 
!   -----   ----    ---------------         -------------
!    2      PID      PCOMP(npcomp,1)        Prop ID
!    3      Z0      RPCOMP(npcomp,1)        Dist from ref plane to bottom surface (default = -0.5 times layer thickness)
!    4      NSM     RPCOMP(npcomp,2)        Non structural mass per unit area
!    5      SB      RPCOMP(npcomp,3)        Allowable interlaminar shear stress. Required if FT is specified
!    6      S FT       PCOMP(npcomp,3)        Failure theory (1=HILL, 2=HOFF, 3=TSAI, 4=STRN)
!    7      TREF    RPCOMP(npcomp,4)        Ref temperature
!    8      GE      RPCOMP(npcomp,5)        Damping coeff
!    9      LAM      PCOMP(npcomp,4)        Symm lamination opt (for 1=SYM only plies on one side of elem centerline are specified)
!                                           (plies are numbered starting with 1 at the bottom layer. If an odd number of plies is
!                                           desired with SYM option the center ply thickness should be 1/2 the actual thickness
!  none              PCOMP(npcomp,2)        Type (0 for PCOMP)
!  none PCOMP_PLIES  PCOMP(npcomp,5)        Number of plies for this PCOMP (not an input - determined herein)
!  none              PCOMP(npcomp,6)        Indicator whether equiv PSHELL, MAT2 entries have been written to F06 for this PCOMP
!  none      TT     RPCOMP(npcomp,6)        Total plate thickness
 
! continuation cards (2 plies specified per continuation entry:
! 
!   FIELD   ITEM     ARRAY ELEMENT          EXPLANATION
!   -----   -----   -----------------       -------------
!    2      MIDi     PCOMP(npcomp,6+i)      Material ID of ply i. See Remark (a)
!    3      Ti      RPCOMP(npcomp,6+i)      Thickness of ply i (real or blank but T1 -1st ply - must be specified). See Remark (b)
!    4      THETAi  RPCOMP(npcomp,7+i)      Orientation angle of longitudinal dir of ply i wrt material axis for the composite elem
!    5      SOUTi    PCOMP(npcomp,7+i)      Stress or strain output request (1=YES or 0=NO) for ply i

!    6      MIDj     repeat w/ j=i+1        Same as above for ply j=i+1
!    7      Tj               
!    8      THETAj          
!    9      SOUTj           

!  none     Zi      RPCOMP(npcomp,8+i)      z coord (+/-) from ref plane to center of ply
 
! Subsequent continuation cards follow the same pattern as the 1st continuation card

! Remarks:
! -------
! (a) Blank entries for Ti   (after ply 1 will indicate thickness for ply i, Ti  ,  equals value from previous ply (i-1)
! (b) Blank entries for MIDi (after ply 1 will indicate mat'l ID  for ply i, MIDi,  equals value from previous ply (i-1)

 
      EPS1 = EPSIL(1)

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Initialize

      LAM(1:)  = ' '
      SOUT(1:) = ' '

! Check for overflow

      NPCOMP = NPCOMP+1
!xx   IF (NPCOMP > LPCOMP) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LPCOMP
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LPCOMP
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), PROPERTY_ID )          ! Read PID  into  PCOMP(npcomp,1) and type into PCOMP(npcomp,2)
      IF (IERRFL(2) == 'N') THEN
         DO I=1,NPCOMP-1
            IF (PROPERTY_ID == PCOMP(I,1)) THEN
               FATAL_ERR = FATAL_ERR + 1 
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
            ENDIF
         ENDDO   
         PCOMP(NPCOMP,1) = PROPERTY_ID
         PCOMP(NPCOMP,2) = 0                               ! 0 for PCOMP B.D entry and 1 for PCOMP1
      ENDIF
 
      Z0      = ZERO
      CALC_Z0 = 'N'
      IF (JCARD(3)(1:) /= ' ') THEN                        ! Read Z0   into RPCOMP(npcomp,1)
         CALL R8FLD ( JCARD(3), JF(3), Z0 )
         IF (IERRFL(3) == 'N') THEN
            RPCOMP(NPCOMP,1) = Z0
         ENDIF
      ELSE
         IF (IERRFL(3) == 'N') THEN
            CALC_Z0 = 'Y'
         ENDIF
      ENDIF
 
      IF (JCARD(4)(1:) /= ' ') THEN                        ! Read NSM  into RPCOMP(npcomp,2)
         CALL R8FLD ( JCARD(4), JF(4), NSM )
         IF (IERRFL(4) == 'N') THEN
            RPCOMP(NPCOMP,2) = NSM
         ENDIF
      ENDIF
 
      FT(1:) = ' '
      IF (JCARD(6)(1:) /= ' ') THEN                        ! Read FT   into  PCOMP(npcomp,3)
         CALL CHAR_FLD ( JCARD(6), JF(6), FT )
         IF (IERRFL(6) == 'N') THEN
            IF      (FT(1:5) == 'HILL ') THEN
               PCOMP(NPCOMP,3) = 1
            ELSE IF (FT(1:5) == 'HOFF ') THEN
               PCOMP(NPCOMP,3) = 2
            ELSE IF (FT(1:5) == 'TSAI ') THEN
               PCOMP(NPCOMP,3) = 3
            ELSE IF (FT(1:4) == 'STRE ') THEN
               PCOMP(NPCOMP,3) = 4
            ELSE IF (FT(1:5) == 'STRN ') THEN
               PCOMP(NPCOMP,3) = 5
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1146) JF(6), JCARD(6)
               WRITE(F06,1146) JF(6), JCARD(6)
            ENDIF
         ENDIF
      ENDIF
 
      SB = ZERO
      IF (JCARD(5)(1:) /= ' ') THEN                        ! Read SB   into RPCOMP(npcomp,3)
         CALL R8FLD ( JCARD(5), JF(5), SB )
         IF (IERRFL(6) == 'N') THEN
            RPCOMP(NPCOMP,3) = SB
         ENDIF
      ELSE                                                 ! SB field is blank so make sure FT not specified
         IF ((FT(1:) /= ' ') .AND. (DABS(SB) <= EPS1)) THEN
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1112) JCARD(5)
            WRITE(F06,1112) JCARD(5)
         ENDIF
      ENDIF
 
      IF (JCARD(7)(1:) /= ' ') THEN                        ! Read TREF into RPCOMP(npcomp,4)
         CALL R8FLD ( JCARD(7), JF(7), TREF )
         IF (IERRFL(7) == 'N') THEN
            RPCOMP(NPCOMP,4) = TREF
         ENDIF
      ENDIF
 
      IF (JCARD(8)(1:) /= ' ') THEN                        ! Read GE   into RPCOMP(npcomp,5)
         CALL R8FLD ( JCARD(8), JF(8), GE )
         IF (IERRFL(8) == 'N') THEN
            RPCOMP(NPCOMP,5) = GE
         ENDIF
      ENDIF
 
      LAM(1:) = ' '
      CALL CHAR_FLD ( JCARD(9), JF(9), LAM )                  ! Read LAM  into  PCOMP(npcomp,4)
      IF (LAM(1:4) == 'SYM ') THEN
         PCOMP(NPCOMP,4) = 1
      ELSE
         PCOMP(NPCOMP,4) = 0
      ENDIF
   
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      IF ((IERRFL(2) == 'Y') .OR. (IERRFL(3) == 'Y') .OR. (IERRFL(4) == 'Y') .OR. (IERRFL(5) == 'Y') .OR. (IERRFL(6) == 'Y') .OR.  &
          (IERRFL(7) == 'Y') .OR. (IERRFL(8) == 'Y') .OR. (IERRFL(9) == 'Y')) THEN
         CRD_ERR = 'Y'
      ENDIF

! Get ply data from continuation entries. There must be at least 1 continuation entry
  
      I1       = 0
      I2       = 0
      CONT_NUM = 0
      PCOMP_PLIES0 = 0
      DO
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            
            CONT_NUM = CONT_NUM + 1 
                                                           ! Read 1st set of  ply data: MID, T, THRTA, SOUT
            IF((JCARD(2)(1:) /= ' ') .OR. (JCARD(3)(1:) /= ' ') .OR. (JCARD(4)(1:) /= ' ') .OR. (JCARD(5)(1:) /= ' ')) THEN

               PCOMP_PLIES0 = PCOMP_PLIES0 + 1

               I1 = I1 + 1
               IF (JCARD(2)(1:) /= ' ') THEN               ! Read MID   into  PCOMP
                  CALL I4FLD ( JCARD(2), JF(2), I4INP )
                  IF (IERRFL(2) == 'N') THEN
                     PCOMP(NPCOMP,MPCOMP0+I1) = I4INP
                  ENDIF
               ENDIF

               I2 = I2 + 1
               IF (JCARD(3)(1:) /= ' ') THEN               ! Read T     into RPCOMP
                  CALL R8FLD ( JCARD(3), JF(3), R8INP )
                  IF (IERRFL(3) == 'N') THEN
                     RPCOMP(NPCOMP,MRPCOMP0+I2) = R8INP
                  ENDIF
               ENDIF

               I2 = I2 + 1
               IF (JCARD(4)(1:) /= ' ') THEN               ! Read THETA into RPCOMP
                  CALL R8FLD ( JCARD(4), JF(4), R8INP )
                  IF (IERRFL(4) == 'N') THEN
                     RPCOMP(NPCOMP,MRPCOMP0+I2) = R8INP
                  ENDIF
               ENDIF

               I2 = I2 + 1                                 ! Make space for ZI to be calc'd below

               I1 = I1 + 1
               SOUT = 'NO      '
               IF (JCARD(5)(1:) /= ' ') THEN               ! Read SOUT  into  PCOMP
                  CALL CHAR_FLD ( JCARD(5), JF(5),  SOUT )
                  IF (IERRFL(5) == 'N') THEN
                     IF      (SOUT(1:4) == 'NO  ') THEN
                        PCOMP(NPCOMP,MPCOMP0+I1) = 0
                     ELSE IF (SOUT(1:4) == 'YES ') THEN
                        PCOMP(NPCOMP,MPCOMP0+I1) = 1
                     ELSE
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1134) SOUT
                        WRITE(F06,1134) SOUT
                     ENDIF
                  ENDIF
               ENDIF

            ENDIF
                                                           ! Read 2nd set of ply data: MID, T, THRTA, SOUT
            IF((JCARD(6)(1:) /= ' ') .OR. (JCARD(7)(1:) /= ' ') .OR. (JCARD(8)(1:) /= ' ') .OR. (JCARD(9)(1:) /= ' ')) THEN

               PCOMP_PLIES0 = PCOMP_PLIES0 + 1

               I1 = I1 + 1
               IF (JCARD(6)(1:) /= ' ') THEN               ! Read MID   into  PCOMP
                  CALL I4FLD ( JCARD(6), JF(6), I4INP )
                  IF (IERRFL(6) == 'N') THEN
                     PCOMP(NPCOMP,MPCOMP0+I1) = I4INP
                  ENDIF
               ENDIF

               I2 = I2 + 1
               IF (JCARD(7)(1:) /= ' ') THEN               ! Read T     into RPCOMP
                  CALL R8FLD ( JCARD(7), JF(7), R8INP )
                  IF (IERRFL(7) == 'N') THEN
                     RPCOMP(NPCOMP,MRPCOMP0+I2) = R8INP
                  ENDIF
               ENDIF

               I2 = I2 + 1
               IF (JCARD(8)(1:) /= ' ') THEN               ! Read THETA into RPCOMP
                  CALL R8FLD ( JCARD(8), JF(8), R8INP )
                  IF (IERRFL(8) == 'N') THEN
                     RPCOMP(NPCOMP,MRPCOMP0+I2) = R8INP
                  ENDIF
               ENDIF

               I2 = I2 + 1                                 ! Make space for ZI to be calc'd below

               I1 = I1 + 1
               SOUT(1:4) = 'NO  '
               IF (JCARD(9)(1:) /= ' ') THEN               ! Read SOUT  into  PCOMP
                  CALL CHAR_FLD ( JCARD(9), JF(9),  SOUT )
                  IF (IERRFL(9) == 'N') THEN
                     IF      (SOUT(1:4) == 'NO  ') THEN
                        PCOMP(NPCOMP,MPCOMP0+I1) = 0
                     ELSE IF (SOUT(1:4) == 'YES ') THEN
                        PCOMP(NPCOMP,MPCOMP0+I1) = 1
                     ELSE
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1134) SOUT
                        WRITE(F06,1134) SOUT
                     ENDIF
                  ENDIF
               ENDIF

            ENDIF

            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9)! Make sure that there are no imbedded blanks in fields 2-9
            CALL CRDERR ( CARD )                           ! CRDERR prints errors found when reading fields

         ELSE

            EXIT

         ENDIF

      ENDDO 
   
      IF ((IERRFL(2) == 'Y') .OR. (IERRFL(3) == 'Y') .OR. (IERRFL(4) == 'Y') .OR. (IERRFL(5) == 'Y') .OR. (IERRFL(6) == 'Y') .OR.  &
          (IERRFL(7) == 'Y') .OR. (IERRFL(8) == 'Y') .OR. (IERRFL(9) == 'Y')) THEN
         CRD_ERR = 'Y'
      ENDIF

! Make sure there is at least 1 continuation entry and, if so, that MID1 = PCOMP(NPCOMP,MPCOMP0+1)
! and T1 = RPCOMP(NPCOMP,MRPCOMP0+1) are specified

      IF (CONT_NUM >= 1) THEN

         IF (PCOMP(NPCOMP,MPCOMP0+1) == 0) THEN
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1114) PROPERTY_ID, PCOMP(NPCOMP,MPCOMP0+1)
            WRITE(F06,1114) PROPERTY_ID, PCOMP(NPCOMP,MPCOMP0+1)
         ENDIF

         IF (RPCOMP(NPCOMP,MRPCOMP0+1) == ZERO) THEN
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1131) PROPERTY_ID, RPCOMP(NPCOMP,MRPCOMP0+1)
            WRITE(F06,1131) PROPERTY_ID, RPCOMP(NPCOMP,MRPCOMP0+1)
         ENDIF

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1147) 'PCOMP'
         WRITE(F06,1147) 'PCOMP'

      ENDIF

! Reset MID and T that are 0 to the values from the previous ply

      IF ((CRD_ERR == 'N') .AND. (CONT_NUM >=1)) THEN

         DO I=2,PCOMP_PLIES0

            J1 = MPCOMP0 + MPCOMP_PLIES*(I-1) + 1
            IF (PCOMP(NPCOMP,J1) == 0) THEN
               PCOMP(NPCOMP,J1) = PCOMP(NPCOMP,J1-MPCOMP_PLIES)
            ENDIF

            J2 = MRPCOMP0 + MRPCOMP_PLIES*(I-1)+ 1
            IF (RPCOMP(NPCOMP,J2) == ZERO) THEN
               RPCOMP(NPCOMP,J2) = RPCOMP(NPCOMP,J2-MRPCOMP_PLIES)
            ENDIF

         ENDDO

! Total plate thickness is TT or 2*TT depending on LAM

         TT = ZERO
         DO I=1,PCOMP_PLIES0
            J2 = MRPCOMP0 + MRPCOMP_PLIES*(I-1) + 1
            TT = TT + RPCOMP(NPCOMP,J2)
         ENDDO

         IF (LAM(1:4) == 'SYM ') THEN
            TT = TWO*TT
         ENDIF
         RPCOMP(NPCOMP,6) = TT

! Rewrite Z0 into RPCOMP if it was calculated

         IF (CALC_Z0 == 'Y') THEN
            Z0 = -HALF*TT
            RPCOMP(NPCOMP,1) = Z0
         ENDIF

! Calc Zi = coord from ref plane to center of ply and add to RPCOMP for eah ply data (after THETAi)

         DO I=1,PCOMP_PLIES0
            I1 = MRPCOMP0 + MRPCOMP_PLIES*I                   ! Index where Di goes
            I2 = MRPCOMP0 + MRPCOMP_PLIES*(I-1) + 1           ! Index where ply i thickness is located
            ZI = Z0
            DO J=1,I-1
               J2 = MRPCOMP0 + MRPCOMP_PLIES*(J-1) + 1        ! Index where ply j thickness is located
               ZI = ZI + RPCOMP(NPCOMP,J2)                    ! At end of j loop, ZI = coord of bottom of ply i
            ENDDO
            ZI = ZI + HALF*RPCOMP(NPCOMP,I2)                       ! Now ZI is coordof middle of ply i
            RPCOMP(NPCOMP,I1) = ZI
         ENDDO

! If layup was symmetric (LAM = 'SYM') then duplicate ply data for symmetric plies   

         IF (LAM(1:4) == 'SYM ') THEN

            DO I=1,PCOMP_PLIES0
               I1 = MPCOMP0 + MPCOMP_PLIES*(I-1) + 1
               I2 = MPCOMP0 + 2*PCOMP_PLIES0*MPCOMP_PLIES - MPCOMP_PLIES*(I-1) - (MPCOMP_PLIES - 1)
               PCOMP(NPCOMP,I2)   = PCOMP(NPCOMP,I1)
               PCOMP(NPCOMP,I2+1) = PCOMP(NPCOMP,I1+1)
            ENDDO

            DO I=1,PCOMP_PLIES0
               I1 = MRPCOMP0 + MRPCOMP_PLIES*(I-1) + 1
               I2 = MRPCOMP0 + 2*PCOMP_PLIES0*MRPCOMP_PLIES - MRPCOMP_PLIES*(I-1) - (MRPCOMP_PLIES - 1)
               RPCOMP(NPCOMP,I2)   =  RPCOMP(NPCOMP,I1)
               RPCOMP(NPCOMP,I2+1) =  RPCOMP(NPCOMP,I1+1)
               RPCOMP(NPCOMP,I2+2) = -RPCOMP(NPCOMP,I1+2)
            ENDDO

         ENDIF

! Now that we know the number of PLIES, enter this into array PCOMP

         IF (LAM(1:4) == 'SYM ') THEN
            PCOMP_PLIES = 2*PCOMP_PLIES0
         ELSE
            PCOMP_PLIES = PCOMP_PLIES0
         ENDIF
         PCOMP(NPCOMP,5) = PCOMP_PLIES

! Set flag in PCOMP col 5 that will indicate if this PCOMP's equiv PSHELL and MAT2 entries have been written to F06

         PCOMP(NPCOMP,6) = 0                               ! Will be reset to 1 when equiv PSHELL, MAT2 entries have been written

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1112 FORMAT(' *ERROR  1112: FT IN FIELD 6 IS > 0 BUT SB IN FIELD 5 = "',A,'". MUST BE > 0')

 1114 FORMAT(' *ERROR  1114: MID1 IN FIELD 2 OF 1ST CONTINUATION OF PCOMP ',I8,' MUST BE > 0 BUT IS = ',I8)

 1131 FORMAT(' *ERROR  1131: T1 IN FIELD 3 OF 1ST CONTINUATION OF PCOMP ',I8,' MUST BE > 0 BUT IS = ',1ES9.1)

 1134 FORMAT(' *ERROR  1134: SOUT IN FIELD 5 OR 9 MUST BE "YES" OF "NO" BUT IS ',A)

 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)

 1146 FORMAT(' *ERROR  1146: FAILURE THEORY IN FIELD ',I2,' MUST BE "HILL", "HOFF", "TSAI", "STRN" OR MUST BE BLANK. HOWEVER'      &
                            ,' "',A,'" WAS ENTERED')

 1147 FORMAT(' *ERROR  1147: THERE MUST BE AT LEAST 1 CONTINUATION ENTRY FOR BULK DATA ',A,' ENTRY')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************

      END SUBROUTINE BD_PCOMP
