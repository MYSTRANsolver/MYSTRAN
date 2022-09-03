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
  
      SUBROUTINE BD_PCOMP1 ( CARD, LARGE_FLD_INP )
  
! Processes PCOMP1 Bulk Data Cards. Data is somewhat different than on the more general PCOMP B.D. entry but the data from this
! PCOMP1 will be put into the same PCOMP, RPCOMP arrays.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPCOMP_PLIES, LPCOMP, MPCOMP0, MRPCOMP0,  &
                                         MPCOMP_PLIES, MRPCOMP_PLIES, NPCOMP
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, TWO
      USE MODEL_STUF, ONLY            :  PCOMP, RPCOMP
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PCOMP1_BEGEND

      USE BD_PCOMP1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PCOMP1'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD               ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)          ! The 10 fields of characters making up CARD
      CHARACTER( 1*BYTE)              :: CALC_Z0     = 'N'  ! If 'Y' then calculate ZO from ply thicknesses
      CHARACTER( 1*BYTE)              :: CRD_ERR     = 'N'  ! If 'Y' then this B.D. entry has error
      CHARACTER(LEN(JCARD))           :: FT                 ! Field 6 of parent entry
      CHARACTER(LEN(JCARD))           :: LAM                ! Field 9 of parent entry
 
      INTEGER(LONG)                   :: CONT_NUM    = 0    ! Count of continuation entries for this PCOMP1 B.D. entry
      INTEGER(LONG)                   :: ICONT       = 0    ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR        = 0    ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: I1,I2              ! Counters
      INTEGER(LONG)                   :: J2                 ! Counters
      INTEGER(LONG)                   :: MID                ! Material ID for all plies
      INTEGER(LONG)                   :: PCOMP_PLIES0       ! Count of number of plies on PCOMP1 entry
      INTEGER(LONG)                   :: PCOMP_PLIES        ! No. of plies in 1 PCOMP1 entry incl sym plies not explicitly defined
      INTEGER(LONG)                   :: PROPERTY_ID = 0    ! Property ID (field 2 of this parent property card)
      INTEGER(LONG)                   :: SOUT_INT    = 0    ! Entry in array PCOMP (not defined on PCOMP1)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PCOMP1_BEGEND
 
      REAL(DOUBLE)                    :: EPS1               ! A small number
      REAL(DOUBLE)                    :: NSM                ! Non structural mass
      REAL(DOUBLE)                    :: R8INP              ! Real value resd from a PCOMP1 field
      REAL(DOUBLE)                    :: SB          = ZERO ! Allowable shear stress
      REAL(DOUBLE)                    :: THETA(LPCOMP_PLIES)! Angle of longitudinal axis of ply 
      REAL(DOUBLE)                    :: THICK       = ZERO ! Thickness of each ply
      REAL(DOUBLE)                    :: TT          = ZERO ! Total plate thickness
      REAL(DOUBLE)                    :: Z0          = ZERO ! Dist (+/-) from ref plane to bottom surface
      REAL(DOUBLE)                    :: ZI          = ZERO ! Dist (+/-) from ref plane to middle of ply i

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PCOMP1 Bulk Data Card:
 
!   FIELD   ITEM     ARRAY ELEMENT          EXPLANATION 
!   -----   ----    ---------------         -------------
!    2      PID      PCOMP(npcomp,1)        Prop ID
!    3      Z0      RPCOMP(npcomp,1)        Dist from ref plane to bottom surface (default = -0.5 times layer thickness)
!    4      NSM     RPCOMP(npcomp,2)        Non structural mass per unit area
!    5      SB      RPCOMP(npcomp,3)        Allowable interlaminar shear stress. Required if FT is specified
!    6      FT       PCOMP(npcomp,3)        Failure theory (1=HILL, 2=HOFF, 3=TSAI, 4=STRN)
!    7      MID      PCOMP(npcomp,6+i)      Material ID of all plies
!    8      T       RPCOMP(npcomp,7+i)      Thickness of all plies
!    9      LAM      PCOMP(npcomp,4)        Symm lamination opt (for 1=SYM only plies on one side of elem centerline are specified)
!                                           (plies are numbered starting with 1 at the bottom layer. If an odd number of plies is
!                                           desired with SYM option the center ply thickness should be 1/2 the actual thickness
!  none              PCOMP(npcomp,2)        Type (1 for PCOMP1)
!  none PCOMP_PLIES  PCOMP(npcomp,5)        Number of plies for this PCOMP1 (not an input - determined herein)
!  none              PCOMP(npcomp,6)        Indicator whether equiv PSHELL, MAT2 entries have been written to F06 for this PCOMP1
!  none             RPCOMP(npcomp,4)        TREF not defined on PCOMP1
!  none             RPCOMP(npcomp,5)        GE not defined on PCOMP1
!  none     TT      RPCOMP(npcomp,6)        Total plate thickness
 
! continuation cards:
! 
!   FIELD   ITEM     ARRAY ELEMENT          EXPLANATION
!   -----   -----   -----------------       -------------
!    2-9    THETAi  RPCOMP(npcomp,7+i)      Orientation angle of longitudinal dir of ply i wrt material axis for the composite elem

!  none     Zi      RPCOMP(npcomp,8+i)      z coord (+/-) from ref plane to center of ply
 
! Subsequent continuation cards follow the same pattern as the 1st continuation card

      EPS1 = EPSIL(1)

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Initialize

      LAM(1:) = ' '

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
         PCOMP(NPCOMP,2) = 1                               ! 1 for PCOMP1 B.D entry and 0 for PCOMP
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
            ELSE IF (FT(1:5) == 'STRE ') THEN
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
 
      IF (JCARD(7)(1:) /= ' ') THEN                        ! Read MID. Save for later entry into array PCOMP when we know # plies
         CALL I4FLD ( JCARD(7), JF(7), MID )
      ENDIF
 
      IF (JCARD(8)(1:) /= ' ') THEN                        ! Read THICK. Save for later entry into array RPCOMP when we know # plies
         CALL R8FLD ( JCARD(8), JF(8), THICK )
      ENDIF
 
      LAM(1:) = ' '
      CALL CHAR_FLD ( JCARD(9), JF(9), LAM )               ! Read LAM  into  PCOMP(npcomp,4)
      IF (LAM(1:4) == 'SYM ') THEN
         PCOMP(NPCOMP,4) = 1
      ELSE
         PCOMP(NPCOMP,4) = 0
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
 
      RPCOMP(NPCOMP,4) = -999.D0                           ! Fictitous TREF and GE entries to make RPCOMP structure the same as
      RPCOMP(NPCOMP,5) =  ZERO                             ! for the B.D. PCOMP entry since PCOMP1 doesn't define these

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      IF ((IERRFL(2) == 'Y') .OR. (IERRFL(3) == 'Y') .OR. (IERRFL(4) == 'Y') .OR. (IERRFL(5) == 'Y') .OR. (IERRFL(6) == 'Y') .OR.  &
          (IERRFL(7) == 'Y') .OR. (IERRFL(8) == 'Y') .OR. (IERRFL(9) == 'Y')) THEN
         CRD_ERR = 'Y'
      ENDIF

! Get ply data from continuation entries. There must be at least 1 continuation entry
  
      CONT_NUM     = 0
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

            DO I=2,9                                       ! Read THETA's on cont entries
               IF (JCARD(I)(1:) /= ' ') THEN
                  PCOMP_PLIES0 = PCOMP_PLIES0 + 1          ! Count of number of plies defined by this PCOMP1 entry (not incl SYM)
                  CALL R8FLD ( JCARD(I), JF(I), R8INP )
                  IF (IERRFL(I) == 'N') THEN
                     THETA(PCOMP_PLIES0) = R8INP
                  ENDIF
               ENDIF
            ENDDO
 
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

! Make sure there was at least 1 continuation entry

      IF (CONT_NUM < 1) THEN

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1147) 'PCOMP'
         WRITE(F06,1147) 'PCOMP'

      ENDIF

! Now that we know the number of PLIES, enter this into array PCOMP

      IF (LAM(1:4) == 'SYM ') THEN
         PCOMP_PLIES = 2*PCOMP_PLIES0
      ELSE
         PCOMP_PLIES = PCOMP_PLIES0
      ENDIF
      PCOMP(NPCOMP,5) = PCOMP_PLIES
                                                           ! Flag to indicate if this PCOMP's equiv PSHELL/MAT2 been written to F06
      PCOMP(NPCOMP,6) = 0                                  ! It will be reset to 1 when equiv PSHELL, MAT2 entries have been written

! If there were no errors on 1st entry and at least 1 cont was read, process other info for arrays PCOMP, RPCOMP

      IF ((CRD_ERR == 'N') .AND. (CONT_NUM >= 1)) THEN     ! Put MID and SOUT into PCOMP (repeated for each ply so that array PCOMP
                                                           ! will be same data structure as for BD_PCOMP
         DO I=1,PCOMP_PLIES0
            I1 = MPCOMP0 + MPCOMP_PLIES*(I-1) + 1          ! I1 = 7+2*(I-1) for params MPCOMP0 = 6, MPCOMP_PLIES = 2 (module SCONTR)
            PCOMP(NPCOMP,I1)   = MID                          ! row 7,  9, 11 etc in array PCOMP for above SCONTR params
            PCOMP(NPCOMP,I1+1) = SOUT_INT                     ! row 8, 10, 12 etc in array PCOMP for above SCONTR params
         ENDDO

         TT = PCOMP_PLIES0*THICK                           ! Tot plate thick is PCOMP_PLIES0*THICK, or twice that, depending on LAM
         IF (LAM(1:4) == 'SYM ') THEN
            TT = TWO*TT
         ENDIF
         RPCOMP(NPCOMP,6) = TT

         IF (CALC_Z0 == 'Y') THEN                          ! Rewrite Z0 into RPCOMP if it was calculated
            Z0 = -HALF*TT
            RPCOMP(NPCOMP,1) = Z0
         ENDIF

         DO I=1,PCOMP_PLIES0                               ! Put THICK, THETA into RPCOMP for each of the PCOMP_PLIES0 plies
            I1 = MRPCOMP0 + MRPCOMP_PLIES*(I-1) + 1           ! I1 = 6+3*(I-1)+1 for params MRPCOMP0 = 6, MRPCOMP_PLIES = 3 (SCONTR)
            RPCOMP(NPCOMP,I1)   = THICK
            RPCOMP(NPCOMP,I1+1) = THETA(I)
         ENDDO
                                                           ! Calc Zi = coord from ref plane to center of ply. Put into array RPCOMP
         DO I=1,PCOMP_PLIES0                                  ! I1 = 6+3*I for params MRPCOMP0 = 6, MRPCOMP_PLIES = 3 (mod SCONTR)
            I1 = MRPCOMP0 + MRPCOMP_PLIES*I                   ! Index where ZI goes
            I2 = MRPCOMP0 + MRPCOMP_PLIES*(I-1) + 1           ! Index where ply i thickness is located
            ZI = Z0
            DO J=1,I-1
               J2 = MRPCOMP0 + MRPCOMP_PLIES*(J-1) + 1        ! Index where ply j thickness is located
               ZI = ZI + RPCOMP(NPCOMP,J2)                    ! At end of j loop, ZI = coord of bottom of ply i
            ENDDO
            ZI = ZI + HALF*RPCOMP(NPCOMP,I2)                  ! Now ZI is coord of middle of ply i
            RPCOMP(NPCOMP,I1) = ZI
         ENDDO

         IF (LAM(1:4) == 'SYM ') THEN                      ! If layup was sym (LAM = 'SYM') then duplicate ply data for sym plies

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

      END SUBROUTINE BD_PCOMP1
