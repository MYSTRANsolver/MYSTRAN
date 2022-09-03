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
 
      SUBROUTINE WRITE_ROD ( ISUBCASE, NUM, FILL_F06, FILL_ANS, ITABLE, &
                             TITLE, SUBTITLE, LABEL,                    &
                             FIELD5_INT_MODE, FIELD6_EIGENVALUE)
 
! Routine for writing output to text files F06 and ANS for ROD element stresses. Up to 2 elements written per line of output.
! Data is first written to character variables and then that character variable is output the F06 and ANS.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ROD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, MSPRNT, OGEL
 
      USE WRITE_ROD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ROD'
      INTEGER(LONG), INTENT(IN)       :: ISUBCASE          ! the current subcase
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_F06          ! Padding for output format
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_ANS          ! Padding for output format
      INTEGER(LONG), INTENT(IN)       :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      CHARACTER(LEN=128), INTENT(IN) :: TITLE              ! the model TITLE
      CHARACTER(LEN=128), INTENT(IN) :: SUBTITLE           ! the subcase SUBTITLE
      CHARACTER(LEN=128), INTENT(IN) :: LABEL              ! the subcase LABEL
      INTEGER(LONG), INTENT(IN) :: FIELD5_INT_MODE
      REAL(DOUBLE),  INTENT(IN) :: FIELD6_EIGENVALUE

      CHARACTER(  1*BYTE)             :: MSFLAG            ! If margin is negative, MSFLAG is an *
      CHARACTER(118*BYTE)             :: RLINE_F06         ! Result of concatenating char. variables below to make a line of
!                                                            stress output for 1 or 2 CROD's
      CHARACTER( 59*BYTE)             :: RLINE_ANS         ! Result of concatenating char. variables below to make a line of
!                                                            stress output for 1 CROD

      CHARACTER(  8*BYTE)             :: REID1             ! Internal file: element ID of 1st CROD
      CHARACTER( 14*BYTE)             :: RSTR11            ! Internal file: axial stress in 1st CROD
      CHARACTER( 10*BYTE)             :: RMS11             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER(  1*BYTE)             :: RMSF11            ! Internal file: MSFLAG for axial M.S. in 1st CROD
      CHARACTER( 14*BYTE)             :: RSTR12            ! Internal file: torsional stress in 1st CROD
      CHARACTER( 10*BYTE)             :: RMS12             ! Internal file: M.S. for torsional stress in 1st CROD
      CHARACTER(  1*BYTE)             :: RMSF12            ! Internal file: MSFLAG for torsional M.S. in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS31             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS32             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS41             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS42             ! Internal file: M.S. for axial stress in 1st CROD

      CHARACTER(  8*BYTE)             :: REID2             ! Internal file: element ID of 2nd CROD
      CHARACTER( 14*BYTE)             :: RSTR21            ! Internal file: axial stress in 2nd CROD
      CHARACTER( 10*BYTE)             :: RMS21             ! Internal file: M.S. for axial stress in 2nd CROD
      CHARACTER(  1*BYTE)             :: RMSF21            ! Internal file: MSFLAG for axial M.S. in 2nd CROD
      CHARACTER( 14*BYTE)             :: RSTR22            ! Internal file: torsional stress in 2nd CROD
      CHARACTER( 10*BYTE)             :: RMS22             ! Internal file: M.S. for torsional stress in 2nd CROD
      CHARACTER(  1*BYTE)             :: RMSF22            ! Internal file: MSFLAG for torsional M.S. in 2nd CROD
 
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ROD_BEGEND
 
      REAL(DOUBLE)                    :: ABS_ANS(4)        ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(4)        ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(4)        ! Min for all grids output for each of the 6 disp components

      INTEGER(LONG)                   :: ELEM_TYPE         ! should be 1=CROD, 3=CTUBE, 10=CONROD
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ELEM_TYPE = 1  ! CROD
      !ELEM_TYPE = 3  ! CTUBE
      !ELEM_TYPE = 10  ! CONROD
      CALL OUTPUT2_WRITE_OES_ROD(ISUBCASE, ELEM_TYPE, NUM, ITABLE, TITLE, SUBTITLE, LABEL, &
                                 FIELD5_INT_MODE, FIELD6_EIGENVALUE)
      DO I=1,NUM,2
 
         RLINE_F06(1:)  = ' '
         RLINE_ANS(1:)  = ' '

         REID1(1:)  = ' '
         RSTR11(1:) = ' '
         RMS11(1:)  = ' '
         RMSF11(1:) = ' '
         RSTR12(1:) = ' '
         RMS12(1:)  = ' '
         RMSF12(1:) = ' '
 
         REID2(1:)  = ' '
         RSTR21(1:) = ' '
         RMS21(1:)  = ' '
         RMSF21(1:) = ' '
         RSTR22(1:) = ' '
         RMS22(1:)  = ' '
         RMSF22(1:) = ' '
 
         RMS31(1:)  = ' '
         RMS32(1:)  = ' '
         RMS41(1:)  = ' '
         RMS42(1:)  = ' '

         WRITE(REID1,2201) EID_OUT_ARRAY(I,1)
 
! Write axial stress output to a temporary internal file for one element
 
         WRITE(RSTR11,2202) OGEL(I,1)
         MSFLAG = ' '
         IF (MSPRNT(I,1) /= '0') THEN
            IF (OGEL(I,2) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(RMS11,2203) OGEL(I,2)
            WRITE(RMS31,2213) OGEL(I,2)
            WRITE(RMSF11,2204) MSFLAG
         ENDIF

! Write torsional stress output to a temporary internal file for one element
 
         WRITE(RSTR12,2202) OGEL(I,3)
         MSFLAG = ' '
         IF (MSPRNT(I,2) /= '0') THEN
            IF (OGEL(I,4) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(RMS12,2203) OGEL(I,4)
            WRITE(RMS32,2213) OGEL(I,4)
            WRITE(RMSF12,2204) MSFLAG
         ENDIF
 
! Write axial stress output to a temporary internal file for another element
 
         IF ((I+1) <= NUM) THEN
 
            WRITE(REID2,2201) EID_OUT_ARRAY(I+1,1)
 
            WRITE(RSTR21,2202) OGEL(I+1,1)
            MSFLAG = ' '
            IF (MSPRNT(I+1,1) /= '0') THEN
               IF (OGEL(I+1,2) < ZERO) THEN
                  MSFLAG = '*'
               ENDIF
               WRITE(RMS21,2203) OGEL(I+1,2)
               WRITE(RMS41,2213) OGEL(I+1,2)
               WRITE(RMSF21,2204) MSFLAG
            ENDIF
 
! Write torsional stress output to a temporary internal file for another element
 
            WRITE(RSTR22,2202) OGEL(I+1,3)
            MSFLAG = ' '
            IF (MSPRNT(I+1,2) /= '0') THEN
               IF (OGEL(I+1,4) < ZERO) THEN
                  MSFLAG = '*'
               ENDIF
               WRITE(RMS22,2203) OGEL(I+1,4)
               WRITE(RMS42,2213) OGEL(I+1,4)
               WRITE(RMSF22,2204) MSFLAG
            ENDIF
 
         ENDIF
 
! Write a line of output, consisting of stress output for one or two elements, to the output file
    
         RLINE_F06 = REID1//RSTR11//RMS11//RMSF11//RSTR12//RMS12//RMSF12//REID2//RSTR21//RMS21//RMSF21//RSTR22//RMS22//RMSF22

         WRITE(F06,2205) FILL_F06, RLINE_F06
         IF (DEBUG(200) > 0) THEN
            RLINE_ANS = REID1//RSTR11//RMS31//RSTR12//RMS32
            WRITE(ANS,2205) FILL_ANS, RLINE_ANS
            IF (I+1 <= NUM) THEN
               RLINE_ANS = REID2//RSTR21//RMS41//RSTR22//RMS42
               WRITE(ANS,2205) FILL_ANS, RLINE_ANS
            ENDIF
         ENDIF
 
      ENDDO   
 
      CALL GET_MAX_MIN_ABS ( 1, 4 )
      WRITE(F06,9104) (MAX_ANS(J),J=1,4),(MIN_ANS(J),J=1,4),(ABS_ANS(J),J=1,4)
      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,9114) (MAX_ANS(J),J=1,4),(MIN_ANS(J),J=1,4),(ABS_ANS(J),J=1,4)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 2201 FORMAT(I8)

 2202 FORMAT(ES14.6)

 2203 FORMAT(ES10.2)

 2213 FORMAT(4X,ES10.2)

 2204 FORMAT(A,A)

 2205 FORMAT(A,A)
 
 2215 FORMAT(A,I8,2(1ES14.6,4X,F10.3))

9104 FORMAT( 1X,'         ------------- ---------  ------------- ---------',/,                                                     &
             1X,'MAX* :  ',ES14.6,ES10.2,1X,ES14.6,ES10.2/,                                                                        &
             1X,'MIN* :  ',ES14.6,ES10.2,1X,ES14.6,ES10.2//,                                                                       &
             1X,'ABS* :  ',ES14.6,ES10.2,1X,ES14.6,ES10.2,/,                                                                       &
             1X,'*for output set')

9114 FORMAT(11X,'              ------------- ------------- ------------- -------------',/,                                         &
             1X,'MAX (for output set):  ',2(ES14.6,ES14.2),/,                                                                      &
             1X,'MIN (for output set):  ',2(ES14.6,ES14.2),//,                                                                     &
             1X,'ABS (for output set):  ',2(ES14.6,ES14.2))

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE GET_MAX_MIN_ABS ( BEG_COL, END_COL )

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: BEG_COL           ! Col number in OGEL where to beg for averaging to get max, min, abs
      INTEGER(LONG), INTENT(IN)       :: END_COL           ! Col number in OGEL where to end for averaging to get max, min, abs
      INTEGER(LONG)                   :: II,JJ,KK          ! DO loop indices or counters

! **********************************************************************************************************************************
! Get MAX, MIN, ABS values

      DO JJ=1,END_COL-BEG_COL+1
         MAX_ANS(JJ) = -MACH_LARGE_NUM 
      ENDDO 

      DO II=1,NUM
         KK = 0
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(II,JJ) > MAX_ANS(JJ)) THEN
               MAX_ANS(KK) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO JJ=1,END_COL-BEG_COL+1
         MIN_ANS(JJ) = MAX_ANS(JJ)
      ENDDO

      DO II=1,NUM
         KK = 0
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(II,JJ) < MIN_ANS(JJ)) THEN
               MIN_ANS(KK) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO II=1,END_COL-BEG_COL+1
         ABS_ANS(II) = MAX( DABS(MAX_ANS(II)), DABS(MIN_ANS(II)) )
      ENDDO

      END SUBROUTINE GET_MAX_MIN_ABS

      END SUBROUTINE WRITE_ROD
!==================================================================================================
      SUBROUTINE OUTPUT2_WRITE_OES_ROD(ISUBCASE, ELEM_TYPE, NUM, ITABLE, TITLE, SUBTITLE, LABEL, &
                                       FIELD5_INT_MODE, FIELD6_EIGENVALUE)
!     writes the CROD/CTUBE/CONROD stress/strain results.
!     Data is first written to character variables and then that character variable is output the F06 and ANS.
!     
!     Parameters
!     ==========
!     ELEM_TYPE : int
!       flag for the element type
!       - 1 : CROD
!       - 3 : CTUBE
!       - 10 : CONROD
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  OP2, ERR
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, OGEL
      USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_Value, IEEE_QUIET_NAN
      USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
      INTEGER(LONG) :: ANALYSIS_CODE        ! static, time, frequency, modal, etc. flag
      INTEGER(LONG), INTENT(IN) :: ISUBCASE  ! subcase id
      INTEGER(LONG), INTENT(IN) :: NUM       ! the number of elements in OGEL to write
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      CHARACTER(LEN=128), INTENT(IN) :: TITLE              ! the model TITLE
      CHARACTER(LEN=128), INTENT(IN) :: SUBTITLE           ! the subcase SUBTITLE
      CHARACTER(LEN=128), INTENT(IN) :: LABEL              ! the subcase LABEL
      INTEGER(LONG), INTENT(IN) :: FIELD5_INT_MODE
      REAL(DOUBLE),  INTENT(IN) :: FIELD6_EIGENVALUE

      INTEGER(LONG) :: ITABLE       ! the current subtable number
      INTEGER(LONG) :: NUM_WIDE     ! the number of "words" for a single element
      INTEGER(LONG) :: DEVICE_CODE  ! PLOT, PRINT, PUNCH flag
      INTEGER(LONG) :: STRESS_CODE  ! flag for what the output result means (e.g., fiber_distance/curvature, max_shear, von_mises)
      INTEGER(LONG) :: NVALUES      ! the number of "words" for the OP2 data
      INTEGER(LONG) :: NTOTAL       ! the number of bytes corresponding to nvalues
      REAL(REAL32)  :: NAN
      LOGICAL       :: IS_PRINT     ! is this a PRINT result -> F06
      LOGICAL       :: IS_PLOT      ! is this a PLOT result -> OP2

      NAN = IEEE_VALUE(NAN, IEEE_QUIET_NAN)
      ! TODO: assuming PLOT
      DEVICE_CODE = 1

! **********************************************************************************************************************************
!******OP2
      ! we're already setup for subtable -3
!==================================================================================================
      
      ! eid, axisl_stress, axial_margin, torsional stress, torsional_margin
      NUM_WIDE = 5
      
      ! dunno???
      STRESS_CODE = 1
      CALL WRITE_OES3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE, TITLE, SUBTITLE, LABEL, &
                             FIELD5_INT_MODE, FIELD6_EIGENVALUE)
      ! ITABLE = -4, -6, ...
      !NWORDS = NUM * NUM_WIDE
      !NTOTAL = NBYTES_PER_WORD * NWORDS

      IS_PRINT = .TRUE.
      IS_PLOT = .TRUE.
      IF (IS_PLOT) THEN
 100    FORMAT("*DEBUG:    ITABLE=",I8, "; NUM=",I8,"; NVALUES=",I8,"; NTOTAL=",I8)
        NVALUES = NUM * NUM_WIDE
        NTOTAL = NVALUES * 4
        WRITE(ERR,100) ITABLE,NUM,NVALUES,NTOTAL
        WRITE(OP2) NVALUES

 101    FORMAT ("*OP2:     EID_DEVICE=",I8," STRESS=",ES10.2,ES10.2)
        DO I=1,NUM
          WRITE(ERR,101) EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, REAL(OGEL(I,1), 4), REAL(OGEL(I,2), 4)
        ENDDO
      ! Nastran OP2 requires this write call be a one liner...so it's a little weird...
      ! translating:
      !    DO I=1,NUM
      !        WRITE(OP2) EID_OUT_ARRAY(I,1)*10+DEVICE_CODE  ! Nastran is weird and requires scaling the ELEMENT_ID
      !
      !        convert from float64 (double precision) to float32 (single precision)
      !        RE1 = REAL(OGEL(I,1), 4)
      !        RE2 = REAL(OGEL(I,2), 4)
      !        RE3 = REAL(OGEL(I,3), 4)
      !        RE4 = REAL(OGEL(I,4), 4)
      !        
      !        write the axisl_stress, axial_margin, torsional stress, torsional_margin
      !        WRITE(OP2) RE1, RE2, RE3, RE4
      !    ENDDO
      !
!      TODO: figure out what's going on with the shear stress
!      TODO: calculate axial/torision margin
        ! write the rod stress/strain data
        !WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, REAL(OGEL(I,1), 4), REAL(OGEL(I,2), 4), &
        !                                               REAL(OGEL(I,3), 4), REAL(OGEL(I,4), 4), I=1,NUM)
        WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, REAL(OGEL(I,1), 4), NAN, &
                                                       REAL(OGEL(I,2), 4), NAN, I=1,NUM)
 102    FORMAT("*DEBUG: WRITE_CROD      ITABLE=",I8," (should be -5, -7,...)")
        WRITE(ERR,102) ITABLE
      ENDIF

!      IF (IS_PRINT) THEN
!        F06
!      ENDIF
      END SUBROUTINE OUTPUT2_WRITE_OES_ROD
