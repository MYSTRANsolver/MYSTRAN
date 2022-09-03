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
 
      SUBROUTINE WRITE_BAR (NUM, FILL_F06, FILL_ANS, ISUBCASE, ITABLE,  &
                            TITLE, SUBTITLE, LABEL,                     &
                            FIELD5_INT_MODE, FIELD6_EIGENVALUE )
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06, OP2
      USE SCONTR, ONLY                :  BARTOR, BLNK_SUB_NAM, MOGEL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_BAR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, MAXREQ, MSPRNT, OGEL
 
      USE WRITE_BAR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_BAR'

      CHARACTER(LEN=*), INTENT(IN)    :: FILL_F06          ! Padding for output format
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_ANS          ! Padding for output format
      INTEGER(LONG), INTENT(IN)       :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      CHARACTER(LEN=128), INTENT(IN)  :: TITLE             ! the model TITLE
      CHARACTER(LEN=128), INTENT(IN)  :: SUBTITLE          ! the subcase SUBTITLE
      CHARACTER(LEN=128), INTENT(IN)  :: LABEL             ! the subcase LABEL
      INTEGER(LONG), INTENT(IN)       :: FIELD5_INT_MODE
      REAL(DOUBLE),  INTENT(IN)       :: FIELD6_EIGENVALUE

      CHARACTER(133*BYTE)             :: BLINE1A           ! Result of concatenating char. variables BOUT1, BMS1, BMSF1, BTOR to
!                                                            make the 1st line of stress output for a CBAR with torsional stress 
      CHARACTER(133*BYTE)             :: BLINE1B           ! Result of concatenating char. variables BOUT1, BMS1, BMSF1, BMS2, BMSF2
!                                                            to make the 1st line of stress output for a CBAR w/o torsional stress
      CHARACTER(133*BYTE)             :: BLINE2A           ! Result of concatenating char. variables BOUT2, BMS2, BMSF2
!                                                            to make the 2nd line of stress output for a CBAR
      CHARACTER(133*BYTE)             :: BLINE2B           ! Result of concatenating char. variables BOUT2, BMS2, BMSF2
!                                                            to make the 2nd line of stress output for a CBAR
      CHARACTER(107*BYTE)             :: BOUT1             ! Internal file: element ID and elem stresses for end A of element
      CHARACTER(107*BYTE)             :: BOUT2             ! Internal file: element ID and elem stresses for end B of element
      CHARACTER( 10*BYTE)             :: BMS1              ! Internal file: margin of safety in tension for element
      CHARACTER( 10*BYTE)             :: BMS2              ! Internal file: margin of safety in tension for element
      CHARACTER( 14*BYTE)             :: BMS3              ! Internal file: margin of safety in tension for element
      CHARACTER(  1*BYTE)             :: BMSF1             ! Internal file: margin of safety flag (MSFLAG) for tension M.S.
      CHARACTER(  1*BYTE)             :: BMSF2             ! Internal file: margin of safety flag (MSFLAG) for compr M.S.
      CHARACTER(  1*BYTE)             :: BMSF3             ! Internal file: margin of safety flag (MSFLAG) for torque M.S.
      CHARACTER( 14*BYTE)             :: BTOR              ! Internal file: torsional stress

      CHARACTER(14*BYTE)              :: ABS_ANS_CHAR(16)  ! Character variable that contains the 6 grid abs  outputs
      CHARACTER(14*BYTE)              :: MAX_ANS_CHAR(16)  ! Character variable that contains the 6 grid max  outputs
      CHARACTER(14*BYTE)              :: MIN_ANS_CHAR(16)  ! Character variable that contains the 6 grid min  outputs
      CHARACTER(  1*BYTE)             :: MSFLAG            ! If margin is negative, MSFLAG is an *
      CHARACTER(14*BYTE)              :: OGEL_CHAR(MOGEL)  ! Char representation of 1 row of OGEL outputs
 
      INTEGER(LONG), INTENT(IN)       :: ISUBCASE          ! The subcase ID
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_BAR_BEGEND
 
      REAL(DOUBLE)                    :: ABS_ANS(16)       ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(16)       ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(16)       ! Min for all grids output for each of the 6 disp components
      ! op2
      INTEGER(LONG)                   :: ELEMENT_TYPE = 34 ! CBAR-34; constant
      INTEGER(LONG)                   :: NUM_WIDE = 16     ! number of fields for the element; constant
      INTEGER(LONG)                   :: DEVICE_CODE = 1   ! PLOT; constant
      INTEGER(LONG)                   :: STRESS_CODE
      INTEGER(LONG)                   :: NVALUES           ! number of values in the op2 block
      STRESS_CODE = 1

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ! Routine for writing output to text files F06 and ANS for BAR element stresses. Up to 2 elements written per line of output.
      ! Data is first written to character variables and then that character variable is output the F06 and ANS.
      ! op2_headers = ['s1a', 's2a', 's3a', 's4a', 'axial', 'smaxa', 'smina', 'MS_tension',
      !                's1b', 's2b', 's3b', 's4b',          'smaxb', 'sminb', 'MS_compression']
 
      NVALUES = NUM_WIDE * NUM
      
      !CALL GET_STRESS_CODE(STRESS_CODE, IS_VON_MISES, IS_STRAIN, IS_FIBER_DISTANCE)
      CALL GET_STRESS_CODE( STRESS_CODE, 1,            0,         0)
      CALL WRITE_OES3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ELEMENT_TYPE, NUM_WIDE, STRESS_CODE, &
                             TITLE, SUBTITLE, LABEL, FIELD5_INT_MODE, FIELD6_EIGENVALUE)

      WRITE(OP2) NVALUES
      !K = 2*I+1
      !
      ! TODO: this is likely not right, but it should be the right shape...
      WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE,   &
                  (REAL(OGEL(2*I-1,J), 4), J=1,8),     & ! s1a-smina; K = 2*I+1
                  (REAL(OGEL(2*I,  J), 4), J=1,7),     & ! s1b-sminb; K = 2*I
                  I=1,NUM)
! ******************************
      K = 0
      DO I=1,NUM
         BLINE1A(1:) = ' '
         BLINE1B(1:) = ' '
         BLINE2A(1:) = ' '
         BLINE2B(1:) = ' '

         BOUT1(1:)   = ' '
         BMS1(1:)    = ' '
         BMSF1       = ' '   
 
         BOUT2(1:)   = ' '
         BMS2(1:)    = ' '
         BMSF2       = ' '

         BTOR(1:)    = ' '   
 
         ! Write first line of output for one element to a temporary internal file
         K = K + 1
         CALL WRT_REAL_TO_CHAR_VAR ( OGEL, MAXREQ, MOGEL, K, OGEL_CHAR )
         WRITE(BOUT1,9011) EID_OUT_ARRAY(I,1), (OGEL_CHAR(J),J=1,7)

         MSFLAG = ' '
         IF (MSPRNT(K,1) /= '0') THEN
            IF (OGEL(K,8) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(BMS1, 9021) OGEL(K,8)
            WRITE(BMSF1,9031) MSFLAG
         ELSE
            WRITE(BMS1, 9022)
            WRITE(BMSF1,9032)
         ENDIF
         IF (BARTOR == 'Y') THEN
            WRITE(BTOR, 9041) OGEL(K,9)
         ENDIF

        ! Write second line of output for one element to a temporary internal file
         K = K + 1
         CALL WRT_REAL_TO_CHAR_VAR ( OGEL, MAXREQ, MOGEL, K, OGEL_CHAR )
         WRITE(BOUT2,9012) (OGEL_CHAR(J),J=1,4), (OGEL_CHAR(J),J=6,7)

         MSFLAG = ' '
         IF (MSPRNT(K,2) /= '0') THEN
            IF (OGEL(K,8) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(BMS2, 9021) OGEL(K,8)
            WRITE(BMSF2,9031) MSFLAG
         ELSE
            WRITE(BMS2, 9022)
            WRITE(BMSF2,9032)
         ENDIF

         MSFLAG = ' '
         IF (MSPRNT(K,3) /= '0') THEN
            IF (OGEL(K,9) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(BMS3, 9023) OGEL(K,9)
            WRITE(BMSF3,9031) MSFLAG
         ELSE
            WRITE(BMS3, 9024)
            WRITE(BMSF3,9032)
         ENDIF

         ! Write the two lines of stress output for one element to F06
         WRITE(F06,*)
         WRITE(ANS,*)
         IF (BARTOR == 'Y') THEN
            BLINE1A = BOUT1//BMS1//BMSF1//BTOR
            BLINE2A = BOUT2//BMS2//BMSF2//BMS3//BMSF3
            WRITE(F06,9031) BLINE1A
            WRITE(F06,9031) BLINE2A
            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,9901) FILL_ANS, EID_OUT_ARRAY(I,1), (OGEL(K-1,J),J=1,9)
               WRITE(ANS,9902) FILL_ANS, (OGEL(K,J),J=1,9)
            ENDIF
         ELSE
            BLINE1B = BOUT1//BMS1//BMSF1
            BLINE2B = BOUT2//BMS2//BMSF2
            WRITE(F06,9031) BLINE1B
            WRITE(F06,9031) BLINE2B
            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,9903) FILL_ANS, EID_OUT_ARRAY(I,1), (OGEL(K-1,J),J=1,8)
               WRITE(ANS,9904) FILL_ANS, (OGEL(K,J),J=1,8)
           ENDIF
         ENDIF

         IF (DEBUG(200) > 0) THEN
         ENDIF

      ENDDO

      CALL GET_MAX_MIN_ABS ( 1, 8 )
      WRITE(F06,9108) (MAX_ANS_CHAR(J),J=1,7), MAX_ANS(8), (MAX_ANS_CHAR(J),J=9,15), MAX_ANS(16),                                  &
                      (MIN_ANS_CHAR(J),J=1,7), MIN_ANS(8), (MIN_ANS_CHAR(J),J=9,15), MIN_ANS(16),                                  &
                      (ABS_ANS_CHAR(J),J=1,7), ABS_ANS(8), (ABS_ANS_CHAR(J),J=9,15), ABS_ANS(16)
      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,9118) (MAX_ANS(J),J=1,16),(MIN_ANS(J),J=1,16), (ABS_ANS(J),J=1,16)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9011 FORMAT(1X,I8,7A)

 9012 FORMAT(1X,8X,4A,14X,2A)

 9021 FORMAT(ES10.2)

 9022 FORMAT('          ')

 9023 FORMAT(4X,ES10.2)

 9024 FORMAT('              ')

 9031 FORMAT(A)

 9032 FORMAT(' ')

 9041 FORMAT(ES14.6)

 9901 FORMAT(A,I8,7(1ES14.6),4X,1ES10.2,2ES14.6)

 9902 FORMAT(A,8X,7(1ES14.6),4X,1ES10.2,2ES14.6)

 9903 FORMAT(A,I8,7(1ES14.6),4X,1ES10.2,1ES14.6)

 9904 FORMAT(A,8X,7(1ES14.6),4X,1ES10.2,1ES14.6)



 9108 FORMAT( 1X,'         ------------- ------------- ------------- ------------- ------------- ------------- -------------',     &
                        ' ---------',/,                                                                                            &
             1X,'MAX* :  ',7A,1ES10.2,/,                                                                                           &
             1X,'MAX* :  ',7A,1ES10.2,//,                                                                                          &
             1X,'MIN* :  ',7A,1ES10.2,/,                                                                                           &
             1X,'MIN* :  ',7A,1ES10.2,//,                                                                                          &
             1X,'ABS* :  ',7A,1ES10.2,/,                                                                                           &
             1X,'ABS* :  ',7A,1ES10.2,/,                                                                                           &
             1X,'*for output set')

 9118 FORMAT(11X,'              ------------- ------------- ------------- ------------- ------------- ------------- -------------',&
                             ' ---------',/,                                                                                       &
             1X,'MAX (for output set):  ',7(1ES14.6),ES14.2,/,                                                                     &
             1X,'MAX (for output set):  ',7(1ES14.6),ES14.2,//,                                                                    &
             1X,'MIN (for output set):  ',7(1ES14.6),ES14.2,/,                                                                     &
             1X,'MIN (for output set):  ',7(1ES14.6),ES14.2,//,                                                                    &
             1X,'ABS (for output set):  ',7(1ES14.6),ES14.2,/,                                                                     &
             1X,'ABS (for output set):  ',7(1ES14.6),ES14.2)

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
      INTEGER(LONG)                   :: II,JJ,KK,LL       ! DO loop indices or counters

! **********************************************************************************************************************************
! Get MAX, MIN, ABS values

      DO JJ=1,2*(END_COL-BEG_COL+1)
         MAX_ANS(JJ) = -MACH_LARGE_NUM
      ENDDO 

      LL = 0
      DO II=1,NUM
         KK = 0
         LL = LL + 1
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(LL,JJ) > MAX_ANS(KK)) THEN
               MAX_ANS(KK) = OGEL(LL,JJ)
            ENDIF
         ENDDO
         LL = LL + 1
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(2*II,JJ) > MAX_ANS(KK)) THEN
               MAX_ANS(KK) = OGEL(LL,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO JJ=1,2*(END_COL-BEG_COL+1)
         MIN_ANS(JJ) = MAX_ANS(JJ)
      ENDDO

      LL = 0
      DO II=1,NUM
         KK = 0
         LL = LL + 1
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(LL,JJ) < MIN_ANS(KK)) THEN
               MIN_ANS(KK) = OGEL(LL,JJ)
            ENDIF
         ENDDO
         LL = LL + 1
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(LL,JJ) < MIN_ANS(KK)) THEN
               MIN_ANS(KK) = OGEL(LL,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO II=1,2*(END_COL-BEG_COL+1)
         ABS_ANS(II) = MAX( DABS(MAX_ANS(II)), DABS(MIN_ANS(II)) )
      ENDDO

      DO II=1,16

         IF (ABS_ANS(II) == 0.0) THEN
            WRITE(ABS_ANS_CHAR(II),'(A)') '  0.0         '
         ELSE
            WRITE(ABS_ANS_CHAR(II),'(1ES14.6)') ABS_ANS(II)
         ENDIF

         IF (MAX_ANS(II) == 0.0) THEN
            WRITE(MAX_ANS_CHAR(II),'(A)') '  0.0         '
         ELSE
            WRITE(MAX_ANS_CHAR(II),'(1ES14.6)') MAX_ANS(II)
         ENDIF

         IF (MIN_ANS(II) == 0.0) THEN
            WRITE(MIN_ANS_CHAR(II),'(A)') '  0.0         '
         ELSE
            WRITE(MIN_ANS_CHAR(II),'(1ES14.6)') MIN_ANS(II)
         ENDIF

      ENDDO

      END SUBROUTINE GET_MAX_MIN_ABS

      END SUBROUTINE WRITE_BAR
