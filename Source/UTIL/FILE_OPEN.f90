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
 
      SUBROUTINE FILE_OPEN (UNIT, FILNAM, OUNT, STATUS, MESSAG, RW_STIME, FORMAT, ACTION, POSITION, WRITE_L1A, WRITE_VER, WRITE_F04)

! Opens formatted files that have STIME for read or write. If open for read, check STIME. If open for write, write STIME
! If file needs to be opened for READWRITE, this subr needs to be called twice:
!     (1) called 1st time for READ (to open file and to read and check STIME)
!     (2) called 2nd time (after closing by calling subr) to position at end for subsequent writing after returning
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ANS, F04, F06, IN1, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, PROG_NAME
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE DEBUG_PARAMETERS
      USE MYSTRAN_Version, ONLY       :  MYSTRAN_VER_NUM, MYSTRAN_VER_MONTH, MYSTRAN_VER_DAY, MYSTRAN_VER_YEAR, MYSTRAN_AUTHOR,  &
                                         MYSTRAN_COMMENT
      USE SUBR_BEGEND_LEVELS, ONLY    :  FILE_OPEN_BEGEND
 
      USE FILE_OPEN_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: FILE_EXIST
      LOGICAL                         :: FILE_OPND

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FILE_OPEN'
      CHARACTER(LEN=*), INTENT(IN)    :: ACTION            ! File description
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: FORMAT            ! File format
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! File description
      CHARACTER(LEN=*), INTENT(IN)    :: POSITION          ! File description 
      CHARACTER(LEN=*), INTENT(IN)    :: STATUS            ! File status indicator (NEW, OLD, REPLACE)
      CHARACTER(LEN=*), INTENT(IN)    :: RW_STIME          ! Indicator of whether to read or write STIME
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_L1A         ! 'Y'/'N' Arg passed to subr OUTA_HERE
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_VER         ! 'Y'/'N' Arg to tell whether to write MYSTRAN version info
      CHARACTER( 9*BYTE)              :: NAM_ACT 
      CHARACTER(11*BYTE)              :: NAM_FOR
      CHARACTER( 6*BYTE)              :: NAM_POS 
      CHARACTER( 7*BYTE)              :: NAM_STA 
      CHARACTER(11*BYTE)              :: NAM_RWS 
      CHARACTER( 3*BYTE)              :: UNIT_NAME = '???' ! Extension of FILNAM (e.g. F06, etc)
 
      INTEGER(LONG), INTENT(IN)       :: UNIT              ! Unit number file is attached to
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to. Input to subr FILE_OPEN  
      INTEGER(LONG)                   :: DEC_PT            ! Position in FILNAM where '.' exists
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERR              ! Error count
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: XTIME             ! Time stamp read from file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FILE_OPEN_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         INQUIRE(FILE=FILNAM,OPENED=FILE_OPND)
         IF ((UNIT /= F04) .AND. (UNIT /= IN1)) THEN
            DEC_PT = INDEX(FILNAM,'.',.TRUE.)
            IF (DEC_PT > 0) THEN
               UNIT_NAME = FILNAM(DEC_PT+1:DEC_PT+4)
            ENDIF
            INQUIRE(FILE=FILNAM,EXIST=FILE_EXIST)
            NAM_STA(1:) = STATUS
            NAM_FOR(1:) = FORMAT
            NAM_ACT(1:) = ACTION
            NAM_POS(1:) = POSITION
            NAM_RWS(1:) = RW_STIME
            WRITE(F04,9001) SUBR_NAME, TSEC, FILE_EXIST, FILE_OPND, NAM_STA, NAM_FOR, NAM_ACT, NAM_POS, NAM_RWS, UNIT_NAME
         ENDIF
 9001    FORMAT(1X,A,' BEGN ',F10.3,2L2,',',1X,A7,',',1X,A11,',',1X,A9,',',1X,A6,',',1X,A11,',  Opening file unit: ',A)
      ENDIF

! **********************************************************************************************************************************
! Check inputs for sensibility (coding errors if wrong)

      IERR = 0
                                                           ! Check FORM
      IF ((FORMAT /= 'FORMATTED') .AND. (FORMAT /= 'UNFORMATTED'))THEN
         IERR = IERR + 1
         DO I=1,2
            IF (OUNT(I) > 0) THEN
               WRITE(OUNT(I),909) SUBR_NAME, 'FORMAT', 'FORMATTED or UNFORMATTED', FORMAT
            ELSE
               WRITE(SC1,909) SUBR_NAME, 'FORMAT', 'FORMATTED or UNFORMATTED', FORMAT
            ENDIF
            IF (OUNT(2) == OUNT(1)) EXIT
         ENDDO
      ENDIF
                                                           ! Ckeck ACTION
      IF ((ACTION /= 'READ') .AND. (ACTION /= 'WRITE') .AND. (ACTION /= 'READWRITE')) THEN
         IERR = IERR + 1
         DO I=1,2
            IF (OUNT(I) > 0) THEN
               WRITE(OUNT(I),909) SUBR_NAME, 'ACTION', 'READ or WRITE', ACTION
            ELSE
               WRITE(SC1,909) SUBR_NAME, 'ACTION', 'READ or WRITE', ACTION
            ENDIF
            IF (OUNT(2) == OUNT(1)) EXIT
         ENDDO
      ENDIF
                                                           ! Check POSITION
      IF ((POSITION /= 'ASIS') .AND. (POSITION /= 'APPEND') .AND. (POSITION /= 'REWIND')) THEN
         IERR = IERR + 1
         DO I=1,2
            IF (OUNT(I) > 0) THEN
               WRITE(OUNT(I),909) SUBR_NAME, 'POSITION', 'ASIS or APPEND or REWIND', POSITION
            ELSE
               WRITE(SC1,909) SUBR_NAME, 'POSITION', 'ASIS or APPEND or REWIND', POSITION
            ENDIF
            IF (OUNT(2) == OUNT(1)) EXIT
         ENDDO
      ENDIF
                                                           ! Check STATUS
!     IF ((STATUS /= 'NEW') .AND.(STATUS /= 'OLD') .AND.(STATUS /= 'REPLACE')) THEN
!        IERR = IERR + 1
!        DO I=1,2
!           IF (OUNT(I) > 0) THEN
!              WRITE(OUNT(I),909) SUBR_NAME, 'STATUS', 'NEW or OLD or REPLACE', STATUS
!           ELSE
!              WRITE(SC1,909) SUBR_NAME, 'STATUS', 'NEW or OLD or REPLACE', STATUS
!           ENDIF
!           IF (OUNT(2) == OUNT(1)) EXIT
!        ENDDO
!     ENDIF 
                                                           ! Check WRITE_L1A
      IF ((WRITE_L1A /= 'Y') .AND. (WRITE_L1A /= 'N'))THEN
         IERR = IERR + 1
         DO I=1,2
            IF (OUNT(I) > 0) THEN
               WRITE(OUNT(I),909) SUBR_NAME, 'WRITE_L1A', 'Y or N', WRITE_L1A
            ELSE
               WRITE(SC1,909) SUBR_NAME, 'WRITE_L1A', 'Y or N', WRITE_L1A
            ENDIF
            IF (OUNT(2) == OUNT(1)) EXIT
         ENDDO
      ENDIF
                                                           ! Check WRITE_STIME
      IF ((RW_STIME /= 'READ_STIME') .AND. (RW_STIME /= 'WRITE_STIME') .AND. (RW_STIME /= 'NEITHER'))THEN
         IERR = IERR + 1
         DO I=1,2
            IF (OUNT(I) > 0) THEN
               WRITE(OUNT(I),909) SUBR_NAME, 'RW_STIME', 'READ_STIME, WRITE_STIME or NEITHER', RW_STIME
            ELSE
               WRITE(SC1,909) SUBR_NAME, 'RW_STIME', 'READ, WRITE or NEITHER', RW_STIME
            ENDIF
            IF (OUNT(2) == OUNT(1)) EXIT
         ENDDO
      ENDIF

      IF ((WRITE_VER /= 'Y') .AND. (WRITE_VER /= 'N'))THEN
         IERR = IERR + 1
         DO I=1,2
            IF (OUNT(I) > 0) THEN
               WRITE(OUNT(I),909) SUBR_NAME, 'WRITE_VER', 'Y or N', WRITE_VER
            ELSE
               WRITE(SC1,909) SUBR_NAME, 'WRITE_VER', 'Y or N', WRITE_VER
            ENDIF
            IF (OUNT(2) == OUNT(1)) EXIT
         ENDDO
      ENDIF

      IF (IERR > 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         IF (WRITE_L1A == 'Y') THEN
            CALL OUTA_HERE ( 'Y' )
         ELSE
            CALL OUTA_HERE ( 'N' )
         ENDIF
      ENDIF

! No problems with inputs, so proceed to open file

      IERR = 0
      OPEN ( UNIT, FILE=FILNAM, STATUS=STATUS, FORM=FORMAT, ACCESS='SEQUENTIAL', ACTION=ACTION, POSITION=POSITION, IOSTAT=IOCHK )

      IF (DEBUG(52) > 0) THEN
         CALL DEB_FILE_OPEN
      ENDIF

      IF (IOCHK == 0) THEN
         IF      (RW_STIME == 'READ_STIME') THEN           ! Read and check STIME
            IF (FORMAT == 'FORMATTED') THEN
               READ(UNIT,'(1X,I11)',IOSTAT=IOCHK) XTIME
            ELSE
               READ(UNIT,IOSTAT=IOCHK) XTIME
            ENDIF
            IF (IOCHK /= 0) THEN
               REC_NO = 1
               CALL READERR ( IOCHK, FILNAM, MESSAG, REC_NO, OUNT, WRITE_F04 )
               IERR = IERR + 1
            ELSE
               IF (XTIME /= STIME) THEN
                  CALL STMERR ( XTIME, FILNAM, OUNT, 'Y' )
                  IERR = IERR +1
               ENDIF
            ENDIF
         ELSE IF (RW_STIME == 'WRITE_STIME') THEN          ! Write STIME  
            IF (FORMAT == 'FORMATTED') THEN
               WRITE(UNIT,'(1X,I11)') STIME
            ELSE
               WRITE(UNIT) STIME
            ENDIF
            IF (WRITE_VER == 'Y') THEN
               IF (UNIT == ANS) THEN
                  WRITE(UNIT,118) PROG_NAME, MYSTRAN_VER_NUM, MYSTRAN_VER_MONTH, MYSTRAN_VER_DAY, MYSTRAN_VER_YEAR,                &
                                  MYSTRAN_AUTHOR
               ELSE
                  WRITE(UNIT,117) PROG_NAME, MYSTRAN_VER_NUM, MYSTRAN_VER_MONTH, MYSTRAN_VER_DAY, MYSTRAN_VER_YEAR,                &
                                  MYSTRAN_AUTHOR, MYSTRAN_COMMENT
               ENDIF
            ENDIF
         ENDIF
      ELSE
         CALL OPNERR ( IOCHK, FILNAM, OUNT, 'Y' )
         IERR = IERR +1
      ENDIF

      IF (IERR > 0) THEN
         CALL FILERR ( OUNT, WRITE_F04 )
         CALL OUTA_HERE ( 'Y' )
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         IF ((UNIT /= F04) .AND. (UNIT /= IN1)) THEN
            INQUIRE(FILE=FILNAM,EXIST=FILE_EXIST)
            INQUIRE(FILE=FILNAM,OPENED=FILE_OPND)
            WRITE(F04,9002) SUBR_NAME,TSEC,FILE_EXIST,FILE_OPND
         ENDIF
 9002    FORMAT(1X,A,' END  ',F10.3,2L2)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  909 FORMAT(' *ERROR   909: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ILLEGAL INPUT FOR ARGUMENT ',A,'. MUST BE ',A,' BUT IS "',A,'". ERROR OCCURRED OPENING FILE:')

  117 FORMAT(/,1X,A,' Version',5(1X,A),/,A)

  118 FORMAT(/,1X,A,' Version',5(1X,A),/)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEB_FILE_OPEN

      IMPLICIT NONE

! **********************************************************************************************************************************
      WRITE(F06,98720)

      WRITE(F06,'(A,I8)') ' In file OPEN, UNIT     = ', UNIT
      WRITE(F06,'(A,A) ') ' In file OPEN, FILE     = ', FILNAM(1:40)
      WRITE(F06,'(A,A) ') ' In file OPEN, STATUS   = ', STATUS
      WRITE(F06,'(A,A) ') ' In file OPEN, FORM     = ', FORMAT
      WRITE(F06,'(A,A) ') ' In file OPEN, ACTION   = ', ACTION
      WRITE(F06,'(A,A) ') ' In file OPEN, POSITION = ', POSITION
      WRITE(F06,'(A,I8)') ' In file OPEN, IOCHK    = ', IOCHK
      WRITE(F06,*)
      WRITE(F06,*) ' Open ', filnam(1:32), 'RW_STIME, STIME = "', rw_stime, '"   ', stime

      WRITE(F06,*)

      WRITE(F06,98799)
 
      WRITE(F06,*)

! **********************************************************************************************************************************
98720 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::::START DEBUG(52) OUTPUT FROM SUBROUTINE FILE_OPEN:::::::::::::::::::::::::',&
             ':::::::::::::::::',/)

98799 FORMAT(' ::::::::::::::::::::::::::::::::::::::::::END DEBUG(52) OUTPUT FROM SUBROUTINE FILE_OPEN::::::::::::::::::::::::::',&
             ':::::::::::::::::'                                                                                                ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE DEB_FILE_OPEN 


      END SUBROUTINE FILE_OPEN
