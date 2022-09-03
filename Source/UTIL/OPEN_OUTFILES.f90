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

      SUBROUTINE OPEN_OUTFILES

! Opens BUGFIL, ERRFIL, F04FIL, F06FIL and, after checking STIME, closes the file so it can be reopened with APPEND.
! This subr is intended for opening these files 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG    , ERR    , F04    , F06    , SC1, BUGOUT, FILE_NAM_MAXLEN,                         &
                                         BUGFIL , ERRFIL , F04FIL , F06FIL ,                                                       &
                                         BUG_MSG, ERR_MSG, F04_MSG, F06_MSG
      USE TIMDAT, ONLY                :  STIME, TSEC

      USE OPEN_OUTFILES_USE_IFs

      IMPLICIT NONE

      CHARACTER( 1*BYTE)              :: QUIT              ! If 'Y' quit

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERR(4)           ! Error indicators
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to


! **********************************************************************************************************************************
! Default units for writing errors the screen (until LINK1A is read)

      OUNT(1) = SC1
      OUNT(2) = SC1

      QUIT = 'N'

      DO I=1,4
         IERR(I) = 0
      ENDDO

! Open BUG, ERR, F04, F06 files, check STIME, and position file at end

      CALL OPENIT ( F04FIL, F04, 'F04', F04_MSG, IERR(1) )

      CALL OPENIT ( BUGFIL, BUG, 'BUG', BUG_MSG, IERR(2) )

      CALL OPENIT ( ERRFIL, ERR, 'ERR', ERR_MSG, IERR(3) )

      CALL OPENIT ( F06FIL, F06, 'F06', F06_MSG, IERR(4) )

! If there were any errors based on opening above files, quit.

      DO I=1,4
         IF (IERR(I) > 0) THEN 
            CALL FILERR ( OUNT, 'Y' )
            QUIT = 'Y'
         ENDIF
      ENDDO

      IF (QUIT == 'Y') THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE OPENIT ( FILNAM, UNIT, UNIT_NAME, FILE_MSG, IERR0 )

! Opens formatted files

      USE IOUNT1, ONLY                        :  F04, FILE_NAM_MAXLEN
      USE SCONTR, ONLY                        :  LINKNO, FATAL_ERR
      USE TIMDAT, ONLY                        :  MONTH,DAY,YEAR,HOUR,MINUTE,SEC,SFRAC

      IMPLICIT NONE

      LOGICAL                                 :: FILE_EXIST! Result from INQUIRE regarding whether a file is open
      LOGICAL                                 :: FILE_OPND ! Result from INQUIRE regarding whether a file is open

      CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(IN)                                                                                  &
                                              :: FILNAM    ! File name (needs to be same length as other file names in MYSTRAN

      CHARACTER(LEN=*)                        :: FILE_MSG  ! Char message describing file
      CHARACTER(LEN=*)                        :: UNIT_NAME ! Char name of unit being opened (e.g. 'BUG')

      INTEGER(LONG)                           :: IERR0     ! Error indicator
      INTEGER(LONG), INTENT(IN)               :: UNIT      ! Unit number for the file to be opened

! **********************************************************************************************************************************
! FILNAM is opened, if not already opened, and positioned at the file end so we can append data.
! First, though, we need to make sure that
!   1) there is no problem opening the file and
!   2) that there is no error reading the time stamp and
!   3) the time stamp is the correct value.
! If this is the case, we close the file and reopen it with STATUS = 'APPEND'

      IERR0 = 0

      INQUIRE (FILE=FILNAM,EXIST=FILE_EXIST)                ! Make sure file exists (will be true if file OR unit number exist)
      IF (FILE_EXIST) THEN
         INQUIRE (FILE=FILNAM,OPENED=FILE_OPND)             ! If it is opened we assume it is already positioned at the end
         IF (.NOT.FILE_OPND) THEN
            IF (UNIT /= SC1) THEN                           ! Open file, check STIME, close file and then reopen as APPEND
               CALL FILE_OPEN ( UNIT, FILNAM, OUNT, 'OLD', FILE_MSG, 'READ_STIME', 'FORMATTED', 'READ'     , 'REWIND','Y','N', 'Y')
               CALL FILE_CLOSE ( UNIT, FILNAM, 'KEEP', 'Y' )
               CALL FILE_OPEN ( UNIT, FILNAM, OUNT, 'OLD', FILE_MSG, 'NEITHER'   , 'FORMATTED', 'READWRITE', 'APPEND','Y','N', 'Y')
            ENDIF
         ENDIF
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         IERR0     = IERR0 + 1
         WRITE(ERR,939) UNIT_NAME,UNIT
         CALL WRITE_FILNAM ( FILNAM, ERR, 15 )
         WRITE(F06,939) UNIT_NAME,UNIT
         CALL WRITE_FILNAM ( FILNAM, F06, 15 )
      ENDIF

! **********************************************************************************************************************************
  939 FORMAT(' *ERROR   939: THE FOLLOWING OUTPUT FILE (CONNECTED TO FILE UNIT ',A,' = ',I3,') DOES NOT EXIST:')

! **********************************************************************************************************************************

      END SUBROUTINE OPENIT

      END SUBROUTINE OPEN_OUTFILES
