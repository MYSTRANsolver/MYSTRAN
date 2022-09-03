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
 
      SUBROUTINE FILE_CLOSE ( UNIT, FILNAM, CLOSE_STAT, WRITE_F04 )
 
! Closes files and writes message if the close fails
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, WRT_ERR, WRT_LOG, F04, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  FILE_OPEN_BEGEND

      USE FILE_CLOSE_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: FILE_EXIST
      LOGICAL                         :: FILE_OPND

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FILE_CLOSE'

!xx   CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(IN) :: FILNAM            ! File name
      CHARACTER(LEN=*)   , INTENT(IN) :: FILNAM            ! File name

      CHARACTER(LEN=*)   , INTENT(IN) :: CLOSE_STAT        ! Status for close
      CHARACTER(LEN=*)   , INTENT(IN) :: WRITE_F04         ! If 'Y' write to F04, otherwise do not
      CHARACTER( 6*BYTE)              :: NAM_CLS 
      CHARACTER( 3*BYTE)              :: UNIT_NAME = '???' ! Extension of FILNAM (e.g. F06, etc)

      INTEGER(LONG), INTENT(IN)       :: UNIT              ! File unit number
      INTEGER(LONG)                   :: DEC_PT            ! Position in FILNAM where '.' exists
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when closing a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FILE_OPEN_BEGEND

! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         INQUIRE(FILE=FILNAM,OPENED=FILE_OPND)
         IF (UNIT /= F04) THEN
            DEC_PT = INDEX(FILNAM,'.',.TRUE.)
            IF (DEC_PT > 0) THEN
               UNIT_NAME = FILNAM(DEC_PT+1:DEC_PT+4)
            ENDIF
            INQUIRE(FILE=FILNAM,EXIST=FILE_EXIST)
            NAM_CLS(1:) = CLOSE_STAT
            WRITE(F04,9001) SUBR_NAME, TSEC, FILE_EXIST, FILE_OPND, NAM_CLS, UNIT_NAME
         ENDIF
 9001    FORMAT(1X,A,' BEGN ',F10.3,2L2,',',1X,A6,1X,',',44X,',  Closing file unit: ',A)
      ENDIF

! **********************************************************************************************************************************
      CLOSE ( UNIT,STATUS=CLOSE_STAT,IOSTAT=IOCHK )

      IF (IOCHK /= 0) THEN
         WRITE(SC1,903) IOCHK
         CALL WRITE_FILNAM ( FILNAM, SC1, 15 )
         WRITE(SC1,9232)
         STOP
      ENDIF

! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         IF (UNIT /= F04) THEN
            INQUIRE(FILE=FILNAM,EXIST=FILE_EXIST)
            INQUIRE(FILE=FILNAM,OPENED=FILE_OPND)
            WRITE(F04,9002) SUBR_NAME,TSEC,FILE_EXIST,FILE_OPND
         ENDIF
 9002    FORMAT(1X,A,' END  ',F10.3,2L2)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  903 FORMAT(' *ERROR   903: ERROR ENCOUNTERED WITH IOSTAT = ',I8,' CLOSING FILE:')

 9232 FORMAT('               IT MAY BE OPEN IN ANOTHER PROGRAM. IF SO, CLOSE IT & START AGAIN.')

! **********************************************************************************************************************************

      END SUBROUTINE FILE_CLOSE
