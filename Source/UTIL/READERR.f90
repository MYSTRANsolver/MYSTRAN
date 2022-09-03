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
 
      SUBROUTINE READERR (IOCHK, FILNAM, MESSAG, REC_NO, OUNT, WRITE_F04 )
 
! Writes message about errors encountered when reading files 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  READERR_BEGEND
 
      USE READERR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READERR'
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! File description. Used for error messaging
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)
 
      INTEGER(LONG), INTENT(IN)       :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to
      INTEGER(LONG), INTENT(IN)       :: REC_NO            ! Indicator of record number when error encountered reading file
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IEND              ! End col for MESSAG
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READERR_BEGEND
 
! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! First, truncate trailing blanks in MESSAG

      DO I=LEN(MESSAG),1,-1
         IF (MESSAG(I:I) == ' ') THEN
            CYCLE
         ELSE
            IEND = I
            EXIT
         ENDIF
      ENDDO

! IOCHK < 0 is due to EOF/EOR during read. IOCHK > 0 is due to error during read.

      DO I=1,2

         IF (OUNT(I) /= SC1) THEN

            IF (IOCHK < 0) THEN
               IF (REC_NO > 0) THEN
                  WRITE(OUNT(I),904) IOCHK, REC_NO, MESSAG
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15 )
               ELSE
                  WRITE(OUNT(I),905) IOCHK, MESSAG
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ENDIF
            ELSE IF (IOCHK > 0) THEN
               IF (REC_NO > 0) THEN
                  WRITE(OUNT(I),906) IOCHK, REC_NO, MESSAG(1:IEND)
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ELSE
                  WRITE(OUNT(I),907) IOCHK, MESSAG
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ENDIF
            ENDIF

         ELSE

            IF (IOCHK < 0) THEN
               IF (REC_NO > 0) THEN
                  WRITE(SC1,904) IOCHK, REC_NO, MESSAG
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ELSE
                  WRITE(SC1,905) IOCHK, MESSAG
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ENDIF
            ELSE IF (IOCHK > 0) THEN
               IF (REC_NO > 0) THEN
                  WRITE(SC1,906) IOCHK, REC_NO, MESSAG(1:IEND)
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ELSE
                  WRITE(SC1,907) IOCHK, MESSAG
                  CALL WRITE_FILNAM(FILNAM,OUNT(I), 15)
               ENDIF
            ENDIF

         ENDIF

         IF (OUNT(2) == OUNT(1)) THEN
            EXIT
         ENDIF

      ENDDO

      FATAL_ERR = FATAL_ERR + 1 
 
! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  904 FORMAT(' *ERROR   904: EOF/EOR WITH IOSTAT = ',I8,' ENCOUNTERED READING FROM THE FOLLOWING FILE ON RECORD NUMBER ',I8,       &
                           ' FOR DATA NAMED "',A,'"')

  905 FORMAT(' *ERROR   905: EOF/EOR WITH IOSTAT = ',I8,' ENCOUNTERED READING FROM THE FOLLOWING FILE FOR DATA NAMED "',A,'"')

  906 FORMAT(' *ERROR   906: ERR WITH IOSTAT = ',I8,' ENCOUNTERED READING FROM THE FOLLOWING FILE ON RECORD NUMBER ',I8,           &
                           ' FOR DATA NAMED "',A,'"')

  907 FORMAT(' *ERROR   907: ERR WITH IOSTAT = ',I8,' ENCOUNTERED READING FROM THE FOLLOWING FILE FOR DATA NAMED "',A,'"')

! **********************************************************************************************************************************
 
      END SUBROUTINE READERR
