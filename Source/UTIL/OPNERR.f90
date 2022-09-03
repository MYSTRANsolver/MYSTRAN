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
 
      SUBROUTINE OPNERR ( IOCHK, FILNAM, OUNT, WRITE_F04 )
 
! Prints error messages when IOSTAT is not zero on a file OPEN. 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F04FIL
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, RESTART
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  OPNERR_BEGEND
 
      USE OPNERR_USE_IFs

      IMPLICIT NONE

      LOGICAL                         :: FILE_EXIST        ! True if FILNAM exists
      LOGICAL                         :: FILE_OPENED       ! True if FILNAM is open

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'OPNERR'
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), INTENT(IN)       :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OPNERR_BEGEND
 
! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         INQUIRE (FILE=F04FIL,OPENED=FILE_OPENED)
         IF (.NOT.FILE_OPENED) THEN
            WRITE(F04,9001) SUBR_NAME,TSEC
         ENDIF
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! IOCHK < 0 is due to EOF/EOR during open. IOCHK > 0 is due to error during open.
 
      INQUIRE (FILE=FILNAM,OPENED=FILE_OPENED)
      INQUIRE (FILE=FILNAM, EXIST=FILE_EXIST)
      DO I=1,2

         WRITE(OUNT(I), * )

         IF (IOCHK < 0) THEN

            WRITE(OUNT(I),902) IOCHK
            CALL WRITE_FILNAM ( FILNAM, OUNT(I), 15 )

         ELSE

            IF (.NOT.FILE_EXIST) THEN

               WRITE(OUNT(I),903) IOCHK
               CALL WRITE_FILNAM ( FILNAM, OUNT(I), 15 )
               IF (RESTART == 'Y') THEN
                  WRITE(OUNT(I),9223)
               ELSE
                  WRITE(OUNT(I),9222)
               ENDIF

            ELSE IF (FILE_OPENED) THEN

               WRITE(OUNT(I),903) IOCHK
               CALL WRITE_FILNAM ( FILNAM, OUNT(I), 15 )
               WRITE(OUNT(I),9232)

            ELSE

               WRITE(OUNT(I),903) IOCHK
               CALL WRITE_FILNAM ( FILNAM, OUNT(I), 15 )
               WRITE(OUNT(I),9242)

            ENDIF               

         ENDIF

         WRITE(OUNT(I), * )

         IF (OUNT(2) == OUNT(1)) EXIT

      ENDDO

      FATAL_ERR = FATAL_ERR + 1
 
      IF (FILE_OPENED) THEN
         CALL OUTA_HERE ( 'N' )
      ENDIF

! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         INQUIRE (FILE=F04FIL,OPENED=FILE_OPENED)
         IF (.NOT.FILE_OPENED) THEN
            WRITE(F04,9002) SUBR_NAME,TSEC
         ENDIF
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  902 FORMAT(' *ERROR   902: EOF/EOR ENCOUNTERED WITH IOSTAT = ',I8,' OPENING FILE:')
 
  903 FORMAT(' *ERROR   903: ERROR ENCOUNTERED WITH IOSTAT = ',I8,' OPENING FILE:')

 9222 FORMAT('               THE FILE DOES NOT EXIST. THIS IS A PROGRAMMING ERROR.',/)

 9223 FORMAT('               THE FILE COULD NOT BE FOUND AND IS REQUIRED IN A RESTART. MAKE SURE THE ORIGINAL RUN PRODUCED AND',   &
                           ' SAVED THIS FILE',/)

 9232 FORMAT('               IT MAY BE OPEN IN ANOTHER PROGRAM. IF SO, CLOSE IT & START AGAIN.')

 9242 FORMAT('               THE FILE EXISTS AND IS NOT OPENED.  THIS IS A PROGRAMMING ERROR.')

! **********************************************************************************************************************************
 
      END SUBROUTINE OPNERR
