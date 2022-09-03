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
 
      SUBROUTINE RW_INCLUDE_FILES ( UNIT_IN, UNIT_OUT )
 
! Reads card images from INCLUDE files and writes them out to the file that will have the complete input data (DAT file + INCLUDE
! files entries) 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, FILE_NAM_MAXLEN, INCFIL
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC 
      USE SUBR_BEGEND_LEVELS, ONLY    :  RW_INCLUDE_FILES_BEGEND

      USE RW_INCLUDE_FILES_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RW_INCLUDE_FILES'
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD              ! Entry from INCL_FILNAM

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RW_INCLUDE_FILES_BEGEND
      INTEGER(LONG), INTENT(IN)       :: UNIT_IN           ! Unit number to read  INCLUDE entries from
      INTEGER(LONG), INTENT(IN)       :: UNIT_OUT          ! Unit number to write INCLUDE entries to
      INTEGER(LONG)                   :: ICNT        = 0   ! Counter
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when reading an entry from INCL_FILNAM
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      WRITE(UNIT_OUT,201,IOSTAT=IOCHK) INCFIL
      IF (IOCHK > 0) THEN
         WRITE(ERR,1029) INCFIL
         WRITE(F06,1029) INCFIL
         WRITE(F06,'(A)') INCFIL
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

      ICNT = 0

main: DO

         READ(UNIT_IN,101,IOSTAT=IOCHK) CARD
         ICNT = ICNT + 1

         IF (IOCHK < 0) THEN                               ! When end of file is encountered, no more entries to read
            IF (ICNT == 1) THEN
               WRITE(ERR,1041) INCFIL
               WRITE(F06,1041) INCFIL
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
            EXIT main
         ENDIF

         IF (IOCHK > 0) THEN

            WRITE(ERR,1010) ICNT, INCFIL
            WRITE(F06,1010) ICNT, INCFIL
            FATAL_ERR = FATAL_ERR + 1
            CYCLE

         ELSE

            WRITE(UNIT_OUT,101,IOSTAT=IOCHK) CARD
            IF (IOCHK > 0) THEN
               WRITE(ERR,1029) ICNT, INCFIL
               WRITE(F06,1029) ICNT, INCFIL
               FATAL_ERR = FATAL_ERR + 1
               CYCLE
            ENDIF

         ENDIF
 
      ENDDO main

      WRITE(UNIT_OUT,202,IOSTAT=IOCHK) INCFIL

      IF (IOCHK > 0) THEN
         WRITE(ERR,1029) ICNT, INCFIL
         WRITE(F06,1029) ICNT, INCFIL
         FATAL_ERR = FATAL_ERR + 1
      ELSE
         WRITE(F06,301) ICNT-1, INCFIL
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

  201 FORMAT('$ -----------------------------------------------------------------------------',/,'$ Beg INCLUDE file ',A)

  202 FORMAT('$ End INCLUDE file ',A,/,'$ -----------------------------------------------------------------------------')

  301 FORMAT(' *INFORMATION: THERE WERE ',I8,' LINE(S) INCLUDED FROM FILE: ',A)

 1010 FORMAT(' *ERROR  1010: ERROR READING ENTRY NUMBER ',I8,' FROM INCLUDE FILE: ',A)

 1029 FORMAT(' *ERROR  1029: ERROR WRITING ENTRY NUMBER ',I8,' FROM INCLUDE FILE: ',A)

 1041 FORMAT(' *ERROR  1041: NO DATA IN INCLUDE FILE: ',A)

! **********************************************************************************************************************************

      END SUBROUTINE RW_INCLUDE_FILES

