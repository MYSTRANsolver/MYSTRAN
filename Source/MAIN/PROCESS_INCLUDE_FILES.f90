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

      SUBROUTINE PROCESS_INCLUDE_FILES ( NUM_INCL_FILES )

! PROCESS_INCLUDE_FILES reads in the complete Bulk Data file, checks for INCLUDE files and calls 2 subrs to process them.
! At completion, a new data file is created (IN0FIL on unit IN0) which has the original Bulk Data file plus the contents of all of
! the INCLUDE files. This file then becomes the input file by re-opening it as INFILE (the normal input file)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, IN0, IN1, INC, INFILE, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  PROCESS_INCLUDE_FILES_BEGEND

      USE PROCESS_INCLUDE_FILES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PROCESS_INCLUDE_FILES'
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD              ! Exec Control deck card
      CHARACTER(LEN=EC_ENTRY_LEN)     :: CARD1             ! CARD shifted to begin in col 1

      INTEGER(LONG), INTENT(OUT)      :: NUM_INCL_FILES    ! Number of INCLUDE files in the Bulk Data file 
      INTEGER(LONG)                   :: CHAR_COL          ! Column number on CARD where character CHAR is found
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator.
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when reading a Case Control card from unit IN1
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PROCESS_INCLUDE_FILES_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

		NUM_INCL_FILES = 0

main: DO

         READ(IN1,101,IOSTAT=IOCHK) CARD
                                                           ! Exit when the complete Bulk Data file has been read
         IF (IOCHK < 0) THEN
            EXIT main
         ENDIF

         IF (IOCHK > 0) THEN                               ! Check if error occurs during read
				WRITE(ERR,1010)
				WRITE(F06,1010)
            WRITE(F06,'(A)') CARD
            FATAL_ERR = FATAL_ERR + 1
            CYCLE main
         ELSE
            WRITE(IN0,101,IOSTAT=IOCHK) CARD
         ENDIF

         CALL CSHIFT ( CARD, ' ', CARD1, CHAR_COL, IERR )

         IERR = 0
         IF (CARD1(1:7) == 'INCLUDE' )  THEN
            NUM_INCL_FILES = NUM_INCL_FILES + 1
            CALL READ_INCLUDE_FILNAM ( CARD1, IERR )
            IF (IERR == 0) THEN
               CALL RW_INCLUDE_FILES ( INC, IN0 )
            ENDIF
         ENDIF     

      ENDDO main

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1010 FORMAT(' *ERROR  1010: ERROR READING FOLLOWING ENTRY FROM BULK DATA FILE WHILE SEARCHING FOR INCLUDE FILES . ENTRY IGNORED')

 1011 FORMAT(' *ERROR  1011: NO ',A10,' ENTRY FOUND BEFORE END OF FILE OR END OF RECORD IN INPUT FILE')

! **********************************************************************************************************************************

      END SUBROUTINE PROCESS_INCLUDE_FILES
