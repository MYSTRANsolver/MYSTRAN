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

      SUBROUTINE READ_INPUT_FILE_NAME ( INI_EXIST )

! Gets the command line when MYSTRAN is invoked. This should contain the filname of the input file to be run. The default for the
! extension of rhe input file is DAT. If the default extension is being used, it does not have to be specified on the command line.
! If any extension other than the default is used it must be supplied with the filename on the command line. Examples of the command
! line are:

! If the input file is filename.dat, the user can invoke MYSTRAN from the directory in which that input file exists with:

!    MYSTRAN filenam

! If the input file is filename.bdf, then that complete name must be supplied

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, WRT_ERR, WRT_LOG, DEFDIR, DEF_INFILE_EXT, INFILE,               &
                                         LEN_INPUT_FNAME, SC1
      USE SCONTR, ONLY                :  PROG_NAME

      USE READ_INPUT_FILE_NAME_USE_IFs

      IMPLICIT NONE

      LOGICAL                         :: LEXIST

      CHARACTER( 1*BYTE), INTENT(IN)  :: INI_EXIST         ! 'Y' if file MYSTRAN.INI exists or 'N' otherwise
      CHARACTER( 1*BYTE)              :: CEXT              ! = 'Y' if there is an extension following a decimal point in FILNAM
      CHARACTER(LEN=LEN(INFILE))      :: FILNAM            ! File name 
      CHARACTER(LEN=LEN(INFILE))      :: DUMFIL            ! File name 
      CHARACTER( 1*BYTE)              :: POINT             ! = 'Y' if we find a decimal point in INFILE (FILNAM)

      INTEGER(LONG)                   :: LEXT              ! Length (chars) of input file extension
      INTEGER(LONG)                   :: NC_TOT            ! Total length (chars) of input file name including directory
      INTEGER(LONG)                   :: NC_DIR            ! Total length (chars) of input file name's directory
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: NC_FILNAM         ! Number of chars in FILNAM read on command line by subr READ_CL

      INTRINSIC                       :: INDEX

! **********************************************************************************************************************************
! If directory for files was input in MYSTRAN.INI, load it into the front end of INFILE. NC_TOT is a count on the
! total number of characters in INFILE (including the dir from MYSTRAN.INI as well as FILNAM from subr READ_CL).

      NC_TOT     = 0
      NC_DIR     = 0
      INFILE(1:) = ' '

      IF (INI_EXIST == 'Y') THEN                           ! Get DEFDIR length. Leading blanks were stripped of in subr READ_INI

         NC_DIR = LEN_TRIM ( DEFDIR )

         IF (NC_DIR > 0) THEN
            IF (NC_DIR+1 > FILE_NAM_MAXLEN) THEN           ! make sure that DEFDIR part of INFILE not too long up to this point
               INFILE(1:FILE_NAM_MAXLEN) = DEFDIR(1:FILE_NAM_MAXLEN)
               WRITE(SC1,1002) FILE_NAM_MAXLEN
               WRITE(SC1,'(A)') TRIM(INFILE)
               CALL OUTA_HERE ( 'Y' )
            ENDIF
            INFILE(1:NC_DIR) = DEFDIR(1:NC_DIR)            ! Set INFILE to DEFDIR
            IF (INFILE(NC_DIR:NC_DIR) /= '\') THEN         ! Add a '\' if INFILE (which is DEFDIR at this point) does not have one
               NC_DIR = NC_DIR + 1
               INFILE(NC_DIR:NC_DIR) = '\'
            ENDIF
         ENDIF
         NC_TOT = NC_DIR                                   ! This is length of DEFDIR including '\' (if needed)

      ENDIF   

      NC_FILNAM = 0
      FILNAM(1:) = ' '
      CALL READ_CL ( FILNAM, NC_FILNAM )
      IF (NC_FILNAM == 0) THEN
         WRITE(SC1,1006)
         WRITE(SC1,*)
         READ(*,'(A)') FILNAM
         DO I=FILE_NAM_MAXLEN,1,-1
            IF (FILNAM(I:I) == ' ') THEN
               CYCLE
            ELSE
               NC_FILNAM = I
               EXIT
            ENDIF
         ENDDO
      ENDIF

outer:DO                                                   ! Loop which sets filename, check if it exists and cycles back if not
         IF (NC_TOT + NC_FILNAM > FILE_NAM_MAXLEN) THEN
            WRITE(SC1,1002) FILE_NAM_MAXLEN
            WRITE(SC1,'(A)') TRIM(INFILE)
            CALL OUTA_HERE ( 'Y' )
         ENDIF
         INFILE(NC_TOT+1:) = FILNAM(1:)
         NC_TOT = NC_TOT + NC_FILNAM
  
         CEXT  = 'N'                                       ! If a file extension was not included, add '.DAT'
         POINT = 'N'
inner_1: DO I=NC_TOT,1,-1
            IF (INFILE(I:I) == ' ') THEN
               CYCLE inner_1
            ELSE IF (INFILE(I:I) == '.') THEN              ! '.' indicates there is an extension if non white space after it      
               POINT = 'Y'
               IF (INFILE(I+1:I+1) == ' ') THEN            ! Check if whote space or not after '.'
                  CEXT  = 'N'                              ! All white space after '.', so no file ext. is in INFILE at this point
               ELSE
                  CEXT = 'Y'                               ! Non-white space after '.', so file extension is in INFILE 
                  EXIT inner_1
               ENDIF
            ENDIF
         ENDDO inner_1
         IF (CEXT == 'N') THEN                             ! If there was no file name extension, add default extension 'DAT'
            IF ((NC_TOT+4) > FILE_NAM_MAXLEN) THEN
               WRITE(SC1,1002) FILE_NAM_MAXLEN
               CALL WRITE_FILNAM ( INFILE, SC1, 1 )
               CALL OUTA_HERE ( 'Y' )
            ELSE
               IF (POINT == 'Y') THEN
                  INFILE(NC_TOT+1:) = DEF_INFILE_EXT       ! Add extension
                  NC_TOT = NC_TOT + 3
               ELSE IF (POINT == 'N') THEN
                  INFILE(NC_TOT+1:) = '.' // DEF_INFILE_EXT
                  NC_TOT = NC_TOT + 4
               ENDIF 
            ENDIF
         ENDIF

         DUMFIL(1:) = ' '
inner_2: DO
            INQUIRE (FILE=INFILE,EXIST=LEXIST)             ! Check whether INFILE exists
            IF (.NOT.LEXIST) THEN                          ! INFILE seems to not exist, but maybe ext is "dat", not "DAT" so check
               IF (CEXT == 'N') THEN
                  DUMFIL(1:NC_TOT-3) = INFILE(1:NC_TOT-3)
                  DUMFIL(NC_TOT-2:NC_TOT) = 'dat'
                  INQUIRE (FILE=DUMFIL,EXIST=LEXIST)
                  IF (LEXIST) THEN
                     INFILE(1:) = DUMFIL(1:)
                     EXIT outer
                  ENDIF
               ENDIF 
               WRITE(SC1,1004)
               CALL WRITE_FILNAM ( INFILE, SC1, 1 )
               WRITE(SC1,1005)
               READ(*,'(A)') INFILE                        ! Get user input of complete file name FILNAM
               CYCLE inner_2
            ELSE
               EXIT outer
            ENDIF
         ENDDO inner_2

      ENDDO outer

! Count length of extension of INFILE (after '.'). File name must have an extension length of at least 1 character. 

      LEXT = 0
      DO I=NC_TOT,1,-1
         IF (INFILE(I:I) == '.') THEN
            EXIT
         ELSE
            LEXT = LEXT + 1
         ENDIF
      ENDDO
      LEN_INPUT_FNAME = NC_TOT - LEXT

      RETURN

! **********************************************************************************************************************************
 1002 FORMAT(' Input data file name was too long. max length is ',I8,' File name input was:')

 1004 FORMAT(' Input data file:')

 1005 FORMAT(' does not exist. Specify complete file name with drive and extension',/,                                             &
             ' or hit Ctrl-Break to start over.',/)

 1006 FORMAT(' Input the input data file name:')

! **********************************************************************************************************************************

      END SUBROUTINE READ_INPUT_FILE_NAME
