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

! ##################################################################################################################################

      SUBROUTINE GET_INI_FILNAM  ( MYSTRAN_DIR, MYSTRAN_DIR_LEN, INIFIL_NAME_LEN )

! Gets name (incl path) of the MYSTRAN.INI initialization file.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, F04, INIFIL, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, PROG_NAME
      USE TIMDAT, ONLY                :  TSEC

      USE GET_INI_FILNAM_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(PROG_NAME)+4) :: FILNAM            ! File name for MYSTRAN initialization file (not including path)

                                                           ! Directory where MYSTRAN executable (and INI file) exist
      CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(IN):: MYSTRAN_DIR
      CHARACTER :: OS_DIR_SEP = '/'                        ! Directory separator

      INTEGER(LONG), INTENT(IN)       :: MYSTRAN_DIR_LEN   ! Length of MYSTRAN_DIR (not including trailing blanks)
      INTEGER(LONG), INTENT(OUT)      :: INIFIL_NAME_LEN   ! Length of INI file name (incl path)

! **********************************************************************************************************************************
      FILNAM( 1:LEN(PROG_NAME)) = PROG_NAME
      FILNAM(LEN(PROG_NAME)+1:LEN(PROG_NAME)+4) = '.INI'

      IF (MYSTRAN_DIR_LEN > 0) THEN
         INIFIL(1:MYSTRAN_DIR_LEN)  = MYSTRAN_DIR
         INIFIL(MYSTRAN_DIR_LEN+1:MYSTRAN_DIR_LEN+2)  = OS_DIR_SEP
         INIFIL(MYSTRAN_DIR_LEN+2:) = FILNAM
         INIFIL_NAME_LEN = MYSTRAN_DIR_LEN +1 + LEN(PROG_NAME) + 4
      ELSE
         INIFIL = FILNAM
         INIFIL_NAME_LEN = LEN(PROG_NAME) + 4
      ENDIF

! **********************************************************************************************************************************

      END SUBROUTINE GET_INI_FILNAM