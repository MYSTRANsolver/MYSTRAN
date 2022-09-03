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

      SUBROUTINE I4FLD ( JCARDI, IFLD, I4INP )

! Reads 8 column field of INTEGER*4 data

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  IERRFL, FATAL_ERR, JCARD_LEN, MAX_INTEGER_LEN

      USE I4FLD_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: JCARDI            ! The field of 8 characters to read
      CHARACTER( 1*BYTE)              :: DEC_PT            ! 'Y'/'N' indicator of whether a decimal point was founr in JCARDI

      INTEGER(LONG), INTENT(IN)       :: IFLD              ! Field (2 - 9) of a Bulk Data card to read
      INTEGER(LONG), INTENT(OUT)      :: I4INP             ! The 4 byte integer value read
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: INTEGER_LEN       ! Length in digits of the integer in JCARDI
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error value from READ

! **********************************************************************************************************************************
      I4INP = 0
      IERRFL(IFLD) = 'N'

! Make sure integer number is not larger than (2^32)/2 = 2,147,483,648. For conservatism do not allow integers with more than 10
! digits and if it has 10 digits, make sure that the leading digit is <= 2

      INTEGER_LEN = JCARD_LEN
      DO I=JCARD_LEN,1,-1
         IF (JCARDI(I:I) == ' ') THEN
            INTEGER_LEN = INTEGER_LEN - 1
         ELSE
            EXIT
         ENDIF
      ENDDO

      IF (INTEGER_LEN > MAX_INTEGER_LEN) THEN              ! First, make sure integer has no more than 10 digits
         IERRFL(IFLD) = 'Y'
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1709) IFLD, INTEGER_LEN, MAX_INTEGER_LEN
         WRITE(F06,1709) IFLD, INTEGER_LEN, MAX_INTEGER_LEN

      ELSE IF (INTEGER_LEN == MAX_INTEGER_LEN) THEN

         IF ((JCARDI(1:1) == '1') .OR. (JCARDI(1:1) == '2')) THEN
            CONTINUE
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1710) IFLD, JCARDI
            WRITE(F06,1710) IFLD, JCARDI
         ENDIF

      ENDIF

! Read integer data in JCARDI

      READ(JCARDI,'(I8)',IOSTAT=IOCHK) I4INP

      IF (IOCHK /= 0) THEN
         IERRFL(IFLD) = 'Y'
         FATAL_ERR    = FATAL_ERR + 1
      ENDIF

! Scan to make sure there was not a decimal point. Don't set IERRFL, since an error message is written here.

      IF (JCARDI /= '        ') THEN
         DEC_PT = 'N'
         DO I=1,JCARD_LEN
            IF (JCARDI(I:I) == '.') THEN
               DEC_PT = 'Y'
               EXIT
            ENDIF
         ENDDO
         IF (DEC_PT == 'Y') THEN
            IERRFL(IFLD) = 'Y'
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1700) IFLD
            WRITE(F06,1700) IFLD
         ENDIF
      ENDIF
 
      RETURN
 
! **********************************************************************************************************************************
 1700 FORMAT(' *ERROR  1700: A  DECIMAL POINT WAS FOUND IN WHAT IS SUPPOSED TO BE A INTEGER NUMBER IN FIELD ',I3,' OF THE',        &
                           ' PREVIOUS BULK DATA CARD') 

 1709 FORMAT(' *ERROR  1709: FIELD ',I3,' OF THE PREVIOUS ENTRY HAS AN INTEGER WITH ',I3,' DIGITS. ALL INTEGERS MUST HAVE NO MORE',&
                           ' THAN ',I3,' DIGITS')

 1710 FORMAT(' *ERROR  1710: FIELD ',I3,' OF THE PREVIOUS ENTRY HAS AN INTEGER = ',A,' GREATER THAN 2,000,000,000. NOT ALLOWED')

! **********************************************************************************************************************************

      END SUBROUTINE I4FLD
