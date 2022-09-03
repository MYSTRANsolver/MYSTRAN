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
 
      SUBROUTINE GET_FORMATTED_INTEGER ( INT, CHAR_INT, NUM_CHARS, NUM_DIGITS )
 
! Converts an integer to a character value with comma format (e.g. 12345 becomes char value 12,345) and writes result to unit UNT

      USE PENTIUM_II_KIND, ONLY             :  BYTE, LONG
      USE IOUNT1, ONLY                      :  WRT_LOG, F04
      USE SCONTR, ONLY                      :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                      :  TSEC 
      USE SUBR_BEGEND_LEVELS, ONLY          :  GET_FORMATTED_INTEGER_BEGEND

      USE GET_FORMATTED_INTEGER_USE_IFs

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER              :: WORD_LEN  = 13    ! Length of character string that INT will be entered into
!                                                                  This allows for a number up to 9,999,999,999

      CHARACTER(LEN=LEN(BLNK_SUB_NAM))      :: SUBR_NAME = 'GET_FORMATTED_INTEGER'
      CHARACTER(WORD_LEN*BYTE), INTENT(OUT) :: CHAR_INT          ! Integer formatted to have comma's (36879 becomes 36,879)
      CHARACTER(WORD_LEN*BYTE)              :: TEMP_CHAR_INT     ! Temporary value of CHAR_INT

      INTEGER(LONG), PARAMETER              :: SUBR_BEGEND = GET_FORMATTED_INTEGER_BEGEND
      INTEGER(LONG), INTENT(IN)             :: INT               ! Integer to be converted to formated value in CHAR_INT
      INTEGER(LONG), INTENT(OUT)            :: NUM_CHARS         ! Num of non blank chars in CHAR_INT after formatting w/ commas
      INTEGER(LONG), INTENT(OUT)            :: NUM_DIGITS        ! Number of digits in INT
      INTEGER(LONG)                         :: I,J,K             ! DO loop indices or counters

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      NUM_CHARS = WORD_LEN

      TEMP_CHAR_INT(1:) = ' '
      CHAR_INT(1:)      = ' '
      IF (WORD_LEN == 13) THEN                             ! This code to make sure the format of the WRITE is same length as 
         WRITE(TEMP_CHAR_INT,'(I13)') INT                  ! above declaratioin for INTEGER PARAMETER WORD_LEN
      ELSE
         CHAR_INT(1:) = '*'
         RETURN
      ENDIF

! Find out haw many digits are in INT

      DO I=WORD_LEN,1,-1
         IF (TEMP_CHAR_INT(I:I) == ' ') THEN
            NUM_DIGITS = WORD_LEN - I
            EXIT
         ENDIF
      ENDDO
         
! Move digits from TEMP_CHAR_INT to CHAR_INT inserting commas

      IF (NUM_DIGITS > 3) THEN

         K = WORD_LEN
         DO I=WORD_LEN,WORD_LEN-NUM_DIGITS,-3

            DO J=1,3
               CHAR_INT(K-J+1:K-J+1) = TEMP_CHAR_INT(I-J+1:I-J+1)
            ENDDO
            K = K - 3

            IF (TEMP_CHAR_INT(I-3:I-3) /= ' ') THEN
               CHAR_INT(K:K) = ','
               K = K - 1
            ENDIF

         ENDDO

      ELSE

         CHAR_INT(1:) = TEMP_CHAR_INT(1:)

      ENDIF
!xxLeft adjust CHAR_INT
!xx
!xx   TEMP_CHAR_INT(1:) = ' '
!xx   IF (CHAR_INT(1:1) == ' ') THEN                       ! We need to shift:
!xx
!xx      TEMP_CHAR_INT(1:) = CHAR_INT(1:)                  ! Set temporary field to CHR8_FLD
!xx
!xx      DO I = 2,WORD_LEN                                       ! Perform shift
!xx         IF (CHAR_INT(I:I) /= ' ') THEN
!xx            TEMP_CHAR_INT(1:) = CHAR_INT(I:)
!xx            EXIT
!xx         ENDIF
!xx      ENDDO
!xx
!xx      CHAR_INT(1:) = TEMP_CHAR_INT(1:)                  ! Reset CHR_FLD and return 
!xx
!xx   ENDIF   
!xx 
!xxCount nonblank characters in CHAR_INT
!xx
!xx   NUM_CHARS = WORD_LEN
!xx   DO I=WORD_LEN,1,-1
!xx      IF ((CHAR_INT(I:I) == ' ') .OR. (CHAR_INT(I:I) == ',')) THEN
!xx         CYCLE
!xx      ELSE
!xx         NUM_CHARS = WORD_LEN - I
!xx      write(f06,'(a,i3,a,a,a,i3)') ' I, CHAR_INT(I:I)= ', i, '  "', char_int(i:i), '"'
!xx      ENDIF
!xx   ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE GET_FORMATTED_INTEGER

