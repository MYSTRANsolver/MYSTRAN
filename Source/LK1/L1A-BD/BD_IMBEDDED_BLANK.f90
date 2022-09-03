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
  
      SUBROUTINE BD_IMBEDDED_BLANK ( JCARD, CF2, CF3, CF4, CF5, CF6, CF7, CF8, CF9 )
  
! Prepares message when some fields of a B.D card have imbedded blanks when they should not (but field can be completely blank)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_IMBEDDED_BLANK_BEGEND
 
      USE BD_IMBEDDED_BLANK_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_IMBEDDED_BLANK'
      CHARACTER(LEN=*), INTENT(IN)    :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 1*BYTE)              :: IMB_BLANK(2:9)    ! 'Y'/'N' indicator of whether fields 2-9 have imbedded blanks
 
      INTEGER(LONG), INTENT(IN)       :: CF2               ! = 2 if field 2 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF3               ! = 3 if field 3 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF4               ! = 4 if field 4 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF5               ! = 5 if field 5 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF6               ! = 6 if field 6 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF7               ! = 7 if field 7 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF8               ! = 8 if field 8 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF9               ! = 9 if field 9 is to be checked, or 0 otherwise
      INTEGER(LONG)                   :: CHK_FLD(2:9)      ! Array containing CF2 through CF9
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: JCARDI_BEG        ! Position where data begins in one JCARD
      INTEGER(LONG)                   :: JCARDI_END        ! Position where data ends   in one JCARD
      INTEGER(LONG)                   :: NUMBER(2:9)       ! Number of imbedded blanks found in a Bulk Data card field
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_IMBEDDED_BLANK_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Load CF2 through CF9 into array CHK_FLD

      CHK_FLD(2) = CF2
      CHK_FLD(3) = CF3
      CHK_FLD(4) = CF4
      CHK_FLD(5) = CF5
      CHK_FLD(6) = CF6
      CHK_FLD(7) = CF7
      CHK_FLD(8) = CF8
      CHK_FLD(9) = CF9

! Initialize IMB_BLANK array

      DO I=2,9
         IMB_BLANK(I) = 'N'
      ENDDO 

! Find beginning and end of data in 

! Check fields for any imbedded blanks and set error if so

      DO I=2,9

         JCARDI_BEG = 1
         JCARDI_END = JCARD_LEN

         NUMBER(I) = 0

         IF      (CHK_FLD(I) == 0) THEN                    ! We don't want to check field I

            CYCLE

         ELSE IF (CHK_FLD(I) == I) THEN                    ! We do want to check field I

            IF (JCARD(I)(1:) /= ' ') THEN                  ! Don't need to check blank fields

j_do1:         DO J=1,JCARD_LEN                            ! Find where data begins in this field
                  IF (JCARD(I)(J:J) == ' ') THEN
                     CYCLE j_do1
                  ELSE
                     JCARDI_BEG = J
                     EXIT j_do1
                  ENDIF
               ENDDO j_do1 

j_do2:         DO J=JCARD_LEN,1,-1                         ! Find where data ends in this field
                  IF (JCARD(I)(J:J) == ' ') THEN
                     CYCLE j_do2
                  ELSE
                     JCARDI_END = J
                     EXIT j_do2
                  ENDIF
               ENDDO j_do2

               IMB_BLANK(I) = 'N'                          ! Check between data begin/end for blanks
               DO J=JCARDI_BEG,JCARDI_END
                  IF(JCARD(I)(J:J) == ' ') THEN
                     IMB_BLANK(I) = 'Y'
                     NUMBER(I)    = NUMBER(I) + 1
                  ENDIF
               ENDDO

            ENDIF

         ELSE                                              ! Coding error, CHK_FLD(I) must be 0 or I

            WRITE(ERR,1121) SUBR_NAME, CHK_FLD(I)
            WRITE(F06,1121) SUBR_NAME, CHK_FLD(I)
            CALL OUTA_HERE ( 'Y' )

         ENDIF

      ENDDO

! Write error if fields checked have imbedded blanks

      DO I=2,9
         IF(IMB_BLANK(I) == 'Y') THEN
            WRITE(ERR,1122) NUMBER(I),I,JCARD(1),JCARD(2)
            WRITE(F06,1122) NUMBER(I),I,JCARD(1),JCARD(2)
            FATAL_ERR = FATAL_ERR + 1
            CYCLE
         ENDIF
      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1121 FORMAT(' *ERROR  1121: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CHK_FLD(I) MUST BE EITHER 0 OR I BUT IS ',I8)

 1122 FORMAT(' *ERROR  1122: THERE WERE ',I2,' IMBEDDED BLANKS FOUND IN FIELD ',I2,' OF BULK DATA ENTRY ',A,' ',A,                 &
                                            '. IMBEDDED BLANKS NOT ALLOWED HERE')
 

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_IMBEDDED_BLANK
