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
 
      SUBROUTINE IP6CHK ( JCARDI, JCARDO, IP6TYP, TOTAL_NUM_DIGITS )
 
! Routine to check DOF component and PINFLG fields on Bulk Data cards to make sure that they contain valid entries:
! Output JCARDO is:

!   (1) If input JCARDI has valid 1,2,3,4,5, and/or 6 then output JCARDO has these integers in ascending order
!                        If   JCARDI = '  5 2  4'
!                        Then JCARDO = '245     '
!       NOTE: if a DOF number is repeated, that is OK (e.g. 1355 is the same as 135); however, a warning is printed

!   (2) If input JCARDI does not have valid 1-6, output JCARDO is left blank and an error is indicated in IP6TYP

! Output IP6TYP is a descriptor of JCARDI and is:

!   (1) 'BLANK   ' if JCARDI is blank
!   (2) 'COMP NOS' if JCARDI has 1,2,3,4,5, and/or 6's
!   (3) 'ZERO    ' if JCARDI has one zero  
!   (4) 'ERROR   ' if JCARDI has anything else

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  IP6CHK_BEGEND
 
      USE IP6CHK_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM))    :: SUBR_NAME = 'IP6CHK'
      CHARACTER(LEN=JCARD_LEN), INTENT(IN):: JCARDI            ! Input 8 character field
      CHARACTER(8*BYTE), INTENT(OUT)      :: IP6TYP            ! Descriptor of JCARDI, see above
      CHARACTER(LEN(JCARDI)), INTENT(OUT) :: JCARDO            ! Output 8 character field, described above
      CHARACTER(LEN(JCARDI))              :: JCARDO_TMP        ! Output 8 character field, described above
 
      INTEGER(LONG), INTENT(OUT)          :: TOTAL_NUM_DIGITS  ! Total of NUM_DIGITS(I)
      INTEGER(LONG)                       :: I                 ! DO loop index
      INTEGER(LONG)                       :: NUM_DIGITS(6)     ! NUM_DIGITS(I) is a count of the num of digits found in JCARDI
      INTEGER(LONG)                       :: POSN              ! An position in JCARDO
      INTEGER(LONG), PARAMETER            :: SUBR_BEGEND = IP6CHK_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      JCARDO(1:)     = ' '
      JCARDO_TMP(1:) = ' '
      IP6TYP(1:)     = ' '
      DO I=1,6
         NUM_DIGITS(I) = 0
      ENDDO

      TOTAL_NUM_DIGITS = 0

! Check for all blank input:
 
      IF (JCARDI == '        ') THEN
         IP6TYP = 'BLANK   '
  
! Check for only 1 zero in the field
  
      ELSE IF ((JCARDI == '0       ') .OR. (JCARDI == ' 0      ') .OR. (JCARDI == '  0     ') .OR. (JCARDI == '   0    ') .OR.  & 
               (JCARDI == '    0   ') .OR. (JCARDI == '     0  ') .OR. (JCARDI == '      0 ') .OR. (JCARDI == '       0')) THEN
         IP6TYP = 'ZERO    '
 
! Check for digits 1, 2, 3, 4, 5 and/or 6 (blanks among them are OK). If anything else found, then error.
 
      ELSE

i_loop:  DO I = 1,JCARD_LEN

            IF (JCARDI(I:I) == ' ') CYCLE i_loop

            IF      (JCARDI(I:I) == '1') THEN

               IP6TYP = 'COMP NOS'
               JCARDO_TMP(1:1) = '1'
               NUM_DIGITS(1) = NUM_DIGITS(1) + 1

            ELSE IF (JCARDI(I:I) == '2') THEN

               IP6TYP = 'COMP NOS'
               JCARDO_TMP(2:2) = '2'
               NUM_DIGITS(2) = NUM_DIGITS(2) + 1

            ELSE IF (JCARDI(I:I) == '3') THEN

               IP6TYP = 'COMP NOS'
               JCARDO_TMP(3:3) = '3'
               NUM_DIGITS(3) = NUM_DIGITS(3) + 1

            ELSE IF (JCARDI(I:I) == '4') THEN

               IP6TYP = 'COMP NOS'
               JCARDO_TMP(4:4) = '4'
               NUM_DIGITS(4) = NUM_DIGITS(4) + 1

            ELSE IF (JCARDI(I:I) == '5') THEN

               IP6TYP = 'COMP NOS'
               JCARDO_TMP(5:5) = '5'
               NUM_DIGITS(5) = NUM_DIGITS(5) + 1

            ELSE IF (JCARDI(I:I) == '6') THEN

               IP6TYP = 'COMP NOS'
               JCARDO_TMP(6:6) = '6'
               NUM_DIGITS(6) = NUM_DIGITS(6) + 1

            ELSE

               IP6TYP = 'ERROR   '
               JCARDO = '        '

               RETURN

            ENDIF

         ENDDO i_loop

         POSN = 0
         DO I=1,JCARD_LEN
            IF (JCARDO_TMP(I:I) == ' ') THEN
               CYCLE
            ELSE
               POSN = POSN + 1
               JCARDO(POSN:POSN) = JCARDO_TMP(I:I)
            ENDIF
         ENDDO
 
      ENDIF   
 
      IF ((NUM_DIGITS(1) > 1) .OR. (NUM_DIGITS(2) > 1) .OR. (NUM_DIGITS(3) > 1) .OR.                                               &
          (NUM_DIGITS(4) > 1) .OR. (NUM_DIGITS(5) > 1) .OR. (NUM_DIGITS(6) > 1)) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1738)
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1738)
         ENDIF
      ELSE
         DO I=1,6
            TOTAL_NUM_DIGITS = TOTAL_NUM_DIGITS + NUM_DIGITS(I)
         ENDDO
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1738 FORMAT(' *WARNING    : SOME DOF COMPONENT NUMBERS 1 THROUGH 6 ARE REPEATED ON ABOVE CARD') 

! **********************************************************************************************************************************
 
      END SUBROUTINE IP6CHK
