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
 
      SUBROUTINE CONVERT_INT_TO_CHAR ( INT_NUM, CHAR_VALUE )
 
! Convert an integer 1, 2, 3, 4, 5 or 6 to character '1', '2' ... '6'
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CONVERT_INT_TO_CHAR_BEGEND
 
      USE CONVERT_INT_TO_CHAR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CONVERT_INT_TO_CHAR'
      CHARACTER(1*BYTE), INTENT(OUT)  :: CHAR_VALUE        ! If INT_NUM = 1, then CHAR_VALUE = '1', etc
 
      INTEGER(LONG), INTENT(IN)       :: INT_NUM           ! Integer 1, 2, 3, 4, 5 O5 6
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CONVERT_INT_TO_CHAR_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      CHAR_VALUE = ' '

! Make sure that INT_NUM is in the range 1-6

      IF ((INT_NUM < 1) .OR. (INT_NUM > 6)) THEN
         WRITE(ERR,935) INT_NUM
         WRITE(F06,935) INT_NUM
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Convert the integer to character
 
      IF      (INT_NUM == 1) THEN
         CHAR_VALUE = '1'
      ELSE IF (INT_NUM == 2) THEN
         CHAR_VALUE = '2'
      ELSE IF (INT_NUM == 3) THEN
         CHAR_VALUE = '3'
      ELSE IF (INT_NUM == 4) THEN
         CHAR_VALUE = '4'
      ELSE IF (INT_NUM == 5) THEN
         CHAR_VALUE = '5'
      ELSE IF (INT_NUM == 6) THEN
         CHAR_VALUE = '6'
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  935 FORMAT(' *ERROR   935: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ILLEGAL INTEGER VALUE = ',I8,' INPUT. VALUE MUST BE AN INTEGER IN THE RANGE 1-6')   

! **********************************************************************************************************************************

      END SUBROUTINE CONVERT_INT_TO_CHAR
