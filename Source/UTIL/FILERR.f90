! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
 
      SUBROUTINE FILERR ( OUNT, WRITE_F04 )
 
! Writes message about file open errors and errors reading data
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  FILERR_BEGEND
 
      USE FILERR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FILERR'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)

      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FILERR_BEGEND

! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,2
         WRITE(OUNT(I),900)
         IF (OUNT(2) == OUNT(1)) EXIT
      ENDDO
 
! **********************************************************************************************************************************
  900 FORMAT(/,' PROCESSING TERMINATED DUE TO ABOVE FILE OPEN/READ ERRORS.')
 
! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE FILERR
