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
 
      SUBROUTINE RDOF ( INTDOF, CDOF )
 
! Convert DOF's from integer to char flag form. This version assumes that the data in INTDOF contains only digits 0-6.
! This subr is only used in situations where we know that this is the case. That is true since all DOF fields on any
! Bulk Data card are checked by subr IP6CHK for validity when the bulk data was read.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  RDOF_BEGEND
 
      USE RDOF_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RDOF'
      CHARACTER( 8*BYTE)              :: CINT              ! 8 char field of integers in INTDOF
      CHARACTER( 1*BYTE), INTENT(OUT) :: CDOF(6)           ! Contains 1 in each of the 6 pos'ns corresponding to a DOF from INTDOF
!                                                            and zero otherwise. For example, if INTDOF = 135 then CDOF = '101010'
 
      INTEGER(LONG), INTENT(IN)       :: INTDOF            ! Integer field which should contain only the digits 1 - 6
      INTEGER(LONG)                   :: I
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RDOF_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize CDOF

      DO I=1,6
         CDOF(I) = '0'
      ENDDO 

! Write INTDOF to CINT
 
      WRITE(CINT,'(I8)') INTDOF
 
! Process CINT to form CDOF
 
      DO I = 1,8
         IF      (CINT(I:I) == '0') THEN                   ! CINT = 0 is SPOINT so make it comp 1 same as T1 for physical grid
            CDOF(1) = '1'
         ELSE IF (CINT(I:I) == '1') THEN
            CDOF(1) = '1'
         ELSE IF (CINT(I:I) == '2') THEN
            CDOF(2) = '1'
         ELSE IF (CINT(I:I) == '3') THEN
            CDOF(3) = '1'
         ELSE IF (CINT(I:I) == '4') THEN
            CDOF(4) = '1'
         ELSE IF (CINT(I:I) == '5') THEN
            CDOF(5) = '1'
         ELSE IF (CINT(I:I) == '6') THEN
            CDOF(6) = '1'
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

      END SUBROUTINE RDOF
