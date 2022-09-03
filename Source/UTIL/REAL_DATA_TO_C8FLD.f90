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

      SUBROUTINE REAL_DATA_TO_C8FLD ( REAL_INP, CHAR8_OUT )

! Converts a real number to 8 character field:

!     -1.45367+05 converts to -1.453+5
!     -1.65743+12 converts to -1.65+12
!     +1.43678+05 converts to 1.4367+5
!     +1.43567+12 converts to 1.435+12

! This is used to create 8 char Bulk Data fields for real numbers with as many significant digits as possible. This subr is called
! when subr WRITE_PCOMP_EQUIV writes the PSHELL equivalent of a PCOMP to the F06 file (based on user request via Bulk Data PARAM
! PCOMPEQ

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE REAL_DATA_TO_C8FLD_USE_IFs                       ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER( 8*BYTE), INTENT(OUT) :: CHAR8_OUT         ! 8 character representation of REAL_INP
      CHARACTER(11*BYTE)              :: TEMP_CHAR         ! Temporary char field to store REAL_INP in 1ES11.4 format    

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IBEG,IEND         ! Locations in char strings

      REAL(DOUBLE), INTENT(IN)        :: REAL_INP          ! Double precision real input number to be converted

! **********************************************************************************************************************************
      WRITE(TEMP_CHAR,'(1ES11.4)') REAL_INP

      CHAR8_OUT(1:) = ' '

      IBEG = 1
      IF (TEMP_CHAR(1:1) == '-') THEN
         CHAR8_OUT(1:1) = '-'
         IBEG = IBEG + 1
      ENDIF
      CHAR8_OUT(IBEG:IBEG) = TEMP_CHAR(2:2)
      IBEG = IBEG + 1
      CHAR8_OUT(IBEG:IBEG) = '.'

      CHAR8_OUT(8:8) = TEMP_CHAR(11:11)
      IEND = 7
      IF (TEMP_CHAR(10:10) /= '0') THEN
         CHAR8_OUT(IEND:IEND) = TEMP_CHAR(10:10)
         IEND = IEND - 1
      ENDIF
      CHAR8_OUT(IEND:IEND) = TEMP_CHAR(9:9)
      IEND=IEND-1
      
      DO I=4,11
         IBEG = IBEG+1
         IF ((CHAR8_OUT(IBEG:IBEG) == '-') .OR. (CHAR8_OUT(IBEG:IBEG) == '+')) exit
         CHAR8_OUT(IBEG:IBEG) = TEMP_CHAR(I:I)
      ENDDO

! **********************************************************************************************************************************

      END SUBROUTINE REAL_DATA_TO_C8FLD

