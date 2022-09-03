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
 
      SUBROUTINE R8FLD ( JCARDI, IFLD, R8INP )
 
! Reads 8 column field of REAL DOUBLE data
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  IERRFL, FATAL_ERR, JCARD_LEN
      USE CONSTANTS_1, ONLY           :  ZERO
 
      USE R8FLD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: JCARDI            ! The field of 8 characters to read
      CHARACTER( 1*BYTE)              :: DEC_PT            ! 'Y'/'N' indicator of whether a decimal point was founr in JCARDI
 
      INTEGER(LONG), INTENT(IN)       :: IFLD              ! Field (2 - 9) of a Bulk Data card to read
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error value from READ
 
      REAL(DOUBLE) , INTENT(OUT)      :: R8INP             ! The 8 byte real value read
 
! **********************************************************************************************************************************
      R8INP = ZERO
      IERRFL(IFLD) = 'N'

      READ(JCARDI,'(F16.0)',IOSTAT=IOCHK) R8INP

      IF (IOCHK /= 0) THEN
         IERRFL(IFLD) = 'Y'
         FATAL_ERR    = FATAL_ERR + 1
      ENDIF
 
! Scan to make sure there was a decimal point. Don't set IERRFL, since an error message is written here.

      IF (JCARDI /= '        ') THEN
         DEC_PT = 'N'
         DO I=1,JCARD_LEN
            IF (JCARDI(I:I) == '.') THEN
               DEC_PT = 'Y'
               EXIT
            ENDIF
         ENDDO
         IF (DEC_PT == 'N') THEN
            IERRFL(IFLD) = 'Y'
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1701) IFLD
            WRITE(F06,1701) IFLD
         ENDIF
      ENDIF
 

      RETURN
 
! **********************************************************************************************************************************
 1701 FORMAT(' *ERROR  1701: NO DECIMAL POINT WAS FOUND IN WHAT IS SUPPOSED TO BE A REAL    NUMBER IN FIELD ',I3,' OF THE',        &
                           ' PREVIOUS BULK DATA CARD') 

! **********************************************************************************************************************************
 
      END SUBROUTINE R8FLD
