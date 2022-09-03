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
 
      SUBROUTINE CLOSE_OUTFILES ( BUG_CLOSE_STAT, ERR_CLOSE_STAT, F04_CLOSE_STAT, OP2_CLOSE_STAT, PCH_CLOSE_STAT ) 
 
! Closes BUGFIL, ERRFIL, F04FIL, F06FIL
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE
      USE IOUNT1, ONLY                :  BUG   , ERR   , F04   , F06   , OP2   , PCH    ,SC1, WRT_LOG,                             &
                                         BUGFIL, ERRFIL, F04FIL, F06FIL, OP2FIL, PCHFIL

      USE CLOSE_OUTFILES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: BUG_CLOSE_STAT    ! Input value for close status for BUG
      CHARACTER(LEN=*), INTENT(IN)    :: ERR_CLOSE_STAT    ! Input value for close status for ERR
      CHARACTER(LEN=*), INTENT(IN)    :: F04_CLOSE_STAT    ! Input value for close status for F04
      CHARACTER(LEN=*), INTENT(IN)    :: OP2_CLOSE_STAT    ! Input value for close status for OP2
      CHARACTER(LEN=*), INTENT(IN)    :: PCH_CLOSE_STAT    ! Input value for close status for PCH

! **********************************************************************************************************************************
      ! close standard output files first
      IF (F06 /= SC1) THEN
         CALL FILE_CLOSE ( F06, F06FIL, 'KEEP', 'Y' )
      ENDIF

      IF (F04 /= SC1) THEN
         IF (WRT_LOG > 0) THEN
            CALL FILE_CLOSE ( F04, F04FIL, 'KEEP', 'Y' )
         ELSE
            CALL FILE_CLOSE ( F04, F04FIL, F04_CLOSE_STAT, 'Y' )
         ENDIF
      ENDIF

      IF (OP2 /= SC1) THEN
         CALL END_OP2_TABLES()
         CALL FILE_CLOSE ( OP2, OP2FIL, OP2_CLOSE_STAT, 'Y' )
      ENDIF

      IF (PCH /= SC1) THEN
         CALL FILE_CLOSE ( PCH, PCHFIL, PCH_CLOSE_STAT, 'Y' )
      ENDIF

      ! close error/log files last
      IF (BUG /= SC1) THEN
         CALL FILE_CLOSE ( BUG, BUGFIL, BUG_CLOSE_STAT, 'Y' )
      ENDIF

      IF (ERR /= SC1) THEN
         CALL FILE_CLOSE ( ERR, ERRFIL, ERR_CLOSE_STAT, 'Y' )
      ENDIF

! **********************************************************************************************************************************

      END SUBROUTINE CLOSE_OUTFILES
