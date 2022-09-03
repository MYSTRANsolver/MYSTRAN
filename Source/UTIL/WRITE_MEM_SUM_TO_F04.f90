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

      SUBROUTINE WRITE_MEM_SUM_TO_F04 ( NAME, WHAT, MB_MEM, NROWS, NCOLS, SUBR_BEGEND )

! Write info regarding matrices when they are allocated or deallocated to the F04 text output file

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  TOT_MB_MEM_ALLOC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name that has MB_ALLOCATED mem allocated
      CHARACTER(LEN=*), INTENT(IN)    :: WHAT              ! Whether to write allocated or deallocated memory
      CHARACTER(15*BYTE)              :: NAMEL             ! 14 bytes of NAME (or padded blanks - for F04 output)

      INTEGER(LONG)   , INTENT(IN)    :: NCOLS             ! Number of cols for matrix NAME
      INTEGER(LONG)   , INTENT(IN)    :: NROWS             ! Number of rows for matrix NAME
      INTEGER(LONG)   , INTENT(IN)    :: SUBR_BEGEND       ! SUBR_BEGEND value from calling subr

      REAL(DOUBLE)    , INTENT(IN)    :: MB_MEM            ! Megabytes of mmemory allocated to array NAME

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         NAMEL(1:) = ' '
         NAMEL(1:) = NAME(1:)
         IF      (WHAT == 'ALLOC') THEN
            IF (DEBUG(107) == 0) THEN
               WRITE(F04,9002) MB_MEM, NAMEL, NROWS, NCOLS, TOT_MB_MEM_ALLOC
            ELSE
               WRITE(F04,9004) MB_MEM, NAMEL, NROWS, NCOLS, TOT_MB_MEM_ALLOC
            ENDIF
         ELSE IF (WHAT == 'DEALLOC') THEN
            IF (DEBUG(107) == 0) then
               WRITE(F04,9003) MB_MEM, NAMEL,               TOT_MB_MEM_ALLOC
            ELSE
               WRITE(F04,9005) MB_MEM, NAMEL,               TOT_MB_MEM_ALLOC
            ENDIF
         ENDIF
      ENDIF

      RETURN
! **********************************************************************************************************************************
 9002 FORMAT(                48X,F13.3,' MB ',A15,':',I12,' row,',I12,' col    , T:',F10.3)

 9003 FORMAT(                48X,F13.3,' MB ',A15,':',39X,'T:',F10.3)

 9004 FORMAT(                48X,F13.6,' MB ',A15,':',I12,' row,',I12,' col    , T:',F13.6)

 9005 FORMAT(                48X,F13.6,' MB ',A15,':',39X,'T:',F13.6)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_MEM_SUM_TO_F04

