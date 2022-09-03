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

      SUBROUTINE DEALLOCATE_STF_ARRAYS ( NAME )
 
!  Deallocate some arrays used in LINK1
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_STF_ARRAYS_BEGEND
      USE STF_ARRAYS, ONLY            :  STFCOL, STFKEY, STFPNT, STF, STF3
 
      USE DEALLOCATE_STF_ARRAYS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_STF_ARRAYS'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_STF_ARRAYS_BEGEND

      REAL(DOUBLE)                    :: CUR_MB_ALLOCATED  ! MB of memory that is currently allocated to ARRAY_NAME when subr
!                                                            ALLOCATED_MEMORY is called (before entering MB_ALLOCATED into array
!                                                            ALLOCATED_ARRAY_MEM

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      JERR = 0

      IF      (NAME == 'STFKEY') THEN

         IF (ALLOCATED(STFKEY)) THEN
            DEALLOCATE (STFKEY,STAT=IERR)
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE IF (NAME == 'STFCOL') THEN

         IF (ALLOCATED(STFCOL)) THEN
            DEALLOCATE (STFCOL,STAT=IERR)
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE IF (NAME == 'STFPNT') THEN

         IF (ALLOCATED(STFPNT)) THEN
            DEALLOCATE (STFPNT,STAT=IERR)
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE IF (NAME == 'STF   ') THEN

         IF (ALLOCATED(STF)) THEN
            DEALLOCATE (STF,STAT=IERR)
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE IF (NAME == 'STF3') THEN

         IF (ALLOCATED(STF3)) THEN
            DEALLOCATE (STF3,STAT=IERR)
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE

         WRITE(ERR,915) SUBR_NAME, 'DEALLOCATED', NAME
         WRITE(F06,915) SUBR_NAME, 'DEALLOCATED', NAME
         FATAL_ERR = FATAL_ERR + JERR
         JERR = JERR + 1

      ENDIF

! Quit if there were errors

      IF (JERR /= 0) THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      WRITE(SC1,12345,ADVANCE='NO') CUR_MB_ALLOCATED, NAME, CR13
      WRITE(F06,1702) CUR_MB_ALLOCATED, NAME

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  915 FORMAT(' *ERROR   915: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NAME OF ARRAY TO BE ',A,' IS INCORRECT. INPUT NAME WAS ',A)

  992 FORMAT(' *ERROR   992: CANNOT DEALLOCATE MEMORY FROM ARRAY ',A,' IN SUBROUTINE ',A)

 1702 FORMAT('               DEALLOCATED ',1ES9.2,' MB MEMORY FROM ARRAY ',A)

12345 FORMAT(5X,'Deallocated ',1ES9.2,' MB of mem from array: ',A, A)

! **********************************************************************************************************************************
 
      END SUBROUTINE DEALLOCATE_STF_ARRAYS  
