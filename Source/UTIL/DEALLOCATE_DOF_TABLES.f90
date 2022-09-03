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

      SUBROUTINE DEALLOCATE_DOF_TABLES ( NAME )
 
! DEallocate arrays used for MYSTRAN DOF tables
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_DOF_TABLES_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI, TDOF_ROW_START, TDOF, TSET
 
      USE DEALLOCATE_DOF_TABLES_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_DOF_TABLES'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name of the matrix to be allocated in sparse format
      CHARACTER(14*BYTE)              :: NAMEL             ! First 14 bytes of NAMEO
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_DOF_TABLES_BEGEND
 
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

!  Deallocate array for DOF sets

      IF (NAME == 'TSET') THEN
         IF (ALLOCATED(TSET)) THEN
            DEALLOCATE (TSET,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

!  Deallocate array for DOF table

      ELSE IF (NAME == 'TDOF') THEN
         IF (ALLOCATED(TDOF)) THEN
            DEALLOCATE (TDOF,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
	       WRITE(F06,992) NAME,SUBR_NAME
              JERR = JERR + 1
            ENDIF
         ENDIF
!  Deallocate array for TDOF_ROW_START

      ELSE IF (NAME == 'TDOF_ROW_START') THEN
         IF (ALLOCATED(TDOF_ROW_START)) THEN
            DEALLOCATE (TDOF_ROW_START,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
	       WRITE(F06,992) NAME,SUBR_NAME
              JERR = JERR + 1
            ENDIF
         ENDIF

!  Deallocate array for DOF inverse table

      ELSE IF (NAME == 'TDOFI') THEN
         IF (ALLOCATED(TDOFI)) THEN
            DEALLOCATE (TDOFI,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
	       WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
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

! **********************************************************************************************************************************
      CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )

      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         NAMEL(1:LEN(NAMEL)) = ' '
         NAMEL(1:) = NAME(1:)
         IF (DEBUG(107) == 0) THEN
            WRITE(F04,9003) SUBR_NAME, TSEC, -CUR_MB_ALLOCATED, NAMEL, TOT_MB_MEM_ALLOC
         ELSE
            WRITE(F04,9005) SUBR_NAME, TSEC, -CUR_MB_ALLOCATED, NAMEL, TOT_MB_MEM_ALLOC
         ENDIF
      ENDIF

      RETURN

! **********************************************************************************************************************************
  915 FORMAT(' *ERROR   915: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NAME OF ARRAY TO BE ',A,' IS INCORRECT. INPUT NAME WAS ',A)

  992 FORMAT(' *ERROR   992: CANNOT DEALLOCATE MEMORY FROM ARRAY ',A,' IN SUBROUTINE ',A)
 
 9003    FORMAT(1X,A,' END  ',F10.3,F13.3,' MB ',A15,':',39X,'T:',F10.3)

 9005    FORMAT(1X,A,' END  ',F10.3,F13.6,' MB ',A15,':',39X,'T:',F13.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE DEALLOCATE_DOF_TABLES 
