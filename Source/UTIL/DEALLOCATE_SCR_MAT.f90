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

      SUBROUTINE DEALLOCATE_SCR_MAT ( NAME_IN )
 
! Deallocates sparse CRS or sparse CCS scratch matrices
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC          
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_SCR_MAT_BEGEND
      USE SCRATCH_MATRICES , ONLY     :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CRS3, J_CRS3, CRS3,  &
                                         I_CCS1, J_CCS1, CCS1, I_CCS2, J_CCS2, CCS2, I_CCS3, J_CCS3, CCS3

      USE DEALLOCATE_SCR_MAT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_SCR_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name (used for output error message)
      CHARACTER(6*BYTE)               :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_SCR_MAT_BEGEND
 
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

      IF (NAME_IN == 'CRS1') THEN

         NAME = 'I_CRS1'
         IF (ALLOCATED(I_CRS1)) THEN
            DEALLOCATE (I_CRS1,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'J_CRS1'
         IF (ALLOCATED(J_CRS1)) THEN
            DEALLOCATE (J_CRS1,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'CRS1'
         IF (ALLOCATED(CRS1)) THEN
            DEALLOCATE (CRS1,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME_IN == 'CRS2') THEN

         NAME = 'I_CRS2'
         IF (ALLOCATED(I_CRS2)) THEN
            DEALLOCATE (I_CRS2,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'J_CRS2'
         IF (ALLOCATED(J_CRS2)) THEN
            DEALLOCATE (J_CRS2,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'CRS2'
         IF (ALLOCATED(CRS2)) THEN
            DEALLOCATE (CRS2,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME_IN == 'CRS3') THEN

         NAME = 'I_CRS3'
         IF (ALLOCATED(I_CRS3)) THEN
            DEALLOCATE (I_CRS3,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'J_CRS3'
         IF (ALLOCATED(J_CRS3)) THEN
            DEALLOCATE (J_CRS3,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'CRS3'
         IF (ALLOCATED(CRS3)) THEN
            DEALLOCATE (CRS3,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME_IN == 'CCS1') THEN

         NAME = 'I_CCS1'
         IF (ALLOCATED(I_CCS1)) THEN
            DEALLOCATE (I_CCS1,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'J_CCS1'
         IF (ALLOCATED(J_CCS1)) THEN
            DEALLOCATE (J_CCS1,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'CCS1'
         IF (ALLOCATED(CCS1)) THEN
            DEALLOCATE (CCS1,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME_IN == 'CCS2') THEN

         NAME = 'I_CCS2'
         IF (ALLOCATED(I_CCS2)) THEN
            DEALLOCATE (I_CCS2,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'J_CCS2'
         IF (ALLOCATED(J_CCS2)) THEN
            DEALLOCATE (J_CCS2,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'CCS2'
         IF (ALLOCATED(CCS2)) THEN
            DEALLOCATE (CCS2,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME_IN == 'CCS3') THEN

         NAME = 'I_CCS3'
         IF (ALLOCATED(I_CCS3)) THEN
            DEALLOCATE (I_CCS3,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'J_CCS3'
         IF (ALLOCATED(J_CCS3)) THEN
            DEALLOCATE (J_CCS3,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
         NAME = 'CCS3'
         IF (ALLOCATED(CCS3)) THEN
            DEALLOCATE (CCS3,STAT=IERR)
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            ELSE
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE                                                 ! NAME not recognized, so coding error

         WRITE(ERR,915) SUBR_NAME, 'DEALLOCATED', NAME_IN
         WRITE(F06,915) SUBR_NAME, 'DEALLOCATED', NAME_IN
         FATAL_ERR = FATAL_ERR + JERR
         JERR = JERR + 1

      ENDIF
 
! Quit if there were errors

      IF (JERR /= 0) THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME, TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  915 FORMAT(' *ERROR   915: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NAME OF ARRAY TO BE ',A,' IS INCORRECT. INPUT NAME WAS ',A)

  992 FORMAT(' *ERROR   992: CANNOT DEALLOCATE MEMORY FROM ARRAY ',A,' IN SUBROUTINE ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE DEALLOCATE_SCR_MAT
