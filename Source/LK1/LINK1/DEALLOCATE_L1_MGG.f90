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

      SUBROUTINE DEALLOCATE_L1_MGG ( NAME_IN ) 
 
!  Deallocate some arrays used in LINK1
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_L1_MGG_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_MGG, I2_MGG, J_MGG, MGG, I_MGGC, J_MGGC, MGGC, I_MGGE, J_MGGE, MGGE, I_MGGS, J_MGGS, MGGS
 
      USE DEALLOCATE_L1_MGG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_L1_MGG'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Name of matrix to be allocated
      CHARACTER(6*BYTE)               :: NAME              ! Name of matrix to be allocated
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_L1_MGG_BEGEND

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

      IF (NAME_IN == 'I2_MGG') THEN

         IF (ALLOCATED(I2_MGG)) THEN
            DEALLOCATE (I2_MGG,STAT=IERR)
            NAME = 'I2_MGG'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE IF (NAME_IN == 'MGGC') THEN

         IF (ALLOCATED(I_MGGC)) THEN
            DEALLOCATE (I_MGGC,STAT=IERR)
            NAME = 'I_MGGC'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

         IF (ALLOCATED(J_MGGC)) THEN
            DEALLOCATE (J_MGGC,STAT=IERR)
            NAME = 'J_MGGC'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

         IF (ALLOCATED(MGGC)) THEN
            DEALLOCATE (MGGC,STAT=IERR)
            NAME = 'MGGC'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

      ELSE IF (NAME_IN == 'MGGE') THEN

         IF (ALLOCATED(I_MGGE)) THEN
            DEALLOCATE (I_MGGE,STAT=IERR)
            NAME = 'I_MGGE'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

         IF (ALLOCATED(J_MGGE)) THEN
            DEALLOCATE (J_MGGE,STAT=IERR)
            NAME = 'J_MGGE'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

         IF (ALLOCATED(MGGE)) THEN
            DEALLOCATE (MGGE,STAT=IERR)
            NAME = 'MGGE'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(F06,992) NAME, SUBR_NAME
            ENDIF 
         ENDIF

      ELSE IF (NAME_IN == 'MGGS') THEN

         IF (ALLOCATED(I_MGGS)) THEN
            DEALLOCATE (I_MGGS,STAT=IERR)
            NAME = 'I_MGGS'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

         IF (ALLOCATED(J_MGGS)) THEN
            DEALLOCATE (J_MGGS,STAT=IERR)
            NAME = 'J_MGGS'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               WRITE(F06,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF 
         ENDIF

         IF (ALLOCATED(MGGS)) THEN
            DEALLOCATE (MGGS,STAT=IERR)
            NAME = 'MGGS'
            CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME, SUBR_NAME
               JERR = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(F06,992) NAME, SUBR_NAME
            ENDIF 
         ENDIF

      ELSE

         WRITE(ERR,915) SUBR_NAME, 'DEALLOCATED', NAME_IN
         WRITE(F06,915) SUBR_NAME, 'DEALLOCATED' ,NAME_IN
         FATAL_ERR = FATAL_ERR + 1
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
 
      END SUBROUTINE DEALLOCATE_L1_MGG
