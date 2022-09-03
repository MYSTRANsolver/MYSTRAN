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

      SUBROUTINE DEALLOCATE_LINK9_STUF
 
! Deallocate some arrays used in LINK9
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_LINK9_STUF_BEGEND
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, EID_OUT_ARRAY, FTNAME, MSPRNT, OGEL, POLY_FIT_ERR,              &
                                         POLY_FIT_ERR_INDEX
 
      USE DEALLOCATE_LINK9_STUF_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_LINK9_STUF'
      CHARACTER(24*BYTE)              :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_LINK9_STUF_BEGEND
 
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

! Deallocate array GID_OUT_ARRAY
 
      IF (ALLOCATED(GID_OUT_ARRAY)) THEN
         DEALLOCATE (GID_OUT_ARRAY,STAT=IERR)
         NAME = 'GID_OUT_ARRAY'
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Deallocate array EID_OUT_ARRAY
 
      IF (ALLOCATED(EID_OUT_ARRAY)) THEN
         DEALLOCATE (EID_OUT_ARRAY,STAT=IERR)
         NAME = 'EID_OUT_ARRAY'
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Deallocate array FTNAME
 
      IF (ALLOCATED(FTNAME)) THEN
         DEALLOCATE (FTNAME,STAT=IERR)
         NAME = 'FTNAME                  '
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Deallocate array MSPRNT
 
      IF (ALLOCATED(MSPRNT)) THEN
         DEALLOCATE (MSPRNT,STAT=IERR)
         NAME = 'MSPRNT                  '
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Deallocate array OGEL
 
      IF (ALLOCATED(OGEL)) THEN
         DEALLOCATE (OGEL,STAT=IERR)
         NAME = 'OGEL                  '
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Deallocate array POLY_FIT_ERR
 
      IF (ALLOCATED(POLY_FIT_ERR)) THEN
         DEALLOCATE (POLY_FIT_ERR,STAT=IERR)
         NAME = 'POLY_FIT_ERR                  '
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Deallocate array POLY_FIT_ERR_INDEX
 
      IF (ALLOCATED(POLY_FIT_ERR_INDEX)) THEN
         DEALLOCATE (POLY_FIT_ERR_INDEX,STAT=IERR)
         NAME = 'POLY_FIT_ERR_INDEX                  '
         CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
         CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'DEALLOC', -CUR_MB_ALLOCATED, 0, 0, SUBR_BEGEND )
         IF (IERR /= 0) THEN
            WRITE(ERR,992) NAME,SUBR_NAME
            WRITE(F06,992) NAME,SUBR_NAME
            JERR = JERR + 1
         ENDIF
      ENDIF 
 
! Quit if there were errors

      IF (JERR /= 0) THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  992 FORMAT(' *ERROR   992: CANNOT DEALLOCATE MEMORY FROM ARRAY ',A,' IN SUBROUTINE ',A)
 
! **********************************************************************************************************************************
 
      END SUBROUTINE DEALLOCATE_LINK9_STUF
