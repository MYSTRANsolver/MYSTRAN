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

      SUBROUTINE ALLOCATE_IN4_FILES ( NAME_IN, NROWS, NCOLS, CALLING_SUBR )
 
! Allocate arrays for IN4 files (USERIN elements)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, IN4FIL, IN4FIL_NUM, LNUM_IN4_FILES, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE INPUTT4_MATRICES, ONLY      :  IN4_COL_MAP, IN4_MAT
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_IN4_FILES_BEGEND
 
      USE ALLOCATE_IN4_FILES_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_IN4_FILES'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name (used for output error message)
      CHARACTER(LEN=LEN(NAME_IN))     :: NAME              ! Specific array name used for output error message
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Nunber of rows in array NAME_IN being allocated
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Nunber of cols in array NAME_IN being allocated
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERR      = 0     ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR      = 0     ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_IN4_FILES_BEGEND

      REAL(DOUBLE)                    :: CUR_MB_ALLOCATED  ! MB of memory that is currently allocated to ARRAY_NAME when subr
!                                                            ALLOCATED_MEMORY is called (before entering MB_ALLOCATED into array
!                                                            ALLOCATED_ARRAY_MEM)
      REAL(DOUBLE)                    :: MB_ALLOCATED      ! Megabytes of mmemory allocated for the arrays to put into array
!                                                            ALLOCATED_ARRAY_MEM when subr ALLOCATED_MEMORY is called
 
      REAL(DOUBLE)                    :: RNCOLS            ! Real value of NCOLS
      REAL(DOUBLE)                    :: RNROWS            ! Real value of NROWS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      RNROWS = REAL(NROWS)
      RNCOLS = REAL(NCOLS)

      MB_ALLOCATED = ZERO
      JERR = 0

!  Allocate IN4 files

      IF (NAME_IN == 'IN4FIL') THEN

         NAME = 'IN4FIL'
         IF (ALLOCATED(IN4FIL)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (IN4FIL(LNUM_IN4_FILES),STAT=IERR)
            MB_ALLOCATED = REAL(DOUBLE)*REAL(LNUM_IN4_FILES)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, LNUM_IN4_FILES, 1, SUBR_BEGEND )
               DO I=1,LNUM_IN4_FILES
                  IN4FIL(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'IN4FIL_NUM'
         IF (ALLOCATED(IN4FIL_NUM)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (IN4FIL_NUM(LNUM_IN4_FILES),STAT=IERR)
            MB_ALLOCATED = REAL(DOUBLE)*REAL(LNUM_IN4_FILES)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, LNUM_IN4_FILES, 1, SUBR_BEGEND )
               DO I=1,LNUM_IN4_FILES
                  IN4FIL_NUM(I) = 0
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME_IN == 'IN4_MAT') THEN

         IF (ALLOCATED(IN4_MAT)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (IN4_MAT(NROWS,NCOLS),STAT=IERR)
            MB_ALLOCATED = REAL(DOUBLE)*RNROWS*RNCOLS/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS
                  DO J=1,NCOLS
                     IN4_MAT(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME_IN == 'IN4_COL_MAP') THEN

         NAME = NAME_IN
         IF (ALLOCATED(IN4_COL_MAP)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (IN4_COL_MAP(NROWS),STAT=IERR)
            MB_ALLOCATED = REAL(DOUBLE)*RNROWS/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  IN4_COL_MAP(I) = 0
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE                                                 ! NAME not recognized, so coding error

         WRITE(ERR,915) SUBR_NAME, 'ALLOCATED', NAME_IN 
         WRITE(F06,915) SUBR_NAME, 'ALLOCATED', NAME_IN
         FATAL_ERR = FATAL_ERR + JERR
         JERR = JERR + 1

      ENDIF
 
! Quit if there were errors

      IF (JERR /= 0) THEN
         WRITE(ERR,1699) TRIM(SUBR_NAME), CALLING_SUBR
         WRITE(F06,1699) SUBR_NAME, CALLING_SUBR
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
  915 FORMAT(' *ERROR   915: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NAME OF ARRAY TO BE ',A,' IS INCORRECT. INPUT NAME WAS ',A)

  990 FORMAT(' *ERROR   990: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT ALLOCATE MEMORY TO ARRAY ',A,'. IT IS ALREADY ALLOCATED')

  991 FORMAT(' *ERROR   991: CANNOT ALLOCATE ',F10.3,' MB OF MEMORY TO ARRAY ',A,' IN SUBROUTINE ',A                               &
                    ,/,14X,' ALLOCATION STAT = ',I8)

 1699 FORMAT('               THE SUBR IN WHICH THESE ERRORS WERE FOUND (',A,') WAS CALLED BY SUBR ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE ALLOCATE_IN4_FILES 
