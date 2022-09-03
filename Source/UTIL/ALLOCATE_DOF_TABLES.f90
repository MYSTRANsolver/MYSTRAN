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

      SUBROUTINE ALLOCATE_DOF_TABLES ( NAME, CALLING_SUBR )
 
! Allocate arrays for MYSTRAN DOF tables
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, SIX, ONEPP6
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LDOFG, LGRID, MTDOF, MTSET, NUM_USET, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_DOF_TABLES_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START, TDOFI, TSET, USET
 
      USE ALLOCATE_DOF_TABLES_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_DOF_TABLES'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name of the matrix to be allocated in sparse format
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(14*BYTE)              :: NAMEL             ! First 14 bytes of NAME
 
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NROWS             ! Number of rows in array
      INTEGER(LONG)                   :: NCOLS             ! Number of cols in array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_DOF_TABLES_BEGEND
 
      REAL(DOUBLE)                    :: CUR_MB_ALLOCATED  ! MB of memory that is currently allocated to ARRAY_NAME when subr
!                                                            ALLOCATED_MEMORY is called (before entering MB_ALLOCATED into array
!                                                            ALLOCATED_ARRAY_MEM
      REAL(DOUBLE)                    :: MB_ALLOCATED      ! Megabytes of mmemory allocated for the arrays to put into array
!                                                            ALLOCATED_ARRAY_MEM when subr ALLOCATED_MEMORY is called

      INTRINSIC                       :: REAL

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      MB_ALLOCATED = ZERO
      JERR = 0

!  Allocate array for DOF sets

      IF (NAME == 'TSET') THEN

         NROWS = LGRID
         NCOLS = MTSET
         IF (ALLOCATED(TSET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TSET(NROWS,NCOLS),STAT=IERR)
            MB_ALLOCATED = REAL(LEN(TSET))*REAL(NROWS)*SIX/ONEPP6
            IF (IERR == 0) THEN
               DO I=1,NROWS
                  DO J=1,NCOLS
                     TSET(I,J) = '  '
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

!  Allocate array for DOF table

      ELSE IF (NAME == 'TDOF') THEN

         NROWS = LDOFG
         NCOLS = MTDOF
         IF (ALLOCATED(TDOF)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TDOF(NROWS,NCOLS),STAT=IERR)
            MB_ALLOCATED = REAL(LONG)*REAL(LGRID)*REAL(NCOLS)/ONEPP6
            IF (IERR == 0) THEN
               DO I=1,NROWS
                  DO J=1,NCOLS
                     TDOF(I,J) = 0
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

!  Allocate array for TDOF_ROW_START

      ELSE IF (NAME == 'TDOF_ROW_START') THEN

         NROWS = LDOFG
         NCOLS = MTDOF
         IF (ALLOCATED(TDOF_ROW_START)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TDOF_ROW_START(NROWS),STAT=IERR)
            MB_ALLOCATED = REAL(LONG)*REAL(LGRID)/ONEPP6
            IF (IERR == 0) THEN
               DO I=1,NROWS
                  TDOF_ROW_START(I) = 0
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

!  Allocate array for inverse DOF table

      ELSE IF (NAME == 'TDOFI') THEN

         NROWS = LDOFG
         NCOLS = MTDOF
         IF (ALLOCATED(TDOFI)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TDOFI(NROWS,NCOLS),STAT=IERR)
            MB_ALLOCATED = REAL(LONG)*REAL(LGRID)*REAL(NCOLS)/ONEPP6
            IF (IERR == 0) THEN
               DO I=1,NROWS
                  DO J=1,NCOLS
                     TDOFI(I,J) = 0
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'USET') THEN

         NROWS = LDOFG
         NCOLS = MTSET
         IF (ALLOCATED(USET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (USET(NROWS,NCOLS),STAT=IERR)
            MB_ALLOCATED = REAL(LONG)*REAL(NROWS)*REAL(NCOLS)/ONEPP6
            IF (IERR == 0) THEN
               DO I=1,NROWS
                  DO J=1,NCOLS
                     USET(I,J) = '  '
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE

         WRITE(ERR,915) SUBR_NAME, 'ALLOCATED', NAME 
         WRITE(F06,915) SUBR_NAME, 'ALLOCATED', NAME
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
      CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )

      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         NAMEL(1:LEN(NAMEL)) = ' '
         NAMEL(1:) = NAME(1:)
         IF (DEBUG(107) == 0) THEN
            WRITE(F04,9002) SUBR_NAME, TSEC, MB_ALLOCATED, NAMEL, NROWS, NCOLS, TOT_MB_MEM_ALLOC
         ELSE
            WRITE(F04,9004) SUBR_NAME, TSEC, MB_ALLOCATED, NAMEL, NROWS, NCOLS, TOT_MB_MEM_ALLOC
         ENDIF
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

 9002 FORMAT(1X,A,' END  ',F10.3,F13.3,' MB ',A15,':',I12,' row,',I12,' col    , T:',F10.3)

 9004 FORMAT(1X,A,' END  ',F10.3,F13.6,' MB ',A15,':',I12,' row,',I12,' col    , T:',F13.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE ALLOCATE_DOF_TABLES 
