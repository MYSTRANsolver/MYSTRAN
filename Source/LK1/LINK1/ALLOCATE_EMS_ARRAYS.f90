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

      SUBROUTINE ALLOCATE_EMS_ARRAYS ( CALLING_SUBR )  
 
!  Allocate some arrays for use in LINK1
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, ONEPP6
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LINKNO, LTERM_MGGE, NDOFG, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE EMS_ARRAYS, ONLY            :  EMS, EMSCOL, EMSKEY, EMSPNT
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_EMS_ARRAYS_BEGEND
 
      USE ALLOCATE_EMS_ARRAYS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_EMS_ARRAYS'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER( 6*BYTE)              :: NAME              ! Array name (used for output error message)
 
      INTEGER(LONG)                   :: I                 ! DO loop index   
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NROWS             ! Number of rows for matrix NAME
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_EMS_ARRAYS_BEGEND

      REAL(DOUBLE)                    :: CUR_MB_ALLOCATED  ! MB of memory that is currently allocated to ARRAY_NAME when subr
!                                                            ALLOCATED_MEMORY is called (before entering MB_ALLOCATED into array
!                                                            ALLOCATED_ARRAY_MEM
      REAL(DOUBLE)                    :: MB_ALLOCATED      ! Megabytes of mmemory allocated for the arrays to put into array
!                                                            ALLOCATED_ARRAY_MEM when subr ALLOCATED_MEMORY is called
      REAL(DOUBLE)                    :: RDOUBLE           ! Real value of DOUBLE
      REAL(DOUBLE)                    :: RLONG             ! Real value of LONG

      INTRINSIC                       :: REAL

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

      RDOUBLE = REAL(DOUBLE)
      RLONG   = REAL(LONG)

      MB_ALLOCATED = ZERO
      JERR = 0

! Allocate array EMSKEY

      NAME = 'EMSKEY'
      NROWS = NDOFG
      IF (ALLOCATED(EMSKEY)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (EMSKEY(NDOFG),STAT=IERR)
         MB_ALLOCATED = RLONG*REAL(NDOFG)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            WRITE(SC1,22345,ADVANCE='NO') NAME, NDOFG, CR13
            DO I=1,NDOFG
!!             WRITE(SC1,12345,ADVANCE='NO') NAME, I, NDOFG, CR13
               EMSKEY(I) = 0
            ENDDO
            WRITE(SC1,*) CR13
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate array EMSCOL

      NAME = 'EMSCOL'
      NROWS = LTERM_MGGE
      IF (ALLOCATED(EMSCOL)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (EMSCOL(LTERM_MGGE),STAT=IERR)
         MB_ALLOCATED = RLONG*REAL(LTERM_MGGE)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            WRITE(SC1,22345,ADVANCE='NO') NAME, LTERM_MGGE, CR13
            DO I=1,LTERM_MGGE
!!             WRITE(SC1,12345,ADVANCE='NO') NAME, I, LTERM_MGGE, CR13
               EMSCOL(I) = 0
            ENDDO
            WRITE(SC1,*) CR13
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate array EMSPNT

      NAME = 'EMSPNT'
      NROWS = LTERM_MGGE
      IF (ALLOCATED(EMSPNT)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (EMSPNT(LTERM_MGGE),STAT=IERR)
         MB_ALLOCATED = RLONG*REAL(LTERM_MGGE)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            WRITE(SC1,22345,ADVANCE='NO') NAME, LTERM_MGGE, CR13
            DO I=1,LTERM_MGGE
!!             WRITE(SC1,12345,ADVANCE='NO') NAME, I, LTERM_MGGE, CR13
               EMSPNT(I) = 0
            ENDDO
            WRITE(SC1,*) CR13
        ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate array EMS

      NAME = 'EMS   '
      NROWS = LTERM_MGGE
      IF (ALLOCATED(EMS)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (EMS(LTERM_MGGE),STAT=IERR)
         MB_ALLOCATED = RDOUBLE*REAL(LTERM_MGGE)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            WRITE(SC1,22345,ADVANCE='NO') NAME, LTERM_MGGE, CR13
            DO I=1,LTERM_MGGE
!!             WRITE(SC1,12345,ADVANCE='NO') NAME, I, LTERM_MGGE, CR13
               EMS(I) = ZERO
            ENDDO
            WRITE(SC1,*) CR13
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

      WRITE(SC1,*) CR13

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
  990 FORMAT(' *ERROR   990: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT ALLOCATE MEMORY TO ARRAY ',A,'. IT IS ALREADY ALLOCATED')

  991 FORMAT(' *ERROR   991: CANNOT ALLOCATE ',F10.3,' MB OF MEMORY TO ARRAY ',A,' IN SUBROUTINE ',A                               &
                    ,/,14X,' ALLOCATION STAT = ',I8)

 1092 FORMAT(1X,I2,'/',A44,18X,2X,I2,':',I2,':',I2,'.',I3)

 1699 FORMAT('               THE SUBR IN WHICH THESE ERRORS WERE FOUND (',A,') WAS CALLED BY SUBR ',A)

12345 FORMAT(5X,'array ',A6,' row ',I8,' of ',I8, A, A)

22345 FORMAT(5X,'Initializing ',A,' with ',I12,' rows', A, A)

! **********************************************************************************************************************************
 
      END SUBROUTINE ALLOCATE_EMS_ARRAYS  

