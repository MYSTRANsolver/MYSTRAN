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

      SUBROUTINE ALLOCATE_FEMAP_DATA ( NAME_IN, NROWS, NCOLS, CALLING_SUBR )
 
! Allocate arrays for FEMAP neutral file
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_FEMAP_COLS, TOT_MB_MEM_ALLOC
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_FEMAP_DATA_BEGEND
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_VECS, FEMAP_EL_NUMS
 
      USE ALLOCATE_FEMAP_DATA_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_FEMAP_DATA'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Name
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this one
      CHARACTER(13*BYTE)              :: NAME              ! Array name of the matrix to be allocated in sparse format
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in array
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in array FEMAP_EL_VECS
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERR              ! STAT from ALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_FEMAP_DATA_BEGEND
 
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
      RDOUBLE = REAL(DOUBLE)
      RLONG   = REAL(LONG)

      MB_ALLOCATED = ZERO
      JERR = 0

! Make sure that NCOLS <= MAX_FEMAP_COLS, else error

      IF (NCOLS > MAX_FEMAP_COLS) THEN
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
         WRITE(ERR,9009) SUBR_NAME, NCOLS, MAX_FEMAP_COLS
         WRITE(F06,9009) SUBR_NAME, NCOLS, MAX_FEMAP_COLS
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Allocate arrays for FEMAP
 
      NAME = 'FEMAP_EL_NUMS'
      IF (ALLOCATED(FEMAP_EL_NUMS)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (FEMAP_EL_NUMS(NROWS,2),STAT=IERR)
         MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            DO I=1,NROWS
               FEMAP_EL_NUMS(I,1) = 0
               FEMAP_EL_NUMS(I,2) = 0
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

      NAME = 'FEMAP_EL_VECS'
      IF (ALLOCATED(FEMAP_EL_VECS)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (FEMAP_EL_VECS(NROWS,NCOLS),STAT=IERR)
         MB_ALLOCATED = RDOUBLE*REAL(NROWS)*REAL(NCOLS)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            DO I=1,NROWS
               DO J=1,NCOLS
                  FEMAP_EL_VECS(I,J) = ZERO
               ENDDO
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
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

 9009 FORMAT(' *ERROR  9009: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INTENT(IN) ARGUMENT NCOLS = ',I8,' MAY NOT BE GREATER THEN VARIABLE MAX_FEMAP_COLS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE ALLOCATE_FEMAP_DATA
