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

      SUBROUTINE ALLOCATE_L1_MGG ( NAME, CALLING_SUBR )
 
!  Allocate some arrays for use in LINK1
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NTERM_MGG, NTERM_MGGC, NTERM_MGGE, NTERM_MGGS,            &
                                         TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_L1_MGG_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_MGG, I2_MGG, J_MGG, MGG, I_MGGC, J_MGGC, MGGC, I_MGGE, J_MGGE, MGGE, I_MGGS, J_MGGS, MGGS
 
      USE ALLOCATE_L1_MGG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_L1_MGG'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Name of matrix to be allocated
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(6*BYTE)               :: NAMEO             ! Array name (used for output error message)

      INTEGER(LONG)                   :: I                 ! DO loop index   
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NROWS             ! Number of rows in array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_L1_MGG_BEGEND

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

      IF (NAME == 'I2_MGG') THEN                           ! Allocate arrays for MGG

         NAMEO = 'I2_MGG'
         NROWS = NTERM_MGG
         IF (ALLOCATED(I2_MGG)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (I2_MGG(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  I2_MGG(I) = 0
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'MGGC') THEN                         ! Allocate arrays for MGGC

         NAMEO = 'I_MGGC'
         NROWS = NDOFG + 1
         IF (ALLOCATED(I_MGGC)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (I_MGGC(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  I_MGGC(I) = 1
               ENDDO
            ELSE
               NAMEO = 'I_MGGC'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAMEO = 'J_MGGC'
         NROWS = NTERM_MGGC
         IF (ALLOCATED(J_MGGC)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (J_MGGC(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  J_MGGC(I) = 0
               ENDDO
            ELSE
               NAMEO = 'J_MGGC'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAMEO = 'MGGC'
         NROWS = NTERM_MGGC
         IF (ALLOCATED(MGGC)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (MGGC(NROWS),STAT=IERR)
            MB_ALLOCATED = RDOUBLE*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  MGGC(I) = ZERO
               ENDDO
            ELSE
               NAMEO = 'MGGC'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'MGGE') THEN                         ! Allocate arrays for MGGE

         NAMEO = 'I_MGGE'
         NROWS = NDOFG+1
         IF (ALLOCATED(I_MGGE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (I_MGGE(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  I_MGGE(I) = 1
               ENDDO
            ELSE
               NAMEO = 'I_MGGE'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAMEO = 'J_MGGE'
         NROWS = NTERM_MGGE
         IF (ALLOCATED(J_MGGE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (J_MGGE(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  J_MGGE(I) = 0
               ENDDO
            ELSE
               NAMEO = 'J_MGGE'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAMEO = 'MGGE'
         NROWS = NTERM_MGGE
         IF (ALLOCATED(MGGE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (MGGE(NROWS),STAT=IERR)
            MB_ALLOCATED = RDOUBLE*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  MGGE(I) = ZERO
               ENDDO
            ELSE
               NAMEO = 'MGGE'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'MGGS') THEN                         ! Allocate arrays for MGGS

         NAMEO = 'I_MGGS'
         NROWS = NDOFG+1
         IF (ALLOCATED(I_MGGS)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (I_MGGS(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  I_MGGS(I) = 1
               ENDDO
            ELSE
               NAMEO = 'I_MGGS'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAMEO = 'J_MGGS'
         NROWS = NTERM_MGGS
         IF (ALLOCATED(J_MGGS)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (J_MGGS(NROWS),STAT=IERR)
            MB_ALLOCATED = RLONG*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  J_MGGS(I) = 0
               ENDDO
            ELSE
               NAMEO = 'J_MGGS'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAMEO = 'MGGS'
         NROWS = NTERM_MGGS
         IF (ALLOCATED(MGGS)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (MGGS(NROWS),STAT=IERR)
            MB_ALLOCATED = RDOUBLE*REAL(NROWS)/ONEPP6
            IF (IERR == 0) THEN
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
               DO I=1,NROWS
                  MGGS(I) = ZERO
               ENDDO
            ELSE
               NAMEO = 'MGGS'
               WRITE(ERR,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               WRITE(F06,991) MB_ALLOCATED,NAMEO,SUBR_NAME,IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE

         WRITE(ERR,915) SUBR_NAME, 'ALLOCATED', NAME
         WRITE(F06,915) SUBR_NAME, 'ALLOCATED', NAME
         FATAL_ERR = FATAL_ERR + 1
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
         WRITE(F04,9002) SUBR_NAME, TSEC
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
 
      END SUBROUTINE ALLOCATE_L1_MGG
