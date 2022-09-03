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

      SUBROUTINE ALLOCATE_CB_GRD_OTM ( NAME_IN )

! Calculates how many rows/cols are going to be needed for grid related OTM's (Output Transformation Matrices) for Craig-Bampton
! model generation runs and allocates memory to the arrays

      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC,                                                &
                                         GROUT_ACCE_BIT, GROUT_DISP_BIT, GROUT_SPCF_BIT, GROUT_MPCF_BIT,                           &
                                         IBIT, NDOFR, NGRID, NUM_CB_DOFS, NVEC,                                                    &
                                         NROWS_OTM_ACCE, NROWS_OTM_DISP, NROWS_OTM_MPCF, NROWS_OTM_SPCF,                           &
                                         NROWS_TXT_ACCE, NROWS_TXT_DISP, NROWS_TXT_MPCF, NROWS_TXT_SPCF
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE PARAMS, ONLY                :  OTMSKIP
      USE MODEL_STUF, ONLY            :  GRID, GROUT
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ACCE, OTM_DISP, OTM_MPCF, OTM_SPCF, TXT_ACCE, TXT_DISP, TXT_MPCF, TXT_SPCF
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_CB_GRD_OTM_BEGEND

      USE ALLOCATE_CB_GRD_OTM_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_COL_VEC'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name of the matrix to be allocated
      CHARACTER(LEN(NAME_IN))         :: NAME              ! Name for output error purposes

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IB                ! If > 0, there are displ or applied load output requests
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NCOLS             ! Number of cols in OTM matrix
      INTEGER(LONG)                   :: NROWS_MAT         ! Number of rows in OTM matrix
      INTEGER(LONG)                   :: NROWS_TXT         ! Number of rows in TXT mmatrix
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_CB_GRD_OTM_BEGEND

      REAL(DOUBLE)                    :: CUR_MB_ALLOCATED  ! MB of memory that is currently allocated to ARRAY_NAME when subr
!                                                            ALLOCATED_MEMORY is called (before entering MB_ALLOCATED into array
!                                                            ALLOCATED_ARRAY_MEM
      REAL(DOUBLE)                    :: MB_ALLOCATED      ! Megabytes of mmemory allocated for the arrays to put into array
!                                                            ALLOCATED_ARRAY_MEM when subr ALLOCATED_MEMORY is called

      INTRINSIC                       :: IAND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      JERR = 0

      IF (NAME_IN == 'OTM_ACCE') THEN

         NROWS_MAT = 0                                     ! Determine how many grids have accel output requested
         NROWS_TXT = 0
         DO I=1,NGRID
            IB = IAND(GROUT(I,1),IBIT(GROUT_ACCE_BIT))
            IF (IB > 0) THEN
               NROWS_MAT = NROWS_MAT + GRID(I,6)
               NROWS_TXT = NROWS_TXT + GRID(I,6) + OTMSKIP
            ENDIF
         ENDDO
         NROWS_OTM_ACCE = NROWS_MAT
         NROWS_TXT_ACCE = NROWS_TXT
         NCOLS = NUM_CB_DOFS

         NAME = 'OTM_ACCE'
         IF (ALLOCATED(OTM_ACCE)) THEN                     ! If not already allocated, allocate OTM_ACCE
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_ACCE(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_ACCE(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_ACCE'
         IF (ALLOCATED(TXT_ACCE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_ACCE(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_ACCE(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (NAME_IN == 'OTM_DISP') THEN

         NROWS_MAT = 0                                     ! Determine how many grids have accel output requested
         NROWS_TXT = 0
         DO I=1,NGRID
            IB = IAND(GROUT(I,1),IBIT(GROUT_DISP_BIT))
            IF (IB > 0) THEN
               NROWS_MAT = NROWS_MAT + GRID(I,6)
               NROWS_TXT = NROWS_TXT + GRID(I,6) + OTMSKIP
            ENDIF
         ENDDO
         NROWS_OTM_DISP = NROWS_MAT
         NROWS_TXT_DISP = NROWS_TXT
         NCOLS = NUM_CB_DOFS

         NAME = 'OTM_DISP'
         IF (ALLOCATED(OTM_DISP)) THEN                     ! If not already allocated, allocateDISP _OTM
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_DISP(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_DISP(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_DISP'
         IF (ALLOCATED(TXT_DISP)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_DISP(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_DISP(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (NAME_IN == 'OTM_MPCF') THEN

         NROWS_MAT = 0                                     ! Determine how many grids have accel output requested
         NROWS_TXT = 0
         DO I=1,NGRID
            IB = IAND(GROUT(I,1),IBIT(GROUT_MPCF_BIT))
            IF (IB > 0) THEN
               NROWS_MAT = NROWS_MAT + GRID(I,6)
               NROWS_TXT = NROWS_TXT + GRID(I,6) + OTMSKIP
            ENDIF
         ENDDO
         NROWS_OTM_MPCF = NROWS_MAT
         NROWS_TXT_MPCF = NROWS_TXT
         NCOLS = NUM_CB_DOFS

         MB_ALLOCATED = REAL(DOUBLE)*NROWS_MAT*NCOLS/ONEPP6

         NAME = 'OTM_MPCF'
         IF (ALLOCATED(OTM_MPCF)) THEN                     ! If not already allocated, allocate OTM_MPCF
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_MPCF(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_MPCF(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_MPCF'
         IF (ALLOCATED(TXT_MPCF)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_MPCF(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_MPCF(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (NAME_IN == 'OTM_SPCF') THEN

         NROWS_MAT = 0                                     ! Determine how many grids have accel output requested
         NROWS_TXT = 0
         DO I=1,NGRID
            IB = IAND(GROUT(I,1),IBIT(GROUT_SPCF_BIT))
            IF (IB > 0) THEN
               NROWS_MAT = NROWS_MAT + GRID(I,6)
               NROWS_TXT = NROWS_TXT + GRID(I,6) + OTMSKIP
            ENDIF
         ENDDO
         NROWS_OTM_SPCF = NROWS_MAT
         NROWS_TXT_SPCF = NROWS_TXT
         NCOLS = NUM_CB_DOFS

         MB_ALLOCATED = REAL(DOUBLE)*NROWS_MAT*NCOLS/ONEPP6

         NAME = 'OTM_SPCF'
         IF (ALLOCATED(OTM_SPCF)) THEN                     ! If not already allocated, allocate OTM_SPCF
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_SPCF(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_SPCF(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_SPCF'
         IF (ALLOCATED(TXT_SPCF)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_SPCF(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_SPCF(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
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

! **********************************************************************************************************************************

      END SUBROUTINE ALLOCATE_CB_GRD_OTM

