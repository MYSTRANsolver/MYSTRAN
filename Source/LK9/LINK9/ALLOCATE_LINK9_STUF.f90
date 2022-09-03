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

      SUBROUTINE ALLOCATE_LINK9_STUF ( CALLING_SUBR )
 
! Allocate some arrays for use in LINK9
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, ONEPP6
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MELGP, MMSPRNT, MOGEL, TOT_MB_MEM_ALLOC
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_LINK9_STUF_BEGEND
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, EID_OUT_ARRAY, FTNAME, MAXREQ, MSPRNT, OGEL, POLY_FIT_ERR,                 &
                                         POLY_FIT_ERR_INDEX
 
      USE ALLOCATE_LINK9_STUF_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_LINK9_STUF'
      CHARACTER(24*BYTE)              :: NAME              ! Array name (used for output error message)
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
 
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERR              ! STAT from ALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NROWS             ! Nunber of rows in array NAME being allocated
      INTEGER(LONG)                   :: NCOLS             ! Nunber of cols in array NAME being allocated
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_LINK9_STUF_BEGEND
 
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
      NROWS = MAXREQ
      JERR = 0

! Allocate array for GID_OUT_ARRAY
 
      NAME = 'GID_OUT_ARRAY'
      NCOLS = MELGP
      IF (ALLOCATED(GID_OUT_ARRAY)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (GID_OUT_ARRAY(MAXREQ,MELGP+1),STAT=IERR)
         MB_ALLOCATED = RLONG*REAL(MAXREQ)*REAL(MELGP)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, MELGP+1, SUBR_BEGEND )
            DO I=1,MAXREQ
               DO J=1,MELGP+1
                  GID_OUT_ARRAY(I,J) = 0
               ENDDO
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate arrays for EID_OUT_ARRAY

      NAME = 'EID_OUT_ARRAY'
      NCOLS = 1
      IF (ALLOCATED(EID_OUT_ARRAY)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (EID_OUT_ARRAY(MAXREQ,2),STAT=IERR)
         MB_ALLOCATED = RLONG*TWO*REAL(MAXREQ)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 2, SUBR_BEGEND )
            DO I=1,MAXREQ
               EID_OUT_ARRAY(I,1) = 0
               EID_OUT_ARRAY(I,2) = 1                      ! Set number of plies to 1 so that we don't have to worry about it
            ENDDO                                          ! except for 2D elems (which will be set in another subr)
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate arrays for FTNAME
 
      NAME = 'FTNAME                  '
      IF (ALLOCATED(FTNAME)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (FTNAME(MAXREQ),STAT=IERR)
         IF (IERR == 0) THEN
         MB_ALLOCATED =REAL(LEN(FTNAME)*MAXREQ)/ONEPP6
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            DO I=1,MAXREQ
               FTNAME(I) = 'none'
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate arrays for MSPRNT
 
      NAME = 'MSPRNT                  '
      NCOLS = MMSPRNT
      IF (ALLOCATED(MSPRNT)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (MSPRNT(MAXREQ,MMSPRNT),STAT=IERR)
         IF (IERR == 0) THEN
         MB_ALLOCATED =REAL(LEN(MSPRNT)*MAXREQ*MMSPRNT)/ONEPP6
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, MMSPRNT, SUBR_BEGEND )
            DO I=1,MAXREQ
               DO J=1,MMSPRNT
                  MSPRNT(I,J)(1:) = ' '
               ENDDO
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate arrays for OGEL
 
      NAME = 'OGEL'
      NCOLS = MOGEL
      IF (ALLOCATED(OGEL)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (OGEL(MAXREQ,MOGEL),STAT=IERR)
         MB_ALLOCATED = RDOUBLE*REAL(MAXREQ)*REAL(MOGEL)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, MOGEL, SUBR_BEGEND )
            DO I=1,MAXREQ
               DO J=1,MOGEL
                  OGEL(I,J) = ZERO
               ENDDO
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate arrays for POLY_FIT_ERR

      NAME = 'POLY_FIT_ERR'
      NCOLS = 1
      IF (ALLOCATED(POLY_FIT_ERR)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (POLY_FIT_ERR(MAXREQ),STAT=IERR)
         MB_ALLOCATED = RDOUBLE*REAL(MAXREQ)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            DO I=1,MAXREQ
               POLY_FIT_ERR(I) = ZERO
            ENDDO
         ELSE
            WRITE(ERR,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            WRITE(F06,991) MB_ALLOCATED,NAME,SUBR_NAME,IERR
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ENDIF
      ENDIF

! Allocate arrays for POLY_FIT_ERR_INDEX

      NAME = 'POLY_FIT_ERR_INDEX'
      NCOLS = 1
      IF (ALLOCATED(POLY_FIT_ERR_INDEX)) THEN
         WRITE(ERR,990) SUBR_NAME, NAME
         WRITE(F06,990) SUBR_NAME, NAME
         FATAL_ERR = FATAL_ERR + 1
         JERR = JERR + 1
      ELSE
         ALLOCATE (POLY_FIT_ERR_INDEX(MAXREQ),STAT=IERR)
         MB_ALLOCATED = BYTE*REAL(MAXREQ)/ONEPP6
         IF (IERR == 0) THEN
            CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
            CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS, 1, SUBR_BEGEND )
            DO I=1,MAXREQ
               POLY_FIT_ERR_INDEX(I) = 0
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
  990 FORMAT(' *ERROR   990: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT ALLOCATE MEMORY TO ARRAY ',A,'. IT IS ALREADY ALLOCATED')

  991 FORMAT(' *ERROR   991: CANNOT ALLOCATE ',F10.3,' MB OF MEMORY TO ARRAY ',A,' IN SUBROUTINE ',A                               &
                    ,/,14X,' ALLOCATION STAT = ',I8)

 1699 FORMAT('               THE SUBR IN WHICH THESE ERRORS WERE FOUND (',A,') WAS CALLED BY SUBR ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE ALLOCATE_LINK9_STUF
