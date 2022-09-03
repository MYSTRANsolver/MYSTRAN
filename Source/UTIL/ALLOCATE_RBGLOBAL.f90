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

      SUBROUTINE ALLOCATE_RBGLOBAL ( SET, CALLING_SUBR )
 
! Allocate arrays for rigid body displ matrices (except RBM0 used in Craig-Bampton analyses). RBGLOBAL matrices are used in
! stiffness matrix equilibrium checks. The TR6 matrices are used in transforming some Craig-Bampton matrices 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NDOFG, NDOFN, NDOFF, NDOFA, NDOFL, NDOFR, BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_RBGLOBAL_BEGEND
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_NSET, RBGLOBAL_FSET, RBGLOBAL_ASET, RBGLOBAL_LSET,                &
                                         TR6_CG, TR6_MEFM, TR6_0

      USE ALLOCATE_RBGLOBAL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_RBGLOBAL'
      CHARACTER(LEN=*), INTENT(IN)    :: SET               ! Set name of the displ matrix
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(14*BYTE)              :: NAME              ! Specific array name used for output error message
      CHARACTER(14*BYTE)              :: NAMEL             ! First 14 bytes of NAME
 
      INTEGER(LONG)                   :: I,J               ! DO loop indices 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NROWS             ! Number of rows in array
      INTEGER(LONG), PARAMETER        :: NCOLS       = 6   ! Number of cols in array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_RBGLOBAL_BEGEND

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

      IF      (SET == 'G ') THEN                           ! Allocate array for G-set rigid body disp matrix

         NROWS = NDOFG
         NAME = 'RBGLOBAL_GSET'
         IF (ALLOCATED(RBGLOBAL_GSET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (RBGLOBAL_GSET(NDOFG,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFG
                  DO J=1,6
                     RBGLOBAL_GSET(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'N ') THEN                           ! Allocate array for N-set rigid body disp matrix

         NROWS = NDOFN
         NAME = 'RBGLOBAL_NSET'
         IF (ALLOCATED(RBGLOBAL_NSET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (RBGLOBAL_NSET(NDOFN,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFN
                  DO J=1,6
                     RBGLOBAL_NSET(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'F ') THEN                           ! Allocate array for F-set rigid body disp matrix

         NROWS = NDOFF
         NAME = 'RBGLOBAL_FSET'
         IF (ALLOCATED(RBGLOBAL_FSET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (RBGLOBAL_FSET(NDOFF,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFF
                  DO J=1,6
                     RBGLOBAL_FSET(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'A ') THEN                           ! Allocate array for A-set rigid body disp matrix

         NROWS = NDOFA
         NAME = 'RBGLOBAL_ASET'
         IF (ALLOCATED(RBGLOBAL_ASET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (RBGLOBAL_ASET(NDOFA,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFA
                  DO J=1,6
                     RBGLOBAL_ASET(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'L ') THEN                           ! Allocate array for L-set rigid body disp matrix

         NROWS = NDOFL
         NAME = 'RBGLOBAL_LSET'
         IF (ALLOCATED(RBGLOBAL_LSET)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (RBGLOBAL_LSET(NDOFL,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFL
                  DO J=1,6
                     RBGLOBAL_LSET(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'R ') THEN                           ! Allocate array for R-set rigid body disp matrix

         NROWS = NDOFR
         NAME = 'TR6_CG'
         IF (ALLOCATED(TR6_CG)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TR6_CG(NDOFR,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFR
                  DO J=1,6
                     TR6_CG(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NROWS = NDOFR
         NAME = 'TR6_MEFM'
         IF (ALLOCATED(TR6_MEFM)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TR6_MEFM(NDOFR,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFR
                  DO J=1,6
                     TR6_MEFM(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NROWS = NDOFR
         NAME = 'TR6_0'
         IF (ALLOCATED(TR6_0)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TR6_0(NDOFR,6),STAT=IERR)
            IF (IERR == 0) THEN
               DO I=1,NDOFR
                  DO J=1,6
                     TR6_0(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME,SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE                                                 ! NAME not recognized, so coding error

         WRITE(ERR,921) SUBR_NAME, 'ALLOCATED', SET
         WRITE(F06,921) SUBR_NAME, 'ALLOCATED', SET
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
      MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS)*REAL(NCOLS)/ONEPP6
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
  921 FORMAT(' *ERROR   921: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DISPL SET FOR MATRIX TO BE ',A,' IS INCORRECT. SET DESIGNATION WAS "',A,'"')

  990 FORMAT(' *ERROR   990: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT ALLOCATE MEMORY TO ARRAY ',A,'. IT IS ALREADY ALLOCATED')

  991 FORMAT(' *ERROR   991: CANNOT ALLOCATE ',F10.3,' MB OF MEMORY TO ARRAY ',A,' IN SUBROUTINE ',A                               &
                    ,/,14X,' ALLOCATION STAT = ',I8)

 1699 FORMAT('               THE SUBR IN WHICH THESE ERRORS WERE FOUND (',A,') WAS CALLED BY SUBR ',A)

 9002 FORMAT(1X,A,' END  ',F10.3,F13.3,' MB ',A15,':',I12,' row,',I12,' col    , T:',F10.3)

 9004 FORMAT(1X,A,' END  ',F10.3,F13.6,' MB ',A15,':',I12,' row,',I12,' col    , T:',F13.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE ALLOCATE_RBGLOBAL
