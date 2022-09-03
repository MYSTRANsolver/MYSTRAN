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

      SUBROUTINE DEALLOCATE_RBGLOBAL ( SET )
 
! Deallocate arrays for rigid body displ matrices (except RBM0 used in Craig-Bampton analyses). RBGLOBAL matrices are used in
! stiffness matrix equilibrium checks. The TR6 matrices are used in transforming some Craig-Bampton matrices 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC          
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_RBGLOBAL_BEGEND
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_NSET, RBGLOBAL_FSET, RBGLOBAL_ASET, RBGLOBAL_LSET,                &
                                         TR6_CG, TR6_MEFM, TR6_0

      USE DEALLOCATE_RBGLOBAL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_RBGLOBAL'
      CHARACTER(LEN=*), INTENT(IN)    :: SET               ! Set name of the displ matrix
      CHARACTER(13*BYTE)              :: NAME              ! Specific array name used for output error message
      CHARACTER(14*BYTE)              :: NAMEL             ! First 14 bytes of NAME
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_RBGLOBAL_BEGEND
 
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

      IF      (SET == 'G ') THEN                           ! Dellocate array for RBGLOBAL_GSET

         NAME = 'RBGLOBAL_GSET'
         IF (ALLOCATED(RBGLOBAL_GSET)) THEN
            DEALLOCATE (RBGLOBAL_GSET,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'N ') THEN                           ! Dellocate array for RBGLOBAL_GSET

         NAME = 'RBGLOBAL_NSET'
         IF (ALLOCATED(RBGLOBAL_NSET)) THEN
            DEALLOCATE (RBGLOBAL_NSET,STAT=IERR)
               NAME = 'RBGLOBAL_NSET'
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'F ') THEN                           ! Dellocate array for RBGLOBAL_GSET

         NAME = 'RBGLOBAL_FSET'
         IF (ALLOCATED(RBGLOBAL_FSET)) THEN
            DEALLOCATE (RBGLOBAL_FSET,STAT=IERR)
            IF (IERR /= 0) THEN
               NAME = 'RBGLOBAL_FSET'
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (SET == 'A ') THEN                           ! Dellocate array for RBGLOBAL_GSET

         NAME = 'RBGLOBAL_ASET'
         IF (ALLOCATED(RBGLOBAL_ASET)) THEN
            DEALLOCATE (RBGLOBAL_ASET,STAT=IERR)
            IF (IERR /= 0) THEN
               NAME = 'RBGLOBAL_ASET'
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

       ELSE IF (SET == 'L ') THEN                           ! Dellocate array for RBGLOBAL_GSET

         NAME = 'RBGLOBAL_LSET'
         IF (ALLOCATED(RBGLOBAL_LSET)) THEN
            DEALLOCATE (RBGLOBAL_LSET,STAT=IERR)
            IF (IERR /= 0) THEN
               NAME = 'RBGLOBAL_LSET'
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

       ELSE IF (SET == 'R ') THEN                           ! Dellocate array for RBGLOBAL_GSET

         NAME = 'TR6_CG'            
         IF (ALLOCATED(TR6_CG)) THEN
            DEALLOCATE (TR6_CG,STAT=IERR)
            IF (IERR /= 0) THEN
               NAME = 'TR6_CG'
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TR6_MEFM'            
         IF (ALLOCATED(TR6_MEFM)) THEN
            DEALLOCATE (TR6_MEFM,STAT=IERR)
            IF (IERR /= 0) THEN
               NAME = 'TR6_MEFM'
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TR6_0'            
         IF (ALLOCATED(TR6_0)) THEN
            DEALLOCATE (TR6_0,STAT=IERR)
            IF (IERR /= 0) THEN
               NAME = 'TR6_0'
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE                                                 ! NAME not recognized, so coding error

         WRITE(ERR,901) SUBR_NAME, 'DEALLOCATED', SET
         WRITE(F06,901) SUBR_NAME, 'DEALLOCATED', SET
         JERR = JERR + 1

      ENDIF
 
! Quit if there were errors

      IF (JERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + JERR
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )

      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         NAMEL(1:LEN(NAMEL)) = ' '
         NAMEL(1:)  = NAME(1:)
         IF (DEBUG(107) == 0) THEN
            WRITE(F04,9003) SUBR_NAME, TSEC, -CUR_MB_ALLOCATED, NAMEL, TOT_MB_MEM_ALLOC
         ELSE
            WRITE(F04,9005) SUBR_NAME, TSEC, -CUR_MB_ALLOCATED, NAMEL, TOT_MB_MEM_ALLOC
         ENDIF
      ENDIF

      RETURN

! **********************************************************************************************************************************
  901 FORMAT(' *ERROR   901: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DISPL SET FOR MATRIX TO BE ',A,' IS INCORRECT. SET DESIGNATION WAS "',A,'"')

  992 FORMAT(' *ERROR   992: CANNOT DEALLOCATE MEMORY FROM ARRAY ',A,' IN SUBROUTINE ',A)

 9003    FORMAT(1X,A,' END  ',F10.3,F13.3,' MB ',A15,':',39X,'T:',F10.3)

 9005    FORMAT(1X,A,' END  ',F10.3,F13.6,' MB ',A15,':',39X,'T:',F13.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE DEALLOCATE_RBGLOBAL
