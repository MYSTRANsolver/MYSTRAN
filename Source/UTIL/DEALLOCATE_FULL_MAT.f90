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

      SUBROUTINE DEALLOCATE_FULL_MAT ( NAME )
 
! Deallocates 2D full arrays (see comments in module FULL_MATRICES)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC         
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_FULL_MAT_BEGEND
      USE FULL_MATRICES, ONLY         :  KNN_FULL, KNM_FULL, KMM_FULL, MNN_FULL, MNM_FULL, MMM_FULL, PN_FULL, PM_FULL,             &
                                         KFF_FULL, KFS_FULL, KSS_FULL, MFF_FULL, MFS_FULL, MSS_FULL, PF_FULL, PS_FULL,             &
                                         KAA_FULL, KAO_FULL, KOO_FULL, MAA_FULL, MAO_FULL, MOO_FULL, PA_FULL, PO_FULL,             &
                                         PFYS_FULL, QSYS_FULL, KFSe_FULL,                                                          &
                                         RMM_FULL, GMN_FULL, GMNt_FULL, GOA_FULL, GOAt_FULL,HMN_FULL, PHIZG_FULL,                  &
                                         DUM1, DUM2, DUM3  

      USE DEALLOCATE_FULL_MAT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_FULL_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name (used for output error message)
      CHARACTER(14*BYTE)              :: NAMEL             ! First 14 bytes of NAMEO
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_FULL_MAT_BEGEND
 
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

! Deallocate array named 'NAME' which was declared allocatable in module SCRATCH_MATRICES

      IF (NAME == 'KNN_FULL') THEN                         ! Deallocate array KNN_FULL

         IF (ALLOCATED(KNN_FULL)) THEN
            DEALLOCATE (KNN_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KNM_FULL') THEN                    ! Deallocate array KNM_FULL

         IF (ALLOCATED(KNM_FULL)) THEN
            DEALLOCATE (KNM_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KMM_FULL') THEN                    ! Deallocate array KMM_FULL

         IF (ALLOCATED(KMM_FULL)) THEN
            DEALLOCATE (KMM_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MNN_FULL') THEN                    ! Deallocate array MNN_FULL

         IF (ALLOCATED(MNN_FULL)) THEN
            DEALLOCATE (MNN_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MNM_FULL') THEN                    ! Deallocate array MNM_FULL

         IF (ALLOCATED(MNM_FULL)) THEN
            DEALLOCATE (MNM_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MMM_FULL') THEN                    ! Deallocate array MMM_FULL

         IF (ALLOCATED(MMM_FULL)) THEN
            DEALLOCATE (MMM_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PN_FULL') THEN                     ! Deallocate array PN_FULL

         IF (ALLOCATED(PN_FULL)) THEN
            DEALLOCATE (PN_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PM_FULL') THEN                     ! Deallocate array PM_FULL

         IF (ALLOCATED(PM_FULL)) THEN
            DEALLOCATE (PM_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KFF_FULL') THEN                    ! Deallocate array KFF_FULL

         IF (ALLOCATED(KFF_FULL)) THEN
            DEALLOCATE (KFF_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KFS_FULL') THEN                    ! Deallocate array KFS_FULL

         IF (ALLOCATED(KFS_FULL)) THEN
            DEALLOCATE (KFS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KFSe_FULL') THEN                   ! Deallocate array KFSe_FULL

         IF (ALLOCATED(KFSe_FULL)) THEN
            DEALLOCATE (KFSe_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KSS_FULL') THEN                    ! Deallocate array KSS_FULL

         IF (ALLOCATED(KSS_FULL)) THEN
            DEALLOCATE (KSS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MFF_FULL') THEN                    ! Deallocate array MFF_FULL

         IF (ALLOCATED(MFF_FULL)) THEN
            DEALLOCATE (MFF_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MFS_FULL') THEN                    ! Deallocate array MFS_FULL

         IF (ALLOCATED(MFS_FULL)) THEN
            DEALLOCATE (MFS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MSS_FULL') THEN                    ! Deallocate array MSS_FULL

         IF (ALLOCATED(MSS_FULL)) THEN
            DEALLOCATE (MSS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PF_FULL') THEN                     ! Deallocate array PF_FULL

         IF (ALLOCATED(PF_FULL)) THEN
            DEALLOCATE (PF_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PS_FULL') THEN                     ! Deallocate array PS_FULL

         IF (ALLOCATED(PS_FULL)) THEN
            DEALLOCATE (PS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KAA_FULL') THEN                    ! Deallocate array KAA_FULL

         IF (ALLOCATED(KAA_FULL)) THEN
            DEALLOCATE (KAA_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KAO_FULL') THEN                    ! Deallocate array KAO_FULL

         IF (ALLOCATED(KAO_FULL)) THEN
            DEALLOCATE (KAO_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'KOO_FULL') THEN                    ! Deallocate array KOO_FULL

         IF (ALLOCATED(KOO_FULL)) THEN
            DEALLOCATE (KOO_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MAA_FULL') THEN                    ! Deallocate array MAA_FULL

         IF (ALLOCATED(MAA_FULL)) THEN
            DEALLOCATE (MAA_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MAO_FULL') THEN                    ! Deallocate array MAO_FULL

         IF (ALLOCATED(MAO_FULL)) THEN
            DEALLOCATE (MAO_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'MOO_FULL') THEN                    ! Deallocate array MOO_FULL

         IF (ALLOCATED(MOO_FULL)) THEN
            DEALLOCATE (MOO_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PA_FULL') THEN                     ! Deallocate array PA_FULL

         IF (ALLOCATED(PA_FULL)) THEN
            DEALLOCATE (PA_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PO_FULL') THEN                     ! Deallocate array PO_FULL

         IF (ALLOCATED(PO_FULL)) THEN
            DEALLOCATE (PO_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'PFYS_FULL') THEN                   ! Deallocate array PFYS_FULL

         IF (ALLOCATED(PFYS_FULL)) THEN
            DEALLOCATE (PFYS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'QSYS_FULL') THEN                   ! Deallocate array QSYS_FULL

         IF (ALLOCATED(QSYS_FULL)) THEN
            DEALLOCATE (QSYS_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'RMM_FULL') THEN                    ! Deallocate array RMM_FULL

         IF (ALLOCATED(RMM_FULL)) THEN
            DEALLOCATE (RMM_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'GMN_FULL') THEN                    ! Deallocate array GMN_FULL

         IF (ALLOCATED(GMN_FULL)) THEN
            DEALLOCATE (GMN_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'GMNt_FULL') THEN                   ! Deallocate array GMNt_FULL

         IF (ALLOCATED(GMNt_FULL)) THEN
            DEALLOCATE (GMNt_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'GOA_FULL') THEN                    ! Deallocate array GOA_FULL

         IF (ALLOCATED(GOA_FULL)) THEN
            DEALLOCATE (GOA_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'GOAt_FULL') THEN                   ! Deallocate array GOAt_FULL

         IF (ALLOCATED(GOAt_FULL)) THEN
            DEALLOCATE (GOAt_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIZG_FULL') THEN                  ! Deallocate array PHIZG_FULL

         IF (ALLOCATED(PHIZG_FULL)) THEN
            DEALLOCATE (PHIZG_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'HMN_FULL') THEN                    ! Deallocate array HMN_FULL

         IF (ALLOCATED(HMN_FULL)) THEN
            DEALLOCATE (HMN_FULL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'DUM1') THEN                        ! Deallocate array DUM2

         IF (ALLOCATED(DUM1)) THEN
            DEALLOCATE (DUM1,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'DUM2') THEN                        ! Deallocate array DUM2

         IF (ALLOCATED(DUM2)) THEN
            DEALLOCATE (DUM2,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE IF (NAME == 'DUM3') THEN                        ! Deallocate array DUM3

         IF (ALLOCATED(DUM3)) THEN
            DEALLOCATE (DUM3,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF
 
      ELSE                                                 ! NAME not recognized, so prog error

         WRITE(ERR,915) SUBR_NAME, 'DEALLOCATED', NAME
         WRITE(F06,915) SUBR_NAME, 'DEALLOCATED', NAME
         FATAL_ERR = FATAL_ERR + JERR
         JERR = JERR + 1

      ENDIF
 
! Quit if there were errors

      IF (JERR /= 0) THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      CALL ALLOCATED_MEMORY ( NAME, ZERO, 'DEALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )

      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         NAMEL(1:LEN(NAMEL)) = ' '
         NAMEL(1:) = NAME(1:)
         IF (DEBUG(107) == 0) THEN
            WRITE(F04,9003) SUBR_NAME, TSEC, -CUR_MB_ALLOCATED, NAMEL, TOT_MB_MEM_ALLOC
         ELSE
            WRITE(F04,9005) SUBR_NAME, TSEC, -CUR_MB_ALLOCATED, NAMEL, TOT_MB_MEM_ALLOC
         ENDIF
      ENDIF

      RETURN

! **********************************************************************************************************************************
  915 FORMAT(' *ERROR   915: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NAME OF ARRAY TO BE ',A,' IS INCORRECT. INPUT NAME WAS ',A)

  992 FORMAT(' *ERROR   992: CANNOT DEALLOCATE MEMORY FROM ARRAY ',A,' IN SUBROUTINE ',A)

 9003    FORMAT(1X,A,' END  ',F10.3,F13.3,' MB ',A15,':',39X,'T:',F10.3)

 9005    FORMAT(1X,A,' END  ',F10.3,F13.6,' MB ',A15,':',39X,'T:',F13.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE DEALLOCATE_FULL_MAT
