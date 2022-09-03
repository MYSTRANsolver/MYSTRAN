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

      SUBROUTINE DEALLOCATE_COL_VEC ( NAME )
 
! Deallocate arrays for 1-D column vectors
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  DEALLOCATE_COL_VEC_BEGEND
      USE OUTPUT4_MATRICES, ONLY      :  OU4_MAT_COL_GRD_COMP, OU4_MAT_ROW_GRD_COMP
      USE COL_VECS, ONLY              :  UG_COL, UN_COL, UM_COL, UF_COL, US_COL, UA_COL, UO_COL, UO0_COL, UR_COL, UL_COL, YSe,     &
                                         FG_COL, FN_COL, FM_COL, FF_COL, FS_COL, FA_COL, FO_COL, FL_COL, FR_COL,                   &
                                         FG_COL, PG_COL, PM_COL, PS_COL, PL_COL, QGm_COL, QGr_COL, QGs_COL, QM_COL, QN_COL, QR_COL,&
                                         QS_COL, QSYS_COL

      USE COL_VECS, ONLY              :  PHIXG_COL, PHIXL_COL, PHIXN_COL, PHIXGP_COL, PHIXLP_COL, PHIXNP_COL

      USE DEALLOCATE_COL_VEC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DEALLOCATE_COL_VEC'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name (used for output error message)
      CHARACTER(14*BYTE)              :: NAMEL             ! First 14 bytes of NAMEO
 
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DEALLOCATE_COL_VEC_BEGEND
 
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

      IF (NAME == 'UG_COL') THEN                           ! Deallocate array for UG_COL

         IF (ALLOCATED(UG_COL)) THEN
            DEALLOCATE (UG_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UN_COL') THEN                      ! Deallocate array for UN_COL

         IF (ALLOCATED(UN_COL)) THEN
            DEALLOCATE (UN_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UM_COL') THEN                      ! Deallocate array for UM_COL

         IF (ALLOCATED(UM_COL)) THEN
            DEALLOCATE (UM_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UF_COL') THEN                      ! Deallocate array for UF_COL

         IF (ALLOCATED(UF_COL)) THEN
            DEALLOCATE (UF_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'US_COL') THEN                      ! Deallocate array for US_COL

         IF (ALLOCATED(US_COL)) THEN
            DEALLOCATE (US_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UA_COL') THEN                      ! Deallocate array for UA_COL

         IF (ALLOCATED(UA_COL)) THEN
            DEALLOCATE (UA_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UO_COL') THEN                      ! Deallocate array for UO_COL

         IF (ALLOCATED(UO_COL)) THEN
            DEALLOCATE (UO_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UO0_COL') THEN                     ! Deallocate array for UO0_COL

         IF (ALLOCATED(UO0_COL)) THEN
            DEALLOCATE (UO0_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UL_COL') THEN                      ! Deallocate array for UL_COL

         IF (ALLOCATED(UL_COL)) THEN
            DEALLOCATE (UL_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'UR_COL') THEN                      ! Deallocate array for UR_COL

         IF (ALLOCATED(UR_COL)) THEN
            DEALLOCATE (UR_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'YSe') THEN                         ! Deallocate array for YSe

         IF (ALLOCATED(YSe)) THEN
            DEALLOCATE (YSe,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIXG_COL') THEN                   ! Deallocate array for PHIXG_COL

         IF (ALLOCATED(PHIXG_COL)) THEN
            DEALLOCATE (PHIXG_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIXL_COL') THEN                   ! Deallocate array for PHIXL_COL

         IF (ALLOCATED(PHIXL_COL)) THEN
            DEALLOCATE (PHIXL_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIXN_COL') THEN                   ! Deallocate array for PHIXN_COL

         IF (ALLOCATED(PHIXN_COL)) THEN
            DEALLOCATE (PHIXN_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIXGP_COL') THEN                  ! Deallocate array for PHIXGP_COL

         IF (ALLOCATED(PHIXGP_COL)) THEN
            DEALLOCATE (PHIXGP_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIXLP_COL') THEN                  ! Deallocate array for PHIXLP_COL

         IF (ALLOCATED(PHIXLP_COL)) THEN
            DEALLOCATE (PHIXLP_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PHIXNP_COL') THEN                  ! Deallocate array for PHIXNP_COL

         IF (ALLOCATED(PHIXNP_COL)) THEN
            DEALLOCATE (PHIXNP_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FA_COL') THEN                      ! Deallocate array for FA_COL

         IF (ALLOCATED(FA_COL)) THEN
            DEALLOCATE (FA_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FF_COL') THEN                      ! Deallocate array for FF_COL

         IF (ALLOCATED(FF_COL)) THEN
            DEALLOCATE (FF_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FG_COL') THEN                      ! Deallocate array for FG_COL

         IF (ALLOCATED(FG_COL)) THEN
            DEALLOCATE (FG_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FL_COL') THEN                      ! Deallocate array for FL_COL

         IF (ALLOCATED(FL_COL)) THEN
            DEALLOCATE (FL_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FM_COL') THEN                      ! Deallocate array for FM_COL

         IF (ALLOCATED(FM_COL)) THEN
            DEALLOCATE (FM_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FN_COL') THEN                      ! Deallocate array for FN_COL

         IF (ALLOCATED(FN_COL)) THEN
            DEALLOCATE (FN_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FO_COL') THEN                      ! Deallocate array for FO_COL

         IF (ALLOCATED(FO_COL)) THEN
            DEALLOCATE (FO_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FR_COL') THEN                      ! Deallocate array for FR_COL

         IF (ALLOCATED(FR_COL)) THEN
            DEALLOCATE (FR_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'FS_COL') THEN                      ! Deallocate array for FS_COL

         IF (ALLOCATED(FS_COL)) THEN
            DEALLOCATE (FS_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PG_COL') THEN                      ! Deallocate array for PG_COL

         IF (ALLOCATED(PG_COL)) THEN
            DEALLOCATE (PG_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PM_COL') THEN                      ! Deallocate array for PM_COL

         IF (ALLOCATED(PM_COL)) THEN
            DEALLOCATE (PM_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PS_COL') THEN                      ! Deallocate array for PS_COL

         IF (ALLOCATED(PS_COL)) THEN
            DEALLOCATE (PS_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'PL_COL') THEN                      ! Deallocate array for PL_COL

         IF (ALLOCATED(PL_COL)) THEN
            DEALLOCATE (PL_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QGm_COL') THEN                     ! Deallocate array for QGm_COL

         IF (ALLOCATED(QGm_COL)) THEN
            DEALLOCATE (QGm_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QGr_COL') THEN                     ! Deallocate array for QGr_COL

         IF (ALLOCATED(QGr_COL)) THEN
            DEALLOCATE (QGr_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QGs_COL') THEN                     ! Deallocate array for QGs_COL

         IF (ALLOCATED(QGs_COL)) THEN
            DEALLOCATE (QGs_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QM_COL') THEN                      ! Deallocate array for QM_COL

         IF (ALLOCATED(QM_COL)) THEN
            DEALLOCATE (QM_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QN_COL') THEN                      ! Deallocate array for QN_COL

         IF (ALLOCATED(QN_COL)) THEN
            DEALLOCATE (QN_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QR_COL') THEN                      ! Deallocate array for QR_COL

         IF (ALLOCATED(QR_COL)) THEN
            DEALLOCATE (QR_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QS_COL') THEN                      ! Deallocate array for QS_COL

         IF (ALLOCATED(QS_COL)) THEN
            DEALLOCATE (QS_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'QSYS_COL') THEN                    ! Deallocate array for QSYS_COL

         IF (ALLOCATED(QSYS_COL)) THEN
            DEALLOCATE (QSYS_COL,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'OU4_MAT_COL_GRD_COMP') THEN        ! Deallocate array for OU4_MAT_COL_GRD_COMP

         IF (ALLOCATED(OU4_MAT_COL_GRD_COMP)) THEN
            DEALLOCATE (OU4_MAT_COL_GRD_COMP,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE IF (NAME == 'OU4_MAT_ROW_GRD_COMP') THEN        ! Deallocate array for OU4_MAT_ROW_GRD_COMP

         IF (ALLOCATED(OU4_MAT_ROW_GRD_COMP)) THEN
            DEALLOCATE (OU4_MAT_ROW_GRD_COMP,STAT=IERR)
            IF (IERR /= 0) THEN
               WRITE(ERR,992) NAME,SUBR_NAME
               WRITE(F06,992) NAME,SUBR_NAME
               JERR = JERR + 1
            ENDIF
         ENDIF

      ELSE                                                 ! NAME not recognized, so coding error

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
 
      END SUBROUTINE DEALLOCATE_COL_VEC
