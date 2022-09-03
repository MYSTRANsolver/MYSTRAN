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
 
      SUBROUTINE CLOSE_LIJFILES ( CLOSE_STAT )
 
! Closes Lij unformatted files
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, RESTART
      USE IOUNT1, ONLY                :  MOU4,    WRT_ERR, WRT_LOG, ERR, F06,                                                      &
                                         L1B,     L1C,     L1D,     L1E,     L1F,     L1G,     L1H,     L1I,     L1J,     L1K,     &
                                         L1L,     L1M,     L1N,     L1O,     L1P,     L1Q,     L1R,     L1S,     L1T,     L1U,     &
                                         L1V,     L1W,     L1X,     L1Y,     L1Z,                                                  &
                                         L2A,     L2B,     L2C,     L2D,     L2E,     L2F,     L2G,     L2H,     L2I,     L2J,     &
                                         L2K,     L2L,     L2M,     L2N,     L2O,     L2P,     L2Q,     L2R,     L2S,     L2T,     &
                                         L3A,     L4A,     L4B,     L4C,     L4D,     L5A,     L5B,     OU4

      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                                                                &
                                         LINK1B,  LINK1C,  LINK1D,  LINK1E,  LINK1F,  LINK1G,  LINK1H,  LINK1I,  LINK1J,  LINK1K,  &
                                         LINK1L,  LINK1M,  LINK1N,  LINK1O,  LINK1P,  LINK1Q,  LINK1R,  LINK1S,  LINK1T,  LINK1U,  &
                                         LINK1V,  LINK1W,  LINK1X,  LINK1Y,  LINK1Z,                                               &
                                         LINK2A,  LINK2B,  LINK2C,  LINK2D,  LINK2E,  LINK2F,  LINK2G,  LINK2H,  LINK2I,  LINK2J,  &
                                         LINK2K,  LINK2L,  LINK2M,  LINK2N,  LINK2O,  LINK2P,  LINK2Q,  LINK2R,  LINK2S,  LINK2T,  &
                                         LINK3A,  LINK4A,  LINK4B,  LINK4C,  LINK4D,  LINK5A,  LINK5B,  OU4FIL

      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                                                                &
                                         L1BSTAT, L1CSTAT, L1DSTAT, L1ESTAT, L1FSTAT, L1GSTAT, L1HSTAT, L1ISTAT, L1JSTAT, L1KSTAT, &
                                         L1LSTAT, L1MSTAT, L1NSTAT, L1OSTAT, L1PSTAT, L1QSTAT, L1RSTAT, L1SSTAT, L1TSTAT, L1USTAT, &
                                         L1VSTAT, L1WSTAT, L1XSTAT, L1YSTAT, L1ZSTAT,                                              &
                                         L2ASTAT, L2BSTAT, L2CSTAT, L2DSTAT, L2ESTAT, L2FSTAT, L2GSTAT, L2HSTAT, L2ISTAT, L2JSTAT, &
                                         L2KSTAT, L2LSTAT, L2MSTAT, L2NSTAT, L2OSTAT, L2PSTAT, L2QSTAT, L2RSTAT, L2SSTAT, L2TSTAT, &
                                         L3ASTAT, L4ASTAT, L4BSTAT, L4CSTAT, L4DSTAT, L5ASTAT, L5BSTAT, OU4STAT

      USE TIMDAT, ONLY                :  STIME, TSEC

      USE CLOSE_LIJFILES_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: LEXIST            ! Result from INQUIRE regarding whether a file exists
      LOGICAL                         :: LOPND             ! Result from INQUIRE regarding whether a file is open

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CLOSE_LIJFILES'
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! Indicator of what to do with file when it is closed

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

      IF ((CLOSE_STAT /= 'DELETE') .AND. (CLOSE_STAT /= 'FILE_STAT')) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,954) SUBR_NAME, CLOSE_STAT
         WRITE(F06,954) SUBR_NAME, CLOSE_STAT
         STOP
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1B, LINK1B, L1BSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1B, LINK1B, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1C, LINK1C, L1CSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1C, LINK1C, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1D, LINK1D, L1DSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1D, LINK1D, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1E, LINK1E, L1ESTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1E, LINK1E, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1F, LINK1F, L1FSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1F, LINK1F, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1G, LINK1G, L1GSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1G, LINK1G, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1H, LINK1H, L1HSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1H, LINK1H, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1I, LINK1I, L1ISTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1I, LINK1I, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1J, LINK1J, L1JSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1J, LINK1J, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1K, LINK1K, L1KSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1K, LINK1K, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1L, LINK1L, L1LSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1L, LINK1L, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1M, LINK1M, L1MSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1M, LINK1M, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1N, LINK1N, L1NSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1N, LINK1N, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1O, LINK1O, L1OSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1O, LINK1O, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1P, LINK1P, L1PSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1P, LINK1P, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1Q, LINK1Q, L1QSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1Q, LINK1Q, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1R, LINK1R, L1RSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1R, LINK1R, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1S, LINK1S, L1SSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1S, LINK1S, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1T, LINK1T, L1TSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1T, LINK1T, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1U, LINK1U, L1USTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1U, LINK1U, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1V, LINK1V, L1VSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1V, LINK1V, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1W, LINK1W, L1WSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1W, LINK1W, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1X, LINK1X, L1XSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1X, LINK1X, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L1Y, LINK1Y, L1YSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L1Y, LINK1Y, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         IF (RESTART == 'N') THEN
            CALL CLOSE_THIS_FILE ( L1Z, LINK1Z, L1ZSTAT )
         ELSE
            CALL CLOSE_THIS_FILE ( L1Z, LINK1Z, 'KEEP' )
         ENDIF
      ELSE
         IF (RESTART == 'N') THEN
            CALL CLOSE_THIS_FILE ( L1Z, LINK1Z, CLOSE_STAT )
         ELSE
            CALL CLOSE_THIS_FILE ( L1Z, LINK1Z, 'KEEP' )
         ENDIF
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2A, LINK2A, L2ASTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2A, LINK2A, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2B, LINK2B, L2BSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2B, LINK2B, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2C, LINK2C, L2CSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2C, LINK2C, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2D, LINK2D, L2DSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2D, LINK2D, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2E, LINK2E, L2ESTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2E, LINK2E, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2F, LINK2F, L2FSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2F, LINK2F, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2G, LINK2G, L2GSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2G, LINK2G, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2H, LINK2H, L2HSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2H, LINK2H, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2I, LINK2I, L2ISTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2I, LINK2I, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2J, LINK2J, L2JSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2J, LINK2J, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2K, LINK2K, L2KSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2K, LINK2K, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2L, LINK2L, L2LSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2L, LINK2L, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2M, LINK2M, L2MSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2M, LINK2M, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2N, LINK2N, L2NSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2N, LINK2N, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2O, LINK2O, L2OSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2O, LINK2O, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2P, LINK2P, L2PSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2P, LINK2P, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2Q, LINK2Q, L2QSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2Q, LINK2Q, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2R, LINK2R, L2RSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2R, LINK2R, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2S, LINK2S, L2SSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2S, LINK2S, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L2T, LINK2T, L2TSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L2T, LINK2T, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L5A, LINK5A, L5ASTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L5A, LINK5A, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L3A, LINK3A, L3ASTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L3A, LINK3A, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L4A, LINK4A, L4ASTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L4A, LINK4A, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L4B, LINK4B, L4BSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L4B, LINK4B, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L4C, LINK4C, L4CSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L4C, LINK4C, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L4D, LINK4D, L4DSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L4D, LINK4D, CLOSE_STAT )
      ENDIF

      IF (CLOSE_STAT == 'FILE_STAT') THEN
         CALL CLOSE_THIS_FILE ( L5B, LINK5B, L5BSTAT )
      ELSE
         CALL CLOSE_THIS_FILE ( L5B, LINK5B, CLOSE_STAT )
      ENDIF

      DO I=1,MOU4
         INQUIRE(FILE=OU4FIL(I),EXIST=LEXIST,OPENED=LOPND)
         IF (LEXIST) THEN
            IF (LOPND) THEN
               CALL FILE_CLOSE ( OU4(I), OU4FIL(I), OU4STAT(I), 'Y' )
            ELSE
               OPEN (OU4(I),FILE=OU4FIL(I),STATUS='OLD',IOSTAT=IOCHK)
               IF (IOCHK /= 0) THEN
                  CALL OPNERR ( IOCHK, OU4FIL(I), OUNT, 'Y' )
                  CALL OUTA_HERE ( 'Y' )
               ELSE
                  CALL FILE_CLOSE ( OU4(I), OU4FIL(I), OU4STAT(I), 'Y' )
               ENDIF
            ENDIF
         ENDIF
      ENDDO

! **********************************************************************************************************************************
  954 FORMAT(' *ERROR   954: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INTENT(IN) ARG CLOSE_STAT MUST BE "DELETE" OR "FILE_STAT" BUT IS "',A,'"')

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE CLOSE_THIS_FILE ( UNT, FILNAM, STATUS )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM
      CHARACTER(LEN=*), INTENT(IN)    :: STATUS

      INTEGER(LONG)   , INTENT(IN)    :: UNT

! **********************************************************************************************************************************

      INQUIRE(FILE=FILNAM,EXIST=LEXIST,OPENED=LOPND)
      IF (LEXIST) THEN
         IF (LOPND) THEN
            CALL FILE_CLOSE ( UNT, FILNAM, STATUS, 'Y' )
         ELSE
            OPEN (UNT,FILE=FILNAM,STATUS='OLD',IOSTAT=IOCHK)
            IF (IOCHK /= 0) THEN
               CALL OPNERR ( IOCHK, FILNAM, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )
            ELSE
               CALL FILE_CLOSE ( UNT, FILNAM, STATUS, 'Y' )
            ENDIF
         ENDIF
      ENDIF

      END SUBROUTINE CLOSE_THIS_FILE

      END SUBROUTINE CLOSE_LIJFILES
