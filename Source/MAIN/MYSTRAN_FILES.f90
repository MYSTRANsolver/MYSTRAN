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
 
      SUBROUTINE MYSTRAN_FILES ( START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC)
 
! Sets all MYSTRAN file names. Opens all files and closes and deletes them so that no confusion about files if MYSTRAN aborts
! Reopen ANS, BUG, ERR, F04, F06

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, MOT4, MOU4, WRT_BUG, WRT_ERR, WRT_LOG, LEN_INPUT_FNAME,                  &
                                         LEN_RESTART_FNAME, RESTART_FILNAM

      USE IOUNT1, ONLY                :  OU4_EXT, OT4_EXT

      USE IOUNT1, ONLY                :  ANS,     BUG,     EIN,     ENF,     ERR,     F04,     F06,     IN0,     PCH,     SC1,     &
                                         SEQ,     SPC,                                                                             &
                                         L1A,     L1B,     L1C,     L1D,     L1E,     L1F,     L1G,     L1H,     L1I,     L1J,     &
                                         L1K,     L1L,     L1M,     L1N,     L1O,     L1P,     L1Q,     L1R,     L1S,     L1T,     &
                                         L1U,     L1V,     L1W,     L1X,     L1Y,     L1Z,                                         &
                                         L2A,     L2B,     L2C,     L2D,     L2E,     L2F,     L2G,     L2H,     L2I,     L2J,     &
                                         L2K,     L2L,     L2M,     L2N,     L2O,     L2P,     L2Q,     L2R,     L2S,     L2T,     &
                                         L3A,     L4A,     L4B,     L4C,     L4D,     L5A,     L5B,                                &
                                         NEU,     F21,     F22,     F23,     F24,     F25,     OP2,     OT4,     OU4

      USE IOUNT1, ONLY                :  ANSFIL,  BUGFIL,  EINFIL,  ENFFIL,  ERRFIL,  F04FIL,  F06FIL,  IN0FIL,  INFILE,  PCHFIL,  &
                                         OT4FIL,  SEQFIL,  SPCFIL,                                                                 &
                                         LINK1A,  LINK1B,  LINK1C,  LINK1D,  LINK1E,  LINK1F,  LINK1G,  LINK1H,  LINK1I,  LINK1J,  &
                                         LINK1K,  LINK1L,  LINK1M,  LINK1N,  LINK1O,  LINK1P,  LINK1Q,  LINK1R,  LINK1S,  LINK1T,  &
                                         LINK1U,  LINK1V,  LINK1W,  LINK1X,  LINK1Y,  LINK1Z,                                      &
                                         LINK2A,  LINK2B,  LINK2C,  LINK2D,  LINK2E,  LINK2F,  LINK2G,  LINK2H,  LINK2I,  LINK2J,  &
                                         LINK2K,  LINK2L,  LINK2M,  LINK2N,  LINK2O,  LINK2P,  LINK2Q,  LINK2R,  LINK2S,  LINK2T,  &
                                         LINK3A,  LINK4A,  LINK4B,  LINK4C,  LINK4D,  LINK5A,  LINK5B,                             &
                                         NEUFIL,  F21FIL,  F22FIL,  F23FIL,  F24FIL,  F25FIL,  OP2FIL,  OT4FIL,  OU4FIL

      USE IOUNT1, ONLY                :  ANS_MSG, BUG_MSG, EIN_MSG, ENF_MSG, ERR_MSG, F04_MSG, F06_MSG, IN0_MSG, OT4_MSG, PCH_MSG, &
                                         SEQ_MSG, L1A_MSG, L1B_MSG, L1C_MSG, L1D_MSG, L1E_MSG, L1F_MSG, L1G_MSG, L1H_MSG, L1I_MSG, &
                                         L1J_MSG, L1K_MSG, L1L_MSG, L1M_MSG, L1N_MSG, L1O_MSG, L1P_MSG, L1Q_MSG, L1R_MSG, L1S_MSG, &
                                         L1T_MSG, L1U_MSG, L1V_MSG, L1W_MSG, L1X_MSG, L1Y_MSG, L1Z_MSG,                            &
                                         L2A_MSG, L2B_MSG, L2C_MSG, L2D_MSG, L2E_MSG, L2F_MSG, L2G_MSG, L2H_MSG, L2I_MSG, L2J_MSG, &
                                         L2K_MSG, L2L_MSG, L2M_MSG, L2N_MSG, L2O_MSG, L2P_MSG, L2Q_MSG, L2R_MSG, L2S_MSG, L2T_MSG, &
                                         L3A_MSG, L4A_MSG, L4B_MSG, L4C_MSG, L4D_MSG, L5A_MSG, L5B_MSG,                            &
                                         NEU_MSG, F21_MSG, F22_MSG, F23_MSG, F24_MSG, F25_MSG, OP2_MSG, OT4_MSG, OU4_MSG, SPC_MSG 

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, RESTART
      USE TIMDAT, ONLY                :  TSEC, stime
      USE SUBR_BEGEND_LEVELS, ONLY    :  MYSTRAN_FILES_BEGEND

      USE MYSTRAN_FILES_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: FILE_EXIST        ! T/F depending on whether a file exists

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME     = 'MYSTRAN_FILES'
      CHARACTER(FILE_NAM_MAXLEN*BYTE) :: FILNAM            ! Name of the input file or restart file

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MYSTRAN_FILES_BEGEND
      INTEGER(LONG), INTENT(IN)       :: START_HOUR        ! The hour     when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_MINUTE      ! The minute   when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_SEC         ! The second   when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_SFRAC       ! The sec frac when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_YEAR        ! The year     when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_MONTH       ! The month    when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_DAY         ! The day      when MYSTRAN started.
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I1                ! Filename (less extension) length
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr READERR  
 
! **********************************************************************************************************************************
! Default units for writing errors the screen (until LINK1A is read) and set filename length
 
      OUNT(1) = SC1
      OUNT(2) = SC1
 
      IF (RESTART == 'Y') THEN
         I1     = LEN_RESTART_FNAME
         FILNAM = RESTART_FILNAM
      ELSE
         I1     = LEN_INPUT_FNAME
         FILNAM = INFILE
      ENDIF

! Form file names then open them and close and delete them. In this way, we can get rid of all of these files before
! we begin and start anew.

! Formatted files. Note: for F04, ANS, ERR, F06, BUG reopen them after deleting any old version and write STIME

      F04FIL(1:I1)  = FILNAM(1:I1)
      F04FIL(I1+1:) = 'F04'
      IF (F04 /= SC1) THEN
         INQUIRE ( FILE=F04FIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( F04, F04FIL, OUNT,'OLD    ', F04_MSG,'NEITHER','FORMATTED','READWRITE','APPEND','N','N','N')
               WRITE(F04,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
               CALL FILE_CLOSE ( F04, F04FIL,'KEEP','N')
               CALL FILE_OPEN ( F04, F04FIL, OUNT,'OLD    ', F04_MSG,'NEITHER','FORMATTED','READWRITE','APPEND','N','N','N')
            ELSE
               CALL FILE_OPEN ( F04, F04FIL, OUNT,'REPLACE', F04_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','N')
               CALL FILE_CLOSE ( F04, F04FIL,'DELETE','N')
               CALL FILE_OPEN ( F04, F04FIL, OUNT,'NEW', F04_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','N')
               WRITE(F04,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( F04, F04FIL, OUNT,'NEW', F04_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
            IF(RESTART == 'Y') THEN
               WRITE(F04,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ELSE
               WRITE(F04,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
         WRITE(F04,*)
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EINFIL(1:I1)  = FILNAM(1:I1)
      EINFIL(I1+1:) = 'EIN'

      ENFFIL(1:I1)  = FILNAM(1:I1)
      ENFFIL(I1+1:) = 'ENF'

      F06FIL(1:I1)  = FILNAM(1:I1)
      F06FIL(I1+1:) = 'F06'
      IF (F06 /= SC1) THEN
         INQUIRE ( FILE=F06FIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( F06, F06FIL, OUNT,'OLD    ', F06_MSG,'NEITHER'    ,'FORMATTED','READWRITE','APPEND','N','N','Y')
               WRITE(F06,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
               CALL FILE_CLOSE ( F06, F06FIL,'KEEP','Y')
               CALL FILE_OPEN ( F06, F06FIL, OUNT,'OLD    ', F06_MSG,'NEITHER'    ,'FORMATTED','READWRITE','APPEND','N','N','Y')
            ELSE
               CALL FILE_OPEN ( F06, F06FIL, OUNT,'UNKNOWN', F06_MSG,'NEITHER'    ,'FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( F06, F06FIL,'DELETE','Y')
               CALL FILE_OPEN ( F06, F06FIL, OUNT,'NEW'    , F06_MSG,'WRITE_STIME','FORMATTED','WRITE'    ,'REWIND','Y','Y','Y')
               WRITE(F06,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( F06, F06FIL, OUNT,'NEW', F06_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
            IF(RESTART == 'Y') THEN
               WRITE(F06,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ELSE
               WRITE(F06,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
         ENDIF
      ENDIF

      IN0FIL(1:I1)  = FILNAM(1:I1)
      IN0FIL(I1+1:) = 'IN0'
      IF (IN0 /= SC1) THEN
         INQUIRE ( FILE=IN0FIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( IN0, IN0FIL, OUNT,'OLD    ', IN0_MSG,'NEITHER','FORMATTED','WRITE','APPEND','N','N','Y')
               CALL FILE_CLOSE ( IN0, IN0FIL,'KEEP','Y')
               CALL FILE_OPEN ( IN0, IN0FIL, OUNT,'OLD    ', IN0_MSG,'NEITHER','FORMATTED','WRITE','APPEND','N','N','Y')
            ELSE
               CALL FILE_OPEN ( IN0, IN0FIL, OUNT,'REPLACE', IN0_MSG,'NEITHER','FORMATTED','WRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( IN0, IN0FIL,'DELETE','Y')
               CALL FILE_OPEN ( IN0, IN0FIL, OUNT,'NEW', IN0_MSG,'NEITHER','FORMATTED','WRITE','REWIND','N','N','N')
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( IN0, IN0FIL, OUNT,'NEW', IN0_MSG,'NEITHER','FORMATTED','WRITE','REWIND','N','N','N')
         ENDIF
      ENDIF

      BUGFIL(1:I1)  = FILNAM(1:I1)
      BUGFIL(I1+1:) = 'BUG'
      IF (BUG /= SC1) THEN
         INQUIRE ( FILE=BUGFIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( BUG, BUGFIL, OUNT,'OLD    ', BUG_MSG,'NEITHER','FORMATTED','READWRITE','APPEND','N','N','Y')
               WRITE(BUG,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
               CALL FILE_CLOSE ( BUG, BUGFIL,'KEEP','Y')
               CALL FILE_OPEN ( BUG, BUGFIL, OUNT,'OLD    ', BUG_MSG,'NEITHER','FORMATTED','READWRITE','APPEND','N','N','Y')
            ELSE
               CALL FILE_OPEN ( BUG, BUGFIL, OUNT,'REPLACE', BUG_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( BUG, BUGFIL,'DELETE','Y')
               CALL FILE_OPEN ( BUG, BUGFIL, OUNT,'NEW', BUG_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
               WRITE(BUG,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( BUG, BUGFIL, OUNT,'NEW', BUG_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
            IF(RESTART == 'Y') THEN
               WRITE(BUG,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ELSE
               WRITE(BUG,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ENDIF
      ENDIF

      ERRFIL(1:I1)  = FILNAM(1:I1)
      ERRFIL(I1+1:) = 'ERR'
      IF (ERR /= SC1) THEN
         INQUIRE ( FILE=ERRFIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( ERR, ERRFIL, OUNT,'OLD    ', ERR_MSG,'NEITHER','FORMATTED','READWRITE','APPEND','N','N','Y')
               WRITE(ERR,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
               CALL FILE_CLOSE ( ERR, ERRFIL,'KEEP','Y')
               CALL FILE_OPEN ( ERR, ERRFIL, OUNT,'OLD    ', ERR_MSG,'NEITHER','FORMATTED','READWRITE','APPEND','N','N','Y')
            ELSE
               CALL FILE_OPEN ( ERR, ERRFIL, OUNT,'REPLACE', ERR_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( ERR, ERRFIL,'DELETE','Y')
               CALL FILE_OPEN ( ERR, ERRFIL, OUNT,'NEW', ERR_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
               WRITE(ERR,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( ERR, ERRFIL, OUNT,'NEW', ERR_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
            IF(RESTART == 'Y') THEN
               WRITE(ERR,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ELSE
               WRITE(ERR,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ENDIF
      ENDIF

      ANSFIL(1:I1)  = FILNAM(1:I1)
      ANSFIL(I1+1:) = 'ANS'
      IF (ANS /= SC1) THEN
         INQUIRE ( FILE=ANSFIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( ANS, ANSFIL, OUNT,'OLD    ', ANS_MSG,'NEITHER'    ,'FORMATTED','READWRITE','APPEND','N','N','Y')
               WRITE(ANS,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
               CALL FILE_CLOSE ( ANS, ANSFIL,'KEEP','Y')
            ELSE
               CALL FILE_OPEN ( ANS, ANSFIL, OUNT,'REPLACE', ANS_MSG,'NEITHER'    ,'FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( ANS, ANSFIL,'DELETE','Y')
               CALL FILE_OPEN ( ANS, ANSFIL, OUNT,'NEW'    , ANS_MSG,'WRITE_STIME','FORMATTED','WRITE'    ,'REWIND','Y','Y','Y')
               WRITE(ANS,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( ANS, ANSFIL, OUNT,'NEW', ANS_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
            WRITE(ANS,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            WRITE(F04,*)
         ENDIF
      ENDIF

      PCHFIL(1:I1)  = FILNAM(1:I1)
      PCHFIL(I1+1:) = 'PCH'
      IF (PCH /= SC1) THEN
         INQUIRE ( FILE=PCHFIL, EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN ( PCH, PCHFIL, OUNT,'OLD    ', PCH_MSG,'NEITHER'    ,'FORMATTED','READWRITE','APPEND','N','N','Y')
               WRITE(PCH,170) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
               CALL FILE_CLOSE ( PCH, PCHFIL,'KEEP','Y')
            ELSE
               CALL FILE_OPEN ( PCH, PCHFIL, OUNT,'REPLACE', PCH_MSG,'NEITHER'    ,'FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( PCH, PCHFIL,'DELETE','Y')
               CALL FILE_OPEN ( PCH, PCHFIL, OUNT,'NEW'    , PCH_MSG,'WRITE_STIME','FORMATTED','WRITE'    ,'REWIND','Y','Y','Y')
               WRITE(PCH,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            ENDIF
            WRITE(F04,*)
         ELSE
            CALL FILE_OPEN ( PCH, PCHFIL, OUNT,'NEW', PCH_MSG,'WRITE_STIME','FORMATTED','WRITE','REWIND','Y','Y','Y')
            WRITE(PCH,150) START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC, INFILE
            WRITE(F04,*)
         ENDIF
      ENDIF

      LINK1A(1:I1)  = FILNAM(1:I1)
      LINK1A(I1+1:) = 'L1A'
      INQUIRE ( FILE=LINK1A, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1A, LINK1A, OUNT,'OLD    ', L1A_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1A, LINK1A,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1A, LINK1A, OUNT,'REPLACE', L1A_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1A, LINK1A,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      NEUFIL(1:I1)  = FILNAM(1:I1)
      NEUFIL(I1+1:) = 'NEU'
      INQUIRE ( FILE=NEUFIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( NEU, NEUFIL, OUNT,'OLD    ', NEU_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( NEU, NEUFIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( NEU, NEUFIL, OUNT,'REPLACE', NEU_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( NEU, NEUFIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      SEQFIL(1:I1)  = FILNAM(1:I1)
      SEQFIL(I1+1:) = 'SEQ'
      INQUIRE ( FILE=SEQFIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( SEQ, SEQFIL, OUNT,'OLD    ', SEQ_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( SEQ, SEQFIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( SEQ, SEQFIL, OUNT,'REPLACE', SEQ_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( SEQ, SEQFIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      SPCFIL(1:I1)  = FILNAM(1:I1)
      SPCFIL(I1+1:) = 'SPC'
      INQUIRE ( FILE=SPCFIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( SPC, SPCFIL, OUNT,'OLD    ', SPC_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( SPC, SPCFIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( SPC, SPCFIL, OUNT,'REPLACE', SPC_MSG,'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( SPC, SPCFIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      DO I=1,MOT4
         OT4FIL(I)(1:I1)  = FILNAM(1:I1)
         OT4FIL(I)(I1+1:) = OT4_EXT(I)
         INQUIRE ( FILE=OT4FIL(I), EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN
               CALL FILE_OPEN (OT4(I), OT4FIL(I), OUNT,'OLD    ', OT4_MSG(I),'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE (OT4(I), OT4FIL(I),'KEEP','Y')
            ELSE
               CALL FILE_OPEN (OT4(I), OT4FIL(I), OUNT,'REPLACE', OT4_MSG(I),'NEITHER','FORMATTED','READWRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE (OT4(I), OT4FIL(I),'DELETE','Y')
            ENDIF
            WRITE(F04,*)
         ENDIF
      ENDDO

! Unformatted files

      OP2FIL(1:I1)  = FILNAM(1:I1)
      OP2FIL(I1+1:) = 'OP2'
      INQUIRE ( FILE=OP2FIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( OP2, OP2FIL, OUNT,'OLD    ', OP2_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( OP2, OP2FIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( OP2, OP2FIL, OUNT,'REPLACE', OP2_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( OP2, OP2FIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      F21FIL(1:I1)  = FILNAM(1:I1)
      F21FIL(I1+1:) = 'F21'
      INQUIRE ( FILE=F21FIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( F21, F21FIL, OUNT,'OLD    ', F21_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F21, F21FIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( F21, F21FIL, OUNT,'REPLACE', F21_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F21, F21FIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      F22FIL(1:I1)  = FILNAM(1:I1)
      F22FIL(I1+1:) = 'F22'
      INQUIRE ( FILE=F22FIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( F22, F22FIL, OUNT,'OLD    ', F22_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F22, F22FIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( F22, F22FIL, OUNT,'REPLACE', F22_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F22, F22FIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      F23FIL(1:I1)  = FILNAM(1:I1)
      F23FIL(I1+1:) = 'F23'
      INQUIRE ( FILE=F23FIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( F23, F23FIL, OUNT,'OLD    ', F23_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F23, F23FIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( F23, F23FIL, OUNT,'REPLACE', F23_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F23, F23FIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      F24FIL(1:I1)  = FILNAM(1:I1)
      F24FIL(I1+1:) = 'F24'
      INQUIRE ( FILE=F24FIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( F24, F24FIL, OUNT,'OLD    ', F24_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F24, F24FIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( F24, F24FIL, OUNT,'REPLACE', F24_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F24, F24FIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      F25FIL(1:I1)  = FILNAM(1:I1)
      F25FIL(I1+1:) = 'F25'
      INQUIRE ( FILE=F25FIL, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( F25, F25FIL, OUNT,'OLD    ', F25_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F25, F25FIL,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( F25, F25FIL, OUNT,'REPLACE', F25_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( F25, F25FIL,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1B(1:I1)  = FILNAM(1:I1)
      LINK1B(I1+1:) = 'L1B'
      INQUIRE ( FILE=LINK1B, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1B, LINK1B, OUNT,'OLD    ', L1B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1B, LINK1B,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1B, LINK1B, OUNT,'REPLACE', L1B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1B, LINK1B,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK1C(1:I1)  = FILNAM(1:I1)
      LINK1C(I1+1:) = 'L1C'
      INQUIRE ( FILE=LINK1C, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1C, LINK1C, OUNT,'OLD    ', L1C_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1C, LINK1C,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1C, LINK1C, OUNT,'REPLACE', L1C_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1C, LINK1C,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1D(1:I1)  = FILNAM(1:I1)
      LINK1D(I1+1:) = 'L1D'
      INQUIRE ( FILE=LINK1D, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1D, LINK1D, OUNT,'OLD    ', L1D_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1D, LINK1D,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1D, LINK1D, OUNT,'REPLACE', L1D_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1D, LINK1D,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1E(1:I1)  = FILNAM(1:I1)
      LINK1E(I1+1:) = 'L1E'
      INQUIRE ( FILE=LINK1E, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1E, LINK1E, OUNT,'OLD    ', L1E_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1E, LINK1E,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1E, LINK1E, OUNT,'REPLACE', L1E_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1E, LINK1E,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1F(1:I1)  = FILNAM(1:I1)
      LINK1F(I1+1:) = 'L1F'
      INQUIRE ( FILE=LINK1F, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1F, LINK1F, OUNT,'OLD    ', L1F_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1F, LINK1F,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1F, LINK1F, OUNT,'REPLACE', L1F_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1F, LINK1F,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1G(1:I1)  = FILNAM(1:I1)
      LINK1G(I1+1:) = 'L1G'
      INQUIRE ( FILE=LINK1G, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1G, LINK1G, OUNT,'OLD    ', L1G_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1G, LINK1G,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1G, LINK1G, OUNT,'REPLACE', L1G_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1G, LINK1G,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1H(1:I1)  = FILNAM(1:I1)
      LINK1H(I1+1:) = 'L1H'
      INQUIRE ( FILE=LINK1H, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1H, LINK1H, OUNT,'OLD    ', L1H_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1H, LINK1H,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1H, LINK1H, OUNT,'REPLACE', L1H_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1H, LINK1H,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1I(1:I1)  = FILNAM(1:I1)
      LINK1I(I1+1:) = 'L1I'
      INQUIRE ( FILE=LINK1I, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1I, LINK1I, OUNT,'OLD    ', L1I_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1I, LINK1I,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1I, LINK1I, OUNT,'REPLACE', L1I_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1I, LINK1I,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1J(1:I1)  = FILNAM(1:I1)
      LINK1J(I1+1:) = 'L1J'
      INQUIRE ( FILE=LINK1J, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1J, LINK1J, OUNT,'OLD    ', L1J_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1J, LINK1J,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1J, LINK1J, OUNT,'REPLACE', L1J_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1J, LINK1J,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1K(1:I1)  = FILNAM(1:I1)
      LINK1K(I1+1:) = 'L1K'
      INQUIRE ( FILE=LINK1K, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1K, LINK1K, OUNT,'OLD    ', L1K_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1K, LINK1K,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1K, LINK1K, OUNT,'REPLACE', L1K_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1K, LINK1K,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1L(1:I1)  = FILNAM(1:I1)
      LINK1L(I1+1:) = 'L1L'
      INQUIRE ( FILE=LINK1L, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1L, LINK1L, OUNT,'OLD    ', L1L_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1L, LINK1L,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1L, LINK1L, OUNT,'REPLACE', L1L_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1L, LINK1L,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1M(1:I1)  = FILNAM(1:I1)
      LINK1M(I1+1:) = 'L1M'
      INQUIRE ( FILE=LINK1M, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1M, LINK1M, OUNT,'OLD    ', L1M_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1M, LINK1M,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1M, LINK1M, OUNT,'REPLACE', L1M_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1M, LINK1M,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1N(1:I1)  = FILNAM(1:I1)
      LINK1N(I1+1:) = 'L1N'
      INQUIRE ( FILE=LINK1N, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1N, LINK1N, OUNT,'OLD    ', L1N_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1N, LINK1N,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1N, LINK1N, OUNT,'REPLACE', L1N_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1N, LINK1N,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1O(1:I1)  = FILNAM(1:I1)
      LINK1O(I1+1:) = 'L1O'
      INQUIRE ( FILE=LINK1O, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1O, LINK1O, OUNT,'OLD    ', L1O_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1O, LINK1O,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1O, LINK1O, OUNT,'REPLACE', L1O_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1O, LINK1O,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1P(1:I1)  = FILNAM(1:I1)
      LINK1P(I1+1:) = 'L1P'
      INQUIRE ( FILE=LINK1P, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1P, LINK1P, OUNT,'OLD    ', L1P_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1P, LINK1P,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1P, LINK1P, OUNT,'REPLACE', L1P_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1P, LINK1P,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1Q(1:I1)  = FILNAM(1:I1)
      LINK1Q(I1+1:) = 'L1Q'
      INQUIRE ( FILE=LINK1Q, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1Q, LINK1Q, OUNT,'OLD    ', L1Q_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1Q, LINK1Q,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1Q, LINK1Q, OUNT,'REPLACE', L1Q_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1Q, LINK1Q,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1R(1:I1)  = FILNAM(1:I1)
      LINK1R(I1+1:) = 'L1R'
      INQUIRE ( FILE=LINK1R, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1R, LINK1R, OUNT,'OLD    ', L1R_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1R, LINK1R,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1R, LINK1R, OUNT,'REPLACE', L1R_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1R, LINK1R,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1S(1:I1)  = FILNAM(1:I1)
      LINK1S(I1+1:) = 'L1S'
      INQUIRE ( FILE=LINK1S, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1S, LINK1S, OUNT,'OLD    ', L1S_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1S, LINK1S,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1S, LINK1S, OUNT,'REPLACE', L1S_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1S, LINK1S,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1T(1:I1)  = FILNAM(1:I1)
      LINK1T(I1+1:) = 'L1T'
      INQUIRE ( FILE=LINK1T, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1T, LINK1T, OUNT,'OLD    ', L1T_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1T, LINK1T,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1T, LINK1T, OUNT,'REPLACE', L1T_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1T, LINK1T,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1U(1:I1)  = FILNAM(1:I1)
      LINK1U(I1+1:) = 'L1U'
      INQUIRE ( FILE=LINK1U, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1U, LINK1U, OUNT,'OLD    ', L1U_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1U, LINK1U,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1U, LINK1U, OUNT,'REPLACE', L1U_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1U, LINK1U,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1V(1:I1)  = FILNAM(1:I1)
      LINK1V(I1+1:) = 'L1V'
      INQUIRE ( FILE=LINK1V, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1V, LINK1V, OUNT,'OLD    ', L1V_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1V, LINK1V,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1V, LINK1V, OUNT,'REPLACE', L1V_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1V, LINK1V,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1W(1:I1)  = FILNAM(1:I1)
      LINK1W(I1+1:) = 'L1W'
      INQUIRE ( FILE=LINK1W, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1W, LINK1W, OUNT,'OLD    ', L1W_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1W, LINK1W,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1W, LINK1W, OUNT,'REPLACE', L1W_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1W, LINK1W,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1X(1:I1)  = FILNAM(1:I1)
      LINK1X(I1+1:) = 'L1X'
      INQUIRE ( FILE=LINK1X, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1X, LINK1X, OUNT,'OLD    ', L1X_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1X, LINK1X,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1X, LINK1X, OUNT,'REPLACE', L1X_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1X, LINK1X,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1Y(1:I1)  = FILNAM(1:I1)
      LINK1Y(I1+1:) = 'L1Y'
      INQUIRE ( FILE=LINK1Y, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1Y, LINK1Y, OUNT,'OLD    ', L1Y_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1Y, LINK1Y,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1Y, LINK1Y, OUNT,'REPLACE', L1Y_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1Y, LINK1Y,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK1Z(1:I1)  = FILNAM(1:I1)
      LINK1Z(I1+1:) = 'L1Z'
      INQUIRE ( FILE=LINK1Z, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L1Z, LINK1Z, OUNT,'OLD    ', L1Z_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1Z, LINK1Z,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L1Z, LINK1Z, OUNT,'REPLACE', L1Z_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L1Z, LINK1Z,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2A(1:I1)  = FILNAM(1:I1)
      LINK2A(I1+1:) = 'L2A'
      INQUIRE ( FILE=LINK2A, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2A, LINK2A, OUNT,'OLD    ', L2A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2A, LINK2A,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2A, LINK2A, OUNT,'REPLACE', L2A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2A, LINK2A,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2B(1:I1)  = FILNAM(1:I1)
      LINK2B(I1+1:) = 'L2B'
      INQUIRE ( FILE=LINK2B, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2B, LINK2B, OUNT,'OLD    ', L2B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2B, LINK2B,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2B, LINK2B, OUNT,'REPLACE', L2B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2B, LINK2B,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2C(1:I1)  = FILNAM(1:I1)
      LINK2C(I1+1:) = 'L2C'
      INQUIRE ( FILE=LINK2C, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2C, LINK2C, OUNT,'OLD    ', L2C_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2C, LINK2C,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2C, LINK2C, OUNT,'REPLACE', L2C_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2C, LINK2C,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2D(1:I1)  = FILNAM(1:I1)
      LINK2D(I1+1:) = 'L2D'
      INQUIRE ( FILE=LINK2D, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2D, LINK2D, OUNT,'OLD    ', L2D_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2D, LINK2D,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2D, LINK2D, OUNT,'REPLACE', L2D_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2D, LINK2D,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2E(1:I1)  = FILNAM(1:I1)
      LINK2E(I1+1:) = 'L2E'
      INQUIRE ( FILE=LINK2E, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2E, LINK2E, OUNT,'OLD    ', L2E_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2E, LINK2E,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2E, LINK2E, OUNT,'REPLACE', L2E_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2E, LINK2E,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2F(1:I1)  = FILNAM(1:I1)
      LINK2F(I1+1:) = 'L2F'
      INQUIRE ( FILE=LINK2F, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2F, LINK2F, OUNT,'OLD    ', L2F_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2F, LINK2F,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2F, LINK2F, OUNT,'REPLACE', L2F_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2F, LINK2F,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2G(1:I1)  = FILNAM(1:I1)
      LINK2G(I1+1:) = 'L2G'
      INQUIRE ( FILE=LINK2G, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2G, LINK2G, OUNT,'OLD    ', L2G_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2G, LINK2G,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2G, LINK2G, OUNT,'REPLACE', L2G_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2G, LINK2G,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2H(1:I1)  = FILNAM(1:I1)
      LINK2H(I1+1:) = 'L2H'
      INQUIRE ( FILE=LINK2H, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2H, LINK2H, OUNT,'OLD    ', L2H_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2H, LINK2H,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2H, LINK2H, OUNT,'REPLACE', L2H_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2H, LINK2H,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2I(1:I1)  = FILNAM(1:I1)
      LINK2I(I1+1:) = 'L2I'
      INQUIRE ( FILE=LINK2I, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2I, LINK2I, OUNT,'OLD    ', L2I_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2I, LINK2I,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2I, LINK2I, OUNT,'REPLACE', L2I_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2I, LINK2I,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2J(1:I1)  = FILNAM(1:I1)
      LINK2J(I1+1:) = 'L2J'
      INQUIRE ( FILE=LINK2J, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2J, LINK2J, OUNT,'OLD    ', L2J_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2J, LINK2J,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2J, LINK2J, OUNT,'REPLACE', L2J_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2J, LINK2J,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2K(1:I1)  = FILNAM(1:I1)
      LINK2K(I1+1:) = 'L2K'
      INQUIRE ( FILE=LINK2K, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2K, LINK2K, OUNT,'OLD    ', L2K_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2K, LINK2K,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2K, LINK2K, OUNT,'REPLACE', L2K_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2K, LINK2K,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2L(1:I1)  = FILNAM(1:I1)
      LINK2L(I1+1:) = 'L2L'
      INQUIRE ( FILE=LINK2L, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2L, LINK2L, OUNT,'OLD    ', L2L_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2L, LINK2L,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2L, LINK2L, OUNT,'REPLACE', L2L_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2L, LINK2L,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2M(1:I1)  = FILNAM(1:I1)
      LINK2M(I1+1:) = 'L2M'
      INQUIRE ( FILE=LINK2M, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2M, LINK2M, OUNT,'OLD    ', L2M_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2M, LINK2M,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2M, LINK2M, OUNT,'REPLACE', L2M_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2M, LINK2M,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK2N(1:I1)  = FILNAM(1:I1)
      LINK2N(I1+1:) = 'L2N'
      INQUIRE ( FILE=LINK2N, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2N, LINK2N, OUNT,'OLD    ', L2N_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2N, LINK2N,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2N, LINK2N, OUNT,'REPLACE', L2N_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2N, LINK2N,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK2O(1:I1)  = FILNAM(1:I1)
      LINK2O(I1+1:) = 'L2O'
      INQUIRE ( FILE=LINK2O, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2O, LINK2O, OUNT,'OLD    ', L2O_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2O, LINK2O,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2O, LINK2O, OUNT,'REPLACE', L2O_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2O, LINK2O,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK2P(1:I1)  = FILNAM(1:I1)
      LINK2P(I1+1:) = 'L2P'
      INQUIRE ( FILE=LINK2P, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2P, LINK2P, OUNT,'OLD    ', L2P_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2P, LINK2P,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2P, LINK2P, OUNT,'REPLACE', L2P_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2P, LINK2P,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK2Q(1:I1)  = FILNAM(1:I1)
      LINK2Q(I1+1:) = 'L2Q'
      INQUIRE ( FILE=LINK2Q, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2Q, LINK2Q, OUNT,'OLD    ', L2Q_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2Q, LINK2Q,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2Q, LINK2Q, OUNT,'REPLACE', L2Q_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2Q, LINK2Q,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK2R(1:I1)  = FILNAM(1:I1)
      LINK2R(I1+1:) = 'L2R'
      INQUIRE ( FILE=LINK2R, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2R, LINK2R, OUNT,'OLD    ', L2R_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2R, LINK2R,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2R, LINK2R, OUNT,'REPLACE', L2R_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2R, LINK2R,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK2S(1:I1)  = FILNAM(1:I1)
      LINK2S(I1+1:) = 'L2S'
      INQUIRE ( FILE=LINK2S, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2S, LINK2S, OUNT,'OLD    ', L2S_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2S, LINK2S,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2S, LINK2S, OUNT,'REPLACE', L2S_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2S, LINK2S,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK2T(1:I1)  = FILNAM(1:I1)
      LINK2T(I1+1:) = 'L2T'
      INQUIRE ( FILE=LINK2T, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L2T, LINK2T, OUNT,'OLD    ', L2T_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2T, LINK2T,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L2T, LINK2T, OUNT,'REPLACE', L2T_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L2T, LINK2T,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF

      LINK3A(1:I1)  = FILNAM(1:I1)
      LINK3A(I1+1:) = 'L3A'
      INQUIRE ( FILE=LINK3A, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L3A, LINK3A, OUNT,'OLD    ', L3A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L3A, LINK3A,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L3A, LINK3A, OUNT,'REPLACE', L3A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L3A, LINK3A,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK4A(1:I1)  = FILNAM(1:I1)
      LINK4A(I1+1:) = 'L4A'
      INQUIRE ( FILE=LINK4A, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L4A, LINK4A, OUNT,'OLD    ', L4A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4A, LINK4A,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L4A, LINK4A, OUNT,'REPLACE', L4A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4A, LINK4A,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK4B(1:I1)  = FILNAM(1:I1)
      LINK4B(I1+1:) = 'L4B'
      INQUIRE ( FILE=LINK4B, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L4B, LINK4B, OUNT,'OLD    ', L4B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4B, LINK4B,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L4B, LINK4B, OUNT,'REPLACE', L4B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4B, LINK4B,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK4C(1:I1)  = FILNAM(1:I1)
      LINK4C(I1+1:) = 'L4C'
      INQUIRE ( FILE=LINK4C, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L4C, LINK4C, OUNT,'OLD    ', L4C_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4C, LINK4C,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L4C, LINK4C, OUNT,'REPLACE', L4C_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4C, LINK4C,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK4D(1:I1)  = FILNAM(1:I1)
      LINK4D(I1+1:) = 'L4D'
      INQUIRE ( FILE=LINK4D, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L4D, LINK4D, OUNT,'OLD    ', L4D_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4D, LINK4D,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L4D, LINK4D, OUNT,'REPLACE', L4D_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L4D, LINK4D,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK5A(1:I1)  = FILNAM(1:I1)
      LINK5A(I1+1:) = 'L5A'
      INQUIRE ( FILE=LINK5A, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L5A, LINK5A, OUNT,'OLD    ', L5A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L5A, LINK5A,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L5A, LINK5A, OUNT,'REPLACE', L5A_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L5A, LINK5A,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      LINK5B(1:I1)  = FILNAM(1:I1)
      LINK5B(I1+1:) = 'L5B'
      INQUIRE ( FILE=LINK5B, EXIST=FILE_EXIST )
      IF (FILE_EXIST) THEN
         IF (RESTART == 'Y') THEN
            CALL FILE_OPEN ( L5B, LINK5B, OUNT,'OLD    ', L5B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L5B, LINK5B,'KEEP','Y')
         ELSE
            CALL FILE_OPEN ( L5B, LINK5B, OUNT,'REPLACE', L5B_MSG,'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
            CALL FILE_CLOSE ( L5B, LINK5B,'DELETE','Y')
         ENDIF
         WRITE(F04,*)
      ENDIF
 
      DO I=1,MOU4
         OU4FIL(I)(1:I1)  = FILNAM(1:I1)
         OU4FIL(I)(I1+1:) = OU4_EXT(I)
         INQUIRE ( FILE=OU4FIL(I), EXIST=FILE_EXIST )
         IF (FILE_EXIST) THEN
            IF (RESTART == 'Y') THEN                       ! Keep these until after reading Exec Cont and finding out which ones
               CALL FILE_OPEN ( OU4(I), OU4FIL(I), OUNT,'OLD    ', OU4_MSG(I),'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( OU4(I), OU4FIL(I),'KEEP','Y') ! will be needed. Then close those and 'KEEP' them
            ELSE
               CALL FILE_OPEN ( OU4(I), OU4FIL(I), OUNT,'REPLACE', OU4_MSG(I),'NEITHER','UNFORMATTED','WRITE','REWIND','N','N','Y')
               CALL FILE_CLOSE ( OU4(I), OU4FIL(I),'DELETE','Y')
            ENDIF
            WRITE(F04,*)
         ENDIF
      ENDDO
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  150 FORMAT(/,' >> MYSTRAN BEGIN  : ',I2,'/',I2,'/',I4,' at ',I2,':',I2,':',I2,'.',I3,' The input file is ',A,/)

  170 FORMAT(/,' >> MYSTRAN RESTART: ',I2,'/',I2,'/',I4,' at ',I2,':',I2,':',I2,'.',I3,' The input file is ',A,/)

! **********************************************************************************************************************************

      END SUBROUTINE MYSTRAN_FILES
