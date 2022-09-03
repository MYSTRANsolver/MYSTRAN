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
 
      SUBROUTINE WRITE_L1A ( CLOSE_STAT, CALL_OUTA_HERE, WRITE_F04 )
 
! Writes data to file LINK1A at the end of each LINK. This is read by all LINK's after LINK1, as they begin. This text file contains
! the names of files opened for a run, the "counter" info (e.g. NGRID, number of grids, etc), solution number, PARAM's
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE IOUNT1, ONLY                :  MOT4,    MOU4,    WRT_ERR, WRT_LOG

      USE IOUNT1, ONLY                :  ANS,     BUG,     EIN,     ENF,     ERR,     F04,     F06,     IN0,     IN1,     INI,     &
                                         L1A,     NEU,     OT4,     PCH,     SEQ,     SPC,     SC1,                                &
                                         F21,     F22,     F23,     F24,     F25,                                                  &
                                         L1B,     L1C,     L1D,     L1E,     L1F,     L1G,     L1H,     L1I,     L1J,     L1K,     &
                                         L1L,     L1M,     L1N,     L1O,     L1P,     L1Q,     L1R,     L1S,     L1T,     L1U,     &
                                         L1V,     L1W,     L1X,     L1Y,     L1Z,                                                  &
                                         L2A,     L2B,     L2C,     L2D,     L2E,     L2F,     L2G,     L2H,     L2I,     L2J,     &
                                         L2K,     L2L,     L2M,     L2N,     L2O,     L2P,     L2Q,     L2R,     L2S,     L2T,     &
                                         L3A,     L4A,     L4B,     L4C,     L4D,     L5A,     L5B,     OP2,     OU4

      USE IOUNT1, ONLY                :  ANSSTAT, BUGSTAT, EINSTAT, ENFSTAT, ERRSTAT, F04STAT, F06STAT, IN0STAT, IN1STAT, INISTAT, &
                                         L1ASTAT, NEUSTAT, OT4STAT, PCHSTAT, SEQSTAT, SPCSTAT,                                     &
                                         F21STAT, F22STAT, F23STAT, F24STAT, F25STAT,                                              &
                                         L1BSTAT, L1CSTAT, L1DSTAT, L1ESTAT, L1FSTAT, L1GSTAT, L1HSTAT, L1ISTAT, L1JSTAT, L1KSTAT, &
                                         L1LSTAT, L1MSTAT, L1NSTAT, L1OSTAT, L1PSTAT, L1QSTAT, L1RSTAT, L1SSTAT, L1TSTAT, L1USTAT, &
                                         L1VSTAT, L1WSTAT, L1XSTAT, L1YSTAT, L1ZSTAT,                                              &
                                         L2ASTAT, L2BSTAT, L2CSTAT, L2DSTAT, L2ESTAT, L2FSTAT, L2GSTAT, L2HSTAT, L2ISTAT, L2JSTAT, &
                                         L2KSTAT, L2LSTAT, L2MSTAT, L2NSTAT, L2OSTAT, L2PSTAT, L2QSTAT, L2RSTAT, L2SSTAT, L2TSTAT, &
                                         L3ASTAT, L4ASTAT, L4BSTAT, L4CSTAT, L4DSTAT, L5ASTAT, L5BSTAT, OP2STAT, OU4STAT

      USE IOUNT1, ONLY                :  ANSFIL,  BUGFIL,  EINFIL,  ENFFIL,  ERRFIL,  F04FIL,  F06FIL,  IN0FIL,  INIFIL,  LINK1A,  &
                                         NEUFIL,  OT4FIL,  PCHFIL,  SEQFIL,  SPCFIL,  F21FIL,  F22FIL,  F23FIL,  F24FIL,  F25FIL,  &
                                         LINK1A,  LINK1B,  LINK1C,  LINK1D,  LINK1E,  LINK1F,  LINK1G,  LINK1H,  LINK1I,  LINK1J,  &
                                         LINK1K,  LINK1L,  LINK1M,  LINK1N,  LINK1O,  LINK1P,  LINK1Q,  LINK1R,  LINK1S,  LINK1T,  &
                                         LINK1U,  LINK1V,  LINK1W,  LINK1X,  LINK1Y,  LINK1Z,                                      &
                                         LINK2A,  LINK2B,  LINK2C,  LINK2D,  LINK2E,  LINK2F,  LINK2G,  LINK2H,  LINK2I,  LINK2J,  &
                                         LINK2K,  LINK2L,  LINK2M,  LINK2N,  LINK2O,  LINK2P,  LINK2Q,  LINK2R,  LINK2S,  LINK2T,  &
                                         LINK3A,  LINK4A,  LINK4B,  LINK4C,  LINK4D,  LINK5A,  LINK5B,  OP2FIL,  OU4FIL

      USE IOUNT1, ONLY                :  ANS_MSG, BUG_MSG, EIN_MSG, ENF_MSG, ERR_MSG, F04_MSG, F06_MSG, IN0_MSG, IN1_MSG, INI_MSG, &
                                         L1A_MSG, NEU_MSG, OT4_MSG, PCH_MSG, SEQ_MSG, SPC_MSG,                                     &
                                         F21_MSG, F22_MSG, F23_MSG, F24_MSG, F25_MSG,                                              &
                                         L1B_MSG, L1C_MSG, L1D_MSG, L1E_MSG, L1F_MSG, L1G_MSG, L1H_MSG, L1I_MSG, L1J_MSG, L1K_MSG, &
                                         L1L_MSG, L1M_MSG, L1N_MSG, L1O_MSG, L1P_MSG, L1Q_MSG, L1R_MSG, L1S_MSG, L1T_MSG, L1U_MSG, &
                                         L1V_MSG, L1W_MSG, L1X_MSG, L1Y_MSG, L1Z_MSG,                                              &
                                         L2A_MSG, L2B_MSG, L2C_MSG, L2D_MSG, L2E_MSG, L2F_MSG, L2G_MSG, L2H_MSG, L2I_MSG, L2J_MSG, &
                                         L2K_MSG, L2L_MSG, L2M_MSG, L2N_MSG, L2O_MSG, L2P_MSG, L2Q_MSG, L2R_MSG, L2S_MSG, L2T_MSG, &
                                         L3A_MSG, L4A_MSG, L4B_MSG, L4C_MSG, L4D_MSG, L5A_MSG, L5B_MSG, OP2_MSG, OU4_MSG
      USE SCONTR
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_L1A_BEGEND
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, ELFORCEN, HEXAXIS, IORQ1B, IORQ1M, IORQ1S, IORQ2B, IORQ2T,&
                                         MATSPARS, MIN4TRED, QUAD4TYP, QUADAXIS, SPARSTOR

      USE WRITE_L1A_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_L1A'
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! STATUS when closing file LINK1A
      CHARACTER(LEN=*), INTENT(IN)    :: CALL_OUTA_HERE    ! 'Y'/'N' indicator of whether to call OUTA_HERE (this should be 'Y'
!                                                             except when this subr is called by OUTA_HERE
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_L1A_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Units for writing open errors
 
      OUNT(1) = ERR
      OUNT(2) = F06

! Open L1A file and write STIME
 
      OPEN (L1A,FILE=LINK1A,STATUS='REPLACE',IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN
         CALL OPNERR (IOCHK, LINK1A, OUNT, WRITE_F04 )
         CALL FILERR ( OUNT, WRITE_F04 )
         IF (CALL_OUTA_HERE == 'Y') THEN
            CALL OUTA_HERE ( 'N' )
         ENDIF
      ENDIF
      WRITE(L1A,110) STIME
 
! Write current LINK number

      WRITE(L1A,110) LINKNO

! Write solution name
 
      WRITE(L1A,120) SOL_NAME
 
! Write I/0 unit numbers, close status and names
 
      WRITE(L1A,140) SC1                                   !   1

      WRITE(L1A,151) ANS,ANSSTAT,ANS_MSG,ANSFIL            !   2
      WRITE(L1A,151) BUG,BUGSTAT,BUG_MSG,BUGFIL            !   3
      WRITE(L1A,151) EIN,EINSTAT,EIN_MSG,EINFIL            !   4
      WRITE(L1A,151) ENF,ENFSTAT,ENF_MSG,ENFFIL            !   5
      WRITE(L1A,151) ERR,ERRSTAT,ERR_MSG,ERRFIL            !   6
      WRITE(L1A,151) F04,F04STAT,F04_MSG,F04FIL            !   7
      WRITE(L1A,151) F06,F06STAT,F06_MSG,F06FIL            !   8
		WRITE(L1A,151) IN0,IN0STAT,IN0_MSG,IN0FIL            !   9
      WRITE(L1A,151) L1A,L1ASTAT,L1A_MSG,LINK1A            !  10
      WRITE(L1A,151) NEU,NEUSTAT,NEU_MSG,NEUFIL            !  11
      WRITE(L1A,151) PCH,PCHSTAT,PCH_MSG,PCHFIL            !  12
      WRITE(L1A,151) SEQ,SEQSTAT,SEQ_MSG,SEQFIL            !  13
      WRITE(L1A,151) SPC,SPCSTAT,SPC_MSG,SPCFIL            !  14

      WRITE(L1A,151) F21,F21STAT,F21_MSG,F21FIL            ! 15
      WRITE(L1A,151) F22,F22STAT,F22_MSG,F22FIL            ! 16
      WRITE(L1A,151) F23,F23STAT,F23_MSG,F23FIL            ! 17
      WRITE(L1A,151) F24,F24STAT,F24_MSG,F24FIL            ! 18
      WRITE(L1A,151) F25,F25STAT,F25_MSG,F25FIL            ! 19
      WRITE(L1A,151) L1B,L1BSTAT,L1B_MSG,LINK1B            ! 20
      WRITE(L1A,151) L1C,L1CSTAT,L1C_MSG,LINK1C            ! 21
      WRITE(L1A,151) L1D,L1DSTAT,L1D_MSG,LINK1D            ! 22
      WRITE(L1A,151) L1E,L1ESTAT,L1E_MSG,LINK1E            ! 23
      WRITE(L1A,151) L1F,L1FSTAT,L1F_MSG,LINK1F            ! 24
      WRITE(L1A,151) L1G,L1GSTAT,L1G_MSG,LINK1G            ! 25
      WRITE(L1A,151) L1H,L1HSTAT,L1H_MSG,LINK1H            ! 26
      WRITE(L1A,151) L1I,L1ISTAT,L1I_MSG,LINK1I            ! 27
      WRITE(L1A,151) L1J,L1JSTAT,L1J_MSG,LINK1J            ! 28
      WRITE(L1A,151) L1K,L1KSTAT,L1K_MSG,LINK1K            ! 29
      WRITE(L1A,151) L1L,L1LSTAT,L1L_MSG,LINK1L            ! 30
      WRITE(L1A,151) L1M,L1MSTAT,L1M_MSG,LINK1M            ! 31
      WRITE(L1A,151) L1N,L1NSTAT,L1N_MSG,LINK1N            ! 32
      WRITE(L1A,151) L1O,L1OSTAT,L1O_MSG,LINK1O            ! 33
      WRITE(L1A,151) L1P,L1PSTAT,L1P_MSG,LINK1P            ! 34
      WRITE(L1A,151) L1Q,L1QSTAT,L1Q_MSG,LINK1Q            ! 35
      WRITE(L1A,151) L1R,L1RSTAT,L1R_MSG,LINK1R            ! 36
      WRITE(L1A,151) L1S,L1SSTAT,L1S_MSG,LINK1S            ! 37
      WRITE(L1A,151) L1T,L1TSTAT,L1T_MSG,LINK1T            ! 38
      WRITE(L1A,151) L1U,L1USTAT,L1U_MSG,LINK1U            ! 39
      WRITE(L1A,151) L1V,L1VSTAT,L1V_MSG,LINK1V            ! 40
      WRITE(L1A,151) L1W,L1WSTAT,L1W_MSG,LINK1W            ! 41
      WRITE(L1A,151) L1W,L1WSTAT,L1X_MSG,LINK1X            ! 42
      WRITE(L1A,151) L1Y,L1YSTAT,L1Y_MSG,LINK1Y            ! 43
      WRITE(L1A,151) L1Z,L1ZSTAT,L1Z_MSG,LINK1Z            ! 44
      WRITE(L1A,151) L2A,L2ASTAT,L2A_MSG,LINK2A            ! 45
      WRITE(L1A,151) L2B,L2BSTAT,L2B_MSG,LINK2B            ! 46
      WRITE(L1A,151) L2C,L2CSTAT,L2C_MSG,LINK2C            ! 47
      WRITE(L1A,151) L2D,L2DSTAT,L2D_MSG,LINK2D            ! 48
      WRITE(L1A,151) L2E,L2ESTAT,L2E_MSG,LINK2E            ! 49
      WRITE(L1A,151) L2F,L2FSTAT,L2F_MSG,LINK2F            ! 50
      WRITE(L1A,151) L2G,L2GSTAT,L2G_MSG,LINK2G            ! 51
      WRITE(L1A,151) L2H,L2HSTAT,L2H_MSG,LINK2H            ! 52
      WRITE(L1A,151) L2I,L2ISTAT,L2I_MSG,LINK2I            ! 53
      WRITE(L1A,151) L2J,L2JSTAT,L2J_MSG,LINK2J            ! 54
      WRITE(L1A,151) L2K,L2KSTAT,L2K_MSG,LINK2K            ! 55
      WRITE(L1A,151) L2L,L2LSTAT,L2L_MSG,LINK2L            ! 56
      WRITE(L1A,151) L2M,L2MSTAT,L2M_MSG,LINK2M            ! 57
      WRITE(L1A,151) L2N,L2NSTAT,L2N_MSG,LINK2N            ! 58
      WRITE(L1A,151) L2O,L2OSTAT,L2O_MSG,LINK2O            ! 59
      WRITE(L1A,151) L2P,L2PSTAT,L2P_MSG,LINK2P            ! 60
      WRITE(L1A,151) L2Q,L2QSTAT,L2Q_MSG,LINK2Q            ! 61
      WRITE(L1A,151) L2R,L2QSTAT,L2R_MSG,LINK2R            ! 62
      WRITE(L1A,151) L2S,L2QSTAT,L2S_MSG,LINK2S            ! 63
      WRITE(L1A,151) L2T,L2QSTAT,L2T_MSG,LINK2T            ! 64
      WRITE(L1A,151) L3A,L3ASTAT,L3A_MSG,LINK3A            ! 65
      WRITE(L1A,151) L4A,L4ASTAT,L4A_MSG,LINK4A            ! 66
      WRITE(L1A,151) L4B,L4BSTAT,L4B_MSG,LINK4B            ! 67
      WRITE(L1A,151) L4C,L4CSTAT,L4C_MSG,LINK4C            ! 68
      WRITE(L1A,151) L4D,L4DSTAT,L4D_MSG,LINK4D            ! 69
      WRITE(L1A,151) L5A,L5ASTAT,L5A_MSG,LINK5A            ! 70
      WRITE(L1A,151) L5B,L5BSTAT,L5B_MSG,LINK5B            ! 71
      WRITE(L1A,151) OP2,OP2STAT,OP2_MSG,OP2FIL            ! 72
      DO I=1,MOU4
         WRITE(L1A,151) OU4(I),OU4STAT(I),OU4FIL(I)        ! 72+I
      ENDDO
      DO I=1,MOT4
         WRITE(L1A,151) OT4(I),OT4STAT(I),OT4FIL(I)        ! 72+MOU4+I
      ENDDO

! Write counter info from module SCONTR
 
      I = 0

      I = I + 1  ;     WRITE(L1A,160) LBAROFF            , 'LBAROFF               (  1)'  !
      I = I + 1  ;     WRITE(L1A,160) LBUSHOFF           , 'LBUSHOFF              (  2)'  !
      I = I + 1  ;     WRITE(L1A,160) LCMASS             , 'LCMASS                (  3)'  !
      I = I + 1  ;     WRITE(L1A,160) LCONM2             , 'LCONM2                (  4)'  !
      I = I + 1  ;     WRITE(L1A,160) LCORD              , 'LCORD                 (  5)'  !
      I = I + 1  ;     WRITE(L1A,160) LDOFG              , 'LDOFG                 (  6)'  !
      I = I + 1  ;     WRITE(L1A,160) LEDAT              , 'LEDAT                 (  7)'  !
      I = I + 1  ;     WRITE(L1A,160) LELE               , 'LELE                  (  8)'  !
      I = I + 1  ;     WRITE(L1A,160) LFORCE             , 'LFORCE                (  9)'  !
      I = I + 1  ;     WRITE(L1A,160) LGRAV              , 'LGRAV                 ( 10)'  !
      I = I + 1  ;     WRITE(L1A,160) LGRID              , 'LGRID                 ( 11)'  !
      I = I + 1  ;     WRITE(L1A,160) LGUSERIN           , 'LGUSERIN              ( 12)'  !
      I = I + 1  ;     WRITE(L1A,160) LIND_GRDS_MPCS     , 'LIND_GRDS_MPCS        ( 13)'  !
      I = I + 1  ;     WRITE(L1A,160) LLOADC             , 'LLOADC                ( 14)'  !
      I = I + 1  ;     WRITE(L1A,160) LLOADR             , 'LLOADR                ( 15)'  !
      I = I + 1  ;     WRITE(L1A,160) LMATANGLE          , 'LMATANGLE             ( 16)'  !
      I = I + 1  ;     WRITE(L1A,160) LMATL              , 'LMATL                 ( 17)'  !
      I = I + 1  ;     WRITE(L1A,160) LMPC               , 'LMPC                  ( 18)'  !
      I = I + 1  ;     WRITE(L1A,160) LMPCADDC           , 'LMPCADDC              ( 19)'  !
      I = I + 1  ;     WRITE(L1A,160) LMPCADDR           , 'LMPCADDR              ( 20)'  !
      I = I + 1  ;     WRITE(L1A,160) LPBAR              , 'LPBAR                 ( 21)'  !
      I = I + 1  ;     WRITE(L1A,160) LPBEAM             , 'LPBEAM                ( 22)'  !
      I = I + 1  ;     WRITE(L1A,160) LPBUSH             , 'LPBUSH                ( 23)'  !
      I = I + 1  ;     WRITE(L1A,160) LPCOMP             , 'LPCOMP                ( 24)'  !
      I = I + 1  ;     WRITE(L1A,160) LPCOMP_PLIES       , 'LPCOMP_PLIES          ( 25)'  !
      I = I + 1  ;     WRITE(L1A,160) LPDAT              , 'LPDAT                 ( 26)'  !
      I = I + 1  ;     WRITE(L1A,160) LPELAS             , 'LPELAS                ( 27)'  !
      I = I + 1  ;     WRITE(L1A,160) LPLATEOFF          , 'LPLATEOFF             ( 28)'  !
      I = I + 1  ;     WRITE(L1A,160) LPLATETHICK        , 'LPLATETHICK           ( 29)'  !
      I = I + 1  ;     WRITE(L1A,160) LPLOAD             , 'LPLOAD                ( 30)'  !
      I = I + 1  ;     WRITE(L1A,160) LPLOTEL            , 'LPLOTEL               ( 31)'  !
      I = I + 1  ;     WRITE(L1A,160) LPMASS             , 'LPMASS                ( 32)'  !
      I = I + 1  ;     WRITE(L1A,160) LPROD              , 'LPROD                 ( 33)'  !
      I = I + 1  ;     WRITE(L1A,160) LPSHEAR            , 'LPSHEAR               ( 34)'  !
      I = I + 1  ;     WRITE(L1A,160) LPSHEL             , 'LPSHEL                ( 35)'  !
      I = I + 1  ;     WRITE(L1A,160) LPSOLID            , 'LPSOLID               ( 36)'  !
      I = I + 1  ;     WRITE(L1A,160) LPUSER1            , 'LPUSER1               ( 37)'  !
      I = I + 1  ;     WRITE(L1A,160) LPUSERIN           , 'LPUSERIN              ( 38)'  !
      I = I + 1  ;     WRITE(L1A,160) LRFORCE            , 'LRFORCE               ( 39)'  !
      I = I + 1  ;     WRITE(L1A,160) LRIGEL             , 'LRIGEL                ( 40)'  !
      I = I + 1  ;     WRITE(L1A,160) LSEQ               , 'LSEQ                  ( 41)'  !
      I = I + 1  ;     WRITE(L1A,160) LSETLN             , 'LSETLN                ( 42)'  !
      I = I + 1  ;     WRITE(L1A,160) LSETS              , 'LSETS                 ( 43)'  !
      I = I + 1  ;     WRITE(L1A,160) LSLOAD             , 'LSLOAD                ( 44)'  !
      I = I + 1  ;     WRITE(L1A,160) LSPC               , 'LSPC                  ( 45)'  !
      I = I + 1  ;     WRITE(L1A,160) LSPC1              , 'LSPC1                 ( 46)'  !
      I = I + 1  ;     WRITE(L1A,160) LSPCADDC           , 'LSPCADDC              ( 47)'  !
      I = I + 1  ;     WRITE(L1A,160) LSPCADDR           , 'LSPCADDR              ( 48)'  !
      I = I + 1  ;     WRITE(L1A,160) LSUB               , 'LSUB                  ( 49)'  !
      I = I + 1  ;     WRITE(L1A,160) LSUSERIN           , 'LSUSERIN              ( 50)'  !
      I = I + 1  ;     WRITE(L1A,160) LTDAT              , 'LTDAT                 ( 51)'  !
      I = I + 1  ;     WRITE(L1A,160) LTERM_KGG          , 'LTERM_KGG             ( 52)'  !
      I = I + 1  ;     WRITE(L1A,160) LTERM_KGGD         , 'LTERM_KGGD            ( 53)'  !
      I = I + 1  ;     WRITE(L1A,160) LTERM_MGGE         , 'LTERM_MGGE            ( 54)'  !
      I = I + 1  ;     WRITE(L1A,160) LVVEC              , 'LVVEC                 ( 55)'  !
      I = I + 1  ;     WRITE(L1A,160) MAX_ELEM_DEGREE    , 'MAX_ELEM_DEGREE       ( 56)'  !
      I = I + 1  ;     WRITE(L1A,160) MAX_GAUSS_POINTS   , 'MAX_GAUSS_POINTS      ( 57)'  !
      I = I + 1  ;     WRITE(L1A,160) MAX_STRESS_POINTS  , 'MAX_STRESS_POINTS     ( 58)'  !
      I = I + 1  ;     WRITE(L1A,160) MDT                , 'MDT                   ( 59)'  !
      I = I + 1  ;     WRITE(L1A,160) MELGP              , 'MELGP                 ( 60)'  !
      I = I + 1  ;     WRITE(L1A,160) MELDOF             , 'MELDOF                ( 61)'  !
      I = I + 1  ;     WRITE(L1A,160) MID1_PCOMP_EQ      , 'MID1_PCOMP_EQ         ( 62)'  !
      I = I + 1  ;     WRITE(L1A,160) MID2_PCOMP_EQ      , 'MID2_PCOMP_EQ         ( 63)'  !
      I = I + 1  ;     WRITE(L1A,160) MID3_PCOMP_EQ      , 'MID3_PCOMP_EQ         ( 64)'  !
      I = I + 1  ;     WRITE(L1A,160) MID4_PCOMP_EQ      , 'MID4_PCOMP_EQ         ( 65)'  !
      I = I + 1  ;     WRITE(L1A,160) MLL_SDIA           , 'MLL_SDIA              ( 66)'  !
      I = I + 1  ;     WRITE(L1A,160) MMPC               , 'MMPC                  ( 67)'  !
      I = I + 1  ;     WRITE(L1A,160) MOFFSET            , 'MOFFSET               ( 68)'  !
      I = I + 1  ;     WRITE(L1A,160) MRBE3              , 'MRBE3                 ( 69)'  !
      I = I + 1  ;     WRITE(L1A,160) MRSPLINE           , 'MRSPLINE              ( 70)'  !
      I = I + 1  ;     WRITE(L1A,160) NAOCARD            , 'NAOCARD               ( 71)'  !
      I = I + 1  ;     WRITE(L1A,160) NBAROFF            , 'NBAROFF               ( 72)'  !
      I = I + 1  ;     WRITE(L1A,160) NBUSHOFF           , 'NBUSHOFF              ( 73)'  !
      I = I + 1  ;     WRITE(L1A,160) NBAROR             , 'NBAROR                ( 74)'  !
      I = I + 1  ;     WRITE(L1A,160) NBEAMOR            , 'NBEAMOR               ( 75)'  !
      I = I + 1  ;     WRITE(L1A,160) NCBAR              , 'NCBAR                 ( 76)'  !
      I = I + 1  ;     WRITE(L1A,160) NCBEAM             , 'NCBEAM                ( 77)'  !
      I = I + 1  ;     WRITE(L1A,160) NCBUSH             , 'NCBUSH                ( 78)'  !
      I = I + 1  ;     WRITE(L1A,160) NCELAS1            , 'NCELAS1               ( 79)'  !
      I = I + 1  ;     WRITE(L1A,160) NCELAS2            , 'NCELAS2               ( 80)'  !
      I = I + 1  ;     WRITE(L1A,160) NCELAS3            , 'NCELAS3               ( 81)'  !
      I = I + 1  ;     WRITE(L1A,160) NC_INFILE          , 'NC_INFILE             ( 82)'  !
      I = I + 1  ;     WRITE(L1A,160) NCELAS4            , 'NCELAS4               ( 83)'  !
      I = I + 1  ;     WRITE(L1A,160) NCHEXA8            , 'NCHEXA8               ( 84)'  !
      I = I + 1  ;     WRITE(L1A,160) NCHEXA20           , 'NCHEXA20              ( 85)'  !
      I = I + 1  ;     WRITE(L1A,160) NCMASS             , 'NCMASS                ( 86)'  !
      I = I + 1  ;     WRITE(L1A,160) NCONM2             , 'NCONM2                ( 87)'  !
      I = I + 1  ;     WRITE(L1A,160) NCORD              , 'NCORD                 ( 88)'  !
      I = I + 1  ;     WRITE(L1A,160) NCORD1             , 'NCORD1                ( 89)'  !
      I = I + 1  ;     WRITE(L1A,160) NCORD2             , 'NCORD2                ( 90)'  !
      I = I + 1  ;     WRITE(L1A,160) NCPENTA6           , 'NCPENTA6              ( 91)'  !
      I = I + 1  ;     WRITE(L1A,160) NCPENTA15          , 'NCPENTA15             ( 92)'  !
      I = I + 1  ;     WRITE(L1A,160) NCQUAD4            , 'NCQUAD4               ( 93)'  !
      I = I + 1  ;     WRITE(L1A,160) NCQUAD4K           , 'NCQUAD4K              ( 94)'  !
      I = I + 1  ;     WRITE(L1A,160) NCROD              , 'NCROD                 ( 95)'  !
      I = I + 1  ;     WRITE(L1A,160) NCSHEAR            , 'NCSHEAR               ( 96)'  !
      I = I + 1  ;     WRITE(L1A,160) NCTETRA4           , 'NCTETRA4              ( 97)'  !
      I = I + 1  ;     WRITE(L1A,160) NCTETRA10          , 'NCTETRA10             ( 98)'  !
      I = I + 1  ;     WRITE(L1A,160) NCTRIA3            , 'NCTRIA3               ( 99)'  !
      I = I + 1  ;     WRITE(L1A,160) NCTRIA3K           , 'NCTRIA3K              (100)'  !
      I = I + 1  ;     WRITE(L1A,160) NCUSER1            , 'NCUSER1               (101)'  !
      I = I + 1  ;     WRITE(L1A,160) NCUSERIN           , 'NCUSERIN              (102)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFA              , 'NDOFA                 (103)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOF_EIG           , 'NDOF_EIG              (104)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFF              , 'NDOFF                 (105)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFG              , 'NDOFG                 (106)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFL              , 'NDOFL                 (107)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFM              , 'NDOFM                 (108)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFN              , 'NDOFN                 (109)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFO              , 'NDOFO                 (110)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFR              , 'NDOFR                 (111)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFS              , 'NDOFS                 (112)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFSA             , 'NDOFSA                (113)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFSB             , 'NDOFSB                (114)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFSE             , 'NDOFSE                (115)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFSG             , 'NDOFSG                (116)'  !
      I = I + 1  ;     WRITE(L1A,160) NDOFSZ             , 'NDOFSZ                (117)'  !
      I = I + 1  ;     WRITE(L1A,160) NEDAT              , 'NEDAT                 (118)'  !
      I = I + 1  ;     WRITE(L1A,160) NELE               , 'NELE                  (119)'  !
      I = I + 1  ;     WRITE(L1A,160) NFORCE             , 'NFORCE                (120)'  !
      I = I + 1  ;     WRITE(L1A,160) NGRAV              , 'NGRAV                 (121)'  !
      I = I + 1  ;     WRITE(L1A,160) NGRDSET            , 'NGRDSET               (122)'  !
      I = I + 1  ;     WRITE(L1A,160) NGRID              , 'NGRID                 (123)'  !
      I = I + 1  ;     WRITE(L1A,160) NIND_GRDS_MPCS     , 'NIND_GRDS_MPCS        (124)'  !
      I = I + 1  ;     WRITE(L1A,160) NLOAD              , 'NLOAD                 (125)'  !
      I = I + 1  ;     WRITE(L1A,160) NMATANGLE          , 'NMATANGLE             (126)'  !
      I = I + 1  ;     WRITE(L1A,160) NMATL              , 'NMATL                 (127)'  !
      I = I + 1  ;     WRITE(L1A,160) NMPC               , 'NMPC                  (128)'  !
      I = I + 1  ;     WRITE(L1A,160) NMPCADD            , 'NMPCADD               (129)'  !
      I = I + 1  ;     WRITE(L1A,160) NPBAR              , 'NPBAR                 (130)'  !
      I = I + 1  ;     WRITE(L1A,160) NPBARL             , 'NPBARL                (131)'  !
      I = I + 1  ;     WRITE(L1A,160) NPBEAM             , 'NPBEAM                (132)'  !
      I = I + 1  ;     WRITE(L1A,160) NPBUSH             , 'NPBUSH                (133)'  !
      I = I + 1  ;     WRITE(L1A,160) NPCARD             , 'NPCARD                (134)'  !
      I = I + 1  ;     WRITE(L1A,160) NPCOMP             , 'NPCOMP                (135)'  !
      I = I + 1  ;     WRITE(L1A,160) NPDAT              , 'NPDAT                 (136)'  !
      I = I + 1  ;     WRITE(L1A,160) NPELAS             , 'NPELAS                (137)'  !
      I = I + 1  ;     WRITE(L1A,160) NPLATEOFF          , 'NPLATEOFF             (138)'  !
      I = I + 1  ;     WRITE(L1A,160) NPLATETHICK        , 'NPLATETHICK           (139)'  !
      I = I + 1  ;     WRITE(L1A,160) NPLOTEL            , 'NPLOTEL               (140)'  !
      I = I + 1  ;     WRITE(L1A,160) NPLOAD             , 'NPLOAD                (141)'  !
      I = I + 1  ;     WRITE(L1A,160) NPLOAD4_3D         , 'NPLOAD4_3D            (142)'  !
      I = I + 1  ;     WRITE(L1A,160) NPMASS             , 'NPMASS                (143)'  !
      I = I + 1  ;     WRITE(L1A,160) NPROD              , 'NPROD                 (144)'  !
      I = I + 1  ;     WRITE(L1A,160) NPSHEAR            , 'NPSHEAR               (145)'  !
      I = I + 1  ;     WRITE(L1A,160) NPSHEL             , 'NPSHEL                (146)'  !
      I = I + 1  ;     WRITE(L1A,160) NPSOLID            , 'NPSOLID               (147)'  !
      I = I + 1  ;     WRITE(L1A,160) NPUSER1            , 'NPUSER1               (148)'  !
      I = I + 1  ;     WRITE(L1A,160) NPUSERIN           , 'NPUSERIN              (149)'  !
      I = I + 1  ;     WRITE(L1A,160) NRBAR              , 'NRBAR                 (150)'  !
      I = I + 1  ;     WRITE(L1A,160) NRBE1              , 'NRBE1                 (151)'  !
      I = I + 1  ;     WRITE(L1A,160) NRBE2              , 'NRBE2                 (152)'  !
      I = I + 1  ;     WRITE(L1A,160) NRFORCE            , 'NRFORCE               (153)'  !
      I = I + 1  ;     WRITE(L1A,160) NRIGEL             , 'NRIGEL                (154)'  !
      I = I + 1  ;     WRITE(L1A,160) NRECARD            , 'NRECARD               (155)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_ACCE     , 'NROWS_OTM_ACCE        (156)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_DISP     , 'NROWS_OTM_DISP        (157)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_MPCF     , 'NROWS_OTM_MPCF        (158)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_SPCF     , 'NROWS_OTM_SPCF        (159)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_ELFE     , 'NROWS_OTM_ELFE        (160)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_ELFN     , 'NROWS_OTM_ELFN        (161)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_STRE     , 'NROWS_OTM_STRE        (162)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_OTM_STRN     , 'NROWS_OTM_STRN        (163)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_ACCE     , 'NROWS_TXT_ACCE        (164)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_DISP     , 'NROWS_TXT_DISP        (165)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_MPCF     , 'NROWS_TXT_MPCF        (166)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_SPCF     , 'NROWS_TXT_SPCF        (167)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_ELFE     , 'NROWS_TXT_ELFE        (168)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_ELFN     , 'NROWS_TXT_ELFN        (169)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_STRE     , 'NROWS_TXT_STRE        (170)'  !
      I = I + 1  ;     WRITE(L1A,160) NROWS_TXT_STRN     , 'NROWS_TXT_STRN        (171)'  !
      I = I + 1  ;     WRITE(L1A,160) NRSPLINE           , 'NRSPLINE              (172)'  !
      I = I + 1  ;     WRITE(L1A,160) NSEQ               , 'NSEQ                  (173)'  !
      I = I + 1  ;     WRITE(L1A,160) NSETS              , 'NSETS                 (174)'  !
      I = I + 1  ;     WRITE(L1A,160) NSLOAD             , 'NSLOAD                (175)'  !
      I = I + 1  ;     WRITE(L1A,160) NSPC               , 'NSPC                  (176)'  !
      I = I + 1  ;     WRITE(L1A,160) NSPC1              , 'NSPC1                 (177)'  !
      I = I + 1  ;     WRITE(L1A,160) NSPCADD            , 'NSPCADD               (178)'  !
      I = I + 1  ;     WRITE(L1A,160) NSPOINT            , 'NSPOINT               (179)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_SPCSIDS        , 'NUM_SPCSIDS           (180)'  !
      I = I + 1  ;     WRITE(L1A,160) NSUB               , 'NSUB                  (181)'  !
      I = I + 1  ;     WRITE(L1A,160) NTCARD             , 'NTCARD                (182)'  !
      I = I + 1  ;     WRITE(L1A,160) NTDAT              , 'NTDAT                 (183)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_ALL          , 'NTERM_ALL             (184)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_CG_LTM       , 'NTERM_CG_LTM          (185)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_DLR          , 'NTERM_DLR             (186)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_GMN          , 'NTERM_GMN             (187)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_GOA          , 'NTERM_GOA             (188)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_HMN          , 'NTERM_HMN             (189)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_IF_LTM       , 'NTERM_IF_LTM          (190)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_IRR          , 'NTERM_IRR             (191)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KAA          , 'NTERM_KAA             (192)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KAAD         , 'NTERM_KAAD            (193)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KAO          , 'NTERM_KAO             (194)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KAOD         , 'NTERM_KAOD            (195)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KFF          , 'NTERM_KFF             (196)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KFFD         , 'NTERM_KFFD            (197)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KFS          , 'NTERM_KFS             (198)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KFSD         , 'NTERM_KFSD            (199)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KFSe         , 'NTERM_KFSe            (200)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KFSDe        , 'NTERM_KFSDe           (201)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KGG          , 'NTERM_KGG             (202)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KGGD         , 'NTERM_KGGD            (203)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KLL          , 'NTERM_KLL             (204)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KLLD         , 'NTERM_KLLD            (205)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KLLDn        , 'NTERM_KLLDn           (206)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KLLs         , 'NTERM_KLLs            (207)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KLLDs        , 'NTERM_KLLDs           (208)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KMM          , 'NTERM_KMM             (209)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KMMD         , 'NTERM_KMMD            (210)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KMSM         , 'NTERM_KMSM            (211)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KMSMn        , 'NTERM_KMSMn           (212)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KMSMs        , 'NTERM_KMSMs           (213)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KNM          , 'NTERM_KNM             (214)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KNMD         , 'NTERM_KNMD            (215)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KNN          , 'NTERM_KNN             (216)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KNND         , 'NTERM_KNND            (217)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KOO          , 'NTERM_KOO             (218)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KOOD         , 'NTERM_KOOD            (219)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KOOs         , 'NTERM_KOOs            (220)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KOODs        , 'NTERM_KOODs           (221)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRL          , 'NTERM_KRL             (222)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRLD         , 'NTERM_KRLD            (223)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRR          , 'NTERM_KRR             (224)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRRD         , 'NTERM_KRRD            (225)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRRcb        , 'NTERM_KRRcb           (226)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRRcbs       , 'NTERM_KRRcbs          (227)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KRRcbn       , 'NTERM_KRRcbn          (228)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KXX          , 'NTERM_KXX             (229)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KSS          , 'NTERM_KSS             (230)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KSSD         , 'NTERM_KSSD            (231)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KSSe         , 'NTERM_KSSe            (232)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_KSSDe        , 'NTERM_KSSDe           (233)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_LMN          , 'NTERM_LMN             (234)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_LTM          , 'NTERM_LTM             (235)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MAA          , 'NTERM_MAA             (236)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MAO          , 'NTERM_MAO             (237)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MFF          , 'NTERM_MFF             (238)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MFS          , 'NTERM_MFS             (239)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MGG          , 'NTERM_MGG             (240)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MGGC         , 'NTERM_MGGC            (241)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MGGE         , 'NTERM_MGGE            (242)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MGGS         , 'NTERM_MGGS            (243)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MLL          , 'NTERM_MLL             (244)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MLLn         , 'NTERM_MLLn            (245)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MLLs         , 'NTERM_MLLs            (246)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MLR          , 'NTERM_MLR             (247)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MMM          , 'NTERM_MMM             (248)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MNM          , 'NTERM_MNM             (249)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MNN          , 'NTERM_MNN             (250)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MOO          , 'NTERM_MOO             (251)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MPF0         , 'NTERM_MPF0            (252)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MRL          , 'NTERM_MRL             (253)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MRN          , 'NTERM_MRN             (254)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MRR          , 'NTERM_MRR             (255)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MRRcb        , 'NTERM_MRRcb           (256)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MRRcbn       , 'NTERM_MRRcbn          (257)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MXX          , 'NTERM_MXX             (258)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MXXn         , 'NTERM_MXXn            (259)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_MSS          , 'NTERM_MSS             (260)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PA           , 'NTERM_PA              (261)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PF           , 'NTERM_PF              (262)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PFYS         , 'NTERM_PFYS            (263)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PG           , 'NTERM_PG              (264)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PHIXA        , 'NTERM_PHIXA           (265)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PHIXG        , 'NTERM_PHIXG           (266)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PHIZG        , 'NTERM_PHIZG           (267)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PHIZL        , 'NTERM_PHIZL           (268)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PHIZL1       , 'NTERM_PHIZL1          (269)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PHIZL2       , 'NTERM_PHIZL2          (270)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PL           , 'NTERM_PL              (271)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PM           , 'NTERM_PM              (272)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PN           , 'NTERM_PN              (273)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PO           , 'NTERM_PO              (274)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PR           , 'NTERM_PR              (275)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_PS           , 'NTERM_PS              (276)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_QM           , 'NTERM_QM              (277)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_QS           , 'NTERM_QS              (278)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_QSYS         , 'NTERM_QSYS            (279)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_RMG          , 'NTERM_RMG             (280)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_RMM          , 'NTERM_RMM             (281)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_RMN          , 'NTERM_RMN             (282)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_ULL          , 'NTERM_ULL             (283)'  !
      I = I + 1  ;     WRITE(L1A,160) NTERM_ULLI         , 'NTERM_ULLI            (284)'  !
      I = I + 1  ;     WRITE(L1A,160) NTSUB              , 'NTSUB                 (285)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_CB_DOFS        , 'NUM_CB_DOFS           (286)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_EIGENS         , 'NUM_EIGENS            (287)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_KLLD_DIAG_ZEROS, 'NUM_KLLD_DIAG_ZEROS   (288)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_MLL_DIAG_ZEROS , 'NUM_MLL_DIAG_ZEROS    (289)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_MPCSIDS        , 'NUM_MPCSIDS           (290)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_PARTVEC_RECORDS, 'NUM_PARTVEC_RECORDS   (291)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_PCHD_SPC1      , 'NUM_PCHD_SPC1         (292)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_SPC_RECORDS    , 'NUM_SPC_RECORDS       (293)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_SPC1_RECORDS   , 'NUM_SPC1_RECORDS      (294)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_SUPT_CARDS     , 'NUM_SUPT_CARDS        (295)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_USET_RECORDS   , 'NUM_USET_RECORDS      (296)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_USETSTR        , 'NUM_USETSTR           (297)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_USET           , 'NUM_USET              (298)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_USET_U1        , 'NUM_USET_U1           (299)'  !
      I = I + 1  ;     WRITE(L1A,160) NUM_USET_U2        , 'NUM_USET_U2           (300)'  !
      I = I + 1  ;     WRITE(L1A,160) NVEC               , 'NVEC                  (301)'  !
      I = I + 1  ;     WRITE(L1A,160) NVVEC              , 'NVVEC                 (302)'  !
      I = I + 1  ;     WRITE(L1A,160) PCH_LINE_NUM       , 'PCH_LINE_NUM          (303)'  !
      I = I + 1  ;     WRITE(L1A,160) SETLEN             , 'SETLEN                (304)'  !

! Write PARAM's (some need to stay the same in a restart)

      WRITE(L1A,191) ELFORCEN, 'PARAM ELFORCEN (needed for restart)'
      WRITE(L1A,191) HEXAXIS , 'PARAM HEXAXIS  (needed for restart)'
      WRITE(L1A,191) MATSPARS, 'PARAM MATSPARS (needed for restart)'
      WRITE(L1A,191) MIN4TRED, 'PARAM MIN4TRED (needed for restart)'
      WRITE(L1A,191) QUAD4TYP, 'PARAM QUAD4TYP (needed for restart)'
      WRITE(L1A,191) QUADAXIS, 'PARAM QUADAXIS (needed for restart)'
      WRITE(L1A,191) SPARSTOR, 'PARAM SPARSTOR (needed for restart)'
      WRITE(L1A,192) IORQ1M  , 'PARAM IORQ1M   (needed for restart)'
      WRITE(L1A,192) IORQ1S  , 'PARAM IORQ1S   (needed for restart)'
      WRITE(L1A,192) IORQ1B  , 'PARAM IORQ1B   (needed for restart)'
      WRITE(L1A,192) IORQ2B  , 'PARAM IORQ2B   (needed for restart)'
      WRITE(L1A,192) IORQ2T  , 'PARAM IORQ2T   (needed for restart)'
      WRITE(L1A,193) CBMIN3  , 'PARAM CBMIN3   (needed for restart)'
      WRITE(L1A,193) CBMIN4  , 'PARAM CBMIN4   (needed for restart)'

! Write COMM

      WRITE(L1A,103) (COMM(I),I=0,49)

      CALL FILE_CLOSE ( L1A, LINK1A, CLOSE_STAT, 'Y' )
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  103 FORMAT(1X,50A1)

  110 FORMAT(1X,I14)

  120 FORMAT(1X,A)

  121 FORMAT(1X,1ES13.6)

  140 FORMAT(1X,I14)

  151 FORMAT(1X,I14,1X,A,1X,A,1X,A)

  160 FORMAT(I15,1X,A)

  191 FORMAT(6X,A9,1X,A)

  192 FORMAT(7X,I8,1X,A)

  193 FORMAT(1X,1ES14.6,1X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_L1A
