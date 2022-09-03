! ###############################################################################################################################
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

   MODULE MYSTRAN_FILES_Interface

   INTERFACE

      SUBROUTINE MYSTRAN_FILES ( START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE, START_SEC, START_SFRAC)

 
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

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MYSTRAN_FILES_BEGEND
      INTEGER(LONG), INTENT(IN)       :: START_HOUR        ! The hour     when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_MINUTE      ! The minute   when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_SEC         ! The second   when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_SFRAC       ! The sec frac when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_YEAR        ! The year     when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_MONTH       ! The month    when MYSTRAN started.
      INTEGER(LONG), INTENT(IN)       :: START_DAY         ! The day      when MYSTRAN started.
 
      END SUBROUTINE MYSTRAN_FILES

   END INTERFACE

   END MODULE MYSTRAN_FILES_Interface

