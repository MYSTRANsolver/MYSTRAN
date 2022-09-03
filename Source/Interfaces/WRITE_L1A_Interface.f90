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

   MODULE WRITE_L1A_Interface

   INTERFACE

      SUBROUTINE WRITE_L1A ( CLOSE_STAT, CALL_OUTA_HERE, WRITE_F04 )

 
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

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! STATUS when closing file LINK1A
      CHARACTER(LEN=*), INTENT(IN)    :: CALL_OUTA_HERE    ! 'Y'/'N' indicator of whether to call OUTA_HERE (this should be 'Y'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_L1A_BEGEND

      END SUBROUTINE WRITE_L1A

   END INTERFACE

   END MODULE WRITE_L1A_Interface

