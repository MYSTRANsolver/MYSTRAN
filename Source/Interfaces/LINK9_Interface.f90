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

   MODULE LINK9_Interface

   INTERFACE

      SUBROUTINE LINK9 ( LK9_PROC_NUM )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG

      USE IOUNT1, ONLY                :  ANS, ERR, F04, F06, F25, L1E, L1M, L1R, L2A, L2B, L2C, L2D, L2I, L2J, L2R, L2S,           &
                                         L5A, L5B, NEU, OT4, OU4, PCH, SC1

      USE IOUNT1, ONLY                :  ANSFIL, F06FIL, F25FIL, LINK1B, LINK1E, LINK1M, LINK1R, LINK2A, LINK2B, LINK2C, LINK2D,   &
                                         LINK2I, LINK2J, LINK2R, LINK2S, LINK5A, LINK5B, MOT4  , MOU4  , NEUFIL, OT4FIL, OU4FIL,   &
                                         PCHFIL

      USE IOUNT1, ONLY                :  L1ASTAT, L1ESTAT, L1MSTAT, L1RSTAT, L2ASTAT, L2BSTAT, L2CSTAT, L2ISTAT, L2JSTAT, L2RSTAT, &
                                         L2SSTAT, OT4STAT, OU4STAT, PCHSTAT

      USE IOUNT1, ONLY                :  ANS_MSG, F25_MSG, L1E_MSG, L1M_MSG, L1R_MSG, L2A_MSG, L2B_MSG, L2C_MSG, L2D_MSG, L2I_MSG, &
                                         L2J_MSG, L2R_MSG, L2S_MSG, L5A_MSG, L5B_MSG, NEU_MSG, PCH_MSG,                            &
                                         OT4_MSG, OU4_MSG, OT4_GRD_OTM, OT4_ELM_OTM, OU4_GRD_OTM, OU4_ELM_OTM

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_ENTRY_LEN, COMM, IBIT, INT_SC_NUM, JTSUB, FATAL_ERR,                     &
                                         FEMAP_VERSION, LINKNO, MBUG,                                                              &
                                         NDOFF, NDOFG, NDOFL, NDOFM, NDOFN, ndofo, NDOFR, NDOFS, NDOFSA, NGRID, NSUB, NVEC,        &
                                         NTERM_IF_LTM, NTERM_GMN, NTERM_HMN, NTERM_KFS, NTERM_KFSD, NTERM_LMN, NTERM_MFS,          &
                                         NTERM_MGG, NTERM_MLL,NTERM_PG, NTERM_PM, NTERM_PS, NTERM_QSYS,                            &
                                         NUM_CB_DOFS, NUM_EIGENS,                                                                  &
                                         NROWS_OTM_ACCE, NROWS_OTM_DISP, NROWS_OTM_MPCF, NROWS_OTM_SPCF,                           &
                                         NROWS_OTM_ELFE, NROWS_OTM_ELFN, NROWS_OTM_STRE, NROWS_OTM_STRN,                           &
                                         NROWS_TXT_ACCE, NROWS_TXT_DISP, NROWS_TXT_MPCF, NROWS_TXT_SPCF,                           &
                                         NROWS_TXT_ELFE, NROWS_TXT_ELFN, NROWS_TXT_STRE, NROWS_TXT_STRN, RESTART, SOL_NAME, WARN_ERR

      USE SCONTR, ONLY                :  GROUT_ACCE_BIT, GROUT_DISP_BIT, GROUT_OLOA_BIT, GROUT_SPCF_BIT, GROUT_MPCF_BIT,           &
                                         GROUT_GPFO_BIT, ELOUT_ELFN_BIT, ELOUT_ELFE_BIT, ELOUT_STRE_BIT, ELOUT_STRN_BIT,           &
                                         ELDT_F25_U_P_BIT

      USE CC_OUTPUT_DESCRIBERS, ONLY  :  DISP_OUT
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  LINK9_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  EPSIL, MPFOUT, POST, SUPINFO, SUPWARN, WTMASS
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE COL_VECS, ONLY              :  FG_COL, UG_COL, PG_COL, PM_COL, PS_COL, QSYS_COL, QGm_COL, QGr_COL, QGs_COL, QR_COL,      &
                                         PHIXG_COL, PHIXN_COL
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, GEN_MASS, MODE_NUM
      USE OUTPUT4_MATRICES, ONLY      :  NUM_OU4_REQUESTS, OU4_PART_MAT_NAMES, HAS_OU4_MAT_BEEN_PROCESSED, OU4_PART_MAT_NAMES
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ACCE, OTM_DISP, OTM_MPCF, OTM_SPCF, OTM_ELFE, OTM_ELFN, OTM_STRE, OTM_STRN,           &
                                         TXT_ACCE, TXT_DISP, TXT_MPCF, TXT_SPCF, TXT_ELFE, TXT_ELFN, TXT_STRE, TXT_STRN

      USE SPARSE_MATRICES, ONLY       :  I_GMN , J_GMN , GMN , I_GMNt, J_GMNt, GMNt, I_HMN , J_HMN , HMN ,                         &
                                         I_KSF , J_KSF , KSF , I_KSFD, J_KSFD, KSFD, I_LMN , J_LMN , LMN ,                         &
                                         I_MGG , J_MGG , MGG , I_MLL , J_MLL , MLL , I_MSF , J_MSF , MSF ,                         &
                                         I_PG  , J_PG  , PG  , I_PM  , J_PM  , PM  , I_PS  , J_PS  , PS  , I_QSYS, J_QSYS, QSYS

      USE SPARSE_MATRICES, ONLY       :  I_IF_LTM, J_IF_LTM, IF_LTM, SYM_MGG, SYM_MSF, SYM_PG, SYM_PM

      USE DOF_TABLES, ONLY            :  TDOF

      USE MODEL_STUF, ONLY            :  ANY_ACCE_OUTPUT, ANY_DISP_OUTPUT, ANY_MPCF_OUTPUT, ANY_SPCF_OUTPUT, ANY_OLOA_OUTPUT,      &
                                         ANY_GPFO_OUTPUT, ANY_ELFE_OUTPUT, ANY_ELFN_OUTPUT, ANY_STRE_OUTPUT, ANY_STRN_OUTPUT,      &
                                         OELDT, OELOUT, OGROUT, GRID, GROUT, MEFFMASS_CALC, MPFACTOR_CALC, SCNUM, SUBLOD, TITLE,   &
                                         STITLE, LABEL
      USE LINK9_STUFF, ONLY           :  MAXREQ

      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(14*BYTE)              :: CTIME             ! A char variable to which STIME will be written (for use in NEU file)

      INTEGER(LONG), INTENT(IN)       :: LK9_PROC_NUM      ! 2 if this is the LINK9 call for the linear buckling step of 
      INTEGER(LONG), PARAMETER        :: NUM1      = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2      = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LINK9_BEGEND + 1
 
      END SUBROUTINE LINK9

   END INTERFACE

   END MODULE LINK9_Interface

