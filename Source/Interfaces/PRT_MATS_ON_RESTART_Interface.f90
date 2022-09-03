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

   MODULE PRT_MATS_ON_RESTART_Interface

   INTERFACE

      SUBROUTINE PRT_MATS_ON_RESTART


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_LOG

      USE IOUNT1, ONLY                :  L1E    , L1H    , L1J    , L1L    , L1R    , L2A    , L2B    , L2C    , L2D    , L2E    , &
                                         L2F    , L2G    , L2H    , L2I    , L2J    , L2K    , L2L    , L2M    , L2N    , L2O    , &
                                         L2P    , L2Q    , L3A    , L5A

      USE IOUNT1, ONLY                :  LINK1E , LINK1H , LINK1J , LINK1L , LINK1R , LINK2A , LINK2B , LINK2C , LINK2D , LINK2E , &
                                         LINK2F , LINK2G , LINK2H , LINK2I , LINK2J , LINK2K , LINK2L , LINK2M , LINK2N , LINK2O , &
                                         LINK2P , LINK2Q , LINK3A , LINK5A

      USE IOUNT1, ONLY                :  L1ESTAT, L1HSTAT, L1JSTAT, L1LSTAT, L1RSTAT, L2ASTAT, L2BSTAT, L2CSTAT, L2DSTAT, L2ESTAT, &
                                         L2FSTAT, L2GSTAT, L2HSTAT, L2ISTAT, L2JSTAT, L2KSTAT, L2LSTAT, L2MSTAT, L2NSTAT, L2OSTAT, &
                                         L2PSTAT, L2QSTAT, L3ASTAT, L5ASTAT

      USE IOUNT1, ONLY                :  L1E_MSG, L1H_MSG, L1J_MSG, L1L_MSG, L1R_MSG, L2A_MSG, L2B_MSG, L2C_MSG, L2D_MSG, L2E_MSG, &
                                         L2F_MSG, L2G_MSG, L2H_MSG, L2I_MSG, L2J_MSG, L2K_MSG, L2L_MSG, L2M_MSG, L2N_MSG, L2O_MSG, &
                                         L2P_MSG, L2Q_MSG, L3A_MSG, L5A_MSG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NSUB, NVEC, SOL_NAME, WARN_ERR

      USE SCONTR, ONLY                :  NDOFA    , NDOFF    , NDOFG    , NDOFL    , NDOFM    , NDOFO    , NDOFR    , NDOFS

      USE SCONTR, ONLY                :  NTERM_KFS , NTERM_GMN , NTERM_GOA , NTERM_HMN , NTERM_HMN , NTERM_KAA , NTERM_KGG ,       &
                                         NTERM_KLL,  NTERM_KRL , NTERM_KRR , NTERM_MAA , NTERM_MGG , NTERM_MLL , NTERM_MRL ,       &
                                         NTERM_MRR , NTERM_PA  , NTERM_PG  , NTERM_PL  , NTERM_PS  , NTERM_QSYS, NTERM_RMG

      USE TIMDAT, ONLY                :  STIME, TSEC

      USE PARAMS, ONLY                :  PRTDISP, PRTFOR, PRTGMN, PRTGOA, PRTHMN, PRTMASS, PRTQSYS, PRTRMG,                        &
                                         PRTSTIFD, PRTSTIFF, PRTUO0, PRTYS, SUPWARN

      USE COL_VECS, ONLY              :  UG_COL, UL_COL, UO0_COL, YSe

      USE SPARSE_MATRICES, ONLY       :  I_GMN , J_GMN , GMN ,I_GOA , J_GOA , GOA ,I_HMN , J_HMN , HMN ,                           &
                                         I_KAA , J_KAA , KAA ,I_KGG , J_KGG , KGG ,I_KLL , J_KLL , KLL ,                           &
                                         I_KRL , J_KRL , KRL ,I_KRR , J_KRR , KRR ,I_KSF , J_KSF , KSF ,                           &
                                         I_MAA , J_MAA , MAA ,I_MGG , J_MGG , MGG ,I_MLL , J_MLL , MLL ,                           &
                                         I_MRL , J_MRL , MRL ,I_MRR , J_MRR , MRR ,                                                &
                                         I_PA  , J_PA  , PA  ,I_PG  , J_PG  , PG  ,I_PL  , J_PL  , PL  ,I_PS  , J_PS  , PS  ,      &
                                         I_QSYS, J_QSYS, QSYS,I_RMG , J_RMG , RMG

      USE SUBR_BEGEND_LEVELS, ONLY    :  PRT_MATS_ON_RESTART_BEGEND

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PRT_MATS_ON_RESTART_BEGEND

      END SUBROUTINE PRT_MATS_ON_RESTART

   END INTERFACE

   END MODULE PRT_MATS_ON_RESTART_Interface

