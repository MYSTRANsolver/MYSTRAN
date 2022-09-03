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

   MODULE OUTPUT4_PROC_Interface

   INTERFACE

      SUBROUTINE OUTPUT4_PROC ( CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY        :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                 :  ERR, F04, F06, MOU4, OU4, OU4_MSG, OU4FIL, WRT_LOG

      USE SCONTR, ONLY                 :  BLNK_SUB_NAM, FATAL_ERR   ,                                                              &
                                          NTERM_CG_LTM, NTERM_DLR   , NTERM_IF_LTM, NTERM_KLL   ,                                  &
                                          NTERM_KRL   , NTERM_KRR   , NTERM_KRRcb , NTERM_KXX   ,                                  &
                                          NTERM_LTM   ,                                                                            &
                                          NTERM_MLL   , NTERM_MRL   , NTERM_MRN   , NTERM_MRR   ,                                  &
                                          NTERM_MRRcb , NTERM_MXX   , NTERM_PHIXG , NTERM_PHIZL ,                                  &
                                          NTERM_KAA, NTERM_MAA, NTERM_KGG, NTERM_MGG, NTERM_PA, NTERM_PG, NTERM_PL, SOL_NAME

      USE SCONTR, ONLY                 :  NTERM_KAA

      USE MODEL_STUF, ONLY             :  MCG
      USE PARAMS, ONLY                 :  MPFOUT

      USE EIGEN_MATRICES_1, ONLY       :  EIGEN_VAL, EIGEN_VEC, GEN_MASS,  MEFFMASS,  MPFACTOR_N6,  MPFACTOR_NR 

      USE FULL_MATRICES, ONLY          :  DUM1, PHIZG_FULL

      USE MODEL_STUF, ONLY             :  MCG

      USE RIGID_BODY_DISP_MATS, ONLY   :  TR6_0, TR6_CG

      USE SPARSE_MATRICES, ONLY        :  I_CG_LTM  ,I_DLR     ,I_IF_LTM  ,I_KAA     ,I_KGG     ,I_KLL     ,I_KRL     ,I_KRR     , &
                                          I_KRRcb   ,I_KXX     ,I_LTM     ,I_MAA     ,I_MGG     ,I_MLL     ,I_MRL     ,I_MRN     , &
                                          I_MRR     ,I_MRRcb   ,I_MXX     ,I_PA      ,I_PG      ,I_PL      ,I_PHIXG

      USE SPARSE_MATRICES, ONLY        :  J_CG_LTM  ,J_DLR     ,J_IF_LTM  ,J_KAA     ,J_KGG     ,J_KLL     ,J_KRL     ,J_KRR     , &
                                          J_KRRcb   ,J_KXX     ,J_LTM     ,J_MAA     ,J_MGG     ,J_MLL     ,J_MRL     ,J_MRN     , &
                                          J_MRR     ,J_MRRcb   ,J_MXX     ,J_PA      ,J_PG      ,J_PL      ,J_PHIXG

      USE SPARSE_MATRICES, ONLY        :  CG_LTM    ,DLR       ,IF_LTM    ,KAA       ,KGG       ,KLL       ,KRL       ,KRR       , &
                                          KRRcb     ,KXX       ,LTM       ,MAA       ,MGG       ,MLL       ,MRL       ,MRN       , &
                                          MRR       ,MRRcb     ,MXX       ,PA        ,PG        ,PL        ,PHIXG

      USE SPARSE_MATRICES, ONLY        :  SYM_CG_LTM,SYM_DLR   ,SYM_IF_LTM,SYM_KAA   ,SYM_KGG   ,SYM_KLL   ,SYM_KRL   ,SYM_KRR   , &
                                          SYM_KRRcb ,SYM_KXX   ,SYM_LTM   ,SYM_MAA   ,SYM_MGG   ,SYM_MLL   ,SYM_MRL   ,SYM_MRN   , &
                                          SYM_MRR   ,SYM_MRRcb ,SYM_MXX   ,SYM_PA    ,SYM_PG    ,SYM_PL    ,SYM_PHIXG

      USE SCRATCH_MATRICES, ONLY       :  I_CRS1, J_CRS1, CRS1

      USE OUTPUT4_MATRICES, ONLY       :  ACT_OU4_MYSTRAN_NAMES, HAS_OU4_MAT_BEEN_PROCESSED, NUM_OU4_REQUESTS, OU4_FILE_UNITS,     &
                                          OU4_PARTVEC_COL, OU4_PARTVEC_ROW,                                                        &
                                          OU4_PART_MAT_NAMES, OU4_PART_VEC_NAMES, RBM0, SUBR_WHEN_TO_WRITE_OU4_MATS

      USE TIMDAT, ONLY                 :  TSEC
      USE RIGID_BODY_DISP_MATS, ONLY   :  TR6_CG, TR6_0            
      USE EIGEN_MATRICES_1, ONLY       :  GEN_MASS, EIGEN_VAL, EIGEN_VEC, MEFFMASS, MPFACTOR_N6, MPFACTOR_NR

      USE SPARSE_MATRICES, ONLY        :  I_CG_LTM, J_CG_LTM, CG_LTM,      I_DLR   , J_DLR   , DLR   ,                             &
                                          I_IF_LTM, J_IF_LTM, IF_LTM,      I_KLL   , J_KLL   , KLL   ,                             &
                                          I_KRL   , J_KRL   , KRL   ,      I_KRR   , J_KRR   , KRR   ,                             &
                                          I_KRRcb , J_KRRcb , KRRcb ,      I_KXX   , J_KXX   , KXX   ,                             &
                                          I_LTM   , J_LTM   , LTM   ,                                                              &
                                          I_MLL   , J_MLL   , MLL   ,      I_MRL   , J_MRL   , MRL   ,                             &
                                          I_MRN   , J_MRN   , MRN   ,      I_MRR   , J_MRR   , MRR   ,                             &
                                          I_MRRcb , J_MRRcb , MRRcb ,      I_MXX   , J_MXX   , MXX   ,                             &
                                          I_PHIXG , J_PHIXG , PHIXG 

      USE SPARSE_MATRICES, ONLY        :  I_KAA, J_KAA, KAA, I_KGG, J_KGG, KGG, I_MAA, J_MAA, MAA, I_MGG, J_MGG, MGG,              &
                                          I_PA , J_PA , PA , I_PG , J_PG , PG , I_PL , J_PL , PL

      USE FULL_MATRICES, ONLY          :  PHIZG_FULL
      USE SUBR_BEGEND_LEVELS, ONLY     :  OUTPUT4_PROC_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this one

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OUTPUT4_PROC_BEGEND

      END SUBROUTINE OUTPUT4_PROC

   END INTERFACE

   END MODULE OUTPUT4_PROC_Interface

