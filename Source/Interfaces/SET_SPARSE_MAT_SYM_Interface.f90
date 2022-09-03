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

   MODULE SET_SPARSE_MAT_SYM_Interface

   INTERFACE

      SUBROUTINE SET_SPARSE_MAT_SYM


      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06

      USE PARAMS, ONLY                :  SPARSTOR, SUPINFO

      USE SPARSE_MATRICES, ONLY       :  SYM_KGG    , SYM_MGG    , SYM_MGGC   , SYM_MGGE   , SYM_MGGS   , SYM_PG     , SYM_RMG    ,&
                                         SYM_KGGD

      USE SPARSE_MATRICES, ONLY       :  SYM_KNN    , SYM_KNM    , SYM_KMM    , SYM_KMN    ,                                       &
                                         SYM_KNND   , SYM_KNMD   , SYM_KMMD   , SYM_KMND   ,                                       &
                                         SYM_MNN    , SYM_MNM    , SYM_MMN    , SYM_MMM    ,                                       &
                                         SYM_PN     , SYM_PM     ,                                                                 &
                                         SYM_RMN    , SYM_RMM    , SYM_GMN    , SYM_GMNt   , SYM_HMN    , SYM_LMN

      USE SPARSE_MATRICES, ONLY       :  SYM_KFF    , SYM_KFS    , SYM_KSF    , SYM_KSS    , SYM_KFSe   , SYM_KSSe   ,             &
                                         SYM_KFFD   , SYM_KFSD   , SYM_KSFD   , SYM_KSSD   , SYM_KFSDe  , SYM_KSSDe  ,             &
                                         SYM_MFF    , SYM_MSF    , SYM_MFS    , SYM_MSS    ,                                       &
                                         SYM_PF     , SYM_PF_TMP , SYM_PFYS   , SYM_PFYS1  , SYM_PS     , SYM_QSYS

      USE SPARSE_MATRICES, ONLY       :  SYM_KAA    , SYM_KAO    , SYM_KOO    , SYM_KMSM   , SYM_KMSMn  ,                          &
                                         SYM_KAAD   , SYM_KAOD   , SYM_KOOD   ,                                                    &
                                         SYM_MAA    , SYM_MAO    , SYM_MOO    ,                                                    &
                                         SYM_PA     , SYM_PO     ,                                                                 &
                                         SYM_GOA    , SYM_GOAt

      USE SPARSE_MATRICES, ONLY       :  SYM_KLL    , SYM_KRL    , SYM_KRR    , SYM_KLLs   , SYM_KRRcb  , SYM_KRRcbn ,&
                                         SYM_KRRcbs , SYM_KXX    , SYM_KLLD   , SYM_KRLD   , SYM_KRRD   , SYM_KLLDs  ,             &
                                         SYM_MPF0   , SYM_MLL    , SYM_MLLn   , SYM_MRL    , SYM_MLR    , SYM_MRR    , SYM_MLLs   ,&
                                         SYM_MRN    , SYM_MRRcb  , SYM_MRRcbn , SYM_MXX    , SYM_MXXn   ,                          &
                                         SYM_PL     , SYM_PR     ,                                                                 &
                                         SYM_DLR    , SYM_DLRt   , SYM_IRR    , SYM_PHIXA  , SYM_IF_LTM , SYM_CG_LTM , SYM_PHIZL  ,&
                                         SYM_PHIZL1 , SYM_PHIZL2 , SYM_LTM    , SYM_PHIZL1t

      IMPLICIT NONE

      END SUBROUTINE SET_SPARSE_MAT_SYM

   END INTERFACE

   END MODULE SET_SPARSE_MAT_SYM_Interface

