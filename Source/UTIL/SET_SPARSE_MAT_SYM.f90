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
 
      SUBROUTINE SET_SPARSE_MAT_SYM

! Sets symmetry indicators for sparse matrices depending on Bulk Data PARAM SPARSTOR
                 
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

! **********************************************************************************************************************************
      WRITE(ERR,101) SPARSTOR
      IF (SUPINFO == 'N') THEN
         WRITE(F06,101) SPARSTOR
      ENDIF

      IF (SPARSTOR == 'SYM   ') THEN

         SYM_KGG      = 'Y'
         SYM_KGGD     = 'Y'
         SYM_MGG      = 'Y'
         SYM_MGGC     = 'Y'
         SYM_MGGE     = 'Y'
         SYM_MGGS     = 'Y'
         SYM_PG       = 'N'
         SYM_RMG      = 'N'

         SYM_KNN      = 'Y'
         SYM_KNM      = 'N'
         SYM_KMM      = 'Y'
         SYM_KMN      = 'N'
         SYM_KNND     = 'Y'
         SYM_KNMD     = 'N'
         SYM_KMMD     = 'Y'
         SYM_KMND     = 'N'
         SYM_MNN      = 'Y'
         SYM_MNM      = 'N'
         SYM_MMN      = 'N'
         SYM_MMM      = 'Y'
         SYM_PN       = 'N'
         SYM_PM       = 'N'
         SYM_RMN      = 'N'
         SYM_RMM      = 'N'
         SYM_GMN      = 'N'
         SYM_GMNt     = 'N'
         SYM_HMN      = 'N'
         SYM_LMN      = 'N'

         SYM_KFF      = 'Y'
         SYM_KFS      = 'N'
         SYM_KSS      = 'Y'
         SYM_KFSe     = 'N'
         SYM_KSSe     = 'N'
         SYM_KFFD     = 'Y'
         SYM_KFSD     = 'N'
         SYM_KSSD     = 'Y'
         SYM_KFSDe    = 'N'
         SYM_KSSDe    = 'N'
         SYM_MFF      = 'Y'
         SYM_MFS      = 'N'
         SYM_MSF      = 'N'
         SYM_MSS      = 'Y'
         SYM_PF       = 'N'
         SYM_PF_TMP   = 'N' 
         SYM_PFYS     = 'N'  
         SYM_PFYS1    = 'N' 
         SYM_PS       = 'N' 
         SYM_QSYS     = 'N' 

         SYM_KAA      = 'Y'
         SYM_KAO      = 'N'
         SYM_KOO      = 'Y'
         SYM_KAAD     = 'Y'
         SYM_KAOD     = 'N'
         SYM_KOOD     = 'Y'
         SYM_MAA      = 'Y'
         SYM_MAO      = 'N'
         SYM_MOO      = 'Y'
         SYM_PA       = 'N'
         SYM_PO       = 'N'
         SYM_GOA      = 'N'
         SYM_GOAt     = 'N'

         SYM_KLL      = 'Y'        
         SYM_KLLs     = 'Y'        
         SYM_KRL      = 'N'      
         SYM_KRR      = 'Y'     
         SYM_KLLD     = 'Y'        
         SYM_KLLDs    = 'Y'        
         SYM_KRLD     = 'N'      
         SYM_KRRD     = 'Y'     
         SYM_MPF0     = 'N'  
         SYM_MLL      = 'Y'  
         SYM_MLLn     = 'N'
         SYM_MLLs     = 'Y'
         SYM_MLR      = 'N'
         SYM_MRL      = 'N'
         SYM_MRR      = 'Y'
         SYM_DLR      = 'N'
         SYM_DLRt     = 'N'
         SYM_PHIZL    = 'N'
         SYM_PHIZL1   = 'N'
         SYM_PHIZL1t  = 'N'
         SYM_PHIZL2   = 'N'
         SYM_CG_LTM   = 'N'
         SYM_IF_LTM   = 'N'
         SYM_LTM      = 'N'
         SYM_IRR      = 'Y'
         SYM_PHIXA    = 'N'
         SYM_KRRcb    = 'Y'
         SYM_KRRcbn   = 'N'
         SYM_KRRcbs   = 'Y'
         SYM_KXX      = 'Y'
         SYM_MRN      = 'N'
         SYM_MRRcb    = 'Y'
         SYM_MRRcbn   = 'N'
         SYM_MXX      = 'Y'
         SYM_MXXn     = 'N'
         SYM_PL       = 'N'
         SYM_PR       = 'N'

         SYM_KMSM     = 'Y'
         SYM_KMSMn    = 'N'

      ELSE IF (SPARSTOR == 'NONSYM') THEN

         SYM_KGG      = 'N'
         SYM_KGGD     = 'N'
         SYM_MGG      = 'N'
         SYM_MGGC     = 'N'
         SYM_MGGe     = 'N'
         SYM_MGGS     = 'N'
         SYM_PG       = 'N'
         SYM_RMG      = 'N'

         SYM_KNN      = 'N'
         SYM_KNM      = 'N'
         SYM_KMM      = 'N'
         SYM_KMN      = 'N'
         SYM_KNND     = 'N'
         SYM_KNMD     = 'N'
         SYM_KMMD     = 'N'
         SYM_KMND     = 'N'
         SYM_MNN      = 'N'
         SYM_MNM      = 'N'
         SYM_MMN      = 'N'
         SYM_MMM      = 'N'
         SYM_PN       = 'N'
         SYM_PM       = 'N'
         SYM_RMN      = 'N'
         SYM_RMM      = 'N'
         SYM_GMN      = 'N'
         SYM_GMNt     = 'N'
         SYM_HMN      = 'N'
         SYM_LMN      = 'N'

         SYM_KFF      = 'N'
         SYM_KFS      = 'N'
         SYM_KSS      = 'N'
         SYM_KFSe     = 'N'
         SYM_KSSe     = 'N'
         SYM_KFFD     = 'N'
         SYM_KFSD     = 'N'
         SYM_KSSD     = 'N'
         SYM_KFSDe    = 'N'
         SYM_KSSDe    = 'N'
         SYM_MFF      = 'N'
         SYM_MFS      = 'N'
         SYM_MSF      = 'N'
         SYM_MSS      = 'N'
         SYM_PF       = 'N'
         SYM_PF_TMP   = 'N' 
         SYM_PFYS     = 'N'  
         SYM_PFYS1    = 'N' 
         SYM_PS       = 'N' 
         SYM_QSYS     = 'N' 

         SYM_KAA      = 'N'
         SYM_KAO      = 'N'
         SYM_KOO      = 'N'
         SYM_KAAD     = 'N'
         SYM_KAOD     = 'N'
         SYM_KOOD     = 'N'
         SYM_MAA      = 'N'
         SYM_MAO      = 'N'
         SYM_MOO      = 'N'
         SYM_PA       = 'N'
         SYM_PO       = 'N'
         SYM_GOA      = 'N'
         SYM_GOAt     = 'N'

         SYM_KLL      = 'N'        
         SYM_KLLs     = 'Y'                                ! KLLs is always symmetric
         SYM_KRL      = 'N'      
         SYM_KRR      = 'N'     
         SYM_KLLD     = 'N'        
         SYM_KLLDs    = 'Y'                                ! KLLs is always symmetric
         SYM_KRLD     = 'N'      
         SYM_KRRD     = 'N'     
         SYM_MPF0     = 'N'  
         SYM_MLL      = 'N'  
         SYM_MLLn     = 'N'
         SYM_MLLs     = 'Y'                                ! MLLs is always symmetric
         SYM_MLR      = 'N'
         SYM_MRL      = 'N'
         SYM_MRR      = 'N'
         SYM_DLR      = 'N'
         SYM_DLRt     = 'N'
         SYM_PHIZL    = 'N'
         SYM_PHIZL1   = 'N'
         SYM_PHIZL1t  = 'N'
         SYM_PHIZL2   = 'N'
         SYM_CG_LTM   = 'N'
         SYM_IF_LTM   = 'N'
         SYM_LTM      = 'N'
         SYM_IRR      = 'N'
         SYM_PHIXA    = 'N'
         SYM_KRRcb    = 'N'
         SYM_KRRcbn   = 'N'
         SYM_KRRcbs   = 'Y'
         SYM_KXX      = 'N'
         SYM_MRN      = 'N'
         SYM_MRRcb    = 'N'
         SYM_MRRcbn   = 'N'
         SYM_MXX      = 'N'
         SYM_MXXn     = 'N'
         SYM_PL       = 'N'
         SYM_PR       = 'N'

         SYM_KMSM     = 'N'
         SYM_KMSMn    = 'N'

      ENDIF

! **********************************************************************************************************************************
  101 FORMAT(' *INFORMATION: SPARSE MATRICES ARE STORED IN ',A,' FORMAT',/)

! **********************************************************************************************************************************

      END SUBROUTINE SET_SPARSE_MAT_SYM
