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

   MODULE ALLOCATE_SPARSE_MAT_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_SPARSE_MAT ( NAME, NROWS, NTERMS, CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFM, NDOFO, NDOFS, NDOFR, TOT_MB_MEM_ALLOC
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_SPARSE_MAT_BEGEND

      USE SPARSE_MATRICES , ONLY      :  I_KGG   , J_KGG   , KGG   , I_MGG   , J_MGG   , MGG   , I_PG    , J_PG    , PG    ,       &
                                         I_KGGD  , J_KGGD  , KGGD  ,                                                               &
                                         I_KNN   , J_KNN   , KNN   , I_KNM   , J_KNM   , KNM   , I_KMM   , J_KMM   , KMM   ,       &
                                         I_KMN   , J_KMN   , KMN   ,                                                               &
                                         I_KNND  , J_KNND  , KNND  , I_KNMD  , J_KNMD  , KNMD  , I_KMMD  , J_KMMD  , KMMD  ,       &
                                         I_KMND  , J_KMND  , KMND  ,                                                               &
                                         I_MNN   , J_MNN   , MNN   , I_MNM   , J_MNM   , MNM   , I_MMN   , J_MMN   , MMN   ,       &
                                         I_MMM   , J_MMM   , MMM   , I_PN    , J_PN    , PN    , I_PM    , J_PM    , PM    ,       &
                                         I_GMNt  , J_GMNt  , GMNt  , I_HMN   , J_HMN   , HMN   , I_LMN   , J_LMN   , LMN   ,       &
                                         I_RMN   , J_RMN   , RMN   , I_RMM   , J_RMM   , RMM   , I_GMN   , J_GMN   , GMN   ,       &
                                         I_RMG   , J_RMG   , RMG

      USE SPARSE_MATRICES , ONLY      :  I_KFF   , J_KFF   , KFF   , I_KFS   , J_KFS   , KFS   , I_KSF   , J_KSF   , KSF   ,       &
                                         I_KSS   , J_KSS   , KSS   , I_KFSe  , J_KFSe  , KFSe  , I_KSSe  , J_KSSe  , KSSe  ,       &
                                         I_KFFD  , J_KFFD  , KFFD  , I_KFSD  , J_KFSD  , KFSD  , I_KSFD  , J_KSFD  , KSFD  ,       &
                                         I_KSSD  , J_KSSD  , KSSD  , I_KFSDe , J_KFSDe , KFSDe , I_KSSDe , J_KSSDe , KSSDe ,       &
                                         I_MFF   , J_MFF   , MFF   , I_MFS   , J_MFS   , MFS   , I_MSF   , J_MSF   , MSF   ,       &
                                         I_MSS   , J_MSS   , MSS   , I_PF    , J_PF    , PF    , I_PS    , J_PS    , PS    ,       &
                                         I_PF_TMP, J_PF_TMP, PF_TMP, I_PFYS  , J_PFYS  , PFYS  , I_PFYS1 , J_PFYS1 , PFYS1 ,       &
                                         I_QSYS  , J_QSYS  , QSYS

      USE SPARSE_MATRICES , ONLY      :  I_KAA   , J_KAA   , KAA   , I_KAO   , J_KAO   , KAO   ,                                   &
                                         I_KOO   , J_KOO   , KOO   , I_KOOs  , J_KOOs  , KOOs  ,                                   &
                                         I_KAAD  , J_KAAD  , KAAD  , I_KAOD  , J_KAOD  , KAOD  ,                                   &
                                         I_KOOD  , J_KOOD  , KOOD  , I_KOODs , J_KOODs , KOODs ,                                   &
                                         I_MAA   , J_MAA   , MAA   , I_MAO   , J_MAO   , MAO   , I_MOO   , J_MOO   , MOO   ,       &
                                         I_PA    , J_PA    , PA    , I_PO    , J_PO    , PO    ,                                   &
                                         I_GOA   , J_GOA   , GOA   , I_GOAt  , J_GOAt  , GOAt

      USE SPARSE_MATRICES , ONLY      :  I_KLL   , J_KLL   , KLL   , I_KLLs  , J_KLLs  , KLLs  , I_KRL   , J_KRL   , KRL   ,       &
                                         I_KRR   , J_KRR   , KRR   ,                                                               &
                                         I_KLLD  , J_KLLD  , KLLD  , I_KLLDs , J_KLLDs , KLLDs , I_KRLD  , J_KRLD  , KRLD  ,       &
                                         I_KRRD  , J_KRRD  , KRRD  , I_KLLDn , J_KLLDn , KLLDn ,                                   &
                                         I_MLL   , J_MLL   , MLL   , I_MLLn  , J_MLLn  , MLLn  , I_MLLs  , J_MLLs  , MLLs  ,       &
                                         I_MLR   , J_MLR   , MLR   ,                                                               &
                                         I_MRL   , J_MRL   , MRL   , I_MRR   , J_MRR   , MRR   , I_MPF0  , J_MPF0  , MPF0  ,       &
                                         I_KMSM  , J_KMSM  , KMSM  , I_KMSMn , J_KMSMn , KMSMn , I_KMSMs , J_KMSMs , KMSMs ,       &
                                         I_DLR   , J_DLR   , DLR   , I_DLRt  , J_DLRt  , DLRt  , I_IRR   , J_IRR   , IRR   ,       &
                                         I_PHIXA , J_PHIXA , PHIXA , I_PHIXG , J_PHIXG , PHIXG ,                                   &
                                         I_KRRcb , J_KRRcb , KRRcb , I_KRRcbn, J_KRRcbn, KRRcbn, I_KRRcbs, J_KRRcbs, KRRcbs,       &
                                         I_KXX   , J_KXX   , KXX   , I_LTM   , J_LTM   , LTM   ,                                   &
                                         I_MRN   , J_MRN   , MRN   , I_MRRcb , J_MRRcb , MRRcb , I_MRRcbn, J_MRRcbn, MRRcbn,       &
                                         I_MXX   , J_MXX   , MXX   , I_MXXn  , J_MXXn  , MXXn  ,                                   &
                                         I_IF_LTM, J_IF_LTM, IF_LTM, I_CG_LTM, J_CG_LTM, CG_LTM,                                   &
                                         I_PHIZL , J_PHIZL , PHIZL , I_PHIZL1, J_PHIZL1, PHIZL1, I_PHIZL1t, J_PHIZL1t, PHIZL1t,    &
                                         I_PHIZL2, J_PHIZL2, PHIZL2, I_PL    , J_PL    , PL    , I_PR     , J_PR     , PR

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Array name of the matrix to be allocated in sparse format
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(6*BYTE)               :: NAME1             ! Array name (used for output error message)
      CHARACTER(6*BYTE)               :: NAME2             ! Array name (used for output error message)
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows for matrix NAME
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzero terms that will be in matrix NAME
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_SPARSE_MAT_BEGEND

      END SUBROUTINE ALLOCATE_SPARSE_MAT

   END INTERFACE

   END MODULE ALLOCATE_SPARSE_MAT_Interface

