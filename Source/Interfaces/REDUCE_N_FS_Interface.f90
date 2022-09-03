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

   MODULE REDUCE_N_FS_Interface

   INTERFACE

      SUBROUTINE REDUCE_N_FS

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1H, LINK1H, L1H_MSG, L2C, LINK2C, L2C_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  LINKNO    , NDOFF, NDOFG, NDOFN, NDOFS, NDOFSE, NSUB,                                     &
                                         NTERM_KNN , NTERM_KFF , NTERM_KFS , NTERM_KSS , NTERM_KSSe ,                              &
                                         NTERM_KNND, NTERM_KFFD, NTERM_KFSD, NTERM_KSSD, NTERM_KSSDe,                              &
                                         NTERM_QSYS, NTERM_PN  , NTERM_PF  , NTERM_PS  , NTERM_MNN  ,                              &
                                         NTERM_MFF , NTERM_MFS , NTERM_MSS , SOL_NAME, BLNK_SUB_NAM        
      USE TIMDAT, ONLY                :  TSEC, YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME       
      USE CONSTANTS_1, ONLY           :  ONE
      USE DOF_TABLES, ONLY            :  TDOFI
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_NSET, RBGLOBAL_FSET
      USE PARAMS, ONLY                :  EQCHK_OUTPUT, MATSPARS, PRTSTIFD, PRTSTIFF, PRTMASS, PRTFOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_N_FS_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KNN  , J_KNN  , KNN  , I_KFF  , J_KFF  , KFF  , I_KFS  , J_KFS  , KFS  ,                &
                                         I_KSS  , J_KSS  , KSS  , I_KSSe , J_KSSe , KSSe ,                                         &
                                         I_KNND , J_KNND , KNND , I_KFFD , J_KFFD , KFFD , I_KFSD , J_KFSD , KFSD ,                &
                                         I_KSSD , J_KSSD , KSSD , I_KSSDe, J_KSSDe, KSSDe,                                         &
                                         I_MNN  , J_MNN  , MNN  , I_MFF  , J_MFF  , MFF  , I_MFS  , J_MFS  , MFS  ,                &
                                         I_MSS  , J_MSS  , MSS  ,                                                                  &
                                         I_PN   , J_PN   , PN   , I_PF   , J_PF   , PF   , I_PS   , J_PS   , PS   ,                &
                                         I_MSF  , J_MSF  , MSF  ,                                                                  &
                                         I_QSYS , J_QSYS , QSYS
      USE SPARSE_MATRICES, ONLY       :  SYM_KFF, SYM_KSSe
      USE COL_VECS, ONLY              :  YSe
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SCRATCH_MATRICES
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG)     , PARAMETER   :: NUM_YS_COLS = 1       ! Variable for number of cols in array YSe 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_N_FS_BEGEND

      END SUBROUTINE REDUCE_N_FS

   END INTERFACE

   END MODULE REDUCE_N_FS_Interface

