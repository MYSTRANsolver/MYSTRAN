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

   MODULE REDUCE_A_LR_Interface

   INTERFACE

      SUBROUTINE REDUCE_A_LR

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LINKNO,   NDOFA, NDOFG, NDOFL, NDOFR, NSUB, SOL_NAME,                       &
                                         NTERM_KAA , NTERM_KLL , NTERM_KRL , NTERM_KRR ,                                           &
                                         NTERM_KAAD, NTERM_KLLD, NTERM_KRLD, NTERM_KRRD,                                           &
                                         NTERM_MAA , NTERM_MLL , NTERM_MRL , NTERM_MRR ,                                           &
                                         NTERM_PA  , NTERM_PL  , NTERM_PR
      USE TIMDAT, ONLY                :  TSEC, YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME       
      USE CONSTANTS_1, ONLY           :  ONE
      USE DOF_TABLES, ONLY            :  TDOFI
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_ASET, RBGLOBAL_GSET, RBGLOBAL_LSET
      USE PARAMS, ONLY                :  EQCHK_OUTPUT, MATSPARS, PRTSTIFD, PRTSTIFF, PRTMASS, PRTFOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_A_LR_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KAA , J_KAA , KAA , I_KLL , J_KLL , KLL , I_KRL , J_KRL , KRL , I_KRR , J_KRR , KRR ,   &
                                         I_KAAD, J_KAAD, KAAD, I_KLLD, J_KLLD, KLLD, I_KRLD, J_KRLD, KRLD, I_KRRD, J_KRRD, KRRD,   &
                                         I_MAA , J_MAA , MAA , I_MLL , J_MLL , MLL , I_MRL , J_MRL , MRL , I_MRR , J_MRR , MRR ,   &
                                         I_PA  , J_PA  , PA  , I_PL  , J_PL  , PL  , I_PR  , J_PR  , PR
      USE SPARSE_MATRICES, ONLY       :  SYM_KLL
      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_MYSTRAN_NAMES, NUM_OU4_REQUESTS
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_A_LR_BEGEND

      END SUBROUTINE REDUCE_A_LR

   END INTERFACE

   END MODULE REDUCE_A_LR_Interface

