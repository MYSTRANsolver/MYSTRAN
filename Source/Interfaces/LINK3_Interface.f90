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

   MODULE LINK3_Interface

   INTERFACE

      SUBROUTINE LINK3

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_LOG, ERR, F04, F06, L3A, SC1, LINK3A, L3A_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, COMM, FATAL_ERR, KLL_SDIA, LINKNO, MBUG, NDOFL, NSUB,                       &
                                         NTERM_KLL, NTERM_PL, RESTART,  SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC       
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, TEN
      USE PARAMS, ONLY                :  CRS_CCS, EPSERR, EPSIL, KLLRAT, RELINK3, RCONDK, SOLLIB, SUPWARN, SPARSE_FLAVOR
      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_PL, J_PL, PL
      USE LAPACK_DPB_MATRICES, ONLY   :  RES
      USE COL_VECS, ONLY              :  UL_COL, PL_COL
      USE MACHINE_PARAMS, ONLY        :  MACH_EPS, MACH_SFMIN
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LAPACK_BLAS_AUX
      USE LAPACK_LIN_EQN_DPB
      USE SCRATCH_MATRICES, ONLY      :  I_CCS1, J_CCS1, CCS1
      USE SuperLU_STUF, ONLY          :  SLU_FACTORS, SLU_INFO

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), PARAMETER        :: P_LINKNO = 2      ! Prior LINK no's that should have run before this LINK can execute

      REAL(DOUBLE)                    :: BETA              ! Multiple for rhs for use in subr FBS

      REAL(DOUBLE)                    :: DUM_COL(NDOFL)    ! Temp variable used in SuperLU
 
      END SUBROUTINE LINK3

   END INTERFACE

   END MODULE LINK3_Interface

