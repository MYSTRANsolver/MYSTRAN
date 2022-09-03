! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

   MODULE WRITE_PCOMP_EQUIV_Interface

   INTERFACE

      SUBROUTINE WRITE_PCOMP_EQUIV ( PCOMP_TM, PCOMP_IB, PCOMP_TS )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  TWELVE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEMATC, MID1_PCOMP_EQ, MID2_PCOMP_EQ, MID3_PCOMP_EQ,                        &
                                         MID4_PCOMP_EQ, MID1_PCOMP_EQ, MID2_PCOMP_EQ, MID3_PCOMP_EQ, MID4_PCOMP_EQ
      USE PARAMS, ONLY                :  EPSIL, PCOMPEQ, SUPINFO
      USE MODEL_STUF, ONLY            :  INTL_PID, PCOMP, RHO, SHELL_ALP, SHELL_A, SHELL_B, SHELL_D, SHELL_T, SHELL_T_MOD,         &
                                         TREF, ZS

      USE TIMDAT, ONLY                :  TSEC

      IMPLICIT NONE

      REAL(DOUBLE), INTENT(IN)        :: PCOMP_TM           ! Membrane thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(IN)        :: PCOMP_IB           ! Bending MOI of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(IN)        :: PCOMP_TS           ! Transverse shear thickness of PCOMP for equivalent PSHELL

      END SUBROUTINE WRITE_PCOMP_EQUIV

   END INTERFACE

   END MODULE WRITE_PCOMP_EQUIV_Interface

