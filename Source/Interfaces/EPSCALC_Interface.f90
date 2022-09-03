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

   MODULE EPSCALC_Interface

   INTERFACE

      SUBROUTINE EPSCALC ( ISUB )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFL, NTERM_KLl, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  EPSCALC_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  EPSIL, SUPINFO, SUPWARN
      USE MACHINE_PARAMS, ONLY        :  MACH_SFMIN
      USE LAPACK_DPB_MATRICES, ONLY   :  RES
      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL
      USE SPARSE_MATRICES, ONLY       :  SYM_KLL
      USE COL_VECS, ONLY              :  UL_COL, PL_COL
      USE LAPACK_BLAS_AUX

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: ISUB              ! Internal subcase no. (1 to NSUB)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EPSCALC_BEGEND

      REAL(DOUBLE) , PARAMETER        :: ALPHA     =  ONE  ! Scalar multiplier for KLL in calc'ing residual vector, RES 
      REAL(DOUBLE) , PARAMETER        :: BETA      = -ONE  ! Scalar multiplier for PL in calc'ing residual vector, RES 

      END SUBROUTINE EPSCALC

   END INTERFACE

   END MODULE EPSCALC_Interface

