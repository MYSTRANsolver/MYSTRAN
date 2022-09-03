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

      SUBROUTINE EPSCALC ( ISUB )

! Calculates, and prints, EPSILON, an indicator of numerical accuracy in the soln for UA:

!   EPSILON = UL(t)*[ PL - KLL*UL ]/[ UL(t)*PL ],  UL: displ's, PL: loads, KLL: stiff matrix for the L-set

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

      USE EPSCALC_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EPSCALC'

      INTEGER(LONG), INTENT(IN)       :: ISUB              ! Internal subcase no. (1 to NSUB)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EPSCALC_BEGEND

      REAL(DOUBLE) , PARAMETER        :: ALPHA     =  ONE  ! Scalar multiplier for KLL in calc'ing residual vector, RES 
      REAL(DOUBLE) , PARAMETER        :: BETA      = -ONE  ! Scalar multiplier for PL in calc'ing residual vector, RES 
      REAL(DOUBLE)                    :: DEN               ! Denominator in EPSILON calculation
      REAL(DOUBLE)                    :: EPSILON           ! The indicator of numerical accuracy of the displ soln, UL
      REAL(DOUBLE)                    :: KU(NDOFL)         ! Result of multiplying KLL and UL_COL
      REAL(DOUBLE)                    :: NUM               ! Numerator in EPSILON calculation

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calculate residual vector. First, multiply KLL x UL_COL and then add -PL_COL:

      CALL MATMULT_SFF ( 'KLL', NDOFL, NDOFL, NTERM_KLL, SYM_KLL, I_KLL, J_KLL, KLL, 'UL_COL', NDOFL, 1, UL_COL, 'Y',             &
                         'KLL*UL_COL',  ONE, KU )
      CALL MATADD_FFF  ( KU, PL_COL, NDOFL, 1, ALPHA, BETA, 0, RES )


! Calculate dot product of UL displ vector and RES residual vector

      NUM = DDOT (NDOFL, UL_COL, 1, RES, 1)

! Calculate dot product of UL displ vector and PL load vector

      DEN = DDOT (NDOFL, UL_COL, 1, PL_COL, 1)

! Calculate EPSILON and print

      IF (DABS(DEN) > MACH_SFMIN) THEN
         EPSILON = NUM/DEN
         WRITE(F06,3701) ISUB,EPSILON
      ELSE
         WARN_ERR = WARN_ERR + 1
         WRITE(F06,3702) ISUB, DEN, MACH_SFMIN
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 3701 FORMAT(' *INFORMATION: FOR INTERNAL SUBCASE NUMBER ',I8,' EPSILON ERROR ESTIMATE            = ',1ES13.6,                     &
                           ' Based on U''*(K*U - P)/(U''*P)',/)

 3702 FORMAT(' *WARNING    : CANNOT CALCULATE EPSILON ERROR ESTIMATE FOR INTERNAL SUBCASE NUMBER ',I8                              &
                    ,/,14X,' THE DOT PRODUCT OF DISPL AND LOAD VECTORS                     = ',1ES15.6,' CANNOT BE INVERTED.'      &
                    ,/,14X,' IT IS TOO SMALL COMPARED TO MACHINE SAFE MINIMUM (MACH_SFMIN) = ',1ES15.6,/)

! **********************************************************************************************************************************

      END SUBROUTINE EPSCALC 