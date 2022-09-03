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

   MODULE STIFF_MAT_EQUIL_CHK_Interface

   INTERFACE

      SUBROUTINE STIFF_MAT_EQUIL_CHK ( OUTPUT, X_SET, SYM_KIN, NROWS, NTERM_KIN, I_KIN, J_KIN, KIN, KIN_DIAG, KIN_MAX_DIAG, RBMAT ) 


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NSPOINT, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DOF_TABLES, ONLY            :  TDOFI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND
      USE LAPACK_BLAS_AUX
      USE PARAMS, ONLY                :  EPSIL, EQCHK_NORM, SUPWARN, SUPINFO
      USE SUBR_BEGEND_LEVELS, ONLY    :  STIFF_MAT_EQUIL_CHK_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: SYM_KIN             ! Whether KIN is stored as symmetric (only nonzero terms on and above
      CHARACTER(LEN=*), INTENT(IN)    :: X_SET               ! The displ set for the equil check (e,g, 'G ', etc

      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in KIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_KIN           ! Number of nonzero terms in KIN
      INTEGER(LONG), INTENT(IN)       :: I_KIN(NROWS+1)      ! Row start indices for KIN 
      INTEGER(LONG), INTENT(IN)       :: J_KIN(NTERM_KIN)    ! Col numbers of terms in KIN 
      INTEGER(LONG), INTENT(IN)       :: OUTPUT              ! =1, output PRB, =2 output RB_STRN_ENRGY, =3 output both
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = STIFF_MAT_EQUIL_CHK_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: KIN(NTERM_KIN)      ! Nonzero terms in KIN
      REAL(DOUBLE), INTENT(IN)        :: KIN_DIAG(NROWS)     ! Diagonal of KIN
      REAL(DOUBLE), INTENT(IN)        :: KIN_MAX_DIAG        ! Max diag term from KIN
      REAL(DOUBLE), INTENT(IN)        :: RBMAT(NROWS,6)      ! Rigid body displacement matrix (6 rigid body modes)

      END SUBROUTINE STIFF_MAT_EQUIL_CHK

   END INTERFACE

   END MODULE STIFF_MAT_EQUIL_CHK_Interface

