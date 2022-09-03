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

   MODULE BAR1_Interface

   INTERFACE

      SUBROUTINE BAR1 ( OPT, L, AREA, I1, I2, JTOR, SCOEFF, K1, K2, I12, E, G, ALPHA, TREF )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, NTSUB, BLNK_SUB_NAM, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BAR1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, TEN, TWELVE
      USE DEBUG_PARAMETERS
      USE PARAMS, ONLY                :  EPSIL, ART_KED, ART_ROT_KED, ART_TRAN_KED
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  ELDOF, DOFPIN, DT, EID, NUM_EMG_FATAL_ERRS, KE, KED, PEL, PTE, SE1, SE2, STE1, STE2, TYPE,&
                                         UEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE 
 
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BAR1_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: ALPHA             ! Coefficient of thermal expansion
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Cross-sectional area
      REAL(DOUBLE) , INTENT(IN)       :: E                 ! Youngs modulus
      REAL(DOUBLE) , INTENT(IN)       :: G                 ! Shear modulus
      REAL(DOUBLE) , INTENT(IN)       :: I1                ! Bending inertia in plane 1
      REAL(DOUBLE) , INTENT(IN)       :: I12               ! Product of inertia
      REAL(DOUBLE) , INTENT(IN)       :: I2                ! Bending inertia in plane 2
      REAL(DOUBLE) , INTENT(IN)       :: JTOR              ! Torsional constant
      REAL(DOUBLE) , INTENT(IN)       :: K1                ! Shear constant for plane 1 (used in K1*AREA*G)
      REAL(DOUBLE) , INTENT(IN)       :: K2                ! Shear constant for plane 2 (used in K1*AREA*G)
      REAL(DOUBLE) , INTENT(IN)       :: L                 ! Elem length
      REAL(DOUBLE) , INTENT(IN)       :: SCOEFF            ! Stress recovery coeff for torsion
      REAL(DOUBLE) , INTENT(IN)       :: TREF              ! Element reference temperature
      REAL(DOUBLE)                    :: BETA              ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: B1(3,6)           ! Intermediate array used in calculating SE1 stress matrix
      REAL(DOUBLE)                    :: B2(3,6)           ! Intermediate array used in calculating SE2 stress matrix
      REAL(DOUBLE)                    :: BT1(3,5)          ! Intermediate array used in calculating STE1 thermal stress effects
      REAL(DOUBLE)                    :: BT2(3,5)          ! Intermediate array used in calculating STE2 thermal stress effects
      REAL(DOUBLE)                    :: C01               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: C02               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: C03               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: G1                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: G2                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K11               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K12               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K1T               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K21               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K22               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K2T               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K3T               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: K4T               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: R0                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: R1                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: R1D               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: R2                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: R2D               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: RA                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: RG                ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: S11(3,6)          ! Intermediate matrix used in calculating SE1 stress matrix
      REAL(DOUBLE)                    :: S12(3,6)          ! Intermediate matrix used in calculating SE1 stress matrix
      REAL(DOUBLE)                    :: S21(3,6)          ! Intermediate matrix used in calculating SE2 stress matrix
      REAL(DOUBLE)                    :: S22(3,6)          ! Intermediate matrix used in calculating SE2 stress matrix

      END SUBROUTINE BAR1

   END INTERFACE

   END MODULE BAR1_Interface

