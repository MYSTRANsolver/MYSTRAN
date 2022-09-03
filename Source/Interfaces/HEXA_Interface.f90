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

   MODULE HEXA_Interface

   INTERFACE

      SUBROUTINE HEXA ( OPT, INT_ELEM_ID,IORD, RED_INT_SHEAR, WRITE_WARN )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MELDOF, MPLOAD4_3D_DATA, NPLOAD4_3D, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  QUARTER, HALF, ZERO, ONE, EIGHT
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  HEXA_BEGEND
      USE PARAMS, ONLY                :  EPSIL
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  AGRID, ALPVEC, BE1, BE2, DT, EID, ELGP, NUM_EMG_FATAL_ERRS, ES, KE, KED, ME,              &
                                         NUM_EMG_FATAL_ERRS, PLOAD4_3D_DATA, PPE, PRESS, PTE, RHO, SE1, SE2, STE1, STRESS, TREF,   &
                                         TYPE, XEL
 
      IMPLICIT NONE 
  
      CHARACTER( 1*BYTE), INTENT(IN)  :: RED_INT_SHEAR          ! If 'Y', use Gaussian weighted avg of B matrices for shear terms
      CHARACTER( 1*BYTE), INTENT(IN)  :: OPT(6)                 ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN             ! If 'Y" write warning messages, otherwise do not

      CHARACTER(46*BYTE)              :: IORD_MSG               ! Character name of an integration order (used for debug output)

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID             ! Internal element ID
      INTEGER(LONG), INTENT(IN)       :: IORD                   ! Gaussian integration order for element
      INTEGER(LONG)                   :: GAUSS_PT               ! Gauss point number (used for DEBUG output in subr SHP3DH
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = HEXA_BEGEND
  
      REAL(DOUBLE)                    :: CBAR(3,3*ELGP)         ! Derivatives of shape fcns wrt x,y,z used in diff stiff matrix
      REAL(DOUBLE)                    :: DUM0(3*ELGP)           ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM1(3*ELGP)           ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM2(6,3*ELGP)         ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM3(3*ELGP,3*ELGP)    ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM4(6,3*ELGP)         ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM5(3*ELGP,3*ELGP)    ! Intermediate matrix used in solving for KE elem matrices
      REAL(DOUBLE)                    :: DUM6(3,3*ELGP)         ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: EALP(6)                ! Variable used in calc PTE therm loads & STEi therm stress coeffs

      REAL(DOUBLE)                    :: M0                     ! An intermediate variable used in calc elem mass, ME
      REAL(DOUBLE)                    :: SUMB                   ! An intermediate variable used in calc B matrix for reduced integ
      REAL(DOUBLE)                    :: SUMD                   ! An intermediate variable used in calc B matrix for reduced integ
      REAL(DOUBLE)                    :: TEMP                   ! Temperature to use in PTE calc
 
      END SUBROUTINE HEXA

   END INTERFACE

   END MODULE HEXA_Interface

