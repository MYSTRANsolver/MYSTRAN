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
 
      SUBROUTINE BREL1 ( OPT, WRITE_WARN )
 
! Calculates, or calls subr's to calculate, quadrilateral element matrices:

!  1) ME        = element mass matrix                  , if OPT(1) = 'Y'
!  2) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  3) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  4) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  5) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BREL1_BEGEND
      USE CONSTANTS_1, ONLY           :  TWO
      USE PARAMS, ONLY                :  EPSIL
      USE DEBUG_PARAMETERS
      USE MODEL_STUF, ONLY            :  EID, ELEM_LEN_AB, EMAT, NUM_EMG_FATAL_ERRS, EPROP, FCONV, ME, ULT_STRE, ULT_STRN, &
                                         TYPE, ZS
 
      USE BREL1_USE_IFs

      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BREL1'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BREL1_BEGEND

      REAL(DOUBLE)                    :: ALPHA             ! Coefficient of thermal expansion
      REAL(DOUBLE)                    :: AREA              ! Cross-sectional area
      REAL(DOUBLE)                    :: E                 ! Youngs modulus
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare for real zero
      REAL(DOUBLE)                    :: G                 ! Shear modulus
      REAL(DOUBLE)                    :: GE                ! Material damping coeff
      REAL(DOUBLE)                    :: I1                ! Bending inertia in plane 1
      REAL(DOUBLE)                    :: I12               ! Product of inertia
      REAL(DOUBLE)                    :: I2                ! Bending inertia in plane 2
      REAL(DOUBLE)                    :: JTOR              ! Torsional constant
      REAL(DOUBLE)                    :: K1                ! Shear constant for plane 1 (used in K1*AREA*G)
      REAL(DOUBLE)                    :: K2                ! Shear constant for plane 2 (used in K1*AREA*G)
      REAL(DOUBLE)                    :: M0                ! Intermediate variable in calculating element mass matrix, ME
      REAL(DOUBLE)                    :: NSM               ! Nonstructural mass
      REAL(DOUBLE)                    :: RHO               ! Material density
      REAL(DOUBLE)                    :: TREF              ! Element reference temperature
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Set element property and material constants
 
      IF (TYPE == 'ROD     ') THEN

         AREA     = EPROP(1)                               ! Cross-sectional area
         JTOR     = EPROP(2)                               ! Torsional constant
         ZS(1)    = EPROP(3)                               ! C (Tors. stress recovery coeff) on PROD
         NSM      = EPROP(4)                               ! Non-structural mass
         FCONV(1) = AREA

      ELSE IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BART    ')) THEN

         AREA     = EPROP( 1)                              ! Cross-sectional area
         I1       = EPROP( 2)                              ! Plane 1 moment of inertia
         I2       = EPROP( 3)                              ! Plane 2 moment of inertia
         JTOR     = EPROP( 4)                              ! Torsional constant
         NSM      = EPROP( 5)                              ! Non-structural mass
         ZS(1)    = EPROP( 6)                              ! y coord of 1st point for stress recovery
         ZS(2)    = EPROP( 7)                              ! z coord of 1st point for stress recovery
         ZS(3)    = EPROP( 8)                              ! y coord of 2nd point for stress recovery
         ZS(4)    = EPROP( 9)                              ! z coord of 2nd point for stress recovery
         ZS(5)    = EPROP(10)                              ! y coord of 3rd point for stress recovery
         ZS(6)    = EPROP(11)                              ! z coord of 3rd point for stress recovery
         ZS(7)    = EPROP(12)                              ! y coord of 4th point for stress recovery
         ZS(8)    = EPROP(13)                              ! z coord of 4th point for stress recovery
         K1       = EPROP(14)                              ! Plane 1 shear factor
         K2       = EPROP(15)                              ! Plane 2 shear factor
         I12      = EPROP(16)                              ! Product of inertia
         ZS(9)    = EPROP(17)                              ! Torsional stress recovery coefficient
         FCONV(1) = AREA

      ELSE IF (TYPE == 'BEAM    ') THEN

      ENDIF

! Need to set some values for materials here since subr for material properties not called for these 1D elements
 
      E             = EMAT( 1,1)                           ! Young's modulus
      G             = EMAT( 2,1)                           ! Shear modulus
      RHO           = EMAT( 4,1)                           ! Mass density
      ALPHA         = EMAT( 5,1)                           ! Coefficient of thermal expansion
      TREF          = EMAT( 6,1)                           ! Reference temperature for thermal expaqnsion
      GE            = EMAT( 7,1)                           ! Structural damping coefficient
      ULT_STRE(1,1) = EMAT( 8,1)                           ! Max allowable stress in tension 
      ULT_STRE(2,1) = EMAT( 9,1)                           ! Max allowable stress in compression
      ULT_STRE(3,1) = EMAT(10,1)                           ! Max allowable stress in shear
 
! **********************************************************************************************************************************
! Generate the mass matrix for this element (array was initialized in subr EMG). 
 
      IF (OPT(1) == 'Y') THEN
         M0 = (RHO*AREA + NSM)*(ELEM_LEN_AB)/TWO
         ME(1,1) = M0
         ME(2,2) = M0
         ME(3,3) = M0
         ME(7,7) = M0
         ME(8,8) = M0
         ME(9,9) = M0
      ENDIF
 
! **********************************************************************************************************************************
! Call routines to calc element matrices (stiffness, etc.)
 
      IF ((OPT(2) == 'Y') .OR. (OPT(3) == 'Y') .OR. (OPT(4) == 'Y') .OR. (OPT(5) == 'Y') .OR. (OPT(6) == 'Y')) THEN
 
         IF      (TYPE == 'ROD     ') THEN

            CALL ROD1 ( OPT, ELEM_LEN_AB, AREA, JTOR, ZS(1), E, G, ALPHA, TREF )
 
         ELSE IF (TYPE == 'BAR     ') THEN                 ! Bernoulli-Euler prismatic beam
         
            IF (DEBUG(249) == 0) THEN

               CALL BAR1 ( OPT, ELEM_LEN_AB, AREA, I1, I2, JTOR, ZS(9), K1, K2, I12, E, G, ALPHA, TREF )

            ELSE
               IF (DABS(I12) < EPS1) THEN
                  CALL BART ( OPT, ELEM_LEN_AB, AREA, I1, I2, JTOR, ZS(9), K1, K2, I12, E, G, ALPHA, TREF )
               ELSE
                  WRITE(ERR,1963) EID
                  WRITE(F06,1963) EID
                  RETURN
               ENDIF
               
            ENDIF

         ELSE IF (TYPE == 'BEAM    ') THEN                 ! General beam

            CALL BEAM

         ENDIF
 
      ENDIF
 
! **********************************************************************************************************************************
 1963 FORMAT(' *ERROR  1962: TIMOSHENKO BAR ELEMENT ',A,' CANNOT HAVE NONZERO I12. IT WILL BE SET TO I12 = 0.')

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BREL1
