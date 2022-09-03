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
 
      SUBROUTINE ROD1 ( OPT, L, AREA, JTOR, SCOEFF, E, G, ALPHA, TREF )
 
! Calculates, for 1-D ROD element

!  1) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  2) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  3) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  4) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NTSUB, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ROD1_BEGEND
      USE CONSTANTS_1, ONLY           :  TWO
      USE MODEL_STUF, ONLY            :  DT, KE, KED, PEL, PTE, SE1, STE1, UEL

      USE ROD1_USE_IFs

      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ROD1'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      INTEGER(LONG)                   :: J                 ! Do loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROD1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: ALPHA             ! Coefficient of thermal expansion
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Cross-sectional area
      REAL(DOUBLE) , INTENT(IN)       :: E                 ! Youngs modulus
      REAL(DOUBLE) , INTENT(IN)       :: G                 ! Shear modulus
      REAL(DOUBLE) , INTENT(IN)       :: JTOR              ! Torsional constant
      REAL(DOUBLE) , INTENT(IN)       :: L                 ! Elem length
      REAL(DOUBLE) , INTENT(IN)       :: SCOEFF            ! Stress recovery coeff for torsion
      REAL(DOUBLE) , INTENT(IN)       :: TREF              ! Element reference temperature
      REAL(DOUBLE)                    :: C01               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: C02               ! Intermediate variable used in calc terms for the stiff matrix, KE
      REAL(DOUBLE)                    :: CT0               ! Intermediate variable used in calc thermal loads, PTE
      REAL(DOUBLE)                    :: Fx                ! Axizl load in the ROD element as a result of the applied loads
      REAL(DOUBLE)                    :: KD0               ! Intermediate variable used in calc KED
      REAL(DOUBLE)                    :: TBAR              ! Average elem temperature 
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Determine element thermal loads. 
 
      IF (OPT(2) == 'Y') THEN
         DO J=1,NTSUB
            TBAR = (DT(1,J) + DT(2,J))/TWO
            CT0 = AREA*E*ALPHA*(TBAR  - TREF)
            PTE( 1,J) = -CT0
            PTE( 7,J) =  CT0 
         ENDDO
      ENDIF
  
! **********************************************************************************************************************************
! Calculate SE matrix (2 x 12) for stress data recovery.
 
      IF ((OPT(3) == 'Y') .OR. (OPT(6) == 'Y')) THEN
         SE1(1, 1,1) = -E/L
         SE1(1, 7,1) =  E/L
         SE1(2, 4,1) = -SCOEFF*G/L                         ! SCOEFF*G/L = (SCOEFF/J)*KE(7,7)
         SE1(2,10,1) =  SCOEFF*G/L
         DO J=1,NTSUB
            TBAR = (DT(1,J) + DT(2,J))/TWO
            STE1(1,J,1) = E*ALPHA*(TBAR - TREF)
         ENDDO
      ENDIF
  
! **********************************************************************************************************************************
! Calculate element stiffness matrix KE(12,12).
 
      IF ((OPT(4) == 'Y') .OR. (OPT(6) == 'Y')) THEN
         C01 = AREA*E/L
         C02 = JTOR*G/L
         KE( 1, 1) = C01
         KE( 1, 7) =-C01
         KE( 7, 1) = KE( 1, 7)
         KE( 4, 4) = C02
         KE( 4,10) =-C02
         KE(10, 4) = KE( 4,10)
         KE( 7, 7) = C01
         KE(10,10) = C02
      ENDIF
  
! **********************************************************************************************************************************
! Calculate element differential stiffness matrix KED(12,12).
 
      IF (OPT(6) == 'Y') THEN

         CALL ELMDIS
         CALL CALC_ELEM_NODE_FORCES
         Fx = -PEL(1)                                      ! Fx  (axial force for BAR or ROD) - NASTRAN Fx
         KD0 = Fx/L

         KED( 2, 2) =  KD0
         KED( 2, 8) = -KD0
         KED( 3, 3) =  KD0
         KED( 3, 9) = -KD0

         KED( 8, 2) =  KED( 2, 8)
         KED( 8, 8) =  KED( 2, 2)
         KED( 9, 3) =  KED( 3, 9)
         KED( 9, 9) =  KED( 3, 3)
      ENDIF
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE ROD1