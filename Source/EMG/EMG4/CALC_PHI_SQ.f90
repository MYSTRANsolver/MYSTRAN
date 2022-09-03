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

      SUBROUTINE CALC_PHI_SQ ( IERROR )

! Calculates the PHI_SQ shear correction factor used in the MIN3 and MIN4 elements (TRIA3, QUAD4 Mindlin elements) in subrs
! TRPLT2, QPLT2. PHI_SQ is used as a multiplier of the shear stiffness and the element stresses

!   PHI_SQ  = CBMIN*PSI_HAT/(1 + CBMIN*PSI_HAT) where

!   PSI_HAT = BENSUM/SHRSUM

!   (1) CBMIN is a constant from the published papers by Alexander TEssler (see refs in the MYSTRAN User's Manual) for the MIN3
!       triangular element (TRIA3) and MIN4 quadrilateral element (QUAD4)

!   (2) BENSUM is the sum of the diagonal terms for rotation DOF's from the bending portion of the element stiffness matrix

!   (3) SHRSUM is the sum of the diagonal terms from the transverse shear portion of the element stiffness matrix

! BENSUM and SHRSUM are determined in subrs TPLT2 (for the MIN3 TRIA3 elem) and QPLT2 (for the MIN4 QUAD4 elem) prior to calling
! this subr.

! For composite elements (PCOMP properties), a correction to PHI_SQ has to be made when PHI_SQ is calculated during stress recovery
! of individual plies. In these cases, the PHI_SQ calculated when the stiffness and stress recovery matrices is based on the
! individual ply thicknesses (bending and transverse shear) but should really be based on the whole element - i.e. the integrated
! effect of all plies. It is known that BENSUM is proportional to the bending inertia of the element and SHRSUM is proportional to
! the transverse shear thickness of the element. Thus, for composite elements, the BENSUM and SHRSUM calculated for 1 ply are
! adjusted by the ratio of these quantities between 1 ply and the composite layup


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEFE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_PHI_SQ_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWELVE
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, CBMIN4T, EPSIL, PCMPTSTM, QUAD4TYP
      USE MODEL_STUF, ONLY            :  BENSUM, EID, EMG_IFE, EMG_RFE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS, INTL_MID, PHI_SQ,  &
                                         PCOMP_PROPS, PLY_NUM, PSI_HAT, SHRSUM, TPLY, TYPE
      USE CALC_PHI_SQ_USE_IFs

      IMPLICIT NONE 

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CALC_PHI_SQ'

      INTEGER(LONG), INTENT(OUT)      :: IERROR            ! Local error indicator
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_PHI_SQ_BEGEND

      REAL(DOUBLE)                    :: CBMIN  = ZERO     ! Either CBMIN3 or CBMIN4
      REAL(DOUBLE)                    :: DEN               ! Denominator term in calculating PHI_SQ
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare to real zero
      REAL(DOUBLE)                    :: PCOMP_TM          ! Membrane thick of PCOMP for equiv PSHELL (see subr SHELL_ABD_MATRICES)
      REAL(DOUBLE)                    :: PCOMP_IB          ! Bending MOI of PCOMP for equiv PSHELL (see subr SHELL_ABD_MATRICES)
      REAL(DOUBLE)                    :: PCOMP_TS          ! Transv shear thick of PCOMP for equiv PSHELL (subr SHELL_ABD_MATRICES)
      REAL(DOUBLE)                    :: PLY_IB            ! Bending MOI of a ply
      REAL(DOUBLE)                    :: PLY_TS            ! Transv shear thick of a ply

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IERROR = 0
      EPS1   = EPSIL(1)

! For composite elements, adjust BENSUM and SHRSUM to be based on the whole composite element properties since they were calculated
! based on only 1 ply

      IF (PCOMP_PROPS == 'Y') THEN
         IF (PLY_NUM /= 0) THEN
            CALL GET_PCOMP_SECT_PROPS ( PCOMP_TM, PCOMP_IB, PCOMP_TS )
            PLY_IB = TPLY*TPLY*TPLY/TWELVE
            PLY_TS = TPLY*PCMPTSTM
            BENSUM = (PCOMP_IB/PLY_IB)*BENSUM
            SHRSUM = (PCOMP_TS/PLY_TS)*SHRSUM
         ENDIF
      ENDIF

! Now calculate PHI_SQ (but only if the element has transverse shear flexibility)

      IF      (TYPE(1:5) == 'QUAD4') THEN                  ! Regardless of who calls this subr, TYPE will det which CBMIN to use
         IF (QUAD4TYP == 'MIN4T') THEN                     ! quad TYPE's will use either CBMIN4 or CBMIN4T
            CBMIN = CBMIN4T
         ELSE
            CBMIN = CBMIN4
         ENDIF
      ELSE IF (TYPE(1:5) == 'TRIA3') THEN                  ! tria TYPE's will use CBMIN3 (NOTE: MIN4T quads will use CBMIN4T, above)
         CBMIN = CBMIN3
      ELSE
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IERROR  = IERROR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1943) TYPE, EID
            WRITE(F06,1943) TYPE, EID
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1943
            ENDIF
         ENDIF
      ENDIF

      IERROR  = 0
      PSI_HAT = ZERO
      IF (DABS(SHRSUM) > EPS1) THEN
         PSI_HAT = BENSUM/SHRSUM
      ELSE
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IERROR  = IERROR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1923) SHRSUM, TYPE, EID
            WRITE(F06,1923) SHRSUM, TYPE, EID
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1923
               EMG_RFE(NUM_EMG_FATAL_ERRS,1) = SHRSUM
            ENDIF
         ENDIF
      ENDIF

      DEN = ONE + CBMIN*PSI_HAT
      IF (DABS(DEN) > EPS1) THEN
         PHI_SQ = CBMIN*PSI_HAT/DEN
      ELSE
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IERROR  = IERROR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1942) DEN, TYPE, EID
            WRITE(F06,1942) DEN, TYPE, EID
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1942
               EMG_RFE(NUM_EMG_FATAL_ERRS,1) = DEN
            ENDIF
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1923 FORMAT(' *ERROR  1923: SHRSUM PARAMETER = ',1ES9.2,' IS TOO CLOSE TO ZERO FOR ',A,' ELEMENT ',I8                             &
                    ,/,14X,' CANNOT CALCULATE PSI_HAT FACTOR NEEDED FOR TRANSVERSE SHEAR STIFFNESS CALCULATION')

 1942 FORMAT(' *ERROR  1942: 1 + CBMIN*PSI_HAT = ',1ES9.2,' IS TOO CLOSE TO ZERO FOR ',A,' ELEMENT ',I8                            &
                    ,/,14X,' CANNOT CALCULATE PHI_SQ FACTOR NEEDED FOR TRANSVERSE SHEAR STIFFNESS CALCULATION')

 1943 FORMAT(' *ERROR  1943: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,  A, 'ELEMENT ',I8,' IS NOT A VALID ELEMENT FOR THIS SUBR') 

! **********************************************************************************************************************************

      END SUBROUTINE CALC_PHI_SQ