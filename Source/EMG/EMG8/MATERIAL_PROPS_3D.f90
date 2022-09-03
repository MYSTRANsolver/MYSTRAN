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
  
      SUBROUTINE MATERIAL_PROPS_3D ( WRITE_WARN )
 
! Calculates material stress/strain matrices for isotropic or anisotropic 3-D solid elements
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATERIAL_PROPS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  ALPVEC, EID, ES, EMAT, NUM_EMG_FATAL_ERRS, MTRL_TYPE, RHO, ULT_STRE, ULT_STRN, TREF, TYPE
  
      USE MATERIAL_PROPS_3D_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATERIAL_PROPS_3D'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: IERROR       = 0  ! Local error indicator
      INTEGER(LONG)                   :: I,J               ! DO loop indices 
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATERIAL_PROPS_BEGEND
  
      REAL(DOUBLE)                    :: ALPHA             ! Isotropic coefficient of thermal expansion
      REAL(DOUBLE)                    :: DEN1              ! An intermaediate variable in calculating outputs
      REAL(DOUBLE)                    :: DEN2              ! An intermaediate variable in calculating outputs
      REAL(DOUBLE)                    :: E                 ! Isotropic Young's modulus
      REAL(DOUBLE)                    :: E0                ! An intermaediate variable in calculating outputs
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare for real zero
      REAL(DOUBLE)                    :: G                 ! Shear modulus
      REAL(DOUBLE)                    :: NU                ! Poisson's ratio
      REAL(DOUBLE)                    :: NU0               ! An intermaediate variable in calculating outputs

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1   = EPSIL(1)
  
      DO I=1,6                                             ! 3D material matrix
         DO J=1,6
            ES(I,J) = ZERO
         ENDDO
      ENDDO

      IF (MTRL_TYPE(1) == 1) THEN                          ! Isotropic properties

         E         = EMAT( 1,1)
         G         = EMAT( 2,1)
         NU        = EMAT( 3,1)
         RHO(1)    = EMAT( 4,1)
         ALPHA     = EMAT( 5,1)
         TREF(1)   = EMAT( 6,1)

         ULT_STRE(1,1) = EMAT( 8,1)                        ! Dir 1 tension  allowable
         ULT_STRE(2,1) = EMAT( 9,1)                        ! Dir 1 compr    allowable
         ULT_STRE(3,1) = ULT_STRE(1,1)                     ! Dir 2 tension  allowable
         ULT_STRE(4,1) = ULT_STRE(2,1)                     ! Dir 2 compr    allowable
         ULT_STRE(5,1) = ULT_STRE(1,1)                     ! Dir 3 tension  allowable
         ULT_STRE(6,1) = ULT_STRE(2,1)                     ! Dir 3 compr    allowable
         ULT_STRE(7,1) = EMAT(10,1)                        ! Plane 12 shear allowable
         ULT_STRE(8,1) = EMAT(10,1)                        ! Plane 23 shear allowable
         ULT_STRE(9,1) = EMAT(10,1)                        ! Plane 31 shear allowable

         ULT_STRN(1,1) = ZERO
         ULT_STRN(2,1) = ZERO
         ULT_STRN(3,1) = ZERO
         ULT_STRN(4,1) = ZERO
         ULT_STRN(5,1) = ZERO
         ULT_STRN(6,1) = ZERO
         ULT_STRN(7,1) = ZERO
         ULT_STRN(8,1) = ZERO
         ULT_STRN(9,1) = ZERO

         IF (DABS(E) > EPS1) THEN
            ULT_STRN(1,1) = ULT_STRE(1,1)/E
            ULT_STRN(2,1) = ULT_STRE(2,1)/E
            ULT_STRN(3,1) = ULT_STRE(3,1)/E
            ULT_STRN(4,1) = ULT_STRE(4,1)/E
            ULT_STRN(5,1) = ULT_STRE(5,1)/E
            ULT_STRN(6,1) = ULT_STRE(6,1)/E
         ENDIF

         IF (DABS(G) > EPS1) THEN
            ULT_STRN(7,1) = ULT_STRE(7,1)/G
            ULT_STRN(8,1) = ULT_STRE(8,1)/G
            ULT_STRN(9,1) = ULT_STRE(9,1)/G
         ENDIF

         DEN1 = ONE + NU
         IF (DABS(DEN1) <= EPS1) THEN
            WRITE(ERR,1919) EID,TYPE
            WRITE(F06,1919) EID,TYPE
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            IERROR = IERROR + 1
         ENDIF

         DEN2 = ONE - TWO*NU
         IF (DABS(DEN2) <= EPS1) THEN
            WRITE(ERR,1936) EID,TYPE
            WRITE(F06,1936) EID,TYPE
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            IERROR = IERROR + 1
         ENDIF

         E0  = E/DEN1
         NU0 = (ONE - NU)/DEN2

         ES(1,1) = E0*NU0
         ES(1,2) = E0*NU/DEN2
         ES(1,3) = ES(1,2)

         ES(2,1) = ES(1,2)
         ES(2,2) = ES(1,1)
         ES(2,3) = ES(1,2)

         ES(3,1) = ES(1,2)
         ES(3,2) = ES(1,2)
         ES(3,3) = ES(1,1)

         ES(4,4) = G
         ES(5,5) = ES(4,4)
         ES(6,6) = ES(4,4)

         ALPVEC(1,1) = ALPHA
         ALPVEC(2,1) = ALPVEC(1,1)
         ALPVEC(3,1) = ALPVEC(1,1)
         ALPVEC(4,1) = ZERO
         ALPVEC(5,1) = ZERO
         ALPVEC(6,1) = ZERO

      ELSE IF (MTRL_TYPE(1) == 9) THEN                          ! Anisotropic properties

         K = 0
         DO I=1,6
            DO J=I,6
               K = K + 1
               ES(I,J) = EMAT(K,1)
               ES(J,I) = ES(I,J)
            ENDDO
         ENDDO

         RHO(1)      = EMAT(22,1)

         ALPVEC(1,1) = EMAT(23,1)
         ALPVEC(2,1) = EMAT(24,1)
         ALPVEC(3,1) = EMAT(25,1)
         ALPVEC(4,1) = EMAT(26,1)
         ALPVEC(5,1) = EMAT(27,1)
         ALPVEC(6,1) = EMAT(28,1)

         TREF(1)     = EMAT(29,1)
         
         DO I=1,9
            ULT_STRE(I,1) = ZERO
            ULT_STRN(I,1) = ZERO
         ENDDO

      ELSE

         WRITE(ERR,1926) MTRL_TYPE(1)
         WRITE(F06,1926) MTRL_TYPE(1)
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
         IERROR = IERROR + 1

      ENDIF

! Return if IERROR > 0

      IF (IERROR > 0) RETURN

! **********************************************************************************************************************************
! Return if IERROR > 0

      IF (IERROR > 0) RETURN

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1919 FORMAT(' *ERROR  1919: MATERIAL ERROR. 1 + NU IS TOO CLOSE TO ZERO FOR ELEMENT ',I8,', TYPE ',A)

 1926 FORMAT(' *ERROR  1926: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATERIAL TYPE MUST BE 1 FOR 3D ELEMENTS BUT TYPE WAS ',I8)

 1936 FORMAT(' *ERROR  1936: MATERIAL ERROR. 1 - 2*NU IS TOO CLOSE TO ZERO FOR ELEMENT ',I8,', TYPE ',A)


! **********************************************************************************************************************************
      END SUBROUTINE MATERIAL_PROPS_3D
