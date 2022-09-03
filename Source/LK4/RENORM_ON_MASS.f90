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

      SUBROUTINE RENORM_ON_MASS ( NVC, EPS1 )
  
! Renormalizes eigenvectors to unit generalized mass 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NDOFL, BLNK_SUB_NAM, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL, SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  RENORM_ON_MASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE EIGEN_MATRICES_1 , ONLY     :  GEN_MASS, EIGEN_VEC
      USE MODEL_STUF, ONLY            :  EIG_NORM, MAXMIJ, MIJ_COL, MIJ_ROW
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
  
      USE RENORM_ON_MASS_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RENORM_ON_MASS'
        
      INTEGER(LONG), INTENT(IN)       :: NVC               ! Number of eigenvectors to be renormalized.
      INTEGER(LONG)                   :: I,J               ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RENORM_ON_MASS_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: EPS1              ! Small number to compare variables against zero
      REAL(DOUBLE)                    :: DEN               ! Normalizing factor in gen mass matrix normalization

      INTRINSIC DSQRT,DABS
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF (EIG_NORM /= 'MASS    ') THEN
         WRITE(ERR,1001) EIG_NORM
         IF (SUPINFO == 'N') THEN
            WRITE(F06,1001) EIG_NORM
         ENDIF
      ENDIF

      DO I=1,NVC
         IF (DABS(GEN_MASS(I)) < EPS1) THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,4301) I, GEN_MASS(I)
            IF (SUPWARN == 'N') THEN
               WRITE(F06,4301) I, GEN_MASS(I)
            ENDIF
            RETURN
         ENDIF
      ENDDO

! Adjust MAXMIJ, the largest off-diag gen mass term. It was originally calculated in subr CALC_GEN_MASS and will change if the
! gen masses have changed as a result of this renormalization

      MAXMIJ = MAXMIJ/(GEN_MASS(MIJ_ROW)*GEN_MASS(MIJ_COL))! NOTE: all gen mass terms checked above for > 0.

! Normalize the eigenvectors so that they produce unit generalized mass and reset the gen masses to unity

      DO J=1,NVC
         DEN = DSQRT(GEN_MASS(J))
         DO I=1,NDOFL
            EIGEN_VEC(I,J) = EIGEN_VEC(I,J)/DEN
         ENDDO
         GEN_MASS(J) = ONE                                 ! Now reset generalized masses to unity 
      ENDDO 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1001 FORMAT(' *INFORMATION: EIGENVECTORS WILL BE RENORMALIZED BASED ON GEN MASS IN LINK4. THEY WILL BE RENORMALIZED TO ',         &
                             A,' LATER IN LINK5',/)

 4301 FORMAT(' *WARNING    : THE GENERALIZED MASS MATRIX HAS A DIAGONAL TERM THAT IS TOO SMALL TO ALLOW RENORMALIZATION OF THE',   &
                           ' EIGENVECTORS.'                                                                                        &
                    ,/,14X,' THE SMALL TERM IS FOR EIGENVECTOR ',I8,' AND ITS VALUE IS ',1ES9.2                                    &
                    ,/,14X,' EIGENVECTORS WILL NOT BE RENORMALIZED TO UNIT MASS IN SUBR RENORM_ON_MASS')

99001 FORMAT(1X,10(1ES13.6))

! **********************************************************************************************************************************

      END SUBROUTINE RENORM_ON_MASS
