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

      SUBROUTINE RENORM ( VEC_NUM, NORM_GRD, NORM_COMP, NORM, NORM_GSET_DOF, GEN_MASS1, PHI_SCALE_FAC )
  
! Renormalizes eigenves based on NORM = POINT or MAX if requested on Bulk Data entry EIGR or EIGRL
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFG, NDOFG, NGRID, WARN_ERR
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  RENORM_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE COL_VECS, ONLY              :  UG_COL
  
      USE RENORM_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RENORM'
      CHARACTER( 8*BYTE), INTENT(IN)  :: NORM              ! Eigenvector renormalization methof from EIGR card (e.g. 'MAX     ')
        
      INTEGER(LONG), INTENT(IN)       :: NORM_COMP         ! Comp. (1-6) for renormalizing eigenvectors (from EIGR card)
      INTEGER(LONG), INTENT(IN)       :: NORM_GRD          ! Grid Point  for renormalizing eigenvectors (from EIGR card)
      INTEGER(LONG), INTENT(IN)       :: NORM_GSET_DOF     ! G-set DOF no. for NORM_GRD/NORM_COMP 
      INTEGER(LONG), INTENT(IN)       :: VEC_NUM           ! Number used to control an output message (only want this information
!                                                            message written if tyhis is the first call to this subr).
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RENORM_BEGEND
  
      REAL(DOUBLE) , INTENT(INOUT)    :: GEN_MASS1         ! Generalized mass for 1 eigenvector
      REAL(DOUBLE) , INTENT(OUT)      :: PHI_SCALE_FAC     ! Scale factor for the eigenvector to renormalize it
      REAL(DOUBLE)                    :: DPHI_MAX          ! Absolute value of PHI_MAX
      REAL(DOUBLE)                    :: EPS1              ! Small number to compare variables against zero
      REAL(DOUBLE)                    :: PHI_MAX           ! Largest DZIJ for all DOF'si for one eigenvector
      REAL(DOUBLE)                    :: PHI_POINT         ! Variable used when normalizing gen. mass and eigenvectors
  
      INTRINSIC DSQRT,DABS
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      PHI_SCALE_FAC = ONE

! Check for renorm = MAX or POINT and renormalize.

      EPS1 = EPSIL(1)

      IF (NORM == 'POINT   ') THEN                         ! Renormalize eigenvector on POINT 
  
         IF (NORM_GSET_DOF > 0) THEN                       ! If not, msg was written in LINK5 for no renorm, so return

            PHI_POINT = UG_COL(NORM_GSET_DOF)
            IF (DABS(PHI_POINT) > EPS1) THEN
               DO I=1,NDOFG
                  UG_COL(I) = UG_COL(I)/PHI_POINT
               ENDDO
               PHI_SCALE_FAC = PHI_POINT 
               GEN_MASS1 = GEN_MASS1/(PHI_SCALE_FAC * PHI_SCALE_FAC)
            ELSE
               PHI_SCALE_FAC = ONE
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,4113) VEC_NUM,NORM_GRD,NORM_COMP
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,4113) VEC_NUM,NORM_GRD,NORM_COMP
               ENDIF
            ENDIF

         ELSE

            RETURN                                         ! No renorm if NORM_GSET_DOF undefined (msg written in LINK5)

         ENDIF

      ELSE                                                 ! NORM is MAX

         PHI_MAX  = ZERO
         DPHI_MAX = ZERO
         DO I=1,NDOFG                                      ! Scan eigenvector to find largest value (+ or -)
            IF (DABS(UG_COL(I)) > DPHI_MAX) THEN
               PHI_MAX  = UG_COL(I)
               DPHI_MAX = DABS(PHI_MAX)
            ENDIF
         ENDDO
  
         IF (DPHI_MAX > EPS1) THEN                         ! Renormalize the eigenvector if PHI_MAX is not 0
            DO I=1,NDOFG
               UG_COL(I) = UG_COL(I)/PHI_MAX
            ENDDO 
            PHI_SCALE_FAC = PHI_MAX 
            GEN_MASS1 = GEN_MASS1/(PHI_SCALE_FAC * PHI_SCALE_FAC)
         ELSE
            PHI_SCALE_FAC = ONE
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,4114) VEC_NUM
            IF (SUPWARN == 'N') THEN
               WRITE(F06,4114) VEC_NUM
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
 4113 FORMAT(' *WARNING    : EIGENVECTOR ',I8,' IS ZERO FOR GRID POINT-COMPONENT ',2I8,'. THIS VECTOR WILL NOT BE RENORMALIZED')

 4114 FORMAT(' *WARNING    : EIGENVECTOR ',I8,' MAX VALUE IS ZERO. THIS VECTOR CANNOT BE RENORMALIZED')

! **********************************************************************************************************************************
 
      END SUBROUTINE RENORM
