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
 
      SUBROUTINE TREL1 ( OPT, WRITE_WARN )
  
! Calculates, or calls subr's to calculate, triangular element matrices:

!  1) ME        = element mass matrix                  , if OPT(1) = 'Y'
!  2) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  3) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  4) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  5) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  6) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEWE, NSUB, NTSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TREL1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TENTH, ONE, TWO, THREE, TWELVE
      USE PARAMS, ONLY                :  SUPWARN
      USE MODEL_STUF, ONLY            :  EID, ELDOF, EMG_IWE, EMG_RWE, INTL_MID, KE, MASS_PER_UNIT_AREA, ME,                       &
                                         NUM_EMG_FATAL_ERRS, PCOMP_LAM, PCOMP_PROPS, SHELL_B, TYPE, XEB, XEL
      USE MODEL_STUF, ONLY            :  BENSUM, SHRSUM, PHI_SQ, PSI_HAT, XTB, XTL
 
      USE TREL1_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TREL1'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: IERROR            ! Local error indicator from one of the subrs called
      INTEGER(LONG)                   :: K,L               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TREL1_BEGEND

      REAL(DOUBLE)                    :: AR                ! Elem aspect ratio
      REAL(DOUBLE)                    :: AREA              ! Elem area

      REAL(DOUBLE)                    :: B2V(3,9)          ! The 3x9     virgin strain   recovery matrix for MIN3 for bending
      REAL(DOUBLE)                    :: B3V(3,9)          ! The 3x9     virgin strain   recovery matrix for MIN3 for transv shear

      REAL(DOUBLE)                    :: BIG_BB(3,ELDOF,1) ! Strain-displ matrix for bending for all Gauss points and all DOF's

      REAL(DOUBLE)                    :: BIG_BBI(3,ELDOF)  ! BIG_BB for 3rd subscript

      REAL(DOUBLE)                    :: BIG_BM(3,ELDOF,1) ! Strain-displ matrix for this elem for all Gauss points (for all DOF's)

      REAL(DOUBLE)                    :: BIG_BMI(3,ELDOF)  ! BIG_BM for 3rd subscript

      REAL(DOUBLE)                    :: DUM1(3,ELDOF)     ! Intermediate result in calc SHELL_B effect on KE
      REAL(DOUBLE)                    :: DUM2(ELDOF,ELDOF) ! Intermediate result in calc SHELL_B effect on KE
      REAL(DOUBLE)                    :: KV(9,9)           ! KB + PHISQ*KS (the 9x9 virgin stiffness matrix for MIN3)
      REAL(DOUBLE)                    :: M0                ! An intermediate variable used in calc elem mass, ME
      REAL(DOUBLE)                    :: PPV(9,NSUB)       ! The 9xNSUB  virgin thermal  load     matrix for MIN3
      REAL(DOUBLE)                    :: PTV(9,NTSUB)      ! The 9xNTSUB virgin pressure load     matrix for MIN3
      REAL(DOUBLE)                    :: S2V(3,9)          ! The 3x9     virgin stress   recovery matrix for MIN3 for bending
      REAL(DOUBLE)                    :: S3V(3,9)          ! The 3x9     virgin stress   recovery matrix for MIN3 for transv shear
      REAL(DOUBLE)                    :: X2E               ! x coord of elem node 2
      REAL(DOUBLE)                    :: X3E               ! x coord of elem node 3
      REAL(DOUBLE)                    :: Y3E               ! y coord of elem node 3
  
! The following 3 args are needed when subr TPLT2 is called when that triangular shell element is used in a MIN4T QUAD4 which is
! made up of 4 non-overlapping TPLT2 elements. Since TPLT2 can also be a stand-alone element, we need these args when that occurs.
! The only time TPLT2 is called from this subr is when it is a stand-alone element.

! We give them the PARAMETER attributes below to make sure this subr won't call TPLT2 with any other values. For the MIN4T QUAD4,
! TPLT2 is called in subr QPLT3 with appropriate values of TRIA_NUM, PSI.

      CHARACTER(LEN=1), PARAMETER     :: MN4T_QD   = 'N'
      INTEGER(LONG)   , PARAMETER     :: TRIA_NUM  = 1
      REAL(DOUBLE)    , PARAMETER     :: PSI       = 0.0D0

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      BENSUM  = ZERO
      SHRSUM  = ZERO
      PSI_HAT = ZERO
      PHI_SQ  = ZERO

! Calculate element geometry parameters from data block XEL
 
      X2E  = XEL(2,1)
      X3E  = XEL(3,1)
      Y3E  = XEL(3,2)
      AREA = X2E*Y3E/TWO
 
! XTB, XTL may be needed when TPLT2 calls BBMIN3, BSMIN3. Since TPLT2 is also called from QPLT3, which is made up of 4 TPLT2's,
! we cannot use XEB and XEL since, in that case, they are the values for the MIN4T QUAD4 element geometry, not for the 4 triangles
! making up that quad eleent

      DO K=1,3
         DO L=1,3
            XTB(K,L) = XEB(K,L)
            XTL(K,L) = XEL(K,L)
         ENDDO
      ENDDO

! Calculate and check element aspect ratio, AR. Print warning if AR > 2.0 for TMEM1
 
      AR = X2E/Y3E
      IF(AR < ONE) THEN
         AR = ONE/AR
      ENDIF
      IF (AR > TWO + TENTH) THEN
         WARN_ERR = WARN_ERR + 1
         IF ((WRT_ERR > 0) .AND. (WRITE_WARN == 'Y')) THEN
            WRITE(ERR,1924) TYPE, EID, AR, TWO
            IF (SUPWARN == 'N') THEN
               WRITE(F06,1924) TYPE, EID, AR, TWO
            ENDIF
         ELSE
            IF (WARN_ERR <= MEWE) THEN
               EMG_IWE(WARN_ERR,1) = 1924
               EMG_RWE(WARN_ERR,1) = AR
               EMG_RWE(WARN_ERR,2) = TWO
            ENDIF
         ENDIF
      ENDIF
  
! **********************************************************************************************************************************
! Generate the mass matrix for this element. For the pure bending element the mass is based only on the non-structural mass.
! The mass matrix was initialized in subr EMG
 
      IF (OPT(1) == 'Y') THEN
         M0 = MASS_PER_UNIT_AREA*AREA/THREE
         ME( 1 ,1) = M0
         ME( 2 ,2) = M0
         ME( 3 ,3) = M0
         ME( 7 ,7) = M0
         ME( 8 ,8) = M0
         ME( 9 ,9) = M0
         ME(13,13) = M0
         ME(14,14) = M0
         ME(15,15) = M0
      ENDIF

! **********************************************************************************************************************************
! If TYPE is 'TRMEM' or 'TRIA3K' or 'TRIA3' generate the membrane stiffness
! If TYPE is 'TRPLT1', 'TRPLT2', 'TRIA3K', or 'TRIA3' generate the bending stiffness
  
      IF ((OPT(2) == 'Y') .OR. (OPT(3) == 'Y') .OR. (OPT(4) == 'Y') .OR. (OPT(5) == 'Y')) THEN
 
         IF (TYPE(1:5) == 'TRIA3') THEN 
            IF (INTL_MID(1) /= 0) THEN
               CALL TMEM1 ( OPT, AREA, X2E, X3E, Y3E, 'Y', BIG_BM )
            ENDIF
         ENDIF
 
         IF (TYPE == 'TRIA3K  ') THEN
            IF (INTL_MID(2) /= 0) THEN
               CALL TPLT1 ( OPT, AREA, X2E, X3E, Y3E )
            ENDIF
         ENDIF
  
         IF (TYPE == 'TRIA3   ') THEN
            IF (INTL_MID(2) /= 0) THEN
               CALL TPLT2 (OPT, AREA, X2E, X3E, Y3E, 'Y', IERROR, KV, PTV, PPV, B2V, B3V, S2V, S3V, BIG_BB, MN4T_QD, TRIA_NUM, PSI)
            ENDIF
         ENDIF
 
      ENDIF        
 
! **********************************************************************************************************************************
! Calc BM'*SHELL_B*BB (and its transpose) and add to KE. Only do this if this is a composite element with nonsym layup

      IF (OPT(4) == 'Y') THEN

         IF (TYPE(1:5) == 'TRIA3') THEN

            IF ((PCOMP_PROPS == 'Y') .AND. (PCOMP_LAM == 'NON')) THEN

               if (type == 'QUAD4K  ') then
                  WRITE(ERR,*) ' *ERROR: Code not written for SHELL_B effect on KE yet for TRIA3K elements'
                  WRITE(ERR,*) '         Or, if QUAD4, make sure that the element has nonzero transverse shear moduli, G1Z, G2Z'
                  WRITE(F06,*) ' *ERROR :Code not written for SHELL_B effect on KE yet for TRIA3K elements'
                  WRITE(F06,*) '         Or, if QUAD4, make sure that the element has nonzero transverse shear moduli, G1Z, G2Z'
                  call outa_here ( 'Y' )
               endif

               DO K=1,3
                  DO L=1,ELDOF
                     BIG_BMI(K,L) = BIG_BM(K,L,1)
                     BIG_BMI(K,L) = BIG_BM(K,L,1)
                  ENDDO
               ENDDO

               DO K=1,3
                  DO L=1,ELDOF
                     BIG_BBI(K,L) = BIG_BB(K,L,1)
                     BIG_BBI(K,L) = BIG_BB(K,L,1)
                  ENDDO
               ENDDO

               CALL MATMULT_FFF ( SHELL_B, BIG_BBI, 3, 3, ELDOF, DUM1 )
               CALL MATMULT_FFF_T ( BIG_BMI, DUM1, 3, ELDOF, ELDOF, DUM2 )

               DO K=1,ELDOF
                  DO L=1,ELDOF
                     KE(K,L) = KE(K,L) + (DUM2(K,L) + DUM2(L,K))
                  ENDDO   
               ENDDO 

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
 1924 FORMAT(' *WARNING    : ASPECT RATIO OF ',A,' ELEMENT ',I8,' IS:',F7.1,'. IT SHOULD BE < ',F3.0)
  
! **********************************************************************************************************************************
 
      END SUBROUTINE TREL1
