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
  
      SUBROUTINE QSHEAR ( OPT, IORD, RED_INT_SHEAR, XSD, YSD )
 
! Isoparametric membrane quadrilateral shear element. Element can be nonplanar. HBAR is the dist that the nodes are away from the
! mean plane (+/-). If HBAR is small, the virgin element has 8 DOF (2 displ DOF's/node) and is expanded to MYSTRAN 24 DOF
! (6 DOF/node) using array ID1. If HBAR is larger than MXWARP, then matrix BMEANT (from subr ELMGM2) is used to acount
! for the non-planarity. If debug(4) = 0 then, in this case, array ID2 will expand the virgin element to MYSTRAN 24 DOF (6 DOF/node)

! Subroutine calculates:

!  1) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  2) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  3) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  4) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS, MEFE, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  QSHEAR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, FOUR
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, BMEANT, DT, EID, ELDOF, ELGP, EM, ERR_SUB_NAM, HBAR, KE, MXWARP,             &
                                         NUM_EMG_FATAL_ERRS, PCOMP_LAM, PCOMP_PROPS, PPE, PRESS, PTE,                              &
                                         SE1, STE1, SHELL_AALP, SHELL_A, TREF, TYPE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE QSHEAR_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'QSHEAR'
      CHARACTER(1*BYTE) , INTENT(IN)  :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER( 1*BYTE), INTENT(IN)  :: RED_INT_SHEAR     ! If 'Y', use Gaussian weighted average of B matrices for shear terms
      CHARACTER(46*BYTE)              :: IORD_MSG          ! Character name of the integration order (used for debug output)

      INTEGER(LONG), INTENT(IN)       :: IORD              ! Gaussian integration order for element
      INTEGER(LONG)                   :: GAUSS_PT          ! Gauss point number (used for output in subr SHP2DQ
      INTEGER(LONG)                   :: I,J,K,L,M         ! DO loop indices
      INTEGER(LONG), PARAMETER        :: ID1( 8) = (/ 1, & ! ID1(1) =  1 means virgin  8X8  elem DOF  1 is MYSTRAN 24X24 elem DOF  1
                                                      2, & ! ID1(2) =  2 means virgin  8X8  elem DOF  2 is MYSTRAN 24X24 elem DOF  2
                                                      7, & ! ID1(3) =  7 means virgin  8X8  elem DOF  3 is MYSTRAN 24X24 elem DOF  7
                                                      8, & ! ID1(4) =  8 means virgin  8X8  elem DOF  4 is MYSTRAN 24X24 elem DOF  8
                                                     13, & ! ID1(5) = 13 means virgin  8X8  elem DOF  5 is MYSTRAN 24X24 elem DOF 13
                                                     14, & ! ID1(6) = 14 means virgin  8X8  elem DOF  6 is MYSTRAN 24X24 elem DOF 14
                                                     19, & ! ID1(7) = 19 means virgin  8X8  elem DOF  7 is MYSTRAN 24X24 elem DOF 19
                                                     20 /) ! ID1(8) = 20 means virgin  8x8  elem DOF  8 is MYSTRAN 24x24 elem DOF 20

      INTEGER(LONG), PARAMETER        :: ID2(12) = (/ 1, & ! ID2( 1)=  1 means expand 12x12 elem DOF  1 is MYSTRAN 24X24 elem DOF  1
                                                      2, & ! ID2( 2)=  2 means expand 12x12 elem DOF  2 is MYSTRAN 24X24 elem DOF  2
                                                      3, & ! ID2( 3)=  3 means expand 12x12 elem DOF  3 is MYSTRAN 24X24 elem DOF  3
                                                      7, & ! ID2( 4)=  7 means expand 12x12 elem DOF  4 is MYSTRAN 24X24 elem DOF  7
                                                      8, & ! ID2( 5)=  8 means expand 12x12 elem DOF  5 is MYSTRAN 24X24 elem DOF  8
                                                      9, & ! ID2( 6)=  9 means expand 12x12 elem DOF  6 is MYSTRAN 24X24 elem DOF  9
                                                     13, & ! ID2( 7)= 13 means expand 12x12 elem DOF  7 is MYSTRAN 24X24 elem DOF 13
                                                     14, & ! ID2( 8)= 14 means expand 12x12 elem DOF  8 is MYSTRAN 24X24 elem DOF 14
                                                     15, & ! ID2( 9)= 15 means expand 12x12 elem DOF  9 is MYSTRAN 24X24 elem DOF 15
                                                     19, & ! ID2(10)= 19 means expand 12x12 elem DOF 10 is MYSTRAN 24X24 elem DOF 19
                                                     20, & ! ID2(11)= 20 means expand 12x12 elem DOF 11 is MYSTRAN 24X24 elem DOF 20
                                                     21 /) ! ID2(12)= 21 means expand 12x12 elem DOF 12 is MYSTRAN 24X24 elem DOF 21
 
      INTEGER(LONG), PARAMETER        :: NUM_NODES = 4     ! Quad has 4 nodes
                                                           ! Indicator of no output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = QSHEAR_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Diffs in x coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Diffs in y coords of quad sides in local coords
      REAL(DOUBLE)                    :: BI(3,2*ELGP)      ! Strain-displ matrix for this element for one Gauss point

                                                           ! Strain-displ matrix for this element for all Gauss points
      REAL(DOUBLE)                    :: BM(3,2*ELGP,IORD*IORD)

      REAL(DOUBLE)                    :: DETJ(IORD*IORD)   ! Determinant of JAC for all Gauss points
      REAL(DOUBLE)                    :: DPSHG(2,4)        ! Output from subr SHP2DQ. Derivatives of PSH wrt elem isopar coords.
      REAL(DOUBLE)                    :: DPSHX(2,4)        ! Derivatives of PSH wrt elem x, y coords.
      REAL(DOUBLE)                    :: DUM1(3,8)         ! Intermediate matrix used in solving for KE stiffness matrix       
      REAL(DOUBLE)                    :: DUM2(8,8)         ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM5(8,8)         ! Intermediate matrix used in solving for KE stiffness matrix
      REAL(DOUBLE)                    :: DUM6(12,8)        ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM7(12,12)       ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM9(3,8)         ! Intermediate matrix used in solving for SEi stress recovery matrices
      REAL(DOUBLE)                    :: EMS(3,3)          ! In-plane shear portion of membrane material matrix

                                                           ! An output from subr ORDER, called herein.  Gauss weights.
      REAL(DOUBLE)                    :: HHH(MAX_ORDER_GAUSS)

      REAL(DOUBLE)                    :: INTFAC            ! An integration factor (constant multiplier for the Gauss integration)

      REAL(DOUBLE)                    :: JAC(2,2)          ! An output from subr JAC2D, called herein. 2 x 2 Jacobian matrix.
      REAL(DOUBLE)                    :: JACI(2,2)         ! An output from subr JAC2D, called herein. 2 x 2 Jacobian inverse.
      REAL(DOUBLE)                    :: PSH(4)            ! Output from subr SHP2DQ, called herein.

                                                           ! An output from subr ORDER, called herein. Gauss abscissa's.
      REAL(DOUBLE)                    :: SSS(MAX_ORDER_GAUSS)

      REAL(DOUBLE)                    :: SUMB              ! An intermediate variable used in calc B matrix for reduced integration
      REAL(DOUBLE)                    :: SUMD              ! An intermediate variable used in calc B matrix for reduced integration
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Generate BM matrices. Dimensions 1 and 2 of BM store a element BM matrix for 1 Gauss point. The 3rd dimension has BM for all other
! Gauss points (denoted by GAUSS_PT index)

      CALL ORDER_GAUSS ( IORD, SSS, HHH )

      DO I=1,3
         DO J=1,2*ELGP
            DO K=1,IORD*IORD
               BM(I,J,K) = ZERO
            ENDDO
         ENDDO
      ENDDO

      IORD_MSG = 'for in-plane shear strains,      input IORD = '
      GAUSS_PT = 0
      DO I=1,IORD
         DO J=1,IORD
            GAUSS_PT = GAUSS_PT + 1
            CALL SHP2DQ ( I, J, NUM_NODES, SUBR_NAME, IORD_MSG, IORD, SSS(I), SSS(J), 'Y', PSH, DPSHG )
            CALL JAC2D ( SSS(I), SSS(J), XSD, YSD, 'N', JAC, JACI, DETJ(GAUSS_PT) )
            CALL MATMULT_FFF ( JACI, DPSHG, 2, 2, 4, DPSHX )
            CALL BMQMEM ( DPSHX, I, J, 'direct strains', 'Y', BI )
            DO L=1,3
               DO M=1,2*ELGP
                  BM(L,M,GAUSS_PT) = BI(L,M)
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      IF (RED_INT_SHEAR == 'Y') THEN
         DO J=1,2*ELGP
            SUMB = ZERO
            SUMD = ZERO
            DO K=1,IORD*IORD
               SUMB = SUMB + DETJ(K)*BM(3,J,K)
               SUMD = SUMD + DETJ(K)
            ENDDO
            DO K=1,IORD*IORD
               BM(3,J,K) = SUMB/SUMD
            ENDDO
         ENDDO
      ENDIF

! **********************************************************************************************************************************
! Calculate element thermal loads. 
  
      IF (OPT(2) == 'Y') THEN

      
      ENDIF
  
! **********************************************************************************************************************************
! Calculate BE1, SE1 matrix (3 x 24) for strain/stress data recovery. All calculated at center of element
! Note: For the SHEAR element only the 3rd row of BE1 should be nonzero
 
      IF (OPT(3) == 'Y') THEN
 
         GAUSS_PT = 1
         IORD_MSG = 'for in-plane direct strains,                = '
         CALL SHP2DQ ( 1, 1, NUM_NODES, SUBR_NAME, 'UNITY ', 1, ZERO, ZERO, 'N', PSH, DPSHG )
         CALL JAC2D ( ZERO, ZERO, XSD, YSD, 'N', JAC, JACI, DETJ(GAUSS_PT) )
         CALL MATMULT_FFF ( JACI, DPSHG, 2, 2, 4, DPSHX )
         CALL BMQMEM ( DPSHX, 1, 1, 'all strains', 'N', BI )

         DO I=1,3                                          ! Strain-displ matrix (only 3rd row, for in-plane shear strains, is /= 0
            DO J=1,8
               BE1(I,ID1(J),1) = ZERO
            ENDDO 
         ENDDO
         DO J=1,8
            BE1(3,ID1(J),1) = BI(3,J)
         ENDDO 
            

! SE1 generated in elem coords

         DO I=1,3
            DO J=1,3
               EMS(I,J) = ZERO
            ENDDO
         ENDDO
         EMS(3,3) = EM(3,3)

         CALL MATMULT_FFF ( EMS, BI, 3, 3, 8, DUM9 )       ! Generate SE1 in element coords (at this point EM is elem coords)
         DO I=1,3
            DO J=1,8
               SE1(I,ID1(J),1) = DUM9(I,J)
            ENDDO 
         ENDDO

      ENDIF  
  
! **********************************************************************************************************************************
! Calculate portion of element stiffness matrix KE due to in-plane shear.
 
      IF(OPT(4) == 'Y') THEN
  
         DO I=1,8
            DO J=1,8
               DUM5(I,J) = ZERO
            ENDDO 
         ENDDO   
 
         CALL ORDER_GAUSS ( IORD, SSS, HHH )
  
         GAUSS_PT = 0
         IORD_MSG = 'for in-plane direct strains,  IORD = '
         DO I=1,IORD
            DO J=1,IORD
               GAUSS_PT = GAUSS_PT + 1
               DO L=1,3
                  DO M=1,2*ELGP
                     BI(L,M) = BM(L,M,GAUSS_PT)
                  ENDDO
               ENDDO
               CALL MATMULT_FFF ( SHELL_A, BI, 3, 3, 8, DUM1 )
               CALL MATMULT_FFF_T ( BI, DUM1, 3, 8, 8, DUM2 )
               CALL JAC2D ( SSS(I), SSS(J), XSD, YSD, 'Y', JAC, JACI, DETJ(GAUSS_PT) )
               INTFAC = DETJ(GAUSS_PT)*HHH(I)*HHH(J)
               DO K=1,8
                  DO L=1,8
                     DUM5(K,L) = DUM5(K,L) + DUM2(K,L)*INTFAC
                  ENDDO 
               ENDDO   
            ENDDO 
         ENDDO   

         IF ((DABS(HBAR) > MXWARP) .AND. (DEBUG(4) == 0)) THEN
            CALL MATMULT_FFF_T ( BMEANT, DUM5, 8, 12, 8, DUM6 )
            CALL MATMULT_FFF ( DUM6, BMEANT, 12, 8, 12, DUM7 )
            DO I=1,12
               DO J=1,12
                  KE(ID2(I),ID2(J)) = KE(ID2(I),ID2(J)) + DUM7(I,J)
               ENDDO   
            ENDDO 
         ELSE
            DO I=1,8
               DO J=1,8
                  KE(ID1(I),ID1(J)) = DUM5(I,J)
               ENDDO   
            ENDDO 
         ENDIF
  
! Set lower triangular portion of KE equal to upper portion
  
         DO I=2,24
            DO J=1,I-1
               KE(I,J) = KE(J,I)
            ENDDO 
         ENDDO 
  
      ENDIF
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1956 FORMAT(' *ERROR  1956: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DIMENSION OF STRESS/STRAIN RECOVERY MATRICES (SEi/BEi) IS NOT SUFFICIENT FOR ',A,I8                   &
                    ,/,14X,' THERE ARE ',I3,' SEi/BEi MATRICES DIMENSIONED BUT ATTEMPT TO CREATE ',I3,' OF THEM WAS MADE' )





! **********************************************************************************************************************************

      END SUBROUTINE QSHEAR
