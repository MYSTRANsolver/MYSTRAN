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
  
      SUBROUTINE QMEM1 ( OPT, IORD, RED_INT_SHEAR, AREA, XSD, YSD, BIG_BM )
 
! Isoparametric membrane quadrilateral. Default iorq1s = 1 gives reduced integration for shear terms. User can override
! this with Bulk Data PARAM iorq1s 2. Element can be nonplanar. HBAR is the dist that the nodes are away from the mean
! plane (+/-). If HBAR is small, the virgin element has 8 DOF (2 displ DOF's/node) and is expanded to MYSTRAN 24 DOF
! (6 DOF/node) using array ID1. If HBAR is larger than MXWARP, then matrix BMEANT (from subr ELMGM2) is used to acount
! for the non-planarity. If debug(4) = 0 then, in this case, array ID2 will expand the virgin element to MYSTRAN 24 DOF (6 DOF/node)

! Subroutine calculates:

!  1) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  2) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  3) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  4) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  5) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS, MEFE, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  QMEM1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, FOUR
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, BMEANT, DT, EID, ELDOF, ELGP, EM, ERR_SUB_NAM, HBAR, KE, MXWARP,             &
                                         NUM_EMG_FATAL_ERRS, PCOMP_LAM, PCOMP_PROPS, PPE, PRESS, PTE,                              &
                                         SE1, STE1, SHELL_AALP, SHELL_A, TREF, TYPE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE QMEM1_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'QMEM1'
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
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Do not change IORD_STRESS_Q4. The algorithm to find Gauss point coords in elem x,y space requires there to be the same number of
! shape functions and Gauss points as elem nodes.

      INTEGER(LONG), PARAMETER        :: IORD_STRESS_Q4 = 2! Gauss integration order for stress/strain recovery matrices
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      INTEGER(LONG), PARAMETER        :: NUM_NODES = 4     ! Quad has 4 nodes
                                                           ! Indicator of no output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = QMEM1_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Diffs in x coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Diffs in y coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(OUT)      :: BIG_BM(3,ELDOF,IORD*IORD)
                                                           ! Strain-displ matrix for this elem for all Gauss points (for all DOF's)

      REAL(DOUBLE)                    :: ALP(3)            ! Col of ALPVEC
      REAL(DOUBLE)                    :: BI(3,2*ELGP)      ! Strain-displ matrix for this element for one Gauss point
      REAL(DOUBLE)                    :: BIT(3,3*ELGP)     ! BI*BMEANT for one Gauss point

                                                           ! Strain-displ matrix for this element for all Gauss points
      REAL(DOUBLE)                    :: BM(3,2*ELGP,IORD*IORD)

      REAL(DOUBLE)                    :: DETJ(IORD*IORD)   ! Determinant of JAC for all Gauss points
      REAL(DOUBLE)                    :: DPSHG(2,4)        ! Output from subr SHP2DQ. Derivatives of PSH wrt elem isopar coords.
      REAL(DOUBLE)                    :: DPSHX(2,4)        ! Derivatives of PSH wrt elem x, y coords.
      REAL(DOUBLE)                    :: DUM1(3,8)         ! Intermediate matrix used in solving for KE stiffness matrix       
      REAL(DOUBLE)                    :: DUM2(8,8)         ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM3(8)           ! Intermediate matrix used in solving for PTE thermal loads
      REAL(DOUBLE)                    :: DUM4(8)           ! Intermediate matrix used in solving for PTE thermal loads
      REAL(DOUBLE)                    :: DUM5(8,8)         ! Intermediate matrix used in solving for KE stiffness matrix
      REAL(DOUBLE)                    :: DUM6(12,8)        ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM7(12,12)       ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM8(12)          ! Intermediate matrix used in solving for PTE thermal loads
      REAL(DOUBLE)                    :: DUM9(3,8)         ! Intermediate matrix used in solving for SEi stress recovery matrices
      REAL(DOUBLE)                    :: DUM10(2,8)        ! Intermediate matrix used in solving for PPE matrices
      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc PTE therm loads & STEi therm stress coeffs

                                                           ! An output from subr ORDER, called herein.  Gauss weights.
      REAL(DOUBLE)                    :: HHH(MAX_ORDER_GAUSS)

      REAL(DOUBLE)                    :: INTFAC            ! An integration factor (constant multiplier for the Gauss integration)

      REAL(DOUBLE)                    :: JAC(2,2)          ! An output from subr JAC2D, called herein. 2 x 2 Jacobian matrix.
      REAL(DOUBLE)                    :: JACI(2,2)         ! An output from subr JAC2D, called herein. 2 x 2 Jacobian inverse.
      REAL(DOUBLE)                    :: NBAR(2,8)         ! Matrix of shape functions (used in PPE calc)
      REAL(DOUBLE)                    :: PQ(8,NSUB)        ! Element pressure load matrix for the 8 local emenent DOF's.
      REAL(DOUBLE)                    :: PSH(4)            ! Output from subr SHP2DQ, called herein.
      REAL(DOUBLE)                    :: QLOAD(2,NSUB)     ! 2 components of the element pressure load (from PRESS) in 1 array
      REAL(DOUBLE)                    :: SSS(MAX_ORDER_GAUSS)
                                                           ! An output from subr ORDER, called herein. Gauss abscissa's.
      REAL(DOUBLE)                    :: SUMB              ! An intermediate variable used in calc B matrix for reduced integration
      REAL(DOUBLE)                    :: SUMD              ! An intermediate variable used in calc B matrix for reduced integration
      REAL(DOUBLE)                    :: TBAR              ! Average elem temperature 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, OPT
 9001    FORMAT(1X,A,' BEGN ',F10.3, ' OPT = ', 6A2)
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

      IORD_MSG = 'for in-plane direct strains,     input IORD = '
      GAUSS_PT = 0
      DO I=1,IORD
         DO J=1,IORD
            GAUSS_PT = GAUSS_PT + 1
            CALL SHP2DQ ( I, J, NUM_NODES, SUBR_NAME, IORD_MSG, IORD, SSS(I), SSS(J), 'Y', PSH, DPSHG )
            CALL JAC2D ( SSS(I), SSS(J), XSD, YSD, 'Y', JAC, JACI, DETJ(GAUSS_PT) )
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
         CALL ORDER_GAUSS ( IORD, SSS, HHH )
  
         DO K=1,8
            DUM3(K) = ZERO
         ENDDO 

         GAUSS_PT = 0
         IORD_MSG = 'for in-plane direct strains        = '
         DO I=1,IORD
            DO J=1,IORD
               GAUSS_PT = GAUSS_PT + 1
               DO L=1,3
                  DO M=1,2*ELGP
                     BI(L,M) = BM(L,M,GAUSS_PT)
                  ENDDO
               ENDDO
               CALL MATMULT_FFF_T ( BI, SHELL_AALP, 3, 8, 1, DUM4 )
               INTFAC = DETJ(GAUSS_PT)*HHH(I)*HHH(J)
               DO K=1,8
                  DUM3(K) = DUM3(K) + DUM4(K)*INTFAC
               ENDDO 
            ENDDO   
         ENDDO
  
         IF ((DABS(HBAR) > MXWARP) .AND. (DEBUG(4) ==  0)) THEN
            CALL MATMULT_FFF_T ( BMEANT, DUM3, 8, 12, 1, DUM8 )
            DO J=1,NTSUB
               TBAR = (DT(1,J) + DT(2,J) + DT(3,J) + DT(4,J))/FOUR
               DO K=1,12
                  PTE(ID2(K),J) = DUM8(K)*(TBAR - TREF(1))
               ENDDO 
            ENDDO 
         ELSE
            DO J=1,NTSUB
               TBAR = (DT(1,J) + DT(2,J) + DT(3,J) + DT(4,J))/FOUR
               DO K=1,8
                  PTE(ID1(K),J) = DUM3(K)*(TBAR - TREF(1))
               ENDDO 
            ENDDO   
         ENDIF
      
      ENDIF
  
! **********************************************************************************************************************************
! Calculate BE1, SE1 matrix (3 x 24) for strain/stress data recovery. All calculated at center of element/ply
! Note: strain/stress recovery matrices only make sense for individual plies (or whole elem if only 1 "ply") so this section of
! code uses EM and ALPVEC rather than SHELL_AALP
 
      IF (OPT(3) == 'Y') THEN
         ALP(1) = ALPVEC(1,1)
         ALP(2) = ALPVEC(2,1)
         ALP(3) = ALPVEC(3,1)

         IORD_MSG = 'for in-plane direct strains                 = '
         GAUSS_PT = 1
         CALL SHP2DQ ( 1, 1, NUM_NODES, SUBR_NAME, IORD_MSG, 1, ZERO, ZERO, 'N', PSH, DPSHG )
         CALL JAC2D ( ZERO, ZERO, XSD, YSD, 'N', JAC, JACI, DETJ(GAUSS_PT) )
         CALL MATMULT_FFF ( JACI, DPSHG, 2, 2, 4, DPSHX )
         CALL BMQMEM ( DPSHX, 1, 1, 'all strains', 'N', BI )

         DO I=1,3                                          ! Strain-displ matrix
            DO J=1,8
               BE1(I,ID1(J),1) = BI(I,J)
            ENDDO 
         ENDDO   

! SE1, STE1 generated in elem coords. Then, in LINK9 the stresses, calc'd in elem coords, will be transformed to ply coords

         CALL MATMULT_FFF ( EM, BI, 3, 3, 8, DUM9 )        ! Generate SE1 in element coords (at this point EM is elem coords)
         DO I=1,3
            DO J=1,8
               SE1(I,ID1(J),1) = DUM9(I,J)
            ENDDO 
         ENDDO

         ALP(1) = ALPVEC(1,1)
         ALP(2) = ALPVEC(2,1)
         ALP(3) = ALPVEC(3,1)

         CALL MATMULT_FFF ( EM, ALP, 3, 3, 1, EALP )
         DO J=1,NTSUB
            TBAR = (DT(1,J) + DT(2,J) + DT(3,J) + DT(4,J))/FOUR
            DO I=1,3
               STE1(I,J,1) = EALP(I)*(TBAR - TREF(1))
            ENDDO
         ENDDO   
      
  

      ENDIF  
  
! Generate BE1, SE1 for the stress recovery Gauss points (order IORD_STRESS_Q4). Put them into arrays BE1(i,j,k) and SE1(i,j,k)
! at i indices 2 through IORD_STRESS_Q4*IORD_STRESS_Q4 since index 1 is for center point stress/strain matrices.
! First make sure we dimensioned BEi/SEi large enough

      IF (IORD_STRESS_Q4*IORD_STRESS_Q4+1 > MAX_STRESS_POINTS) THEN
         FATAL_ERR          = FATAL_ERR + 1
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         WRITE(ERR,1956) SUBR_NAME, TYPE, EID, MAX_STRESS_POINTS, IORD_STRESS_Q4+IORD_STRESS_Q4+1
         WRITE(F06,1956) SUBR_NAME, TYPE, EID, MAX_STRESS_POINTS, IORD_STRESS_Q4+IORD_STRESS_Q4+1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      CALL ORDER_GAUSS ( IORD_STRESS_Q4, SSS, HHH )

      GAUSS_PT = 0
      DO I=1,IORD_STRESS_Q4

         DO J=1,IORD_STRESS_Q4

            GAUSS_PT = GAUSS_PT + 1
            IORD_MSG = 'for in-plane direct strains, IORD_STRESS_Q4 = '
            CALL SHP2DQ ( I, J, NUM_NODES, SUBR_NAME, IORD_MSG, IORD_STRESS_Q4, SSS(I), SSS(J), 'N', PSH, DPSHG )
            CALL JAC2D ( SSS(I), SSS(J), XSD, YSD, 'N', JAC, JACI, DETJ(GAUSS_PT) )
            CALL MATMULT_FFF ( JACI, DPSHG, 2, 2, 4, DPSHX )
            CALL BMQMEM ( DPSHX, I, J, 'direct strains', 'N', BI )

            DO L=1,3
               DO M=1,2*ELGP
                  BE1(L,ID1(M),GAUSS_PT+1) = BI(L,M)
               ENDDO
            ENDDO

            CALL MATMULT_FFF ( EM, BI, 3, 3, 8, DUM9 )        ! Generate SE1 in element coords (at this point EM is elem coords)
            DO L=1,3
               DO M=1,2*ELGP
                  SE1(L,ID1(M),GAUSS_PT+1) = DUM9(L,M)
               ENDDO 
            ENDDO

         ENDDO

      ENDDO

! **********************************************************************************************************************************
! Calculate element stiffness matrix KE.
 
      IF(OPT(4) == 'Y') THEN
         DO I=1,8
            DO J=1,8
               DUM5(I,J) = ZERO
            ENDDO 
         ENDDO   
 
         CALL ORDER_GAUSS ( IORD, SSS, HHH )
  
         GAUSS_PT = 0
         IORD_MSG = 'for in-plane direct strains        = '
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
               CALL JAC2D ( SSS(I), SSS(J), XSD, YSD, 'N', JAC, JACI, DETJ(GAUSS_PT) )
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
  
         IF (DEBUG(18) > 0) THEN
            CALL QMEM1_RB_STRAIN_ENERGY
         ENDIF

! Set lower triangular portion of KE equal to upper portion
  
         DO I=2,24
            DO J=1,I-1
               KE(I,J) = KE(J,I)
            ENDDO 
         ENDDO 
  
      ENDIF
  
! **********************************************************************************************************************************
! If element is a composite and if it is a nonsym layup we need to calc BIG_BM for later use

      DO I=1,3
         DO J=1,ELDOF
            DO K=1,IORD*IORD
               BIG_BM(I,J,K) = ZERO
            ENDDO
         ENDDO
      ENDDO

      IF ((PCOMP_PROPS == 'Y') .AND. (PCOMP_LAM == 'NON')) THEN

         GAUSS_PT = 0
         IORD_MSG = 'for in-plane direct strains        = '
         DO I=1,IORD

            DO J=1,IORD

               GAUSS_PT = GAUSS_PT + 1
               DO L=1,3
                  DO M=1,2*ELGP
                     BI(L,M) = BM(L,M,GAUSS_PT)
                  ENDDO
               ENDDO

               IF ((DABS(HBAR) > MXWARP) .AND. (DEBUG(4) == 0)) THEN
                  CALL MATMULT_FFF ( BI, BMEANT, 3, 8, 12, BIT )
                  DO L=1,3
                     DO M=1,12
                        BIG_BM(L,ID2(M),GAUSS_PT) = BIT(L,M)
                     ENDDO   
                  ENDDO 
               ELSE
                  DO L=1,3
                     DO M=1,8
                        BIG_BM(L,ID1(M),GAUSS_PT) = BI(L,M)
                     ENDDO   
                  ENDDO 
               ENDIF

            ENDDO

         ENDDO  

      ENDIF

! **********************************************************************************************************************************
! Determine element pressure loads   
  
      IF (OPT(5) == 'Y') THEN
         IF (DEBUG(16) == 0) THEN                          ! Generate PPE as work equilavent loads

            DO I=1,2
               DO J=1,8
                  NBAR(I,J)  = ZERO
                  DUM10(I,J) = ZERO
               ENDDO
            ENDDO

            DO J=1,NSUB
               QLOAD(1,J) = PRESS(1,J)
               QLOAD(2,J) = PRESS(2,J)
            ENDDO

            CALL ORDER_GAUSS ( IORD, SSS, HHH )
  
            DO I=1,2
               DO J=1,8
                  DUM10(I,J) = ZERO
               ENDDO
            ENDDO   

            GAUSS_PT = 0
            IORD_MSG = 'for in-plane direct strains,     input IORD = '
            DO I=1,IORD
               DO J=1,IORD
                  GAUSS_PT = GAUSS_PT + 1
                  CALL SHP2DQ ( I, J, NUM_NODES, SUBR_NAME, IORD_MSG, IORD, SSS(I), SSS(J), 'N', PSH, DPSHG )
                  CALL JAC2D ( SSS(I), SSS(J), XSD, YSD, 'N', JAC,  JACI, DETJ(GAUSS_PT) )
                  INTFAC     =  DETJ(GAUSS_PT)*HHH(I)*HHH(J)
                  NBAR(1,1) = PSH(1)
                  NBAR(1,3) = PSH(2)
                  NBAR(1,5) = PSH(3)
                  NBAR(1,7) = PSH(4)
                  NBAR(2,2) = PSH(1)
                  NBAR(2,4) = PSH(2)
                  NBAR(2,6) = PSH(3)
                  NBAR(2,8) = PSH(4)
                  DO K=1,2
                     DO L=1,8
                        DUM10(K,L) = DUM10(K,L) + INTFAC*NBAR(K,L)
                     ENDDO
                  ENDDO 
               ENDDO 
            ENDDO   
  
            CALL MATMULT_FFF_T ( DUM10, QLOAD, 2, 8, NSUB, PQ )
            
            DO I=1,8
               DO J=1,NSUB
                  PPE(ID1(I),J) = PQ(I,J)
               ENDDO 
            ENDDO

         ELSE                                              ! Generate PPE as static equilavent loads

            DO J=1,NSUB
               PPE( 1,J) = AREA*PRESS(1,J)/FOUR
               PPE( 2,J) = AREA*PRESS(2,J)/FOUR
               PPE( 7,J) = AREA*PRESS(1,J)/FOUR
               PPE( 8,J) = AREA*PRESS(2,J)/FOUR
               PPE(13,J) = AREA*PRESS(1,J)/FOUR
               PPE(14,J) = AREA*PRESS(2,J)/FOUR
               PPE(19,J) = AREA*PRESS(1,J)/FOUR
               PPE(20,J) = AREA*PRESS(2,J)/FOUR
            ENDDO 
  
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
 1956 FORMAT(' *ERROR  1956: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DIMENSION OF STRESS/STRAIN RECOVERY MATRICES (SEi/BEi) IS NOT SUFFICIENT FOR ',A,I8                   &
                    ,/,14X,' THERE ARE ',I3,' SEi/BEi MATRICES DIMENSIONED BUT ATTEMPT TO CREATE ',I3,' OF THEM WAS MADE' )





! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE QMEM1_RB_STRAIN_ENERGY

      USE IOUNT1, ONLY                :  BUG
      USE CONSTANTS_1, ONLY           :  ONE
      USE MODEL_STUF, ONLY            :  XEL

      CHARACTER(14*BYTE)              :: NAME(12)             ! Used for output purposes

      INTEGER(LONG)                   :: II,IR,JJ,KK          ! Indices
      INTEGER(LONG)                   :: ROW,COL              ! Row/col where max term in RB_STRAIN_ENERGY exists

      REAL(DOUBLE)                    :: C(8,6)               ! BMEAN(t)*D
      REAL(DOUBLE)                    :: D(12,6)              ! Rigid body displ matrix
      REAL(DOUBLE)                    :: DUMZZ(8,6)           ! Intermediate matrix in the comp of RB_STRAIN_ENERGY
      REAL(DOUBLE)                    :: L12                  ! Length of element side 1-2
      REAL(DOUBLE)                    :: MAX_ABS              ! Max abs value from RB_STRAIN_ENERGY
      REAL(DOUBLE)                    :: RB_STRAIN_ENERGY(6,6)! 6x6 matrix of element R.B. strain energy due to R.B. displs
      REAL(DOUBLE)                    :: X3                   ! Distance from node 1 to node 3 in x direction
      REAL(DOUBLE)                    :: X4                   ! Distance from node 1 to node 4 in x direction
      REAL(DOUBLE)                    :: Y3                   ! Distance from node 1 to node 3 in y direction
      REAL(DOUBLE)                    :: Y4                   ! Distance from node 1 to node 4 in xy direction

! **********************************************************************************************************************************
      WRITE(BUG,1000) TYPE, EID

      WRITE(BUG,1001)
      DO I=1,8
         WRITE(BUG,2001) (DUM5(I,J),J=1,8)
      ENDDO
      WRITE(BUG,*)

      DO I=1,12
         DO J=1,6
            D(I,J) = ZERO
         ENDDO
      ENDDO

      L12 = XEL(2,1)
      X3  = XEL(3,1)
      X4  = XEL(4,1)
      Y3  = XEL(3,2)
      Y4  = XEL(4,2)

      D( 1,1) = ONE
      D( 2,2) = D(1,1)
      D( 3,3) = D(1,1)
      D( 4,1) = D(1,1)
      D( 5,2) = D(1,1)
      D( 6,3) = D(1,1)
      D( 7,1) = D(1,1)
      D( 8,2) = D(1,1)
      D( 9,3) = D(1,1)
      D(10,1) = D(1,1)
      D(11,2) = D(1,1)
      D(12,3) = D(1,1)

!     D( 1,5) =  DABS(HBAR)
      d( 1,5) = -hbar
      D( 2,4) = -D( 1,5)
      D( 4,5) = -D( 1,5)
      D( 5,4) =  D( 1,5)
      D( 7,5) =  D( 1,5)
      D( 8,4) = -D( 1,5)
      D(10,5) = -D( 1,5)
      D(11,4) =  D( 1,5)

      D( 5,6) =  L12
      D( 6,5) = -D( 5,6)

      D( 7,6) = -Y3
      D( 8,6) =  X3
      D( 9,4) = -D( 7,6)
      D( 9,5) = -D( 8,6)

      D(10,6) = -Y4
      D(11,6) =  X4
      D(12,4) = -D(10,6)
      D(12,5) = -D(11,6)

      CALL MATMULT_FFF   ( BMEANT, D    ,  8, 12, 6, C )
      CALL MATMULT_FFF   ( DUM5  , C    ,  8,  8, 6, DUMZZ )
      CALL MATMULT_FFF_T ( C     , DUMZZ,  8,  6, 6, RB_STRAIN_ENERGY )

      MAX_ABS = ZERO
      ROW = 1
      COL = 1
      DO II=1,6
         DO JJ=1,6
            IF(DABS(RB_STRAIN_ENERGY(II,JJ)) > MAX_ABS) THEN
               MAX_ABS = DABS(RB_STRAIN_ENERGY(II,JJ))
               ROW = II
               COL = JJ
            ENDIF
         ENDDO
      ENDDO

      NAME( 1) = 'grid 1 x displ'
      NAME( 2) = 'grid 1 y displ'
      NAME( 3) = 'grid 1 z displ'
      NAME( 4) = 'grid 2 x displ'
      NAME( 5) = 'grid 2 y displ'
      NAME( 6) = 'grid 2 z displ'
      NAME( 7) = 'grid 3 x displ'
      NAME( 8) = 'grid 3 y displ'
      NAME( 9) = 'grid 3 z displ'
      NAME(10) = 'grid 4 x displ'
      NAME(11) = 'grid 4 y displ'
      NAME(12) = 'grid 4 z displ'
      WRITE(BUG,1002)
      WRITE(BUG,1004)
      DO II=1,4
         DO KK=1,3
            IR = 3*(II - 1) + KK
            WRITE(BUG,2002) NAME(IR), (D(IR,JJ),JJ=1,6)
         ENDDO
         WRITE(BUG,*)
      ENDDO
      WRITE(BUG,*) 

      NAME( 1) = 'node 1 x displ'
      NAME( 2) = 'node 1 y displ'
      NAME( 3) = 'node 2 x displ'
      NAME( 4) = 'node 2 y displ'
      NAME( 5) = 'node 3 x displ'
      NAME( 6) = 'node 3 y displ'
      NAME( 7) = 'node 4 x displ'
      NAME( 8) = 'node 4 y displ'
      WRITE(BUG,1003)
      WRITE(BUG,1004)
      DO II=1,4
         DO KK=1,2
            IR = 2*(II - 1) + KK
            WRITE(BUG,2003) NAME(IR), (C(IR,JJ),JJ=1,6)
         ENDDO
         WRITE(BUG,*)
      ENDDO
      WRITE(BUG,*) 

      WRITE(BUG,1005) MAX_ABS, ROW, COL 
      DO II=1,6
         WRITE(BUG,2004) (RB_STRAIN_ENERGY(II,JJ),JJ=1,6)
      ENDDO
      WRITE(BUG,*) 

! **********************************************************************************************************************************
 1000 FORMAT(' Strain energy calculation for membrane portion of ',A,' element number ',I8,/,                                      &
             ' ==================================================================================',/)

 1001 FORMAT(' KE virgin element stiffness matrix for the 2 in-plane transl DOFs at 4 grids (prior to processing with BMEAN:',/,   &
             ' ------------------------------------------------------------------------------------------------------------')

 1002 FORMAT(' D = R.B. displs of element grids due to R.B. displs of element mean plane node 1:',/,                               &
             ' --------------------------------------------------------------------------------',/,                                &
             '   The rows are the 3 R.B. translation displacement components of the 4 grids of the element',/,                     &
             '   The cols are the 6 R.B. translation & rotation displacement components of the element at node 1 in the mean plane'&
             ,/)

 1003 FORMAT(' C = BMEANT*D, R.B. displs of the element mean plane nodes due to R.B. displs of element mean plane node 1:',/,      &
             ' ---------------------------------------------------------------------------------------------------------',/,       &
             '   The rows are the 2 R.B. in-plane translation displacement components of the 4 mean plane nodes of the element',/, &
             '   The cols are the 6 R.B. translation & rotation displacement components of the element at node 1 in the mean plane'&
             ,/)


 1004 FORMAT('                                  Cols are R.B. motion of element mean plane node 1',/,                              &
             '                      x displ        y displ        z displ         x rot          y rot          z rot')  


 1005 FORMAT(' Rigid body strain energy calculated from C(t)*KE*C (max absolute value = ',1ES9.2,' at row',I2,', col',I2,'):',/,   &
             ' --------------------------------------------------------------------------------------------------')

 2001 FORMAT(3X,8(1ES15.6))  

 2002 FORMAT(3X,A,6(1ES15.6)) 

 2003 FORMAT(3X,A,6(1ES15.6)) 

 2004 FORMAT(3X,6(1ES15.6)) 

! **********************************************************************************************************************************
  
      END SUBROUTINE QMEM1_RB_STRAIN_ENERGY      

      END SUBROUTINE QMEM1
