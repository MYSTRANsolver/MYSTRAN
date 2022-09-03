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
  
      SUBROUTINE TETRA ( OPT, INT_ELEM_ID, IORD, RED_INT_SHEAR, WRITE_WARN )
 
! Isoparametric tetrahedron solid element (4 or 10 nodes). Full gaussian integration is the only option (no reduced integration) 

! Subroutine calculates:

!  1) ME        = element mass matrix                  , if OPT(1) = 'Y'
!  2) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  3) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  4) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  5) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_TETRA, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  HALF, QUARTER, ZERO, FOUR
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  TETRA_BEGEND
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, BE2, DT, EID, ELGP, NUM_EMG_FATAL_ERRS, ES, KE, KED, ME, PTE, RHO,           &
                                         SE1, SE2, STE1, STRESS, TREF, TYPE
 
      USE TETRA_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TETRA'
      CHARACTER( 1*BYTE), INTENT(IN)  :: RED_INT_SHEAR           ! If 'Y', use Gaussian weighted avg of B matrices for shear terms
      CHARACTER( 1*BYTE), INTENT(IN)  :: OPT(6)                  ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN              ! If 'Y" write warning messages, otherwise do not
      INTEGER(LONG)                   :: GAUSS_PT                ! Gauss point number (used for DEBUG output in subr SHP3DP
      CHARACTER(46*BYTE)              :: IORD_MSG                ! Character name of an integ order (used for debug output)

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID             ! Internal element ID
      INTEGER(LONG), INTENT(IN)       :: IORD                    ! Gaussian integ order for element
      INTEGER(LONG)                   :: I,J,K,L,M,N             ! DO loop indices
      INTEGER(LONG)                   :: II,JJ                   ! Counters
      INTEGER(LONG)                   :: ID(3*ELGP)              ! Array which shows equivalence of DOF's in virgin element with the
     !                                                             6 DOF/grid of the final element stiffness matrix
                                                                 ! Indicator of no output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TETRA_BEGEND
  
      REAL(DOUBLE)                    :: ALP(6)                  ! First col of ALPVEC

      REAL(DOUBLE)                    :: B(6,3*ELGP,IORD*IORD*IORD)
                                                                 ! Strain-displ matrix for this element for all Gauss points

      REAL(DOUBLE)                    :: BI(6,3*ELGP)            ! Strain-displ matrix for this element for one Gauss point

      REAL(DOUBLE)                    :: CBAR(3,3*ELGP)          ! Derivatives of shape fcns wrt x,y,z used in diff stiff matrix
!                                                                  (contains terms from DPSHX matrices for each grid of the HEXA)

      REAL(DOUBLE)                    :: DETJ(IORD*IORD*IORD)    ! Determinant of JAC for all Gauss points

      REAL(DOUBLE)                    :: DPSHG(3,ELGP)           ! Output from subr SHP3DT. Derivatives of PSH wrt elem isopar coord
      REAL(DOUBLE)                    :: DPSHX(3,ELGP)           ! Derivatives of PSH wrt elem x, y coords.
      REAL(DOUBLE)                    :: DUM0(3*ELGP)            ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM1(3*ELGP)            ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM2(6,3*ELGP)          ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM3(3*ELGP,3*ELGP)     ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM4(6,3*ELGP)          ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: DUM5(3*ELGP,3*ELGP)     ! Intermediate matrix used in solving for KE elem matrices
      REAL(DOUBLE)                    :: DUM6(3,3*ELGP)          ! Intermediate matrix used in solving for elem matrices
      REAL(DOUBLE)                    :: EALP(6)                 ! Intermed var used in calc PTE therm lds & STEi therm stress coeff
      REAL(DOUBLE)                    :: EPS1                    ! A small number to compare to real zero
                                                                 ! Array of all DT values at the grids GRID_DT_ARRAY(i,j) = DT(i,j)
      REAL(DOUBLE)                    :: GRID_DT_ARRAY(ELGP,NTSUB)

      REAL(DOUBLE)                    :: HHH_IJK(MAX_ORDER_TETRA)! An output from subr ORDER_TRIA, called herein.  Gauss weights.
      REAL(DOUBLE)                    :: INTFAC                  ! An integ factor (constant multiplier for the Gauss integ)
      REAL(DOUBLE)                    :: JAC(3,3)                ! An output from subr JAC3D, called herein. 3 x 3 Jacobian matrix.
      REAL(DOUBLE)                    :: JACI(3,3)               ! An output from subr JAC3D, called herein. 3 x 3 Jacobian inverse.
      REAL(DOUBLE)                    :: KWW(3,3)                ! Portion of differential stiffness matrix
      REAL(DOUBLE)                    :: M0                      ! An intermediate variable used in calc elem mass, ME
      REAL(DOUBLE)                    :: PSH(ELGP)               ! Output from subr SHP3DT. Shape fcn at Gauss pts SSI, SSJ
      REAL(DOUBLE)                    :: SIGxx                   ! Normal stress in the elem x  direction
      REAL(DOUBLE)                    :: SIGyy                   ! Normal stress in the elem y  direction
      REAL(DOUBLE)                    :: SIGzz                   ! Normal stress in the elem z  direction
      REAL(DOUBLE)                    :: SIGxy                   ! Shear  stress in the elem xy direction
      REAL(DOUBLE)                    :: SIGyz                   ! Shear  stress in the elem yz direction
      REAL(DOUBLE)                    :: SIGzx                   ! Shear  stress in the elem zx direction
      REAL(DOUBLE)                    :: SSS_I(MAX_ORDER_TETRA)  ! An output from subr ORDER_TRIA, called herein. Gauss abscissa's.
      REAL(DOUBLE)                    :: SSS_J(MAX_ORDER_TETRA)  ! An output from subr ORDER_TRIA, called herein. Gauss abscissa's.
      REAL(DOUBLE)                    :: SSS_K(MAX_ORDER_TETRA)  ! An output from subr ORDER_TRIA, called herein. Gauss abscissa's.
      REAL(DOUBLE)                    :: TBAR(NTSUB)             ! Average elem temperature for each subcase
      REAL(DOUBLE)                    :: TEMP                    ! Temperature to use in PTE calc
      REAL(DOUBLE)                    :: TGAUSS(1,NTSUB)         ! Temp at a Gauss point for a theral subcase
      REAL(DOUBLE)                    :: TREF1                   ! TREF(1)
      REAL(DOUBLE)                    :: VOLUME                  ! 3D element volume
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Calculate volume by Gaussian integration

      CALL ORDER_TETRA ( IORD, SSS_I, SSS_J, SSS_K, HHH_IJK )
      IORD_MSG = ' '
      IORD_MSG = 'for 3-D solid strains,           input IORD = '
      VOLUME = ZERO
      DO I=1,IORD
         CALL SHP3DT ( I, ELGP, SUBR_NAME, IORD_MSG, IORD, SSS_I(I), SSS_J(I), SSS_K(I), 'Y', PSH, DPSHG )
         CALL JAC3D ( SSS_I(I), SSS_J(I), SSS_K(I), DPSHG, 'Y', JAC, JACI, DETJ(I) )
         VOLUME = VOLUME + HHH_IJK(I)*DETJ(I)
      ENDDO

! If VOLUME <= 0, write error and return
          
      IF (VOLUME < EPS1) THEN
         WRITE(ERR,1925) EID, TYPE, 'VOLUME', VOLUME
         WRITE(F06,1925) EID, TYPE, 'VOLUME', VOLUME
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF

! Calculate ID array

      JJ = 0
      DO I=1,ELGP
         II = 6*(I - 1)
         DO J=1,3
            II = II + 1
            JJ = JJ + 1
            ID(JJ) = II
         ENDDO
      ENDDO

! ALP is first col of ALPVEC

      ALP(1) = ALPVEC(1,1)
      ALP(2) = ALPVEC(2,1)
      ALP(3) = ALPVEC(3,1)
      ALP(4) = ZERO
      ALP(5) = ZERO
      ALP(6) = ZERO
    
      TREF1 = TREF(1)
 
! EALP is needed to calculate both PTE and STE2
 
      CALL MATMULT_FFF ( ES, ALP, 6, 6, 1, EALP )
  
! Calc TBAR (used for PTE, STEi)

      IF ((OPT(2) == 'Y') .OR. (OPT(3) == 'Y')) THEN
         DO J=1,NTSUB
            TBAR(J) = ZERO
            DO K=1,ELGP
               TBAR(J) = TBAR(J) + DT(K,J)
            ENDDO
            TBAR(J) = TBAR(J)/ELGP - TREF1
         ENDDO
      ENDIF   

! **********************************************************************************************************************************
! Generate the mass matrix for this element.
 
      IF (OPT(1) == 'Y') THEN

         M0 = (RHO(1))*VOLUME/ELGP

         DO I=1,ELGP
            DO J=1,3
               K = 6*(I-1) + J
               ME(K,K) = M0
            ENDDO
         ENDDO

      ENDIF

! **********************************************************************************************************************************
! Generate B matrices. The B matrix terms for Gauss point k are B(i,j,k) 

      DO I=1,6
         DO J=1,3*ELGP
            DO K=1,IORD*IORD*IORD
               B(I,J,K) = ZERO
            ENDDO
         ENDDO
      ENDDO

      IORD_MSG = 'for 3-D solid strains,           input IORD = '
      DO I=1,IORD
         CALL SHP3DT ( I, ELGP, SUBR_NAME, IORD_MSG, IORD, SSS_I(I), SSS_J(I), SSS_K(I), 'N', PSH, DPSHG )
         CALL JAC3D ( SSS_I(I), SSS_J(I), SSS_K(I), DPSHG, 'N', JAC, JACI, DETJ(I) )
         CALL MATMULT_FFF ( JACI, DPSHG, 3, 3, ELGP, DPSHX )
         CALL B3D_ISOPARAMETRIC ( DPSHX, I, I, I, I, 'direct strains', 'Y', BI )
         DO L=1,6
            DO M=1,3*ELGP
               B(L,M,I) = BI(L,M)
            ENDDO
         ENDDO
      ENDDO

! **********************************************************************************************************************************
! Calculate element thermal loads. 
  
      IF (OPT(2) == 'Y') THEN

         DO N=1,NTSUB
            DO L=1,ELGP
               GRID_DT_ARRAY(L,N) = DT(L,N)
            ENDDO
         ENDDO

         DO N=1,NTSUB

            DO L=1,3*ELGP
               DUM0(L) = ZERO
               DUM1(L) = ZERO
            ENDDO

            IORD_MSG = 'for 3-D solid strains,           input IORD = '
            GAUSS_PT = 0
            DO I=1,IORD
               GAUSS_PT = GAUSS_PT + 1
               DO L=1,6
                  DO M=1,3*ELGP
                     BI(L,M) = B(L,M,GAUSS_PT)
                  ENDDO
               ENDDO
               CALL MATMULT_FFF_T ( BI, EALP, 6, 3*ELGP, 1, DUM0 )
               INTFAC = DETJ(GAUSS_PT)*HHH_IJK(I)
               IF (DEBUG(192) == 0) THEN                ! Use temperatures at Gauss points for PTE
                  CALL SHP3DT ( I, ELGP, SUBR_NAME, IORD_MSG, IORD, SSS_I(I), SSS_J(I), SSS_K(I), 'N', PSH, DPSHG )
                  CALL MATMULT_FFF ( PSH, GRID_DT_ARRAY, 1, ELGP, NTSUB, TGAUSS )
                  TEMP = TGAUSS(1,N) - TREF1
               ELSE                                     ! Use avg element temperature for PTE
                  TEMP = TBAR(N)
               ENDIF 
               DO L=1,3*ELGP
                  DUM1(L) = DUM1(L) + DUM0(L)*TEMP*INTFAC
               ENDDO
            ENDDO 
  
            DO L=1,3*ELGP
               PTE(ID(L),N) = DUM1(L)
            ENDDO

         ENDDO

      ENDIF

! **********************************************************************************************************************************
! Calculate BEi, SEi matrices for strain, stress data recovery.
 
      IF ((OPT(3) == 'Y') .OR. (OPT(6) == 'Y')) THEN
 
         DO K=1,6
            DO L=1,3*ELGP
               DUM2(K,L) = ZERO
            ENDDO
         ENDDO

         GAUSS_PT = 1                                      ! Calc SE1,2,3
         IORD_MSG = 'for 3-D solid strains,                      = '
         CALL SHP3DT ( 1, ELGP, SUBR_NAME, IORD_MSG, 1, QUARTER, QUARTER, QUARTER, 'N', PSH, DPSHG )
         CALL JAC3D ( QUARTER, QUARTER, QUARTER, DPSHG, 'N', JAC, JACI, DETJ(GAUSS_PT) )
         CALL MATMULT_FFF ( JACI, DPSHG, 3, 3, ELGP, DPSHX )
         CALL B3D_ISOPARAMETRIC ( DPSHX, GAUSS_PT, 1, 1, 1, 'all strains', 'N', BI )
         CALL MATMULT_FFF ( ES, BI, 6, 6, 3*ELGP, DUM2 )

         DO I=1,3
            DO J=1,3*ELGP
               SE1(I,ID(J),1) = DUM2(I  ,J)
               SE2(I,ID(J),1) = DUM2(I+3,J)
            ENDDO 
         ENDDO   

         DO J=1,NTSUB                                      ! STE thermal stress terms
            STE1(1,J,1) = EALP(1)*TBAR(J)
            STE1(2,J,1) = EALP(2)*TBAR(J)
            STE1(3,J,1) = EALP(3)*TBAR(J)
         ENDDO 
      
         DO I=1,3                                          ! Strain-displ matrix
            DO J=1,3*ELGP
               BE1(I,ID(J),1) = BI(I  ,J)
               BE2(I,ID(J),1) = BI(I+3,J)
            ENDDO 
         ENDDO   

      ENDIF  
  
! **********************************************************************************************************************************
! Calculate element stiffness matrix KE.
 
      IF (OPT(4) == 'Y') THEN
  
         DO I=1,3*ELGP
            DO J=1,3*ELGP
               DUM3(I,J) = ZERO
            ENDDO 
         ENDDO   
 
         IORD_MSG = ' '
         DO I=1,IORD

            DO L=1,6
               DO M=1,3*ELGP
                  BI(L,M) = B(L,M,I)
               ENDDO
            ENDDO

            CALL MATMULT_FFF ( ES, BI, 6, 6, 3*ELGP, DUM4 )

            CALL MATMULT_FFF_T ( BI, DUM4, 6, 3*ELGP, 3*ELGP, DUM5 )

            INTFAC = DETJ(I)*HHH_IJK(I)

            DO L=1,3*ELGP
               DO M=1,3*ELGP
                  DUM3(L,M) = DUM3(L,M) + DUM5(L,M)*INTFAC
               ENDDO 
            ENDDO

         ENDDO   
  
         DO I=1,3*ELGP
            DO J=1,3*ELGP
               KE(ID(I),ID(J)) = DUM3(I,J)
            ENDDO   
         ENDDO 

! Set lower triangular portion of KE equal to upper portion
  
         DO I=2,6*ELGP
            DO J=1,I-1
               KE(I,J) = KE(J,I)
            ENDDO 
         ENDDO 
  
      ENDIF
    
! **********************************************************************************************************************************
! Calculate linear differential stiffness matrix
      IF ((OPT(6) == 'Y') .AND. (LOAD_ISTEP > 1)) THEN

         CALL ELMDIS
         CALL ELEM_STRE_STRN_ARRAYS ( 1 )

         SIGxx = STRESS(1)
         SIGyy = STRESS(2)
         SIGzz = STRESS(3)
         SIGxy = STRESS(4)
         SIGyz = STRESS(5)
         SIGzx = STRESS(6)

         KWW(1,1) = SIGyy + SIGzz   ;   KWW(1,2) = -SIGxy   ;   KWW(1,3) = -SIGzx
         KWW(2,2) = SIGxx + SIGzz   ;   KWW(2,3) = -SIGyz
         KWW(3,3) = SIGxx + SIGyy
         KWW(2,1) = KWW(1,2)
         KWW(3,1) = KWW(1,3)
         KWW(3,2) = KWW(2,3)
         DO I=1,IORD
            CALL SHP3DT ( I, ELGP, SUBR_NAME, IORD_MSG, IORD, SSS_I(I), SSS_J(I), SSS_K(I), 'N', PSH, DPSHG )
            CALL JAC3D ( SSS_I(I), SSS_J(I), SSS_K(I), DPSHG, 'N', JAC, JACI, DETJ(I) )
            CALL MATMULT_FFF ( JACI, DPSHG, 3, 3, ELGP, DPSHX )
         ENDDO
         DO I=1,ELGP
            CBAR(1,3*(I-1)+1) =  ZERO             ;  CBAR(1,3*(I-1)+2) = -HALF*DPSHX(3,I)  ;  CBAR(1,3*(I-1)+3)=  HALF*DPSHX(2,I)         
            CBAR(2,3*(I-1)+1) =  HALF*DPSHX(3,I)  ;  CBAR(2,3*(I-1)+2) =  ZERO             ;  CBAR(2,3*(I-1)+3)= -HALF*DPSHX(1,I)         
            CBAR(3,3*(I-1)+1) = -HALF*DPSHX(2,I)  ;  CBAR(3,3*(I-1)+2) =  HALF*DPSHX(1,I)  ;  CBAR(3,3*(I-1)+3)=  ZERO
         ENDDO

         DO I=1,3*ELGP
            DO J=1,3*ELGP
               DUM3(I,J) = ZERO
            ENDDO 
         ENDDO   
 
         IORD_MSG = ' '
         GAUSS_PT = 0
         DO I=1,IORD
            GAUSS_PT = GAUSS_PT + 1
            CALL MATMULT_FFF ( KWW, CBAR, 3, 3, 3*ELGP, DUM6 )
            CALL MATMULT_FFF_T ( CBAR, DUM6, 3, 3*ELGP, 3*ELGP, DUM5 )
            INTFAC = DETJ(I)*HHH_IJK(I)
            DO L=1,3*ELGP
               DO M=1,3*ELGP
                  DUM3(L,M) = DUM3(L,M) + DUM5(L,M)*INTFAC
               ENDDO 
            ENDDO
         ENDDO   
  
   
         DO I=1,3*ELGP
            DO J=1,3*ELGP
               KED(ID(I),ID(J)) = DUM3(I,J)
            ENDDO   
         ENDDO 
  
         DO I=2,6*ELGP                                     ! Set lower triangular portion of KE equal to upper portion
            DO J=1,I-1
               KED(I,J) = KED(J,I)
            ENDDO 
         ENDDO 
  
         IF (DEBUG(178) >= 1) THEN
            WRITE(F06,2001) TRIM(SUBR_NAME), OPT(6), LOAD_ISTEP, TYPE, EID
            K = 0
            DO I=1,ELGP
               K = 6*(I-1) + 1
               WRITE(F06,2101) (KED(K  ,J),J=1,3*ELGP)
               WRITE(F06,2101) (KED(K+1,J),J=1,3*ELGP)
               WRITE(F06,2101) (KED(K+2,J),J=1,3*ELGP)
               WRITE(F06,*)
               WRITE(F06,2101) (KED(K+3,J),J=1,3*ELGP)
               WRITE(F06,2101) (KED(K+4,J),J=1,3*ELGP)
               WRITE(F06,2101) (KED(K+5,J),J=1,3*ELGP)
               WRITE(F06,*)
            ENDDO
            WRITE(F06,*)
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
 1925 FORMAT(' *ERROR  1925: ELEMENT ',I8,', TYPE ',A,', HAS ZERO OR NEGATIVE ',A,' = ',1ES9.1)

 2001 FORMAT(' In ', A, ' with OPT(6) = ', A, ', and LOAD_ISTEP = ', I8, ': KED Differential Stiffness Matrix for ', A,            &
             ' element number ', I8)

 2101 FORMAT(1000(1ES14.6))


! **********************************************************************************************************************************

      END SUBROUTINE TETRA
