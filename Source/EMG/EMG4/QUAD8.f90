! #################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
      SUBROUTINE QUAD8 ( OPT, INT_ELEM_ID )
 
! Calculates, or calls subr's to calculate, quadrilateral element matrices:

!  1) ME        = element mass matrix                  , if OPT(1) = 'Y'
!  2) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  3) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  4) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  5) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  6) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  NUM_EMG_FATAL_ERRS, PCOMP_PROPS, ELGP, ES, KE
      USE CONSTANTS_1, ONLY           :  ZERO

      USE ORDER_GAUSS_Interface
      USE OUTA_HERE_Interface
      USE MATMULT_FFF_Interface
      USE MATMULT_FFF_T_Interface

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'QUAD8'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID
      INTEGER(LONG), PARAMETER        :: IORD_IJ = 3       ! Integration order for stiffness matrix
      INTEGER(LONG), PARAMETER        :: IORD_K = 2        ! Integration order for stiffness matrix in thickness direction
      INTEGER(LONG)                   :: GAUSS_PT          ! Gauss point number
      INTEGER(LONG)                   :: I,J,K,L,M         ! DO loop indices
  
                                                           ! Strain-displ matrix for this element for all Gauss points      
      REAL(DOUBLE)                    :: B(6,6*ELGP,IORD_IJ*IORD_IJ*IORD_K)  
      
      REAL(DOUBLE)                    :: HH_IJ(MAX_ORDER_GAUSS) ! Gauss weights for integration in in-layer directions
      REAL(DOUBLE)                    :: SS_IJ(MAX_ORDER_GAUSS) ! Gauss abscissa's for integration in in-layer directions
      REAL(DOUBLE)                    :: HH_K(MAX_ORDER_GAUSS)  ! Gauss weights for integration in thickness direction
      REAL(DOUBLE)                    :: SS_K(MAX_ORDER_GAUSS)  ! Gauss abscissa's for integration in thickness direction
      REAL(DOUBLE)                    :: BI(6,6*ELGP)      ! Strain-displ matrix for this element for one Gauss point
      REAL(DOUBLE)                    :: DUM1(6,6*ELGP)    ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM2(6*ELGP,6*ELGP)    ! Intermediate matrix
      REAL(DOUBLE)                    :: INTFAC            ! An integration factor (constant multiplier for the Gauss integration)
      REAL(DOUBLE)                    :: DETJ              ! Jacobian determinant

 

! **********************************************************************************************************************************
      
      
      IF (PCOMP_PROPS == 'Y') THEN
        WRITE(ERR,*) ' *ERROR: Code not written for composite material with QUAD8'
        WRITE(F06,*) ' *ERROR: Code not written for composite material with QUAD8'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
      ENDIF

        
      
! **********************************************************************************************************************************
! Generate the mass matrix for this element.
 
      IF (OPT(1) == 'Y') THEN

        !Not implememented yet but we can't make it a fatal error because this gets called even when it doesn't need it.

      ENDIF

 

! **********************************************************************************************************************************
! Calculate element thermal loads. 
  
      IF (OPT(2) == 'Y') THEN
      
        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 thermal loads'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 thermal loads'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
      
      ENDIF
  
! **********************************************************************************************************************************
! Calculate BE1 matrix (6 x 48) for strain/stress data recovery. All calculated at center of element/ply
 
      IF (OPT(3) == 'Y') THEN

        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 stress recovery matrices'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 stress recovery matrices'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF  
  
  
! Generate BE1 for the stress recovery Gauss points (order IORD_STRESS_Q8). Put them into array BE1(i,j,k)
! at i indices 2 through IORD_STRESS_Q8*IORD_STRESS_Q8 + 1 since index 1 is for center point stress/strain matrices.
! First make sure we dimensioned BEi large enough

      IF ((OPT(3) == 'Y') .OR. (OPT(6) == 'Y')) THEN

        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 differential stiffness matrix'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 differential stiffness matrix'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF

! **********************************************************************************************************************************
! Calculate element stiffness matrix KE.
 
      IF(OPT(4) == 'Y') THEN

        ! K = int( [B]^T [ES] [B] dV )
        !    dV = |det(J)|dr ds dt
        ! K = int( [B]^T [ES] [B] |det(J)| dr ds dt )
        !
        ! [ES] is material elasticity matrix in cartesian local coordinates
        ! stress = [ES] * strain
        ! strain = [B] * displacement
        ! K is in the basic coordinate system

!victor todo see if I can make local cartisian coordiantes be the same as Nastran element local coordinates and material coordiantes are that rotated about the normal by THETA.
!victor todo generate ES in EMG similar to for CHEXA but without the transform. But that's only for isotropic. For 2D materials we need to generate 3D elasticitiy matrices.
!victor todo generate B above. This is the meat of MITC8.
!             Maybe call a function in here to generate it on the fly.
!             Only do it above if I'm reusing it at the same gauss points. 
!             Or just don't do it above at all.
!             Mecway caches B at each gauss point but I'm not sure where it reuses them.
!victor todo implement QUAD8_DETJ function. Use XEB which is grid point coordinates in basic coordinate system.

        DO I=1,6*ELGP
          DO J=1,6*ELGP
            KE(I,J) = ZERO
          ENDDO 
        ENDDO   

        CALL ORDER_GAUSS ( IORD_IJ, SS_IJ, HH_IJ )
        CALL ORDER_GAUSS ( IORD_K, SS_K, HH_K )

        GAUSS_PT = 0
        DO I=1,IORD_IJ
          DO J=1,IORD_IJ
            DO K=1,IORD_K
              GAUSS_PT = GAUSS_PT + 1
              
              DO L=1,6
                DO M=1,6*ELGP
                  BI(L,M) = B(L,M,GAUSS_PT)
                ENDDO
              ENDDO
              CALL MATMULT_FFF ( ES, BI, 6, 6, 6*ELGP, DUM1 )
              CALL MATMULT_FFF_T ( BI, DUM1, 6, 6*ELGP, 6*ELGP, DUM2 )
              DETJ = 0.0 !victor todo QUAD8_DETJ ( SS_IJ(I), SS_IJ(J), SS_K(K) )
              INTFAC = DETJ*HH_IJ(I)*HH_IJ(J)*HH_K(K)
              DO L=1,6*ELGP
                DO M=1,6*ELGP
                  KE(L,M) = KE(L,M) + DUM2(L,M)*INTFAC
                ENDDO 
              ENDDO   
            ENDDO
          ENDDO 
        ENDDO   
   
  
      ENDIF
  

! **********************************************************************************************************************************
! Determine element pressure loads   
  
      IF (OPT(5) == 'Y') THEN

        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 pressure loads'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 pressure loads'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )
          
      ENDIF
  
! **********************************************************************************************************************************
! Calculate linear differential stiffness matrix
  
      IF ((OPT(6) == 'Y') .AND. (LOAD_ISTEP > 1)) THEN

        WRITE(ERR,*) ' *ERROR: Code not written for QUAD8 differential stiffness matrix'
        WRITE(F06,*) ' *ERROR: Code not written for QUAD8 differential stiffness matrix'
        NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF


 
 


      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************
  
      END SUBROUTINE QUAD8
