! ##################################################################################################################################
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
 	
      SUBROUTINE TMEM1 ( OPT, AREA, X2E, X3E, Y3E, WRT_BUG_THIS_TIME, BIG_BM )
  
! Constant strain membrane triangle

! Subroutine calculates:

!  1) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  2) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  3) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  4) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  5) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, BUG, F04, WRT_BUG, WRT_LOG, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BCHK_BIT, ELDT_BUG_BMAT_BIT, NSUB, NTSUB, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TMEM1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, THREE
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, EID, DT, EM, ELDOF, KE, PCOMP_LAM, PCOMP_PROPS, PRESS, PPE, PTE, SE1, STE1,  &
                                         SHELL_AALP, SHELL_A, SHELL_PROP_ALP, TREF, TYPE, XEB, XEL, ELGP, FCONV, STRESS, KED,      &
                                         NUM_EMG_FATAL_ERRS
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE TMEM1_USE_IFs

      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TMEM1'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ID(18)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TMEM1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: X2E               ! x coord of elem node 2
      REAL(DOUBLE) , INTENT(IN)       :: X3E               ! x coord of elem node 3
      REAL(DOUBLE) , INTENT(IN)       :: Y3E               ! y coord of elem node 3

      REAL(DOUBLE) , INTENT(OUT)      :: BIG_BM(3,ELDOF,1) ! Strain-displ matrix for this elem for all Gauss points (for all DOF's)

      REAL(DOUBLE)                    :: BW(3,14)          ! Output from subr BCHECK (matrix of 3 elem strains for 14 various elem
!                                                            rigid body motions/constant strain distortions)

      REAL(DOUBLE)                    :: ALP(3)            ! Col of ALPVEC
      REAL(DOUBLE)                    :: KS(ELGP,ELGP)     ! KED matrix for one DOF
      REAL(DOUBLE)                    :: BM(3,ELDOF)       ! Strain-displ matrix for this elem
      REAL(DOUBLE)                    :: AMB(3,ELDOF)      ! SHELL_A matrix times strain-displ matrix for this elem
      REAL(DOUBLE)                    :: DPSHX(2,ELGP)     ! Derivatives of PSH wrt elem x, y coords.
      REAL(DOUBLE)                    :: DUM(ELDOF,ELDOF)  ! Needed for calc 18 x 18 KE  using MATMULT, since KE is MELDOF x MELDOF 
      REAL(DOUBLE)                    :: DUM1(ELDOF,1)     ! Intermediate matrix used in determining PTE thermal loads
      REAL(DOUBLE)                    :: DUM11(2,2)        ! Intermediate matrix used in solving for KED matrices
      REAL(DOUBLE)                    :: DUM12(ELGP,2)     ! Intermediate matrix used in solving for KED matrices
      REAL(DOUBLE)                    :: DUM13(ELGP,ELGP)  ! Intermediate matrix used in solving for KED matrices
      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc STEi therm stress coeffs
      REAL(DOUBLE)                    :: EMB(3,ELDOF)      ! Mat'l matrix times strain-displ matrix for this elem
      REAL(DOUBLE)                    :: C01               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: C02               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: C03               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: C04               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: CT0               ! Intermediate variable used in calc PTE thermal loads
      REAL(DOUBLE)                    :: TBAR              ! Average elem temperature 
      REAL(DOUBLE)                    :: FORCEx            ! Engineering force in the elem x direction
      REAL(DOUBLE)                    :: FORCEy            ! Engineering force in the elem x direction
      REAL(DOUBLE)                    :: FORCExy           ! Engineering force in the elem xy direction

 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Determine element strain-displacement matrix.

      DO I=1,3
         DO J=1,ELDOF
            BM(I,J) = ZERO
         ENDDO 
      ENDDO 

      C01 = ONE/X2E
      C02 = ONE/Y3E
      C03 = (X3E - X2E)*C01*C02
      C04 = X3E*C01*C02

      BM(1, 1) = -C01
      BM(1, 7) =  C01

      BM(2, 2) =  C03
      BM(2, 8) = -C04
      BM(2,14) =  C02

      BM(3, 1) =  C03
      BM(3, 2) = -C01
      BM(3, 7) = -C04
      BM(3, 8) =  C01
      BM(3,13) =  C02

      DO I=1,18
         ID(I) = I
      ENDDO
 
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(8) > 0)) THEN

         WRITE(BUG,1101) ELDT_BUG_BMAT_BIT, TYPE, EID
         WRITE(BUG,8901) SUBR_NAME
         DO I=1,3
            WRITE(BUG,8902) I,(BM(I,J),J=1,ELDOF)
            WRITE(BUG,*)
         ENDDO 
         WRITE(BUG,*)

      ENDIF

      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(9) > 0)) THEN
        IF (DEBUG(202) > 0) THEN
           WRITE(BUG,1101) ELDT_BUG_BCHK_BIT, TYPE, EID
           WRITE(BUG,9100)
           WRITE(BUG,9101)
           WRITE(BUG,9102)
           WRITE(BUG,9103)
           WRITE(BUG,9104)
           CALL BCHECK_2D ( BM, 'M', ID, 3, 18, 3, XEL, XEB, BW )
        ENDIF
      ENDIF

! **********************************************************************************************************************************
! If element is a composite and if it is a nonsym layup we need to calc BIG_BB for later use

      DO I=1,3
         DO J=1,ELDOF
            BIG_BM(I,J,1) = ZERO
         ENDDO
      ENDDO
  
      IF ((PCOMP_PROPS == 'Y') .AND. (PCOMP_LAM == 'NON')) THEN

         DO I=1,3
            DO J=1,18
               BIG_BM(I,J,1) = BM(I,J)
            ENDDO
         ENDDO

      ENDIF

! **********************************************************************************************************************************
! Determine element thermal loads. 

      IF (OPT(2) == 'Y') THEN

         CALL MATMULT_FFF_T ( BM, SHELL_AALP, 3, ELDOF, 1, DUM1 )

         DO J=1,NTSUB
            TBAR = (DT(1,J) + DT(2,J) + DT(3,J))/THREE
            CT0 = AREA*(TBAR - TREF(1))
            DO I=1,ELDOF
               PTE(I,J) = CT0*DUM1(I,1)
            ENDDO
         ENDDO

      ENDIF
  
! **********************************************************************************************************************************
! Calculate BE1, SE1 matrices (3 x ELDOF) for strain/stress data recovery.
! Note: strain/stress recovery matrices only make sense for individual plies (or whole elem if only 1 "ply")
 
      IF (OPT(3) == 'Y' .OR. OPT(6) == "Y") THEN

         DO I=1,3
            DO J=1,ELDOF
               BE1(I,J,1) = BM(I,J)
            ENDDO
         ENDDO

! SE1, STE1 generated in elem coords. Then, in LINK9 the stresses, calc'd in elem coords, will be transformed to ply coords

         CALL MATMULT_FFF ( EM, BM, 3, 3, ELDOF, EMB )     ! Generate SE1 in element coords (at this point EM is elem coords)
         DO I=1,3
            DO J=1,ELDOF
               SE1(I,J,1) = EMB(I,J)
            ENDDO
         ENDDO

         ALP(1) = ALPVEC(1,1)
         ALP(2) = ALPVEC(2,1)
         ALP(3) = ALPVEC(3,1)

         CALL MATMULT_FFF ( EM, ALP, 3, 3, 1, EALP )
         DO J=1,NTSUB
            TBAR = (DT(1,J) + DT(2,J) + DT(3,J))/THREE
            DO I=1,3
               STE1(I,J,1) = EALP(I)*(TBAR - TREF(1))
            ENDDO
         ENDDO

      ENDIF
  
! **********************************************************************************************************************************
! Calculate element stiffness matrix KE.
 
      IF (OPT(4) == 'Y') THEN

         CALL MATMULT_FFF ( SHELL_A, BM, 3, 3, ELDOF, AMB )
         CALL MATMULT_FFF_T ( BM, AMB, 3, ELDOF, ELDOF, DUM )
         DO I=1,ELDOF
            DO J=1,ELDOF
               KE(I,J) = KE(I,J) + AREA*DUM(I,J)
            ENDDO
         ENDDO
 
      ENDIF
  
! **********************************************************************************************************************************
! Calculate element pressure load matrix PPE.
! NOTE: for this element work equivalent and static equivalent loads are the same
 
      IF (OPT(5) == 'Y') THEN

         DO J=1,NSUB
            PPE( 1,J) = AREA*PRESS(1,J)/THREE
            PPE( 2,J) = AREA*PRESS(2,J)/THREE
            PPE( 7,J) = AREA*PRESS(1,J)/THREE
            PPE( 8,J) = AREA*PRESS(2,J)/THREE
            PPE(13,J) = AREA*PRESS(1,J)/THREE
            PPE(14,J) = AREA*PRESS(2,J)/THREE
         ENDDO 
   
      ENDIF

! **********************************************************************************************************************************
! Calculate linear differential stiffness matrix
 
      IF ((OPT(6) == 'Y') .AND. (LOAD_ISTEP > 1)) THEN

        IF (PCOMP_PROPS == 'Y') THEN
          FATAL_ERR          = FATAL_ERR + 1
          NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
          WRITE(ERR,*) ' *ERROR: Code not written for CTRIA3 composite buckling or differential stiffness.'
          WRITE(F06,*) ' *ERROR: Code not written for CTRIA3 composite buckling or differential stiffness.'
          CALL OUTA_HERE ( 'Y' )
        ENDIF

! Accoring to:
!   Robert D. Cook, David S. Malkus, Michael E. Plesha Concepts and Applications of Finite Element Analysis, 3rd Edition  1989
!   Section 14.3 Stress Stiffness Matrix Of A Plate Element

!         +1  +1
! [  ]   ⌠   ⌠  [   ]T [   ]-T [ Nx  Nxy ] [   ]-1 [   ] 
! [k ] = |   |  [ G ]  [ J ]   [ Nxy Ny  ] [ J ]   [ G ]  |J|  dξ dη
! [ σ]   ⌡   ⌡  [  I]  [   ]               [   ]   [  I]
!        -1  -1

! k_σ is the stress stiffness (differential stiffness) matrix
! Nx, Ny, Nxy are membrane engineering forces
! |J| is the Jacobian determinant
! G_I is a 2xELGP matrix of shape function derivatives with respect to isoparametric coordinates ξ  and η.

! DPSHX = J^-1 G_I is the 2 x ELGP matrix of shape function derivatives with respect to element coordinates x and y.

!        +1  +1
! [k ]   ⌠   ⌠        T [ Nx  Nxy ]   
! [ σ] = ⌡   ⌡   DPSHX  [ Nxy Ny  ]  DPSHX  |J|  dξ dη
!        -1  -1  

        CALL ELMDIS

        DO I=1,ELGP
          DO J=1,ELGP
            KS(I,J) = ZERO
          ENDDO   
        ENDDO 

        CALL ELEM_STRE_STRN_ARRAYS (1)                     ! Stress at the Gauss point
                                                       
        FORCEx  = FCONV(1)*STRESS(1)                       ! Engineering forces at the Gauss point
        FORCEy  = FCONV(1)*STRESS(2)
        FORCExy = FCONV(1)*STRESS(3)
        DUM11(1,1) = FORCEx  ; DUM11(1,2) = FORCExy
        DUM11(2,1) = FORCExy ; DUM11(2,2) = FORCEy

        DO I=1,ELGP                                        ! Shape function derivatives at the Gauss point.
          DPSHX(1,I) = BM(1,6*(I-1)+1)
          DPSHX(2,I) = BM(2,6*(I-1)+2)
        ENDDO

        CALL MATMULT_FFF_T ( DPSHX, DUM11, 2, ELGP, 2, DUM12 )
        CALL MATMULT_FFF ( DUM12, DPSHX, ELGP, 2, ELGP, DUM13 )
                           
                                                           ! Accumulate integrand into the result
        DO I=1,ELGP
          DO J=1,ELGP
            KS(I,J) = KS(I,J) + DUM13(I,J) * AREA
          ENDDO   
        ENDDO 
                                                           ! Copy KS into KED for each translational DOF.
        DO I=1,6*ELGP
          DO J=1,6*ELGP
            KED(I,J) = 0
          ENDDO   
        ENDDO 
        DO I=1,ELGP
          DO J=1,ELGP
            KED(6*(I-1) + 1,6*(J-1) + 1) = KS(I,J)
            KED(6*(I-1) + 2,6*(J-1) + 2) = KS(I,J)
            KED(6*(I-1) + 3,6*(J-1) + 3) = KS(I,J)
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

 1101 FORMAT(' ------------------------------------------------------------------------------------------------------------------',&
             '-----------------',/,                                                                                                &
             ' ELDATA(',I2,',PRINT) requests for ',A,' element number ',I8,/,                                                      &
             ' ==============================================================',/)

 8901 FORMAT(' Strain-displacement matrix BM for membrane portion of element in subr ',A,/)

 8902 FORMAT(' Row ',I2,/,9(1ES14.6))

 9100 FORMAT(14X,'Check on strain-displacement matrix BM for membrane portion of the element in subr BCHECK'/)

 9101 FORMAT(63X,'S T R A I N S'/,62X,'(direct strains)')

 9102 FORMAT('                                                     Exx            Eyy            Exy')

 9103 FORMAT(7X,'Element displacements consistent with:')

 9104 FORMAT(7X,'---------------------------------------')

! **********************************************************************************************************************************

      END SUBROUTINE TMEM1
