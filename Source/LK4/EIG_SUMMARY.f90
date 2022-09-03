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

      SUBROUTINE EIG_SUMMARY
  
! Prints eigenvalue analysis summary table
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ANS, ANSFIL, ANS_MSG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFL, NUM_EIGENS, NVEC, NUM_KLLD_DIAG_ZEROS, NUM_MLL_DIAG_ZEROS, SOL_NAME, &
                                         WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  ART_MASS, ART_ROT_MASS, ART_TRAN_MASS, DARPACK, SOLLIB, SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  EIG_SUMMARY_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, PI
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS, MODE_NUM, EIGEN_VAL
      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_GRID, EIG_LAP_MAT_TYPE, EIG_METH, EIG_MODE, EIG_N2, EIG_NORM,     &
                                         EIG_SIGMA, MAXMIJ, MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT
 
      USE EIG_SUMMARY_USE_IFs

      IMPLICIT NONE
  
      LOGICAL                         :: FILE_OPND         ! .TRUE. if a file is opened

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EIG_SUMMARY'
      CHARACTER( 1*BYTE)              :: ASTERISK = '*'    ! Used for denoting negative eigenvalues
  
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: NUM_FINITE_EIGENS ! Number of eigenvalues that are finite (excluding zero mass modes)
      INTEGER(LONG)                   :: MAX_LANCZOS_EIGENS! Max number of eigenvalues that can be found by Lanczos method
      INTEGER(LONG)                   :: NUM_NEG_EIGENS    ! Number of eigenvalues that are negative
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EIG_SUMMARY_BEGEND
  
      REAL(DOUBLE)                    :: CYCLES1           ! Circular frequency of a mode
      REAL(DOUBLE)                    :: GEN_STIFF1        ! Generalized stiffness for a mode
      REAL(DOUBLE)                    :: RADS1             ! Radian frequency of a mode
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

      IF (DEBUG(200) > 0) THEN
         INQUIRE (FILE=ANSFIL, OPENED=FILE_OPND)
         IF (.NOT.FILE_OPND) THEN                          ! Otherwise we assume it is positioned at its end and ready for write
            CALL FILE_OPEN ( ANS, ANSFIL, OUNT, 'OLD', ANS_MSG, 'WRITE_STIME', 'FORMATTED', 'READWRITE', 'REWIND', 'Y', 'Y', 'Y' )
         ENDIF
         WRITE(ANS,*)
         WRITE(ANS,*)
      ENDIF

      IF (EIG_METH == 'LANCZOS') THEN

         IF      (SOLLIB == 'BANDED  ') THEN
            WRITE(F06,90001) EIG_METH, EIG_MODE, TRIM(EIG_LAP_MAT_TYPE), EIG_SIGMA, '(BANDED solution)'
         ELSE IF (SOLLIB == 'SPARSE  ') THEN
            WRITE(F06,90001) EIG_METH, EIG_MODE, TRIM(EIG_LAP_MAT_TYPE), EIG_SIGMA, '(SPARSE solution)'
         ENDIF

      ELSE

         IF      (SOLLIB == 'BANDED  ') THEN
            WRITE(F06,90003) EIG_METH, '(BANDED solution)'
         ELSE IF (SOLLIB == 'SPARSE  ') THEN
            WRITE(F06,90003) EIG_METH, '(SPARSE solution)'
         ENDIF

      ENDIF
      WRITE(F06,90004) NUM_EIGENS

      IF (NVEC > 0) THEN

         IF (DEBUG(48) == 0) THEN                          ! Off diag terms were calculated, so write results

            IF (EIG_NORM == 'MASS') THEN
               WRITE(F06,91001) MAXMIJ
            ELSE IF (EIG_NORM == 'MAX') THEN
               WRITE(F06,91002) MAXMIJ
            ELSE IF (EIG_NORM == 'POINT') THEN
               WRITE(F06,91003) MAXMIJ,EIG_GRID,EIG_COMP
            ELSE IF (EIG_NORM == 'NONE') THEN
               WRITE(F06,91004) MAXMIJ
            ENDIF

            WRITE(F06,92004) MIJ_ROW
            WRITE(F06,92005)
            WRITE(F06,92006) MIJ_COL
            WRITE(F06,92007)
            WRITE(F06,92008) EIG_CRIT, NUM_FAIL_CRIT

         ELSE

            WRITE(F06,91005) DEBUG(48)

         ENDIF

         WRITE(F06,*)
         WRITE(F06,*)

      ELSE

         WRITE(F06,92010)

      ENDIF
  
      IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         WRITE(F06,94101)
         WRITE(F06,94102)
      ELSE
         WRITE(F06,94201)
         WRITE(F06,94202)
      ENDIF
      IF (DEBUG(200) > 0) THEN
         IF (SOL_NAME(1:8) == 'BUCKLING') THEN
            WRITE(ANS,94301)
            WRITE(ANS,94302)
         ELSE
            WRITE(ANS,94401)
            WRITE(ANS,94402)
         ENDIF
      ENDIF

      NUM_NEG_EIGENS = 0
      DO I=1,NUM_EIGENS
         RADS1      = DSQRT(DABS(EIGEN_VAL(I)))
         CYCLES1    = RADS1/(TWO*PI)
         GEN_STIFF1 = EIGEN_VAL(I)*GEN_MASS(I)
         IF (EIGEN_VAL(I) < ZERO) THEN
            NUM_NEG_EIGENS = NUM_NEG_EIGENS + 1
            IF (SOL_NAME(1:8) == 'BUCKLING') THEN
               WRITE(F06,95301) MODE_NUM(I),I,EIGEN_VAL(I),ASTERISK
            ELSE
               WRITE(F06,95302) MODE_NUM(I),I,EIGEN_VAL(I),ASTERISK,RADS1,CYCLES1,GEN_MASS(I),GEN_STIFF1
            ENDIF
            IF (DEBUG(200) > 0) THEN
               IF (SOL_NAME(1:8) == 'BUCKLING') THEN
                  WRITE(ANS,95311) MODE_NUM(I),I,EIGEN_VAL(I),ASTERISK
               ELSE
                  WRITE(ANS,95312) MODE_NUM(I),I,EIGEN_VAL(I),ASTERISK,RADS1,CYCLES1,GEN_MASS(I),GEN_STIFF1
               ENDIF
            ENDIF
         ELSE
            IF (SOL_NAME(1:8) == 'BUCKLING') THEN
               WRITE(F06,95401) MODE_NUM(I),I,EIGEN_VAL(I)
            ELSE
               WRITE(F06,95402) MODE_NUM(I),I,EIGEN_VAL(I),         RADS1,CYCLES1,GEN_MASS(I),GEN_STIFF1
            ENDIF
            IF (DEBUG(200) > 0) THEN
               IF (SOL_NAME(1:8) == 'BUCKLING') THEN
                  WRITE(ANS,95411) MODE_NUM(I),I,EIGEN_VAL(I)
               ELSE
                  WRITE(ANS,95412) MODE_NUM(I),I,EIGEN_VAL(I),         RADS1,CYCLES1,GEN_MASS(I),GEN_STIFF1
               ENDIF
            ENDIF
         ENDIF
      ENDDO 

      IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         NUM_FINITE_EIGENS = NDOFL - NUM_KLLD_DIAG_ZEROS
      ELSE
         NUM_FINITE_EIGENS = NDOFL - NUM_MLL_DIAG_ZEROS
      ENDIF

      IF  (EIG_N2 > NUM_FINITE_EIGENS) THEN

         WRITE(F06,*)
         WARN_ERR = WARN_ERR + 1

         IF (SOL_NAME(1:8) == 'BUCKLING') THEN
            WRITE(ERR,98006) EIG_N2, NUM_FINITE_EIGENS, NUM_KLLD_DIAG_ZEROS
            IF (SUPWARN == 'N') THEN
               WRITE(F06,98006) EIG_N2, NUM_FINITE_EIGENS, NUM_MLL_DIAG_ZEROS
            ENDIF
         ELSE
            WRITE(ERR,98006) EIG_N2, NUM_FINITE_EIGENS, NUM_MLL_DIAG_ZEROS
            IF (SUPWARN == 'N') THEN
               WRITE(F06,98006) EIG_N2, NUM_FINITE_EIGENS, NUM_MLL_DIAG_ZEROS
            ENDIF
         ENDIF

      ELSE

         IF (ART_MASS == 'Y') THEN
            WRITE(F06,*)
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,98007) ART_TRAN_MASS, ART_ROT_MASS
            IF (SUPWARN == 'N') THEN
               WRITE(F06,98007) ART_TRAN_MASS, ART_ROT_MASS
            ENDIF
         ENDIF

      ENDIF

      IF ((EIG_METH == 'LANCZOS') .AND. (DEBUG(185) == 0)) THEN
         MAX_LANCZOS_EIGENS = NUM_FINITE_EIGENS - 1 - DARPACK
         IF (EIG_N2 > MAX_LANCZOS_EIGENS) THEN
            WRITE(F06,*)
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,98008) NUM_EIGENS, NUM_FINITE_EIGENS, DARPACK
            IF (SUPWARN == 'N') THEN
               WRITE(F06,98008) NUM_EIGENS, NUM_FINITE_EIGENS, DARPACK
            ENDIF
         ENDIF
      ENDIF

      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,*)
      ENDIF

      IF (NUM_NEG_EIGENS > 0) THEN
         WRITE(F06,*)
         WRITE(F06,99000) NUM_NEG_EIGENS
      ENDIF
      WRITE(F06,*)
      WRITE(F06,*)

      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,*)
         WRITE(ANS,*)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
99001 FORMAT(A1)

90001 FORMAT(/,27X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R Y',3X,'(',A8,' Mode',I2,1X,A,', Shift eigen = ',1ES9.2,')',&
             /,70X,A,/)

90003 FORMAT(/,27X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R Y',3X,'(',A8,')',/,A,/)

90004 FORMAT(32X,'NUMBER OF EIGENVALUES EXTRACTED  . . . . . .',2X,I8,/)

91001 FORMAT(32X,'LARGEST OFF-DIAGONAL GENERALIZED MASS TERM  ',1ES10.1,' (Vecs renormed to 1.0 for gen masses)',/)

91002 FORMAT(32X,'LARGEST OFF-DIAGONAL GENERALIZED MASS TERM  ',1ES10.1,' (Vecs renormed to 1.0 for max value)',/)

91003 FORMAT(32X,'LARGEST OFF-DIAGONAL GENERALIZED MASS TERM  ',1ES10.1,' (Vecs renormed to 1.0 at grid-comp',I8,'-',I1,')',/)

91004 FORMAT(32X,'LARGEST OFF-DIAGONAL GENERALIZED MASS TERM  ',1ES10.1,' (Vecs not renormalized)',/)

91005 FORMAT(32X,'OFF-DIAGONAL GENERALIZED MASS TERMS NOT CALCULATED BASED ON DEBUG(48) = ',I8)

92004 FORMAT(32X,'                                       . . .',2X,I8)

92005 FORMAT(32X,'          MODE PAIR . . . . . . . . . .')

92006 FORMAT(32X,'                                       . . .',2X,I8,/)

92007 FORMAT(32X,'NUMBER OF OFF DIAGONAL GENERALIZED MASS')

92008 FORMAT(32X,'TERMS FAILING CRITERION OF ',1ES8.1,'. . . . .',2X,I8,/)
 
92009 FORMAT(32X,'RATIO OF HIGHEST REAL MGIV EIGENVALUE'                                                                           &
          ,/,32X,'TO LOWEST "INFINITE" EIGENVALUE. . . . . . .  ',1ES8.1)

92010 FORMAT(4X,'NO EIGENVECTORS WERE REQUESTED TO BE OUTPUT, SO NO GENERALIZED MASS OR GENERALIZED STIFFNESS HAS BEEN CALCULATED',&
             /)

94101 FORMAT(44X,'R E A L   E I G E N V A L U E S',/,43X,'(subcase 1 buckling load factors)',/)

94102 FORMAT(40X,' MODE  EXTRACTION      EIGENVALUE',/,40X,'NUMBER   ORDER',/)

94201 FORMAT(44X,'R E A L   E I G E N V A L U E S')

94202 FORMAT(3X,' MODE  EXTRACTION      EIGENVALUE           RADIANS              CYCLES            GENERALIZED         GENERALIZED&
&        ',/,3X,'NUMBER   ORDER                                                                        MASS              STIFFNESS'&
          ,/)

94301 FORMAT(44X,'R E A L   E I G E N V A L U E S',/,43X,'(subcase 1 buckling load factors)',/)

94302 FORMAT(40X,' MODE  EXTRACTION      EIGENVALUE',/,40X,'NUMBER   ORDER',/)

94401 FORMAT(40X,'R E A L   E I G E N V A L U E S')

94402 FORMAT(9X,' MODE    EXTRACT  EIGENVALUE     RADIANS        CYCLES      GENERALIZED   GENERALIZED&
&        ',/,9X,'NUMBER    ORDER                                                MASS        STIFFNESS',/)

95301 FORMAT(38X,2I8,1ES20.6,A)

95302 FORMAT(1X,2I8,1ES20.6,A,1ES19.6,3(1ES20.6))

95311 FORMAT(6X,2I9,1ES14.6,A)

95312 FORMAT(6X,2I9,1ES14.6,A,1ES13.6,3(1ES14.6))

95401 FORMAT(38X,2I8,1ES20.6)

95402 FORMAT(1X,2I8,5(1ES20.6))

95411 FORMAT(6X,2I9,1ES14.6)

95412 FORMAT(6X,2I9,5(1ES14.6))

98006 FORMAT(' *WARNING    : THE BULK DATA EIGR/EIGRL ENTRY ASKED FOR MODES UP TO NUMBER',I8,'. HOWEVER, THIS MODEL HAS ONLY',I8   &
                    ,/,14X,' FINITE EIGENVALUES DUE TO THE FACT THAT THE L-SET MASS MATRIX HAS',I8,' ZERO MASS DEGREES OF FREEDOM.'&
                    ,/,14x,' (USE OF BULK DATA PARAM ART_MASS WITH SMALL VALUE MAY HELP TO AVOID EIGENVALUES THAT ARE',      &
                           ' THEORETICALLY INFINITE)') 

98007 FORMAT(' *WARNING    : THE USE OF BULK DATA PARAM ART_MASS MAY HAVE MASKED SOME OTHERWISE INFINITE EIGENVALUES. IF A SMALL', &
                           ' VALUE FOR'                                                                                            &
                    ,/,14X,' ART_MASS WAS USED THERE MAY BE SOME LARGE EIGENVALUES AND, IF SO, THESE SHOULD BE IGNORED.',          &
                           ' IF A LARGE VALUE FOR'                                                                                 &
                    ,/,14X,' ART_MASS WAS USED THEN ALL EIGENVALUES MAY BE SUSPECT. THIS RUN USED THE FOLLOWING ART_MASS VALUES:',/&
                    ,/,14X,'                   Artificial mass for translational G-set DOFs = ',1ES14.6                            &
                    ,/,14X,'                   Artificial mass for rotational    G-set DOFs = ',1ES14.6) 

98008 FORMAT(' *WARNING    : LANCZOS ONLY FOUND',I8,' EIGENVALUES DUE TO:'                                                         &
                    ,/,14X,' (1) IT CAN NEVER FIND ALL',I8,' FINITE EIGENVALUES (IT IS ONLY POSSIBLE TO FIND 1 LESS THAN ALL)'     &
                    ,/,14X,' (2) IT ALSO DOES NOT PRINT THE ',I2,' HIGHEST MODES (DEFAULT VALUE FOR BULK DATA PARAM DARPACK) DUE', &
                               ' TO ROUNDOFF ERROR.'                                                                               &
                    ,/,18X,    ' THE USER CAN CHANGE THIS NUMBER VIA BULK DATA PARAM DARPACK.')


99000 FORMAT(' *INFORMATION: ASTERISK INDICATES ' ,I8, ' NEGATIVE EIGENVALUES. MYSTRAN TOOK THE ABSOLUTE VALUES OF THESE TO CALC', &
                                  ' RADIAN FREQUENCY.',/,                                                                          &
                    '               IF NEGATIVE EIGENVALUES ARE NOT SMALL THEY MAY BE IN ERROR',/,                                 &
                    '               (NOTE THAT LARGE EIGENVALUE MAGNITUDES IN MGIV WILL RESULT IF THERE ARE MASSLESS DOF''s IN THE'&
                                  ,' A-SET)')

! **********************************************************************************************************************************
 
      END SUBROUTINE EIG_SUMMARY
