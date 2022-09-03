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

      SUBROUTINE REDUCE_A_LR
 
! Call routines to reduce stiffness, mass, loads from A-set to L, R-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LINKNO,   NDOFA, NDOFG, NDOFL, NDOFR, NSUB, SOL_NAME,                       &
                                         NTERM_KAA , NTERM_KLL , NTERM_KRL , NTERM_KRR ,                                           &
                                         NTERM_KAAD, NTERM_KLLD, NTERM_KRLD, NTERM_KRRD,                                           &
                                         NTERM_MAA , NTERM_MLL , NTERM_MRL , NTERM_MRR ,                                           &
                                         NTERM_PA  , NTERM_PL  , NTERM_PR
      USE TIMDAT, ONLY                :  TSEC, YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME       
      USE CONSTANTS_1, ONLY           :  ONE
      USE DOF_TABLES, ONLY            :  TDOFI
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_ASET, RBGLOBAL_GSET, RBGLOBAL_LSET
      USE PARAMS, ONLY                :  EQCHK_OUTPUT, MATSPARS, PRTSTIFD, PRTSTIFF, PRTMASS, PRTFOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_A_LR_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KAA , J_KAA , KAA , I_KLL , J_KLL , KLL , I_KRL , J_KRL , KRL , I_KRR , J_KRR , KRR ,   &
                                         I_KAAD, J_KAAD, KAAD, I_KLLD, J_KLLD, KLLD, I_KRLD, J_KRLD, KRLD, I_KRRD, J_KRRD, KRRD,   &
                                         I_MAA , J_MAA , MAA , I_MLL , J_MLL , MLL , I_MRL , J_MRL , MRL , I_MRR , J_MRR , MRR ,   &
                                         I_PA  , J_PA  , PA  , I_PL  , J_PL  , PL  , I_PR  , J_PR  , PR
      USE SPARSE_MATRICES, ONLY       :  SYM_KLL
      USE FULL_MATRICES, ONLY         :  KAA_FULL
      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_MYSTRAN_NAMES, NUM_OU4_REQUESTS
      USE DEBUG_PARAMETERS
 
      USE REDUCE_A_LR_USE_IFs
                   
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_A_LR'
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KAA = 'Y'  ! Indicator of whether we need to keep KAA allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KRL = 'Y'  ! Indicator of whether we need to keep KRL allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KRR = 'Y'  ! Indicator of whether we need to keep KRR allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KAAD= 'Y'  ! Indicator of whether we need to keep KAA allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KRLD= 'Y'  ! Indicator of whether we need to keep KRL allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_MAA = 'Y'  ! Indicator of whether we need to keep MAA allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_MRL = 'Y'  ! Indicator of whether we need to keep MRL allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_MRR = 'Y'  ! Indicator of whether we need to keep MRR allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_PA  = 'Y'  ! Indicator of whether we need to keep PA  allocated for OU4 output
      CHARACTER(132*BYTE)             :: MATRIX_NAME           ! Name of matrix for printout 
      CHARACTER( 44*BYTE)             :: MODNAM                ! Name to write to screen to describe module being run
 
      INTEGER(LONG)                   :: DO_WHICH_CODE_FRAG    ! 1 or 2 depending on which seg of code to run (depends on BUCKLING)
      INTEGER(LONG)                   :: L_SET_COL             ! Col no. in array TDOFI where the F-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: L_SET_DOF             ! F-set DOF number
      INTEGER(LONG)                   :: I,J                   ! DO loop indices               
      INTEGER(LONG)                   :: PART_VEC_A_LR(NDOFA)  ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG)                   :: PART_VEC_SUB(NSUB)    ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_A_LR_BEGEND

      REAL(DOUBLE)                    :: KLL_DIAG(NDOFL)       ! Diagonal terms from KLL
      REAL(DOUBLE)                    :: KLL_MAX_DIAG          ! Max diag term from  KLL
      REAL(DOUBLE)                    :: KLLD_DIAG(NDOFL)      ! Diagonal terms from KLLD
      REAL(DOUBLE)                    :: KLLD_MAX_DIAG         ! Max diag term from  KLLD

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Determine if we need to keep any OUTPUT4 matrices allocated until after they are processed in LINK2

      IF (NUM_OU4_REQUESTS > 0) THEN
         DO I=1,NUM_OU4_REQUESTS
            IF      (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'KAA') THEN
               DEALLOCATE_KAA = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'KRL') THEN
               DEALLOCATE_KRL = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'KRR') THEN
               DEALLOCATE_KRR = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'MAA') THEN
               DEALLOCATE_MAA = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'MRL') THEN
               DEALLOCATE_MRL = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'MRR') THEN
               DEALLOCATE_MRR = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'PA' ) THEN
               DEALLOCATE_PA  = 'N'
            ENDIF
         ENDDO
      ENDIF

! **********************************************************************************************************************************
! Depending on whether this is a BUCKLING soln (and LOAD_ISTEP value) or not, one or another segment of code will be run

      IF ((SOL_NAME(1:8) == 'BUCKLING')) THEN
         IF      (LOAD_ISTEP == 1) THEN
            DO_WHICH_CODE_FRAG = 1
         ELSE IF (LOAD_ISTEP == 2) THEN
            DO_WHICH_CODE_FRAG = 2
         ENDIF
      ELSE
         DO_WHICH_CODE_FRAG = 1
      ENDIF

! **********************************************************************************************************************************
      IF (DO_WHICH_CODE_FRAG == 1) THEN                    ! This is for all except BUCKLING w LOAD_ISTEP=2 (eigen part of BUCKLING)

! Do reduction

         IF (NDOFR > 0) THEN                               ! If NDOFR > 0 reduce KAA to KLL

! Reduce KAA to KLL

            CALL PARTITION_VEC ( NDOFA, 'A ', 'L ', 'R ', PART_VEC_A_LR )

            DO I=1,NSUB
               PART_VEC_SUB = 1
            ENDDO

            IF (NTERM_KAA > 0) THEN

               CALL OURTIM
               MODNAM = '  REDUCE KAA TO KLL (PARTITION, ONLY)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_KAA_TO_KLL ( PART_VEC_A_LR )

            ELSE

               NTERM_KLL = 0
               NTERM_KRL = 0
               NTERM_KRR = 0
               CALL ALLOCATE_SPARSE_MAT ( 'KLL', NDOFL, NTERM_KLL, SUBR_NAME )

            ENDIF

! Reduce MAA to MLL

            IF (NTERM_MAA > 0) THEN

               CALL OURTIM
               MODNAM = '  REDUCE MAA TO MLL(PARTITION, ONLY)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_MAA_TO_MLL ( PART_VEC_A_LR )

            ELSE

               NTERM_MLL = 0
               NTERM_MRL = 0
               NTERM_MRR = 0
               CALL ALLOCATE_SPARSE_MAT ( 'MLL', NDOFL, NTERM_MLL, SUBR_NAME )

            ENDIF

! Reduce PA to PL. 

            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               CALL OURTIM
               IF (MATSPARS == 'Y') THEN
                  MODNAM = '  REDUCE PA TO PL (SPARSE MATRIX ROUTINES)'
               ELSE
                  MODNAM = '  REDUCE PA TO PL (FULL MATRIX ROUTINES)'
               ENDIF
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_PA_TO_PL ( PART_VEC_A_LR, PART_VEC_SUB )

            ENDIF

         ELSE

! There is no R-set, so equate A, L sets

            CALL OURTIM
            MODNAM = '  EQUATING L-SET TO A-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFL     = NDOFA

            NTERM_KLL = NTERM_KAA
            NTERM_KRL = 0
            NTERM_KRR = 0

            NTERM_MLL = NTERM_MAA
            NTERM_MRL = 0
            NTERM_MRR = 0

            NTERM_PL  = NTERM_PA
            NTERM_PR  = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KLL', NDOFL, NTERM_KLL, SUBR_NAME )

            CALL ALLOCATE_SPARSE_MAT ( 'MLL', NDOFL, NTERM_MLL, SUBR_NAME )

            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               CALL ALLOCATE_SPARSE_MAT ( 'PL', NDOFL, NTERM_PL, SUBR_NAME )

            ENDIF

         ENDIF

! Deallocate A-set arrays

         IF (DEBUG(57) > 0) THEN
            CALL ALLOCATE_FULL_MAT ( 'KAA_FULL', NDOFA, NDOFA, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'KAA', NTERM_KAA, NDOFA, NDOFA, 'Y', I_KAA, J_KAA, KAA, KAA_FULL )
            WRITE(F06,*) ' A-Set Stiffness Matrix, KAA'
            DO I=1,NDOFA
               IF (DEBUG(57) == 2) THEN
                  WRITE(F06,*) '  ROW',I
               ENDIF
               WRITE(F06,3006) (KAA_FULL(I,J),J=1,NDOFA)
               WRITE(F06,*)
            ENDDO
 3006       FORMAT(6(1ES15.6))
         ENDIF

         MODNAM = '  DEALLOCATE A-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         

         IF (DEALLOCATE_KAA == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAA', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KAA' )
            WRITE(SC1,*) CR13
         ENDIF

         IF (DEALLOCATE_MAA == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MAA', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MAA' )
            WRITE(SC1,*) CR13
         ENDIF

         IF (DEALLOCATE_PA == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PA ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PA' )
            WRITE(SC1,*) CR13
         ENDIF

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(5) == 1) .OR. ( PRTSTIFF(5) == 3)) THEN
            IF (NTERM_KLL > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KLL'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'L ', 'L ', NTERM_KLL, NDOFL, I_KLL, J_KLL, KLL )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(5) == 2) .OR. ( PRTSTIFF(5) == 3)) THEN
            IF (NTERM_KRL > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KRL'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'L ', NTERM_KRL, NDOFR, I_KRL, J_KRL, KRL )
            ENDIF
            IF (NTERM_KRR > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KRR'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'R ', NTERM_KRR, NDOFR, I_KRR, J_KRR, KRR )
            ENDIF
         ENDIF

         MODNAM = '  DEALLOCATE R-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         

         IF (DEALLOCATE_KRL == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KRL', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KRL' )
            WRITE(SC1,*) CR13
         ENDIF

         IF (DEALLOCATE_KRR == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KRR', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KRR' )
            WRITE(SC1,*) CR13
         ENDIF

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(2) = 0 since we need KNN_DIAG, KNN_MAX_DIAG for the equilibrium check

         CALL GET_MATRIX_DIAG_STATS ( 'KLL', 'L ', NDOFL, NTERM_KLL, I_KLL, J_KLL, KLL, PRTSTIFD(5), KLL_DIAG, KLL_MAX_DIAG )

! Print out mass matrix partitions, if requested

         IF (( PRTMASS(5) == 1) .OR. ( PRTMASS(5) == 3)) THEN
            IF (NTERM_MLL > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MLL'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'L ', 'L ', NTERM_MLL, NDOFL, I_MLL, J_MLL, MLL )
            ENDIF
         ENDIF

         IF (( PRTMASS(5) == 2) .OR. ( PRTMASS(5) == 3)) THEN
            IF (NTERM_MRL > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MRL'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'L ', NTERM_MRL, NDOFR, I_MRL, J_MRL, MRL )
            ENDIF
            IF (NTERM_MRR > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MRR'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'R ', NTERM_MRR, NDOFR, I_MRR, J_MRR, MRR )
            ENDIF
         ENDIF

         IF (DEALLOCATE_MRL == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MRL', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MRL' )
            WRITE(SC1,*) CR13
         ENDIF

         IF (DEALLOCATE_MRR == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MRR', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MRR' )
            WRITE(SC1,*) CR13
         ENDIF

! Print out load matrix partitions, if requested

         IF (( PRTFOR(5) == 1) .OR. ( PRTFOR(5) == 3)) THEN
            IF (NTERM_PL  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PL'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'L ', 'SUBCASE', NTERM_PL, NDOFL, I_PL, J_PL, PL )
            ENDIF
         ENDIF

         IF (( PRTFOR(5) == 2) .OR. ( PRTFOR(5) == 3)) THEN
            IF (NTERM_PR  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PR'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'SUBCASE', NTERM_PR, NDOFR, I_PR, J_PR, PR )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PR ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PR' )
         WRITE(SC1,*) CR13

! Do equilibrium check on the L-set stiffness matrix, if requested

         IF (EQCHK_OUTPUT(5) > 0) THEN
            CALL ALLOCATE_RBGLOBAL ( 'L ', SUBR_NAME )
            IF (NDOFR > 0) THEN
               CALL TDOF_COL_NUM ( 'L ', L_SET_COL )
               DO I=1,NDOFG
                  L_SET_DOF = TDOFI(I,L_SET_COL)
                  IF (L_SET_DOF > 0) THEN
                     DO J=1,6
                        RBGLOBAL_LSET(L_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                     ENDDO
                  ENDIF
               ENDDO
            ELSE
               DO I=1,NDOFL
                  DO J=1,6
                     RBGLOBAL_LSET(I,J) = RBGLOBAL_ASET(I,J)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         IF (EQCHK_OUTPUT(5) > 0) THEN
            CALL OURTIM
            MODNAM = '  EQUILIBRIUM CHECK ON KLL                '
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL STIFF_MAT_EQUIL_CHK ( EQCHK_OUTPUT(3),'L ', SYM_KLL, NDOFL, NTERM_KLL, I_KLL, J_KLL, KLL, KLL_DIAG, KLL_MAX_DIAG, &
                                       RBGLOBAL_LSET)
         ENDIF
         CALL DEALLOCATE_RBGLOBAL ( 'A ' )
         CALL DEALLOCATE_RBGLOBAL ( 'L ' )



! **********************************************************************************************************************************
      ELSE                                                 ! This is BUCKLING with LOAD_ISTEP = 2 (eigen part of BUCKLING)

! Do reduction

         IF (NDOFR > 0) THEN                               ! If NDOFR > 0 reduce KAA to KLL

! Reduce KAAD to KLLD

            CALL PARTITION_VEC ( NDOFA, 'A ', 'L ', 'R ', PART_VEC_A_LR )

            DO I=1,NSUB
               PART_VEC_SUB = 1
            ENDDO

            IF (NTERM_KAAD > 0) THEN

               CALL OURTIM
               MODNAM = '  REDUCE KAAD TO KLLD (PARTITION, ONLY)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_KAAD_TO_KLLD ( PART_VEC_A_LR )

            ELSE

               NTERM_KLLD = 0
               NTERM_KRLD = 0
               NTERM_KRRD = 0
               CALL ALLOCATE_SPARSE_MAT ( 'KLLD', NDOFL, NTERM_KLLD, SUBR_NAME )

            ENDIF

         ELSE

! There is no R-set, so equate A, L sets

            CALL OURTIM
            MODNAM = '  EQUATING L-SET TO A-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFL     = NDOFA

            NTERM_KLLD = NTERM_KAAD
            NTERM_KRLD = 0
            NTERM_KRRD = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KLLD', NDOFL, NTERM_KLLD, SUBR_NAME )

         ENDIF

! Deallocate A-set arrays

         MODNAM = '  DEALLOCATE A-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         

         IF (DEALLOCATE_KAAD == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAAD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KAAD' )
            WRITE(SC1,*) CR13
         ENDIF

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(5) == 1) .OR. ( PRTSTIFF(5) == 3)) THEN
            IF (NTERM_KLLD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KLLD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'L ', 'L ', NTERM_KLLD, NDOFL, I_KLLD, J_KLLD, KLLD )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(5) == 2) .OR. ( PRTSTIFF(5) == 3)) THEN
            IF (NTERM_KRLD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KRLD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'L ', NTERM_KRLD, NDOFR, I_KRLD, J_KRLD, KRLD )
            ENDIF
            IF (NTERM_KRRD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KRRD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'R ', 'R ', NTERM_KRRD, NDOFR, I_KRRD, J_KRRD, KRRD )
            ENDIF
         ENDIF

         MODNAM = '  DEALLOCATE R-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         

         IF (DEALLOCATE_KRLD == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KRLD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KRLD' )
            WRITE(SC1,*) CR13
         ENDIF

         IF (DEALLOCATE_KRR == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KRRD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KRRD' )
            WRITE(SC1,*) CR13
         ENDIF

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(2) = 0 since we need KNN_DIAG, KNN_MAX_DIAG for the equilibrium check

         CALL GET_MATRIX_DIAG_STATS ( 'KLLD', 'L ', NDOFL, NTERM_KLLD, I_KLLD, J_KLLD, KLLD, PRTSTIFD(5), KLLD_DIAG, KLLD_MAX_DIAG )

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 2092 FORMAT(4X,A44,20X,I2,':',I2,':',I2,'.',I3)

 9995 FORMAT(/,' PROCESSING ENDED IN LINK ',I3,' DUE TO ABOVE ',I8,' ERRORS')



12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_A_LR   
