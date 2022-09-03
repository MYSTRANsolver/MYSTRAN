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

      SUBROUTINE REDUCE_G_NM
 
! Call routines to reduce stiffness, mass, loads and constraint matrices from G-set to N, M-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1C, LINK1C, L1C_MSG, SC1, WRT_ERR, WRT_LOG

      USE SCONTR, ONLY                :  LINKNO    , NDOFG, NDOFN, NDOFM, NGRID, NSUB,                                             &
                                         NTERM_KGG , NTERM_KNN , NTERM_KNM , NTERM_KMM ,                                           &
                                         NTERM_KGGD, NTERM_KNND, NTERM_KNMD, NTERM_KMMD,                                           &
                                         NTERM_MGG , NTERM_MNN , NTERM_MNM , NTERM_MMM , NTERM_PG, NTERM_PN, NTERM_PM ,            &
                                         NTERM_GMN , NTERM_RMN , NTERM_RMM ,                                                       &
                                         PROG_NAME , SOL_NAME, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_NSET, EQCHK_OUTPUT, MATSPARS, PRTSTIFD, PRTSTIFF, PRTMASS, PRTFOR, SUPINFO
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI
      USE MODEL_STUF, ONLY            :  GRID_ID
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_NSET
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_G_NM_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KGG , J_KGG , KGG , I_KGGD, J_KGGD, KGGD,                                               &
                                         I_KNN , J_KNN , KNN , I_KNM , J_KNM , KNM , I_KMM , J_KMM , KMM ,                         &
                                         I_KNND, J_KNND, KNND, I_KNMD, J_KNMD, KNMD, I_KMMD, J_KMMD, KMMD,                         &
                                         I_MGG , J_MGG , MGG , I_MNN , J_MNN , MNN , I_MNM , J_MNM , MNM , I_MMM , J_MMM , MMM ,   &
                                         I_PG  , J_PG  , PG  , I_PN  , J_PN  , PN  , I_PM  , J_PM  , PM  ,                         &
                                         I_RMG , J_RMG , RMG
                                         
      USE SPARSE_MATRICES, ONLY       :  SYM_KNN
      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_MYSTRAN_NAMES, NUM_OU4_REQUESTS
      USE SCRATCH_MATRICES
 
      USE REDUCE_G_NM_USE_IFs

      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_G_NM'
      CHARACTER(  8*BYTE)             :: ASPC_SUM_MSG1       ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(100*BYTE)             :: ASPC_SUM_MSG2       ! Message to be printed out in the AUTOSPC summary table
      CHARACTER( 13*BYTE)             :: ASPC_SUM_MSG3       ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KGG = 'Y'! Indicator of whether we need to keep KGG  allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_KGGD= 'Y'! Indicator of whether we need to keep KGGD allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_MGG = 'Y'! Indicator of whether we need to keep MGG allocated for OU4 output
      CHARACTER(  1*BYTE)             :: DEALLOCATE_PG  = 'Y'! Indicator of whether we need to keep PG  allocated for OU4 output
      CHARACTER(132*BYTE)             :: MATRIX_NAME         ! Name of matrix for printout 
      CHARACTER(44*BYTE)              :: MODNAM              ! Name to write to screen to describe module being run
 
      INTEGER(LONG)                   :: DO_WHICH_CODE_FRAG    ! 1 or 2 depending on which seg of code to run (depends on BUCKLING)
      INTEGER(LONG)                   :: I,J,K               ! DO loop indices
      INTEGER(LONG)                   :: N_SET_COL           ! Col no. in array TDOFI where the N-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: N_SET_DOF           ! N-set DOF number
      INTEGER(LONG)                   :: NUM_ASPC_BY_COMP(6) ! Number of AUTOSPC's by component number
      INTEGER(LONG)                   :: NUM_COMPS           ! 6 if GRID_NUM is an physical grid, 1 if an SPOINT
      INTEGER(LONG)                   :: PART_VEC_G_NM(NDOFG)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG)                   :: PART_VEC_M(NDOFM)   ! Partitioning vector (1's for all M set DOF's) 
      INTEGER(LONG)                   :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG)                   :: SA_SET_COL          ! Col no. in array TDOF where the SA-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: TOT_NUM_ASPC        ! Sum of NUM_ASPC_BY_COMP(6)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_G_NM_BEGEND

      REAL(DOUBLE)                    :: KNN_DIAG(NDOFN)     ! Diagonal terms from KNN
      REAL(DOUBLE)                    :: KNN_MAX_DIAG        ! Max diag term from  KNN
      REAL(DOUBLE)                    :: KNND_DIAG(NDOFN)    ! Diagonal terms from KNND
      REAL(DOUBLE)                    :: KNND_MAX_DIAG       ! Max diag term from  KNND

      INTRINSIC                       :: DABS

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
            IF      (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'KGG') THEN
               DEALLOCATE_KGG = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'MGG') THEN
               DEALLOCATE_MGG = 'N'
            ELSE IF (ACT_OU4_MYSTRAN_NAMES(I)(1:3) == 'PG' ) THEN
               DEALLOCATE_PG  = 'N'
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

! If there is an M-set, reduce KGG to KNN, MGG to MNN, PG to PN using UM = GMN*UN, where GMN = -RMM(-1)*RMN (partitions of RMG)
! If there is no M-set, then equate KNN to KGG, MNN to MGG, PN to PG

! First, need to create partitioning vectors used in the reduction (if NDOFM > 0)

         IF (NDOFM > 0) THEN

            CALL PARTITION_VEC (NDOFG,'G ','N ','M ',PART_VEC_G_NM)

            DO I=1,NDOFM
               PART_VEC_M(I) = 1
            ENDDO
    
            DO I=1,NSUB
               PART_VEC_SUB = 1
            ENDDO

            CALL OURTIM
            MODNAM = '  SOLVE FOR GMN CONSTRAINT MATRIX'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL SOLVE_GMN ( PART_VEC_G_NM, PART_VEC_M )   ! First, solve for GMN
      !xx   WRITE(SC1,  * )                                ! Advance 1 line for screen messages
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate RMG', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'RMG' ) 

            IF (NTERM_KGG > 0) THEN                        ! Reduce KGG to KNN 

               CALL OURTIM
               IF (MATSPARS == 'Y') THEN
                  MODNAM = '  REDUCE KGG TO KNN (SPARSE MATRIX ROUTINES)'
               ELSE
                  MODNAM = '  REDUCE KGG TO KNN (FULL MATRIX ROUTINES)'
               ENDIF
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_KGG_TO_KNN ( PART_VEC_G_NM )

            ELSE

               NTERM_KNN = 0
               NTERM_KNM = 0
               NTERM_KMM = 0
               CALL ALLOCATE_SPARSE_MAT ( 'KNN', NDOFN, NTERM_KNN, SUBR_NAME )

            ENDIF

            IF (NTERM_MGG > 0) THEN                        ! Reduce MGG to MNN

               CALL OURTIM
               IF (MATSPARS == 'Y') THEN
                  MODNAM = '  REDUCE MGG TO MNN (SPARSE MATRIX ROUTINES)'
               ELSE
                  MODNAM = '  REDUCE MGG TO MNN (FULL MATRIX ROUTINES)'
               ENDIF
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_MGG_TO_MNN ( PART_VEC_G_NM )

            ELSE

               NTERM_MNN = 0
               NTERM_MNM = 0
               NTERM_MMM = 0
               CALL ALLOCATE_SPARSE_MAT ( 'MNN', NDOFN, NTERM_MNN, SUBR_NAME )

            ENDIF

            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               IF (NTERM_PG > 0) THEN                      ! Reduce PG to PN

                  CALL OURTIM
                  IF (MATSPARS == 'Y') THEN
                     MODNAM = '  REDUCE PG  TO PN  (SPARSE MATRIX ROUTINES)'
                  ELSE
                     MODNAM = '  REDUCE PG  TO PN  (FULL MATRIX ROUTINES)'
                  ENDIF
                  WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

                  CALL REDUCE_PG_TO_PN ( PART_VEC_G_NM, PART_VEC_SUB )

               ELSE

                  NTERM_PN = 0
                  NTERM_PM = 0
                  CALL ALLOCATE_SPARSE_MAT ( 'PN', NDOFN, NTERM_PN, SUBR_NAME )

               ENDIF

            ENDIF

         ELSE                                              ! There is no M-set, so equate N and G sets

            CALL OURTIM
            MODNAM = '  EQUATING N-SET TO G-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFN     = NDOFG

            NTERM_KNN = NTERM_KGG
            NTERM_KNM = 0
            NTERM_KMM = 0

            NTERM_MNN = NTERM_MGG
            NTERM_MNM = 0
            NTERM_MMM = 0

            NTERM_PN  = NTERM_PG
            NTERM_PM  = 0

            NTERM_GMN = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KNN', NDOFN, NTERM_KNN, SUBR_NAME )

!xx         DO I=1,NDOFN+1
!xx            I_KNN(I) = I_KGG(I)
!xx         ENDDO
!xx
!xx         DO I=1,NTERM_KNN
!xx            J_KNN(I) = J_KGG(I)
!xx              KNN(I) =   KGG(I)
!xx         ENDDO
!xx
            CALL ALLOCATE_SPARSE_MAT ( 'MNN', NDOFN, NTERM_MNN, SUBR_NAME )

!xx         DO I=1,NDOFN+1
!xx            I_MNN(I) = I_MGG(I)
!xx         ENDDO
!xx
!xx         DO I=1,NTERM_MNN
!xx            J_MNN(I) = J_MGG(I)
!xx              MNN(I) =   MGG(I)
!xx         ENDDO
!xx
            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               CALL ALLOCATE_SPARSE_MAT ( 'PN', NDOFN, NTERM_PN, SUBR_NAME )

!xx            DO I=1,NDOFN+1
!xx               I_PN(I) = I_PG(I)
!xx            ENDDO
!xx
!xx            DO I=1,NTERM_PN
!xx            J_PN(I) = J_PG(I)
!xx              PN(I) =   PG(I)
!xx            ENDDO
!xx
            ENDIF

         ENDIF

! Deallocate G-set arrays

         MODNAM = '  DEALLOCATE G-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         

         IF (DEALLOCATE_KGG == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KGG', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KGG' )
         ENDIF

         IF (DEALLOCATE_MGG == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MGG', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MGG' )
         ENDIF

         IF (DEALLOCATE_PG == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PG ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PG' )
         ENDIF
         WRITE(SC1,12345,ADVANCE='NO')    '       Deallocate GMN', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'GMN' )

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(2) == 1) .OR. ( PRTSTIFF(2) == 3)) THEN
            IF (NTERM_KNN > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KNN'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'N ', NTERM_KNN, NDOFN, I_KNN, J_KNN, KNN )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(2) == 2) .OR. ( PRTSTIFF(2) == 3)) THEN
            IF (NTERM_KNM > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KNM'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'M ', NTERM_KNM, NDOFN, I_KNM, J_KNM, KNM )
            ENDIF
            IF (NTERM_KMM > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KMM'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'M ', 'M ', NTERM_KMM, NDOFM, I_KMM, J_KMM, KMM )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNM', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KNM' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KMM', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KMM' )

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(2) = 0 since we need KNN_DIAG, KNN_MAX_DIAG for the equilibrium check

         CALL GET_MATRIX_DIAG_STATS ( 'KNN', 'N ', NDOFN, NTERM_KNN, I_KNN, J_KNN, KNN, PRTSTIFD(2), KNN_DIAG, KNN_MAX_DIAG )

! Print out mass matrix partitions, if requested

         IF (( PRTMASS(2) == 1) .OR. ( PRTMASS(2) == 3)) THEN
            IF (NTERM_MNN > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MNN'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'N ', NTERM_MNN, NDOFN, I_MNN, J_MNN, MNN )
            ENDIF
         ENDIF

         IF (( PRTMASS(2) == 2) .OR. ( PRTMASS(2) == 3)) THEN
            IF (NTERM_MNM > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MNM'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'M ', NTERM_MNM, NDOFN, I_MNM, J_MNM, MNM )
            ENDIF
            IF (NTERM_MMM > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MMM'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'M ', 'M ', NTERM_MMM, NDOFM, I_MMM, J_MMM, MMM )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MMN', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MMN' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MNM', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MNM' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MMM', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MMM' )

! Print out load matrix partitions, if requested

         IF (( PRTFOR(2) == 1) .OR. ( PRTFOR(2) == 3)) THEN
            IF (NTERM_PN  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PN'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'SUBCASE', NTERM_PN, NDOFN, I_PN, J_PN, PN )
            ENDIF
         ENDIF

         IF (( PRTFOR(2) == 2) .OR. ( PRTFOR(2) == 3)) THEN
            IF (NTERM_PM  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PM'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'M ', 'SUBCASE', NTERM_PM, NDOFM, I_PM, J_PM, PM )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PM ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PM' )
         WRITE(SC1,*) CR13

! Do equilibrium check on the N-set stiffness matrix, if requested

         IF ((EQCHK_OUTPUT(2) > 0) .OR. (EQCHK_OUTPUT(3) > 0) .OR. (EQCHK_OUTPUT(4) > 0) .OR. (EQCHK_OUTPUT(5) > 0)) THEN
            CALL ALLOCATE_RBGLOBAL ( 'N ', SUBR_NAME )
            IF (NDOFM > 0) THEN
               CALL TDOF_COL_NUM ( 'N ', N_SET_COL )
               DO I=1,NDOFG
                  N_SET_DOF = TDOFI(I,N_SET_COL)
                  IF (N_SET_DOF > 0) THEN
                     DO J=1,6
                        RBGLOBAL_NSET(N_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                     ENDDO
                  ENDIF
               ENDDO
            ELSE
               DO I=1,NDOFN
                  DO J=1,6
                     RBGLOBAL_NSET(I,J) = RBGLOBAL_GSET(I,J)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         IF (EQCHK_OUTPUT(2) > 0) THEN
            CALL OURTIM
            MODNAM = '  EQUILIBRIUM CHECK ON KNN                '
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL STIFF_MAT_EQUIL_CHK ( EQCHK_OUTPUT(2),'N ', SYM_KNN, NDOFN, NTERM_KNN, I_KNN, J_KNN, KNN, KNN_DIAG, KNN_MAX_DIAG,&
                                       RBGLOBAL_NSET)
         ENDIF

! If AUTOSPC = 'Y' check to see if any rows of KNN are null for DOF's that are not already in the S or O sets
! (in KGG_SINGULARIYT_PROC we passed by grids that were indep on MPC and some of these may have zero stiffness)
! Also, if user requested, check for small terms on the diag of KNN and AUTOSPC if their ratio to max diag term is small enough

         IF (AUTOSPC == 'Y') THEN
            IF ((AUTOSPC_NSET == 1) .OR. (AUTOSPC_NSET == 3)) THEN ! Check for null rows
            CALL OURTIM
            MODNAM = '  N SET AUTOSPC PROC #1                   '
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL N_SET_AUTOSPC_PROC_1
            ENDIF
            IF ((AUTOSPC_NSET == 2) .OR. (AUTOSPC_NSET == 3)) THEN ! Check for small diag terms
            CALL OURTIM
            MODNAM = '  N SET AUTOSPC PROC #2                   '
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
               CALL N_SET_AUTOSPC_PROC_2
            ENDIF
         ENDIF

! Now print final AUTOSPC summary table. Need to calc NUM_ASPC_BY_COMP from TDOF table

         CALL TDOF_COL_NUM ( 'SA', SA_SET_COL )
         
         DO J=1,6
            NUM_ASPC_BY_COMP(J) = 0
         ENDDO

         CALL OURTIM
         MODNAM = '  AUTOSPC SUMMARY TABLE                   '
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
         K = 0
         DO I=1,NGRID
            CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               K = K + 1
               IF (TDOF(K,SA_SET_COL) /= 0) THEN
                  NUM_ASPC_BY_COMP(J) = NUM_ASPC_BY_COMP(J) + 1
               ENDIF
            ENDDO
         ENDDO

         TOT_NUM_ASPC = 0
         DO J=1,6
            TOT_NUM_ASPC = TOT_NUM_ASPC + NUM_ASPC_BY_COMP(J)
         ENDDO

         ASPC_SUM_MSG1(1:) = 'Overall:'
         ASPC_SUM_MSG2(1:) = 'after identification of all AUTOSPC''s'
         ASPC_SUM_MSG3(1:) = 'overall      '
         CALL AUTOSPC_SUMMARY_MSGS ( ASPC_SUM_MSG1, ASPC_SUM_MSG2, ASPC_SUM_MSG3, 'Y', NUM_ASPC_BY_COMP )

! **********************************************************************************************************************************
      ELSE                                                 ! This is BUCKLING with LOAD_ISTEP = 2 (eigen part of BUCKLING)

         IF (NDOFM > 0) THEN

            CALL PARTITION_VEC (NDOFG,'G ','N ','M ',PART_VEC_G_NM)

            DO I=1,NDOFM
               PART_VEC_M(I) = 1
            ENDDO
    
            DO I=1,NSUB
               PART_VEC_SUB = 1
            ENDDO

! Reduce KGG to KNN

            IF (NTERM_KGGD > 0) THEN                          ! Reduce KGGD to KNND 

               CALL OURTIM
               MODNAM = '  REDUCE KGGD TO KNND (SPARSE MATRIX ROUTINES)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_KGGD_TO_KNND ( PART_VEC_G_NM )

            ELSE

               NTERM_KNND = 0
               NTERM_KNMD = 0
               NTERM_KMMD = 0
               CALL ALLOCATE_SPARSE_MAT ( 'KNND', NDOFN, NTERM_KNND, SUBR_NAME )

            ENDIF

            CALL DEALLOCATE_SPARSE_MAT ( 'RMG' ) 

! There is no M-set, so equate N and G sets

         ELSE

            CALL OURTIM
            MODNAM = '  EQUATING N-SET TO G-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFN      = NDOFG

            NTERM_KNND = NTERM_KGGD
            NTERM_KNMD = 0
            NTERM_KMMD = 0
            CALL ALLOCATE_SPARSE_MAT ( 'KNND', NDOFN, NTERM_KNND, SUBR_NAME )

         ENDIF

! Deallocate G-set arrays

         MODNAM = '  DEALLOCATE G-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         

         IF (DEALLOCATE_KGGD == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KGGD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KGGD' )
         ENDIF

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(2) == 1) .OR. ( PRTSTIFF(2) == 3)) THEN
            IF (NTERM_KNND > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KNND'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'N ', NTERM_KNND, NDOFN, I_KNND, J_KNND, KNND )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(2) == 2) .OR. ( PRTSTIFF(2) == 3)) THEN
            IF (NTERM_KNMD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KNMD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'N ', 'M ', NTERM_KNMD, NDOFN, I_KNMD, J_KNMD, KNMD )
            ENDIF
            IF (NTERM_KMMD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KMMD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'M ', 'M ', NTERM_KMMD, NDOFM, I_KMMD, J_KMMD, KMMD )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNMD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KNMD' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KMMD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KMMD' )

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(2) = 0 since we need KNN_DIAG, KNN_MAX_DIAG for the equilibrium check

         CALL GET_MATRIX_DIAG_STATS ( 'KNND', 'N ', NDOFN, NTERM_KNND, I_KNND, J_KNND, KNND, PRTSTIFD(2), KNND_DIAG, KNND_MAX_DIAG )

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

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE N_SET_AUTOSPC_PROC_1

! Checks KNN to see if any rows are null for DOF's not already in the S or O-sets, and, if so, puts these in the SA set and 
! reruns subr TDOF_PROC and writes the new TSET, TDOF, TDOFI tables to file L1C

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  DATA_NAM_LEN, NDOFG, NDOFSA, NGRID, NUM_PCHD_SPC1
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1C, L1C_MSG, LINK1C, SPC, SPCFIL
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_INFO, AUTOSPC_NSET, PCHSPC1, PRTTSET, SPC1SID
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI, TSET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID, GRID_SEQ

      IMPLICIT NONE

      CHARACTER(  7*BYTE)             :: ASPC_SUM_MSG1      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(100*BYTE)             :: ASPC_SUM_MSG2      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER( 13*BYTE)             :: ASPC_SUM_MSG3      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(132*BYTE)             :: TDOF_MSG           ! Msg to be printed out regarding at what point in the run the TDOF,I
!                                                             tables are printed out

      INTEGER(LONG)                   :: AGRID              ! Actual grid ID for IGRID
      INTEGER(LONG)                   :: COMP               ! DOF component number (1-6)
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM    ! Row number in array GRID_ID where AGRID is found
      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: IOCHK              ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: JSTART             ! DO loop start point
      INTEGER(LONG)                   :: NUM_ASPC_BY_COMP(6)! Number of AUTOSPC's by component number
      INTEGER(LONG)                   :: NUM_N_SET_ROWS_NULL! Number of rows in KNN that are null and are not S or O-set members
      INTEGER(LONG)                   :: N_SET_COL          ! Col no. in array TDOF where the  N-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: R_SET_COL          ! Col no. in array TDOF where the  R-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: S_SET_COL          ! Col no. in array TDOF where the  S-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: OUNT(2)            ! File units to write messages to. Input to subr UNFORMATTED_OPEN  

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

! Open file SPC to write SPC1 records if this subr finds singularities and user wants SPCFIL written

      IF (NUM_PCHD_SPC1 > 0) THEN                          ! Subr KGG_SINGULARITY_PROC already opened and wrote to this file
         OPEN (SPC, FILE=SPCFIL, STATUS='OLD', POSITION='APPEND', IOSTAT=IOCHK)
      ELSE                                                 ! File has not been written to, so open as replace
         OPEN (SPC, FILE=SPCFIL, STATUS='REPLACE', IOSTAT=IOCHK)
      ENDIF
      IF (IOCHK /= 0) THEN
         CALL OPNERR ( IOCHK, SPCFIL, OUNT, 'Y' )
         CALL FILERR ( OUNT, 'Y' )
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      CALL TDOF_COL_NUM ( 'N ',  N_SET_COL )
      CALL TDOF_COL_NUM ( 'R ',  R_SET_COL )
      CALL TDOF_COL_NUM ( 'S ',  S_SET_COL )

      WRITE(ERR,101) AUTOSPC_NSET, PROG_NAME
      IF (SUPINFO == 'N') THEN
         WRITE(F06,101) AUTOSPC_NSET, PROG_NAME
      ENDIF

! Look for null rows in KNN. If found, move that DOF to the SA set

      DO I=1,6                                             ! Initialize NUM_ASPC_BY_COMP
         NUM_ASPC_BY_COMP(I) = 0
      ENDDO 

      NUM_N_SET_ROWS_NULL = 0
      JSTART = 1
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
i_do: DO I=1,NDOFN
         WRITE(SC1,12345,ADVANCE='NO') I, NDOFN, CR13
         IF (I_KNN(I+1) == I_KNN(I)) THEN                  ! If true, row i is null
j_do:       DO J=JSTART,NDOFG                              ! Loop over rows of TDOFI to find where this N-set row is null
               IF (TDOFI(J,N_SET_COL) == I) THEN
                  IF ((TDOFI(J,S_SET_COL) == 0) .AND. (TDOFI(J,R_SET_COL) == 0)) THEN
                     NUM_N_SET_ROWS_NULL = NUM_N_SET_ROWS_NULL + 1
                     AGRID = TDOFI(J,1)
                     COMP  = TDOFI(J,2)
                     NUM_ASPC_BY_COMP(COMP) = NUM_ASPC_BY_COMP(COMP) + 1
                     CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
                     TSET ( GRID_ID_ROW_NUM, COMP ) = 'SA'
                     NDOFSA = NDOFSA + 1
                     IF (AUTOSPC_INFO == 'Y') THEN
                        WRITE(ERR,102) AGRID, COMP, AUTOSPC
                        IF (SUPINFO == 'N') THEN
                           WRITE(F06,102) AGRID, COMP, AUTOSPC
                        ENDIF
                     ENDIF
                     IF (PCHSPC1 == 'Y') THEN
                        WRITE(SPC,109) SPC1SID, COMP, AGRID
                        NUM_PCHD_SPC1 = NUM_PCHD_SPC1 + 1
                     ENDIF
                     JSTART = J
                     EXIT j_do
                  ENDIF
               ENDIF
            ENDDO j_do
         ENDIF
      ENDDO i_do
      WRITE(SC1,*) CR13

! Close SPC file

      IF (NUM_PCHD_SPC1 > 0) THEN
         CALL FILE_CLOSE ( SPC, SPCFIL,  'KEEP', 'Y' )
      ELSE
         CALL FILE_CLOSE ( SPC, SPCFIL,  'DELETE', 'Y' )
      ENDIF

! IF we changed some DOF's from the N-set to the SA-set regenerate TDOF, TDOFI tables and write them to L1C

      WRITE(F06,*)
      IF  (NUM_N_SET_ROWS_NULL > 0) THEN

         WRITE(ERR,103) PROG_NAME, NUM_N_SET_ROWS_NULL
         IF (SUPINFO == 'N') THEN
            WRITE(F06,103) PROG_NAME, NUM_N_SET_ROWS_NULL
         ENDIF

         IF (PRTTSET > 0) THEN
            WRITE(F06,56)
            WRITE(F06,57)
            DO J = 1,NGRID
               WRITE(F06,58) GRID(J,1), GRID_SEQ(J), (TSET(J,K),K = 1,6)
            ENDDO   
            WRITE(F06,'(//)')
         ENDIF

         ASPC_SUM_MSG1(1:) = 'Stage 2:'
         ASPC_SUM_MSG2(1:) = 'after identification of AUTOSPC''s to eliminate null rows in the N-set stiffness matrix'
         ASPC_SUM_MSG3(1:) = 'in this stage'
         CALL AUTOSPC_SUMMARY_MSGS ( ASPC_SUM_MSG1, ASPC_SUM_MSG2, ASPC_SUM_MSG3, 'N', NUM_ASPC_BY_COMP )

         TDOF_MSG(1:)  = ' '
         TDOF_MSG(22:) = ASPC_SUM_MSG2(1:)
         CALL TDOF_PROC ( TDOF_MSG )

         OUNT(1) = ERR
         OUNT(2) = F06
         CALL FILE_OPEN ( L1C, LINK1C, OUNT, 'REPLACE', L1C_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         CALL WRITE_DOF_TABLES
         CALL FILE_CLOSE ( L1C, LINK1C, 'KEEP', 'Y' )

      ELSE

         WRITE(ERR,104) PROG_NAME
         IF (SUPINFO == 'N') THEN
            WRITE(F06,104) PROG_NAME
         ENDIF

      ENDIF

! **********************************************************************************************************************************
   56 FORMAT(64X,'DEGREE OF FREEDOM SET TABLE (TSET)')

   57 FORMAT(33x,'     GRID SEQUENCE       T1       T2       T3       R1       R2       R3',/)

   58 FORMAT(33x,2(1X,I8),6(7X,A2))

  101 FORMAT(' *INFORMATION: BASED ON PARAMETER AUTOSPC_NSET = ',I2,1X,A,' IS CHECKING KNN TO SEE IF THERE ARE NULL ROWS',         &
                           ' THAT SHOULD BE AUTOSPC''d',/)

  102 FORMAT(' *INFORMATION: GRID POINT ',I8,' HAS SINGULARITY FOR DISPL COMPONENT    ',5X,I1,'. SINCE PARAM AUTOSPC = ',A,        &
                          ', THIS  WILL BE AUTOSPC''d')

  103 FORMAT(' *INFORMATION: ',A,' WILL AUTOSPC ',I8,' DOF''s FROM THE N-SET THAT WERE PREVIOUSLY NOT MEMBERS OF THE S-SET',/)

  104 FORMAT(' *INFORMATION: ',A,' FOUND NO N-SET DOF''s THAT WERE SINGULAR AND THAT WERE NOT ALREADY MEMBERS OF THE S-SET',/)

  109 FORMAT('SPC1    ',3I8)

12345 FORMAT('       Proc N-set DOF ',I8,' of ',I8,A)

! **********************************************************************************************************************************

      END SUBROUTINE N_SET_AUTOSPC_PROC_1

! ##################################################################################################################################

      SUBROUTINE N_SET_AUTOSPC_PROC_2

! Checks KNN to see if any diag terms are small (compared to AUTOSPC_RAT) for DOF's not already in the S or O-sets, and, if so, puts
! these in the SA set and reruns subr TDOF_PROC and writes the new TSET, TDOF, TDOFI tables to file L1C

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  DATA_NAM_LEN, NDOFN, NDOFG, NDOFSA, NGRID, NUM_PCHD_SPC1, PROG_NAME
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1C, L1C_MSG, LINK1C, SPC, SPCFIL
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_INFO, AUTOSPC_NSET, AUTOSPC_RAT, PCHSPC1, PRTTSET, SPC1SID
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI, TSET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID, GRID_SEQ
      USE SPARSE_MATRICES, ONLY       :  I_KFF, KFF

      IMPLICIT NONE

      CHARACTER(  7*BYTE)             :: ASPC_SUM_MSG1      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(100*BYTE)             :: ASPC_SUM_MSG2      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER( 13*BYTE)             :: ASPC_SUM_MSG3      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(132*BYTE)             :: TDOF_MSG           ! Msg to be printed out regarding at what point in the run the TDOF,I
!                                                             tables are printed out

      INTEGER(LONG)                   :: AGRID              ! Actual grid ID for IGRID
      INTEGER(LONG)                   :: COMP               ! DOF component number (1-6)
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM    ! Row number in array GRID_ID where AGRID is found
      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: IOCHK              ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: JSTART             ! DO loop start point
      INTEGER(LONG)                   :: NUM_ASPC_BY_COMP(6)! Number of AUTOSPC's by component number
      INTEGER(LONG)                   :: NUM_NSET_DOFS_SPCD ! Number of rows in KNN that are null and are not S or O-set members
      INTEGER(LONG)                   :: N_SET_COL          ! Col no. in array TDOF where the  N-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: R_SET_COL          ! Col no. in array TDOF where the  R-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: S_SET_COL          ! Col no. in array TDOF where the  S-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: OUNT(2)            ! File units to write messages to. Input to subr UNFORMATTED_OPEN  

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

! Open file SPC to write SPC1 records if this subr finds singularities

      IF (NUM_PCHD_SPC1 > 0) THEN                          ! Subr KGG_SINGULARITY_PROC already opened and wrote to this file
         OPEN (SPC, FILE=SPCFIL, STATUS='OLD', POSITION='APPEND', IOSTAT=IOCHK)
      ELSE                                                 ! File has not been written to, so open as replace
         OPEN (SPC, FILE=SPCFIL, STATUS='REPLACE', IOSTAT=IOCHK)
      ENDIF
      IF (IOCHK /= 0) THEN
         CALL OPNERR ( IOCHK, SPCFIL, OUNT, 'Y' )
         CALL FILERR ( OUNT, 'Y' )
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      CALL TDOF_COL_NUM ( 'N ',  N_SET_COL )
      CALL TDOF_COL_NUM ( 'R ',  R_SET_COL )
      CALL TDOF_COL_NUM ( 'S ',  S_SET_COL )

      WRITE(ERR,101) AUTOSPC_NSET, PROG_NAME, AUTOSPC_RAT
      IF (SUPINFO == 'N') THEN
         WRITE(F06,101) AUTOSPC_NSET, PROG_NAME, AUTOSPC_RAT
      ENDIF

! Check ratios of diag to max diag term. If smaller than requirement, move that DOF to the SA set

      DO I=1,6                                             ! Initialize NUM_ASPC_BY_COMP
         NUM_ASPC_BY_COMP(I) = 0
      ENDDO 

      NUM_NSET_DOFS_SPCD = 0
      JSTART = 1
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
i_do: DO I=1,NDOFN
         WRITE(SC1,22345,ADVANCE='NO') I, NDOFN, CR13
         IF ((DABS(KNN_DIAG(I)/KNN_MAX_DIAG) < AUTOSPC_RAT) .OR. (KNN_DIAG(I) < ZERO)) THEN
j_do:       DO J=JSTART,NDOFG                               ! Loop over rows of TDOFI to find where this N-set row is null
               IF (TDOFI(J,N_SET_COL) == I) THEN
                  IF ((TDOFI(J,S_SET_COL) == 0) .AND. (TDOFI(J,R_SET_COL) == 0)) THEN
                     NUM_NSET_DOFS_SPCD = NUM_NSET_DOFS_SPCD + 1
                     AGRID = TDOFI(J,1)
                     COMP  = TDOFI(J,2)
                     NUM_ASPC_BY_COMP(COMP) = NUM_ASPC_BY_COMP(COMP) + 1
                     CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
                     TSET ( GRID_ID_ROW_NUM, COMP ) = 'SA'
                     NDOFSA = NDOFSA + 1
                     IF (AUTOSPC_INFO == 'Y') THEN
                        WRITE(ERR,102) AGRID, COMP, AUTOSPC
                        IF (SUPINFO == 'N') THEN
                           WRITE(F06,102) AGRID, COMP, AUTOSPC
                        ENDIF
                     ENDIF
                     IF (PCHSPC1 == 'Y') THEN
                        WRITE(SPC,109) SPC1SID, COMP, AGRID
                        NUM_PCHD_SPC1 = NUM_PCHD_SPC1 + 1
                     ENDIF
                     JSTART = J
                     EXIT j_do
                  ENDIF
               ENDIF
            ENDDO j_do
         ENDIF
      ENDDO i_do
      WRITE(SC1,*) CR13

! Close SPC file

      IF (NUM_PCHD_SPC1 > 0) THEN
         CALL FILE_CLOSE ( SPC, SPCFIL,  'KEEP', 'Y' )
      ELSE
         CALL FILE_CLOSE ( SPC, SPCFIL,  'DELETE', 'Y' )
      ENDIF

! IF we changed some DOF's from the N-set to the SA-set regenerate TDOF, TDOFI tables and write them to L1C

      WRITE(F06,*)
      IF  (NUM_NSET_DOFS_SPCD > 0) THEN

         WRITE(ERR,103) PROG_NAME, NUM_NSET_DOFS_SPCD
         IF (SUPINFO == 'N') THEN
            WRITE(F06,103) PROG_NAME, NUM_NSET_DOFS_SPCD
         ENDIF

         IF (PRTTSET > 0) THEN
            WRITE(F06,56)
            WRITE(F06,57)
            DO J = 1,NGRID
               WRITE(F06,58) GRID(J,1), GRID_SEQ(J), (TSET(J,K),K = 1,6)
            ENDDO   
            WRITE(F06,'(//)')
         ENDIF

         ASPC_SUM_MSG1(1:) = 'Stage 3:'
         ASPC_SUM_MSG2(1:) ='after identification of AUTOSPC''s to eliminate N-set DOFs with stiffness ratios < PARAM AUTOSPC_RAT'
         ASPC_SUM_MSG3(1:) = 'in this stage'
         CALL AUTOSPC_SUMMARY_MSGS ( ASPC_SUM_MSG1, ASPC_SUM_MSG2, ASPC_SUM_MSG3, 'Y', NUM_ASPC_BY_COMP )

         TDOF_MSG(1:)  = ' '
         TDOF_MSG(16:) = ASPC_SUM_MSG2(1:)
         CALL TDOF_PROC ( TDOF_MSG )

         OUNT(1) = ERR
         OUNT(2) = F06
         CALL FILE_OPEN ( L1C, LINK1C, OUNT, 'REPLACE', L1C_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

         CALL WRITE_DOF_TABLES

         CALL FILE_CLOSE ( L1C, LINK1C, 'KEEP', 'Y' )

      ELSE

         WRITE(ERR,104) PROG_NAME, AUTOSPC_RAT
         IF (SUPINFO == 'N') THEN
            WRITE(F06,104) PROG_NAME, AUTOSPC_RAT
         ENDIF

      ENDIF

! **********************************************************************************************************************************
   56 FORMAT(64X,'DEGREE OF FREEDOM SET TABLE (TSET)')

   57 FORMAT(33x,'     GRID SEQUENCE       T1       T2       T3       R1       R2       R3',/)

   58 FORMAT(33x,2(1X,I8),6(7X,A2))

  101 FORMAT(' *INFORMATION: BASED ON PARAMETER AUTOSPC_NSET = ',I2,1X,A,' IS CHECKING KNN TO SEE IF THERE ARE DOF''s THAT ARE'    &
                          ,' NOT ALREADY IN THE'                                                                                   &
                    ,/,14X,' S-SET BUT SHOULD BE AUTOSPC''d BASED ON SMALL DIAGONAL TERMS WHOSE RATIO WITH MAX DIAGONAL TERM IS < '&
                    ,1ES13.6,/)

  102 FORMAT(' *INFORMATION: GRID POINT ',I8,' HAS SINGULARITY FOR DISPL COMPONENT    ',5X,I1,'. SINCE PARAM AUTOSPC = ',A,        &
                          ', THIS  WILL BE AUTOSPC''d')

  103 FORMAT(' *INFORMATION: ',A,' HAS AUTOSPC''d ',I8,' DOF''s FROM THE N-SET THAT WERE PREVIOUSLY NOT MEMBERS OF THE S-SET',/)

  104 FORMAT(' *INFORMATION: ',A,' FOUND NO N-SET DOF''s THAT HAD SMALL DIAG TERMS (RATIO TO MAX DIAG TERM < ',1ES15.6,')'         &
                    ,/,14X,' AND THAT WERE NOT ALREADY MEMBERS OF THE S-SET',/)

  105 FORMAT('               AUTOSPC_RAT = ',1ES13.6)

  109 FORMAT('SPC1    ',3I8)

22345 FORMAT('       Proc N-set DOF ',I8,' of ',I8,A)

! **********************************************************************************************************************************

      END SUBROUTINE N_SET_AUTOSPC_PROC_2

      END SUBROUTINE REDUCE_G_NM

