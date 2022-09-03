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

      SUBROUTINE REDUCE_N_FS
 
! Call routines to reduce stiffness, mass, loads from N-set to F, S-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1H, LINK1H, L1H_MSG, L2C, LINK2C, L2C_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  LINKNO    , NDOFF, NDOFG, NDOFN, NDOFS, NDOFSE, NSUB,                                     &
                                         NTERM_KNN , NTERM_KFF , NTERM_KFS , NTERM_KSS , NTERM_KSSe ,                              &
                                         NTERM_KNND, NTERM_KFFD, NTERM_KFSD, NTERM_KSSD, NTERM_KSSDe,                              &
                                         NTERM_QSYS, NTERM_PN  , NTERM_PF  , NTERM_PS  , NTERM_MNN  ,                              &
                                         NTERM_MFF , NTERM_MFS , NTERM_MSS , SOL_NAME, BLNK_SUB_NAM        
      USE TIMDAT, ONLY                :  TSEC, YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME       
      USE CONSTANTS_1, ONLY           :  ONE
      USE DOF_TABLES, ONLY            :  TDOFI
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_NSET, RBGLOBAL_FSET
      USE PARAMS, ONLY                :  EQCHK_OUTPUT, MATSPARS, PRTSTIFD, PRTSTIFF, PRTMASS, PRTFOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_N_FS_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KNN  , J_KNN  , KNN  , I_KFF  , J_KFF  , KFF  , I_KFS  , J_KFS  , KFS  ,                &
                                         I_KSS  , J_KSS  , KSS  , I_KSSe , J_KSSe , KSSe ,                                         &
                                         I_KNND , J_KNND , KNND , I_KFFD , J_KFFD , KFFD , I_KFSD , J_KFSD , KFSD ,                &
                                         I_KSSD , J_KSSD , KSSD , I_KSSDe, J_KSSDe, KSSDe,                                         &
                                         I_MNN  , J_MNN  , MNN  , I_MFF  , J_MFF  , MFF  , I_MFS  , J_MFS  , MFS  ,                &
                                         I_MSS  , J_MSS  , MSS  ,                                                                  &
                                         I_PN   , J_PN   , PN   , I_PF   , J_PF   , PF   , I_PS   , J_PS   , PS   ,                &
                                         I_MSF  , J_MSF  , MSF  ,                                                                  &
                                         I_QSYS , J_QSYS , QSYS
      USE SPARSE_MATRICES, ONLY       :  SYM_KFF, SYM_KSSe
      USE COL_VECS, ONLY              :  YSe
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SCRATCH_MATRICES
 
      USE REDUCE_N_FS_USE_IFs
                   
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_N_FS'
                                                               ! If 'Y' then matrix is stored as all nonzeros on & above diag.
      CHARACTER(  1*BYTE)             :: CLOSE_IT              ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close file or not 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT            ! Char constant for the CLOSE status of a file
      CHARACTER( 44*BYTE)             :: MODNAM                ! Name to write to screen to describe module being run
      CHARACTER(132*BYTE)             :: MATRIX_NAME           ! Name of matrix for printout
 
      INTEGER(LONG)                   :: AROW_MAX_TERMS        ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: DO_WHICH_CODE_FRAG    ! 1 or 2 depending on which seg of code to run (depends on BUCKLING)
      INTEGER(LONG)                   :: F_SET_COL             ! Col no. in array TDOFI where the F-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: F_SET_DOF             ! F-set DOF number
      INTEGER(LONG)                   :: IERROR                ! Error count
      INTEGER(LONG)                   :: IOCHK                 ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: I,J                   ! DO loop indices               
      INTEGER(LONG)     , PARAMETER   :: NUM_YS_COLS = 1       ! Variable for number of cols in array YSe 
      INTEGER(LONG)                   :: OUNT(2)               ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: PART_VEC_F(NDOFF)     ! Partitioning vector (1's for all of F set) 
      INTEGER(LONG)                   :: PART_VEC_N_FS(NDOFN)  ! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG)                   :: PART_VEC_S(NDOFS)     ! Partitioning vector (1's for all of S set) 
      INTEGER(LONG)                   :: PART_VEC_S_SzSe(NDOFS)! Partitioning vector (S set into SZ and SE sets) 
      INTEGER(LONG)                   :: PART_VEC_SUB(NSUB)    ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG)                   :: REC_NO                ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_N_FS_BEGEND

      REAL(DOUBLE)                    :: KFF_DIAG(NDOFF)       ! Diagonal terms from KFF
      REAL(DOUBLE)                    :: KFF_MAX_DIAG          ! Max diag term from KFF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Set partitioning vectors

      CALL PARTITION_VEC ( NDOFN, 'N ', 'F ', 'S ', PART_VEC_N_FS )

      CALL PARTITION_VEC ( NDOFS, 'S ', 'SZ', 'SE', PART_VEC_S_SzSe)

      DO I=1,NDOFF
         PART_VEC_F(I) = 1
      ENDDO
 
      DO I=1,NDOFS
         PART_VEC_S(I) = 1
      ENDDO

      DO I=1,NSUB
         PART_VEC_SUB = 1
      ENDDO

! Read enforced displ's if there are any

      CALL ALLOCATE_COL_VEC ( 'YSe', NDOFSE, SUBR_NAME )
      IF (NDOFSE > 0) THEN

         CALL FILE_OPEN ( L1H, LINK1H, OUNT, 'OLD', L1H_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

         IERROR = 0
         DO I=1,NDOFSE
            READ(L1H,IOSTAT=IOCHK) YSe(I)
            IF (IOCHK /= 0) THEN
               IERROR = IERROR + 1
               REC_NO = I
               CALL READERR ( IOCHK, LINK1H, L1H_MSG, REC_NO, OUNT, 'Y' )
            ENDIF
         ENDDO
         IF (IERROR /= 0) THEN
            WRITE(ERR,9995) LINKNO,IERROR
            WRITE(F06,9995) LINKNO,IERROR
            CALL OUTA_HERE ( 'Y' )                                 ! Quit due to read errors in YSe array file
         ENDIF
  
         CALL FILE_CLOSE ( L1H, LINK1H, 'KEEP', 'Y' )

         IF (DEBUG(26) == 1) THEN
            CALL WRITE_VECTOR ( 'SE-SET YS ENFORCED DISPLS','DISPL', NDOFSE, YSe )
         ENDIF

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

         IF (NDOFS > 0) THEN                                  ! If NDOFS > 0 reduce KNN to KFF

! Reduce KNN to KFF

            IF (NTERM_KNN > 0) THEN

               CALL OURTIM
               MODNAM = '  REDUCE KNN TO KFF (PARTITION, ONLY)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_KNN_TO_KFF ( PART_VEC_N_FS, PART_VEC_S_SzSe, PART_VEC_F, PART_VEC_S )

            ELSE

               NTERM_KFF = 0
               NTERM_KFS = 0
               NTERM_KSS = 0
               CALL ALLOCATE_SPARSE_MAT ( 'KFF', NDOFF, NTERM_KFF, SUBR_NAME )
               WRITE(SC1,*) CR13

            ENDIF

! Reduce MNN to MFF

            IF (NTERM_MNN > 0) THEN

               CALL OURTIM
               MODNAM = '  REDUCE MNN TO MFF (PARTITION, ONLY)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_MNN_TO_MFF ( PART_VEC_N_FS )

            ELSE

               NTERM_MFF = 0
               NTERM_MFS = 0
               NTERM_MSS = 0
               CALL ALLOCATE_SPARSE_MAT ( 'MFF', NDOFF, NTERM_MFF, SUBR_NAME )

            ENDIF

! Reduce PN to PF. 

            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               CALL OURTIM
               IF (MATSPARS == 'Y') THEN
                  MODNAM = '  REDUCE PN TO PF (SPARSE MATRIX ROUTINES)'
               ELSE
                  MODNAM = '  REDUCE PN TO PF (FULL MATRIX ROUTINES)'
               ENDIF
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_PN_TO_PF ( PART_VEC_N_FS, PART_VEC_SUB )

            ENDIF

         ELSE

! There is no S-set, so equate N, F sets

            CALL OURTIM
            MODNAM = '  EQUATING F-SET TO N-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFF     = NDOFN

            NTERM_KFF = NTERM_KNN
            NTERM_KFS = 0
            NTERM_KSS = 0

            NTERM_MFF = NTERM_MNN
            NTERM_MFS = 0
            NTERM_MSS = 0

            NTERM_PF  = NTERM_PN
            NTERM_PS  = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KFF', NDOFF, NTERM_KFF, SUBR_NAME )

!xx         DO I=1,NDOFF+1
!xx            I_KFF(I) = I_KNN(I)
!xx         ENDDO
!xx
!xx         DO I=1,NTERM_KFF
!xx            J_KFF(I) = J_KNN(I)
!xx              KFF(I) =   KNN(I)
!xx         ENDDO
!xx
            CALL ALLOCATE_SPARSE_MAT ( 'MFF', NDOFF, NTERM_MFF, SUBR_NAME )

!xx         DO I=1,NDOFF+1
!xx            I_MFF(I) = I_MNN(I)
!xx         ENDDO
!xx
!xx         DO I=1,NTERM_MFF
!xx            J_MFF(I) = J_MNN(I)
!xx              MFF(I) =   MNN(I)
!xx         ENDDO

            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               CALL ALLOCATE_SPARSE_MAT ( 'PF', NDOFF, NTERM_PF, SUBR_NAME )

!xx            DO I=1,NDOFF+1
!xx               I_PF(I)  = I_PN(I)
!xx            ENDDO
!xx
!xx            DO I=1,NTERM_PF
!xx               J_PF(I) = J_PN(I)
!xx                 PF(I) =   PN(I)
!xx            ENDDO

            ENDIF

         ENDIF

! Deallocate N-set arrays

         MODNAM = '   DEALLOCATE SOME ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNN', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KNN' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MNN', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MNN' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PN ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PN' )
         WRITE(SC1,*) CR13

! Calc QSYS = KSSe*YSe and write to L2C for constraint force recovery in LINK9.
  
            IF (NTERM_KSSe > 0) THEN                          ! Calc QSYS = KSSe * YSe

               CALL MATMULT_SFS_NTERM ( 'KSSe', NDOFS, NTERM_KSSe, SYM_KSSe, I_KSSe, J_KSSe                                        &
                                        ,'YSe', NDOFSE, NUM_YS_COLS, YSe, AROW_MAX_TERMS, 'QSYS', NTERM_QSYS )

               CALL ALLOCATE_SPARSE_MAT ( 'QSYS', NDOFS, NTERM_QSYS, SUBR_NAME )
 
               IF ((DEBUG(24) == 2) .OR.(DEBUG(24) == 3)) THEN
                  MATRIX_NAME = 'STIFFNESS PARTITION KSSe (columns of KSS for enforced displs)'
                  CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'S ', 'SE', NTERM_KSSe, NDOFS, I_KSSe, J_KSSe, KSSe )
               ENDIF

               IF (NTERM_QSYS > 0) THEN

                  CALL MATMULT_SFS ( 'KSSe', NDOFS, NTERM_KSSe, SYM_KSSe, I_KSSe, J_KSSe, KSSe                                     &
                                    ,'YSe', NDOFSE, NUM_YS_COLS, YSe, AROW_MAX_TERMS, 'QSYS', ONE, NTERM_QSYS, I_QSYS, J_QSYS, QSYS)
               
                  CLOSE_IT   = 'Y'
                  CLOSE_STAT = 'KEEP'
                  CALL WRITE_MATRIX_1 ( LINK2C, L2C, CLOSE_IT, CLOSE_STAT, L2C_MSG, 'QSYS', NTERM_QSYS, NDOFS, I_QSYS, J_QSYS, QSYS)

               ENDIF

            ENDIF

! Print out QSYS

         IF ((DEBUG(25) == 2) .OR.(DEBUG(25) == 3)) THEN      ! DEBUG(25) controls output of QSYS
            IF (NTERM_QSYS > 0) THEN
               MATRIX_NAME = 'MATRIX QSYS = KSSe*YSe (portion of SPC forces due to enforced displs)'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'S ', '  ', NTERM_QSYS, NDOFS, I_QSYS, J_QSYS, QSYS )
            ENDIF
         ENDIF

         MODNAM = '   DEALLOCATE SOME ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KSSe', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KSSe' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate QSYS', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'QSYS' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate YSe ', CR13   ;   CALL DEALLOCATE_COL_VEC ( 'YSe' )
         WRITE(SC1,*) CR13

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(3) == 1) .OR. ( PRTSTIFF(3) == 3)) THEN
            IF (NTERM_KFF > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KFF'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'F ', NTERM_KFF, NDOFF, I_KFF, J_KFF, KFF )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(3) == 2) .OR. ( PRTSTIFF(3) == 3)) THEN
            IF (NTERM_KFS > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KFS'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'S ', NTERM_KFS, NDOFF, I_KFS, J_KFS, KFS )
            ENDIF
            IF (NTERM_KSS > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KSS'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'S ', 'S ', NTERM_KSS, NDOFS, I_KSS, J_KSS, KSS )
            ENDIF
         ENDIF

         WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KFS', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KFS' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KSF', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KSF' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KSS', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KSS' )
         WRITE(SC1,*) CR13

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(3) = 0 since we need KFF_DIAG, KFF_MAX_DIAG for the equilibrium check

         IF (NDOFF > 0) THEN
            CALL GET_MATRIX_DIAG_STATS ( 'KFF', 'F ', NDOFF, NTERM_KFF, I_KFF, J_KFF, KFF, PRTSTIFD(3), KFF_DIAG, KFF_MAX_DIAG )
         ENDIF

! Print out mass matrix partitions, if requested

         IF (( PRTMASS(3) == 1) .OR. ( PRTMASS(3) == 3)) THEN
            IF (NTERM_MFF > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MFF'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'F ', NTERM_MFF, NDOFF, I_MFF, J_MFF, MFF )
            ENDIF
         ENDIF

         IF (( PRTMASS(3) == 2) .OR. ( PRTMASS(3) == 3)) THEN
            IF (NTERM_MFS > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MFS'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'S ', NTERM_MFS, NDOFF, I_MFS, J_MFS, MFS )
            ENDIF
            IF (NTERM_MSS > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MSS'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'S ', 'S ', NTERM_MSS, NDOFS, I_MSS, J_MSS, MSS )
            ENDIF
         ENDIF

         WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MFS', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MFS' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MSF', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MSF' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MSS', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MSS' )
         WRITE(SC1,*) CR13

! Print out load matrix partitions, if requested

         IF (( PRTFOR(3) == 1) .OR. ( PRTFOR(3) == 3)) THEN
            IF (NTERM_PF  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PF'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'SUBCASE', NTERM_PF, NDOFF, I_PF, J_PF, PF )
            ENDIF
         ENDIF

         IF (( PRTFOR(3) == 2) .OR. ( PRTFOR(3) == 3)) THEN
            IF (NTERM_PS  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PS'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'S ', 'SUBCASE', NTERM_PS, NDOFS, I_PS, J_PS, PS )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PS ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PS' )
         WRITE(SC1,*) CR13

! Do equilibrium check on the F-set stiffness matrix, if requested

         IF ((EQCHK_OUTPUT(3) > 0) .OR. (EQCHK_OUTPUT(4) > 0) .OR. (EQCHK_OUTPUT(5) > 0)) THEN
            CALL ALLOCATE_RBGLOBAL ( 'F ', SUBR_NAME )
            IF (NDOFS > 0) THEN
               CALL TDOF_COL_NUM ( 'F ', F_SET_COL )
               DO I=1,NDOFG
                  F_SET_DOF = TDOFI(I,F_SET_COL)
                  IF (F_SET_DOF > 0) THEN
                     DO J=1,6
                        RBGLOBAL_FSET(F_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                     ENDDO
                  ENDIF
               ENDDO
            ELSE
               DO I=1,NDOFF
                  DO J=1,6
                     RBGLOBAL_FSET(I,J) = RBGLOBAL_NSET(I,J)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         IF (EQCHK_OUTPUT(3) > 0) THEN
            CALL OURTIM
            MODNAM = '   EQUILIBRIUM CHECK ON KFF                '
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL STIFF_MAT_EQUIL_CHK ( EQCHK_OUTPUT(3),'F ', SYM_KFF, NDOFF, NTERM_KFF, I_KFF, J_KFF, KFF, KFF_DIAG, KFF_MAX_DIAG, &
                                       RBGLOBAL_FSET)
         ENDIF
         CALL DEALLOCATE_RBGLOBAL ( 'N ' )

! **********************************************************************************************************************************
      ELSE                                                 ! This is BUCKLING with LOAD_ISTEP = 2 (eigen part of BUCKLING)

         IF (NDOFS > 0) THEN                                  ! If NDOFS > 0 reduce KNN to KFF

! Reduce KNND to KFFD

            IF (NTERM_KNND > 0) THEN

               CALL OURTIM
               MODNAM = '  REDUCE KNND TO KFFD (PARTITION, ONLY)'
               WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

               CALL REDUCE_KNND_TO_KFFD ( PART_VEC_N_FS, PART_VEC_S_SzSe, PART_VEC_F, PART_VEC_S )

            ELSE

               NTERM_KFFD = 0
               NTERM_KFSD = 0
               NTERM_KSSD = 0
               CALL ALLOCATE_SPARSE_MAT ( 'KFFD', NDOFF, NTERM_KFFD, SUBR_NAME )

            ENDIF

         ELSE

! There is no S-set, so equate N, F sets

            CALL OURTIM
            MODNAM = '  EQUATING F-SET TO N-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFF     = NDOFN

            NTERM_KFFD = NTERM_KNND
            NTERM_KFSD = 0
            NTERM_KSSD = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KFFD', NDOFF, NTERM_KFFD, SUBR_NAME )

         ENDIF

! Deallocate N-set arrays

         MODNAM = '   DEALLOCATE SOME ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KNND', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KNND' )
         WRITE(SC1,*) CR13

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(3) == 1) .OR. ( PRTSTIFF(3) == 3)) THEN
            IF (NTERM_KFFD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KFFD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'F ', NTERM_KFFD, NDOFF, I_KFFD, J_KFFD, KFFD )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(3) == 2) .OR. ( PRTSTIFF(3) == 3)) THEN
            IF (NTERM_KFSD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KFSD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'S ', NTERM_KFSD, NDOFF, I_KFSD, J_KFSD, KFSD )
            ENDIF
            IF (NTERM_KSSD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KSSD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'S ', 'S ', NTERM_KSSD, NDOFS, I_KSSD, J_KSSD, KSSD )
            ENDIF
         ENDIF

         WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate YSe ', CR13   ;   CALL DEALLOCATE_COL_VEC    ( 'YSe' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KFSD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KFSD' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KSFD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KSFD' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KSSD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KSSD' )
         WRITE(SC1,*) CR13

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

      END SUBROUTINE REDUCE_N_FS   
