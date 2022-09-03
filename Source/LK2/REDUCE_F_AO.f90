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

      SUBROUTINE REDUCE_F_AO
 
! Call routines to reduce stiffness, mass, loads from F-set to A, O-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LINKNO, KOO_SDIA, NDOFF, NDOFG, NDOFA, NDOFO, NSUB, SOL_NAME,               &
                                         NTERM_KFF , NTERM_KAA , NTERM_KAO , NTERM_KOO ,                                           &
                                         NTERM_KFFD, NTERM_KAAD, NTERM_KAOD, NTERM_KOOD,                                           &
                                         NTERM_MFF , NTERM_MAA , NTERM_MAO , NTERM_MOO ,                                           &
                                         NTERM_PF  , NTERM_PA  , NTERM_PO  , NTERM_GOA
      USE PARAMS, ONLY                :  EQCHK_OUTPUT, MATSPARS, PRTSTIFD, PRTSTIFF, PRTMASS, PRTFOR, SOLLIB, SPARSE_FLAVOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE DOF_TABLES, ONLY            :  TDOFI
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET, RBGLOBAL_FSET, RBGLOBAL_ASET
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_F_AO_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KFF , J_KFF , KFF , I_KAA , J_KAA , KAA , I_KAO , J_KAO , KAO , I_KOO , J_KOO , KOO ,   &
                                         I_KFFD, J_KFFD, KFFD, I_KAAD, J_KAAD, KAAD, I_KAOD, J_KAOD, KAOD, I_KOOD, J_KOOD, KOOD,   &
                                         I_MFF , J_MFF , MFF , I_MAA , J_MAA , MAA , I_MAO , J_MAO , MAO , I_MOO , J_MOO , MOO ,   &
                                         I_PF  , J_PF  , PF  , I_PA  , J_PA  , PA  , I_PO  , J_PO  , PO
      USE SPARSE_MATRICES, ONLY       :  SYM_KAA
      USE SCRATCH_MATRICES
      USE SuperLU_STUF, ONLY          :  SLU_FACTORS, SLU_INFO
 
      USE REDUCE_F_AO_USE_IFs

      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_F_AO'
      CHARACTER(132*BYTE)             :: MATRIX_NAME         ! Name of matrix for printout 
      CHARACTER(44*BYTE)              :: MODNAM              ! Name to write to screen to describe module being run
 
      INTEGER(LONG)                   :: A_SET_COL           ! Col no. in array TDOFI where the A-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: A_SET_DOF           ! A-set DOF number
      INTEGER(LONG)                   :: DO_WHICH_CODE_FRAG    ! 1 or 2 depending on which seg of code to run (depends on BUCKLING)
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: PART_VEC_F_AO(NDOFF)! Partitioning vector (G set into N and M sets) 
      INTEGER(LONG)                   :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_F_AO_BEGEND

      REAL(DOUBLE)                    :: DUM_COL(NDOFO)      ! Temp variable used in SuperLU
      REAL(DOUBLE)                    :: KAA_DIAG(NDOFA)     ! Diagonal terms from KAA
      REAL(DOUBLE)                    :: KAA_MAX_DIAG        ! Max diag term  from KAA
      REAL(DOUBLE)                    :: KAAD_DIAG(NDOFA)    ! Diagonal terms from KAAD
      REAL(DOUBLE)                    :: KAAD_MAX_DIAG       ! Max diag term  from KAAD

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
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

! If there is an O-set, reduce KFF to KAA, MFF to MAA, PF to PA using UO = GOA*UA + UO0, where GOA = -KOO(-1)*KAO', UO0 = KOO(-1)*PO
! If there is no O-set, then equate KAA to KFF, MAA to MFF, PA to PF

         IF (NDOFO > 0) THEN
                                                           ! First, need to create part vectors used in the reduction (if NDOFO > 0)
            CALL PARTITION_VEC (NDOFF,'F ','A ','O ',PART_VEC_F_AO)

            DO I=1,NSUB
               PART_VEC_SUB = 1
            ENDDO

            CALL OURTIM                                    ! Reduce KFF to KAA 
            IF (MATSPARS == 'Y') THEN
               MODNAM = 'REDUCE KFF TO KAA (SPARSE MATRIX ROUTINES)'
            ELSE
               MODNAM = 'REDUCE KFF TO KAA (FULL MATRIX ROUTINES)'
            ENDIF
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL REDUCE_KFF_TO_KAA ( PART_VEC_F_AO )

            CALL OURTIM                                    ! Reduce MFF to MAA 
            IF (MATSPARS == 'Y') THEN
               MODNAM = 'REDUCE MFF TO MAA (SPARSE MATRIX ROUTINES)'
            ELSE
               MODNAM = 'REDUCE MFF TO MAA (FULL MATRIX ROUTINES)'
            ENDIF
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL REDUCE_MFF_TO_MAA ( PART_VEC_F_AO )


            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               IF (NTERM_PF > 0) THEN                      ! Reduce PF to PA 

                  CALL OURTIM
                  IF (MATSPARS == 'Y') THEN
                     MODNAM = 'REDUCE PF  TO PA  (SPARSE MATRIX ROUTINES)'
                  ELSE
                     MODNAM = 'REDUCE PF  TO PA  (FULL MATRIX ROUTINES)'
                  ENDIF
                  WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

                  CALL REDUCE_PF_TO_PA ( PART_VEC_F_AO, PART_VEC_SUB )

               ELSE

                  NTERM_PA = 0
                  NTERM_PO = 0
                  CALL ALLOCATE_SPARSE_MAT ( 'PA', NDOFA, NTERM_PA, SUBR_NAME )

               ENDIF

            ENDIF

FreeS:      IF (SOLLIB == 'SPARSE  ') THEN                       ! Last, free the storage allocated inside SuperLU

               IF (SPARSE_FLAVOR(1:7) == 'SUPERLU') THEN

                  DO J=1,NDOFO
                     DUM_COL(J) = ZERO
                  ENDDO

                  CALL C_FORTRAN_DGSSV( 3, NDOFO, NTERM_KOO, 1, KOO , I_KOO , J_KOO , DUM_COL, NDOFO, SLU_FACTORS, SLU_INFO )

                  IF (SLU_INFO .EQ. 0) THEN
                     WRITE (*,*) 'SUPERLU STORAGE FREED'
                  ELSE
                     WRITE(*,*) 'SUPERLU STORAGE NOT FREED. INFO FROM SUPERLU FREE STORAGE ROUTINE = ', SLU_INFO
                  ENDIF

               ENDIF

            ENDIF FreeS
 
         ELSE                                              ! There is no O-set, so equate F, A sets

            CALL OURTIM
            MODNAM = 'EQUATING A-SET TO F-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFA     = NDOFF

            NTERM_KAA = NTERM_KFF
            NTERM_KAO = 0
            NTERM_KOO = 0

            NTERM_MAA = NTERM_MFF
            NTERM_MAO = 0
            NTERM_MOO = 0

            NTERM_PA  = NTERM_PF
            NTERM_PO  = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KAA', NDOFA, NTERM_KAA, SUBR_NAME )

!xx      DO I=1,NDOFA+1
!xx         I_KAA(I) = I_KFF(I)
!xx      ENDDO
!xx
!xx      DO I=1,NTERM_KAA
!xx         J_KAA(I) = J_KFF(I)
!xx           KAA(I) =   KFF(I)
!xx      ENDDO

            CALL ALLOCATE_SPARSE_MAT ( 'MAA', NDOFA, NTERM_MAA, SUBR_NAME )

!xx      DO I=1,NDOFA+1
!xx         I_MAA(I) = I_MFF(I)
!xx      ENDDO
!xx
!xx      DO I=1,NTERM_MAA
!xx         J_MAA(I) = J_MFF(I)
!xx           MAA(I) =   MFF(I)
!xx      ENDDO

            IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL')) THEN

               CALL ALLOCATE_SPARSE_MAT ( 'PA', NDOFA, NTERM_PA, SUBR_NAME )

!xx         DO I=1,NDOFA+1
!xx            I_PA(I)  = I_PF(I)
!xx         ENDDO
!xx
!xx         DO I=1,NTERM_PA
!xx            J_PA(I) = J_PF(I)
!xx              PA(I) =   PF(I)
!xx         ENDDO

            ENDIF

         ENDIF

! Deallocate F set arrays

         MODNAM = 'DEALLOCATE F-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KFF  ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KFF' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MFF  ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MFF' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PF   ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PF' )
         WRITE(SC1,*) CR13

! Deallocate ABAND (which was KOO before decomp and TOO later) and GOA

         MODNAM = 'DEALLOCATE GOA, ABAND ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GOA  ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'GOA' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate ABAND', CR13   ;   CALL DEALLOCATE_LAPACK_MAT ( 'ABAND' )
         WRITE(SC1,*) CR13

         IF (NDOFO == 0) THEN
            NTERM_GOA = 0
         ENDIF

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(4) == 1) .OR. ( PRTSTIFF(4) == 3)) THEN
            IF (NTERM_KAA > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KAA'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'A ', NTERM_KAA, NDOFA, I_KAA, J_KAA, KAA )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(4) == 2) .OR. ( PRTSTIFF(4) == 3)) THEN
            IF (NTERM_KAO > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KAO'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'O ', NTERM_KAO, NDOFA, I_KAO, J_KAO, KAO )
            ENDIF
            IF (NTERM_KOO > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KOO'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'O ', 'O ', NTERM_KOO, NDOFO, I_KOO, J_KOO, KOO )
            ENDIF
         ENDIF

         MODNAM = 'DEALLOCATE O SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAO', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KAO' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KOO', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KOO' )
         WRITE(SC1,*) CR13

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(4) = 0 since we need KAA_DIAG, KAA_MAX_DIAG for the equilibrium check

         CALL GET_MATRIX_DIAG_STATS ( 'KAA', 'A ', NDOFA, NTERM_KAA, I_KAA, J_KAA, KAA, PRTSTIFD(4), KAA_DIAG, KAA_MAX_DIAG )

! Print out mass matrix partitions, if requested

         IF (( PRTMASS(4) == 1) .OR. ( PRTMASS(4) == 3)) THEN
            IF (NTERM_MAA > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MAA'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'A ', NTERM_MAA, NDOFA, I_MAA, J_MAA, MAA )
            ENDIF
         ENDIF

         IF (( PRTMASS(4) == 2) .OR. ( PRTMASS(4) == 3)) THEN
            IF (NTERM_MAO > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MAO'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'O ', NTERM_MAO, NDOFA, I_MAO, J_MAO, MAO )
            ENDIF
            IF (NTERM_MOO > 0) THEN
               MATRIX_NAME = 'MASS MATRIX MOO'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'O ', 'O ', NTERM_MOO, NDOFO, I_MOO, J_MOO, MOO )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MAO', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MAO' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MOO', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MOO' )
         WRITE(SC1,*) CR13

! Print out load matrix partitions, if requested

         IF (( PRTFOR(4) == 1) .OR. ( PRTFOR(4) == 3)) THEN
            IF (NTERM_PA  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PA'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'SUBCASE', NTERM_PA, NDOFA, I_PA, J_PA, PA )
            ENDIF
         ENDIF

         IF (( PRTFOR(4) == 2) .OR. ( PRTFOR(4) == 3)) THEN
            IF (NTERM_PO  > 0) THEN
               MATRIX_NAME = 'LOAD MATRIX PO'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'O ', 'SUBCASE', NTERM_PO, NDOFO, I_PO, J_PO, PO )
            ENDIF
         ENDIF

         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PO ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PO' )
         WRITE(SC1,*) CR13

! Do equilibrium check on the A-set stiffness matrix, if requested

         IF ((EQCHK_OUTPUT(4) > 0) .OR. (EQCHK_OUTPUT(5) > 0)) THEN
            CALL ALLOCATE_RBGLOBAL ( 'A ', SUBR_NAME )
            IF (NDOFO > 0) THEN
               CALL TDOF_COL_NUM ( 'A ', A_SET_COL )
               DO I=1,NDOFG
                  A_SET_DOF = TDOFI(I,A_SET_COL)
                  IF (A_SET_DOF > 0) THEN
                     DO J=1,6
                        RBGLOBAL_ASET(A_SET_DOF,J) = RBGLOBAL_GSET(I,J)
                     ENDDO
                  ENDIF
               ENDDO
            ELSE
               DO I=1,NDOFA
                  DO J=1,6
                     RBGLOBAL_ASET(I,J) = RBGLOBAL_FSET(I,J)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         IF (EQCHK_OUTPUT(4) > 0) THEN
            CALL OURTIM
            MODNAM = 'EQUILIBRIUM CHECK ON KAA                '
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL STIFF_MAT_EQUIL_CHK ( EQCHK_OUTPUT(4),'A ', SYM_KAA, NDOFA, NTERM_KAA, I_KAA, J_KAA, KAA, KAA_DIAG, KAA_MAX_DIAG, &
                                       RBGLOBAL_ASET)
         ENDIF
!xx      CALL DEALLOCATE_RBGLOBAL ( 'A ' )
         CALL DEALLOCATE_RBGLOBAL ( 'F ' )

! **********************************************************************************************************************************
      ELSE                                                 ! This is BUCKLING with LOAD_ISTEP = 2 (eigen part of BUCKLING)

         IF (NDOFO > 0) THEN
                                                           ! First, need to create part vectors used in the reduction (if NDOFO > 0)
            CALL PARTITION_VEC (NDOFF,'F ','A ','O ',PART_VEC_F_AO)

            DO I=1,NSUB
               PART_VEC_SUB = 1
            ENDDO

            CALL OURTIM                                       ! Reduce KFF to KAA 
            IF (MATSPARS == 'Y') THEN
               MODNAM = 'REDUCE KFFD TO KAAD (SPARSE MATRIX ROUTINES)'
            ELSE
               MODNAM = 'REDUCE KFFD TO KAAD (FULL MATRIX ROUTINES)'
            ENDIF
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            CALL REDUCE_KFFD_TO_KAAD ( PART_VEC_F_AO )

         ELSE                                                 ! There is no O-set, so equate F, A sets

            CALL OURTIM
            MODNAM = 'EQUATING A-SET TO F-SET'
            WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC

            NDOFA     = NDOFF

            NTERM_KAAD = NTERM_KFFD
            NTERM_KAOD = 0
            NTERM_KOOD = 0

            CALL ALLOCATE_SPARSE_MAT ( 'KAAD', NDOFA, NTERM_KAAD, SUBR_NAME )

         ENDIF

! Deallocate F set arrays

         MODNAM = 'DEALLOCATE F-SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KFFD  ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KFFD' )
         WRITE(SC1,*) CR13

! Deallocate ABAND (which was KOO before decomp and TOO later) and GOA

         MODNAM = 'DEALLOCATE GOA, ABAND ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate GOA  ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'GOA' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate ABAND', CR13   ;   CALL DEALLOCATE_LAPACK_MAT ( 'ABAND' )
         WRITE(SC1,*) CR13

         IF (NDOFO == 0) THEN
            NTERM_GOA = 0
         ENDIF

! Print out stiffness matrix partitions, if requested

         IF (( PRTSTIFF(4) == 1) .OR. ( PRTSTIFF(4) == 3)) THEN
            IF (NTERM_KAAD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KAAD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'A ', NTERM_KAAD, NDOFA, I_KAAD, J_KAAD, KAAD )
            ENDIF
         ENDIF

         IF (( PRTSTIFF(4) == 2) .OR. ( PRTSTIFF(4) == 3)) THEN
            IF (NTERM_KAOD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KAOD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'A ', 'O ', NTERM_KAOD, NDOFA, I_KAOD, J_KAOD, KAOD )
            ENDIF
            IF (NTERM_KOOD > 0) THEN
               MATRIX_NAME = 'STIFFNESS MATRIX KOOD'
               CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'O ', 'O ', NTERM_KOOD, NDOFO, I_KOOD, J_KOOD, KOOD )
            ENDIF
         ENDIF

         MODNAM = 'DEALLOCATE O SET ARRAYS'
         WRITE(SC1,2092) MODNAM,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAOD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KAOD' )
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KOOD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KOOD' )
         WRITE(SC1,*) CR13

! Write matrix diagonal and stats, if requested.
! NOTE: call this subr even if PRTSTIFFD(4) = 0 since we need KAA_DIAG, KAA_MAX_DIAG for the equilibrium check

         CALL GET_MATRIX_DIAG_STATS ( 'KAAD', 'A ', NDOFA, NTERM_KAAD, I_KAAD, J_KAAD, KAAD, PRTSTIFD(4), KAAD_DIAG, KAAD_MAX_DIAG )

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 2092 FORMAT(6X,A44,18X,I2,':',I2,':',I2,'.',I3)

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_F_AO


