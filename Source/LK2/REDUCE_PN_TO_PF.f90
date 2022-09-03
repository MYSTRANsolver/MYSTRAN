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

      SUBROUTINE REDUCE_PN_TO_PF ( PART_VEC_N_FS, PART_VEC_SUB )
 
! Call routines to reduce the PN grid point load matrix from the N-set to the F, S-sets. See Appendix B to the MYSTRAN User's
! Reference Manual for the derivation of the reduction equations.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2D, LINK2D, L2D_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFN, NDOFF, NDOFS, NDOFSE, NSUB, NTERM_KFSe, NTERM_PN,         &
                                         NTERM_PF, NTERM_PFYS, NTERM_PS
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_PN_TO_PF_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE PARAMS, ONLY                :  MATSPARS
      USE SPARSE_MATRICES, ONLY       :  I_KFSe, J_KFSe, KFSe, I_PN, J_PN, PN, I_PF, J_PF, PF, I_PS, J_PS, PS, I_PF_TMP, J_PF_TMP, &
                                         PF_TMP, I_PFYS, J_PFYS, PFYS, I_PFYS1, J_PFYS1, PFYS1, I_QSYS, J_QSYS, QSYS
      USE SPARSE_MATRICES, ONLY       :  SYM_KFSe, SYM_PN, SYM_PF, SYM_PFYS, SYM_PF_TMP, SYM_PS
      USE COL_VECS, ONLY              :  YSe 
      USE FULL_MATRICES, ONLY         :  KFSe_FULL, PF_FULL, PFYS_FULL, DUM1
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
   
      USE SCRATCH_MATRICES
 
      USE REDUCE_PN_TO_PF_USE_IFs

      IMPLICIT NONE
               
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_PN_TO_PF'
      CHARACTER(  1*BYTE)             :: CLOSE_IT            ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close a file or not 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT          ! Char constant for the CLOSE status of a file
      CHARACTER(132*BYTE)             :: MATRIX_NAME         ! Name of matrix for printout 
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_N_FS(NDOFN)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_SUB(NSUB)  ! Partitioning vector (1's for all subcases) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: K                   ! Counter or DO loop index
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM_YS_COLS = 1     ! Variable for number of cols in array YSe 
      INTEGER(LONG)                   :: NTERM_PF_TMP   = 0  ! No. of terms in matrix PF_TMP
      INTEGER(LONG)                   :: NTERM_PFYS1 = 0     ! No. of terms in matrix PFYS1
      INTEGER(LONG)                   :: PF_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: PS_ROW_MAX_TERMS    ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_PN_TO_PF_BEGEND

      REAL(DOUBLE)                    :: ALPHA               ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: BETA                ! Scalar multiplier for matrix

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition PF from PN (This is PF before reduction, or PF(bar) )

      IF (NDOFF > 0) THEN

         CALL PARTITION_SS_NTERM ( 'PN' , NTERM_PN , NDOFN, NSUB , SYM_PN , I_PN , J_PN ,      PART_VEC_N_FS, PART_VEC_SUB,        &
                                    NUM1, NUM1, PF_ROW_MAX_TERMS, 'PF', NTERM_PF, SYM_PF ) 

         CALL ALLOCATE_SPARSE_MAT ( 'PF', NDOFF, NTERM_PF, SUBR_NAME )

         IF (NTERM_PF  > 0) THEN
            CALL PARTITION_SS ( 'PN' , NTERM_PN , NDOFN, NSUB , SYM_PN , I_PN , J_PN , PN , PART_VEC_N_FS, PART_VEC_SUB,           &
                                 NUM1, NUM1, PF_ROW_MAX_TERMS, 'PF', NTERM_PF , NDOFF, SYM_PF, I_PF , J_PF , PF  )
         ENDIF

      ENDIF
                            
! Partition PS from PN

      IF (NDOFS > 0) THEN

         CALL PARTITION_SS_NTERM ( 'PN' , NTERM_PN , NDOFN, NSUB , SYM_PN , I_PN , J_PN ,      PART_VEC_N_FS, PART_VEC_SUB,        &
                                    NUM2, NUM1, PS_ROW_MAX_TERMS, 'PS', NTERM_PS, SYM_PS ) 

         CALL ALLOCATE_SPARSE_MAT ( 'PS', NDOFS, NTERM_PS, SUBR_NAME )

         IF (NTERM_PS  > 0) THEN
            CALL PARTITION_SS ( 'PN' , NTERM_PN , NDOFN, NSUB , SYM_PN , I_PN , J_PN , PN , PART_VEC_N_FS, PART_VEC_SUB,           &
                                 NUM2, NUM1, PS_ROW_MAX_TERMS, 'PS', NTERM_PS , NDOFS, SYM_PS, I_PS , J_PS , PS  )
         ENDIF

      ENDIF

! Deallocate PN

!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
      WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PN ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PN' )

! Reduce PN to PF = PF(bar) - PFYS where PF(bar) was original F-set partition from PN. If there are no enforced displs then
! PF = PF(bar) and no reduction is required. If there are enforced displs (NDOFSE is used to indicate if there are enforced
! displs) then reduction is required. So, when NDOFSE > 0 the reduction is done in 3 steps:

!     (1) calc PFYS1 = KFSe*YSe. This is 1 col since YSe is for all subcases.
!     (2) expand PFYS1 from 1 col to NSUB cols in PFYS with all cols = KFSe*YSe
!     (3) Calc reduced PF from PF(bar) and PFYS. This step is done 1 of 2 ways:
!         (a) If there are applied loads on the F-set DOF's (i.e. if NTERM_PF > 0), then we calc PF = PF(bar) - PFYS
!         (b) If there are no applied loads on the F-set DOF's (i.e. if NTERM_PF = 0), then we calc PF = -PFYS
 
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN

         IF ((NDOFF > 0) .AND. (NDOFSE > 0)) THEN          ! Do reduction only if there is an SE-set and an F-set
                                                           ! Step (1), calc PFYS1 = KFSe * YSe
            CALL MATMULT_SFS_NTERM ( 'KFSe', NDOFF, NTERM_KFSe, SYM_KFSe, I_KFSe, J_KFSe                                           &
                                     ,'YSe', NDOFSE, NUM_YS_COLS, YSe, AROW_MAX_TERMS, 'PFYS1', NTERM_PFYS1 )
 
            CALL ALLOCATE_SPARSE_MAT ( 'PFYS1', NDOFF, NTERM_PFYS1, SUBR_NAME )

            CALL MATMULT_SFS ( 'KFSe', NDOFF, NTERM_KFSe, SYM_KFSe, I_KFSe, J_KFSe, KFSe                                           &
                              ,'YSe', NDOFSE, NUM_YS_COLS, YSe, AROW_MAX_TERMS, 'PFYS1', ONE, NTERM_PFYS1, I_PFYS1, J_PFYS1, PFYS1 )


            NTERM_PFYS = NSUB*NTERM_PFYS1                  ! Step (2), expand PFS1 (1 col) into PFYS (NSUB cols, all = KFSe*YSe)

            CALL ALLOCATE_SPARSE_MAT ( 'PFYS', NDOFF, NTERM_PFYS, SUBR_NAME )

            I_PFYS(1) = 1
            DO I=2,NDOFF+1
               I_PFYS(I) = I_PFYS(I-1) + NSUB*(I_PFYS1(I) - I_PFYS1(I-1))
            ENDDO

            K = 0
            DO I=1,NTERM_PFYS1
               DO J=1,NSUB
                  K         = K + 1
                  J_PFYS(K) = J_PFYS1(I) + (J-1)
                    PFYS(K) = PFYS1(I)
               ENDDO 
            ENDDO

      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PFYS1', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PFYS1' )


            IF (NTERM_PF > 0) THEN                         ! Step (3a), NTERM_PF > 0 so reduced PF = PF(bar) - PFYS

               IF (NTERM_PFYS > 0) THEN                    ! Only do step (3a) if there are nonzero's in PFYS, otherwise PF=PF(bar)
                  ALPHA =  ONE
                  BETA  = -ONE

                  CALL ALLOCATE_SPARSE_MAT ( 'PF_TMP', NDOFF, NTERM_PF, SUBR_NAME )

                  DO I=1,NDOFF+1                           ! PF_TMP = PF(bar) so we can use MATADD to get PF = PF_TMP - PFYS
                     I_PF_TMP(I) = I_PF(I)
                  ENDDO 
                  NTERM_PF_TMP = NTERM_PF
                  DO K=1,NTERM_PF_TMP
                     J_PF_TMP(K) = J_PF(K)
                       PF_TMP(K) =   PF(K)
                  ENDDO
                                                           ! Recalc NTERM_PF for new PF = PF_TMP + PFYS
                  CALL MATADD_SSS_NTERM ( NDOFF, 'PF-bar', NTERM_PF_TMP, I_PF_TMP, J_PF_TMP, SYM_PF_TMP,                           &
                                                 'KFse*YSe', NTERM_PFYS  , I_PFYS  , J_PFYS  , SYM_PFYS  , 'PFYS', NTERM_PF )
            !xx   WRITE(SC1, * )                           ! Advance 1 line for screen messages         
                  WRITE(SC1, * ) '    Reallocate PF '
            !xx   WRITE(SC1, * )                           ! Advance 1 line for screen messages         
                  WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PF ', CR13
                  CALL DEALLOCATE_SPARSE_MAT ( 'PF' )
                  WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PF ', CR13
                  CALL ALLOCATE_SPARSE_MAT ( 'PF', NDOFF, NTERM_PF, SUBR_NAME )

                  CALL MATADD_SSS ( NDOFF, 'PF-bar', NTERM_PF_TMP, I_PF_TMP, J_PF_TMP, PF_TMP, ALPHA,                              &
                                   'KFse*YSe'  , NTERM_PFYS  , I_PFYS  , J_PFYS  , PFYS  , BETA ,'PF', NTERM_PF, I_PF, J_PF, PF)
            !xx   WRITE(SC1, * )                           ! Advance 1 line for screen messages         
                  WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PF_TMP', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PF_TMP' )
         
               ENDIF

            ELSE                                           ! Step (3b), NTERM_PF = 0 so reduced PF = PF(bar)

               IF (NTERM_PFYS > 0) THEN                    ! Only do step (3b) if there are nonzero's in PFYS, otherwise PF = 0

                  NTERM_PF = NTERM_PFYS*NSUB
                  WRITE(SC1, * ) '    Reallocate PF '
            !xx   WRITE(SC1, * )                           ! Advance 1 line for screen messages         
                  WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PF ', CR13
                  CALL DEALLOCATE_SPARSE_MAT ( 'PF' )
                  WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PF ', CR13
                  CALL ALLOCATE_SPARSE_MAT ( 'PF', NDOFF, NTERM_PF, SUBR_NAME )

                  DO I=1,NDOFF+1
                     I_PF(I) = I_PFYS(I)
                  ENDDO
                  K = 0
                  DO I=1,NTERM_PFYS
                     DO J=1,NSUB
                        K = K + 1
                        J_PF(K) = J_PFYS(I)
                          PF(K) =  -PFYS(I)
                     ENDDO 
                  ENDDO


                  IF ((DEBUG(24) == 1) .OR.(DEBUG(24) == 3)) THEN
                     MATRIX_NAME = 'STIFFNESS PARTITION KFSe (partition of KFS for cols of enforced displs)   '
                     CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', 'SE', NTERM_KFSe, NDOFF, I_KFSe, J_KFSe, KFSe )
                  ENDIF

                  IF ((DEBUG(25) == 1) .OR.(DEBUG(25) == 3)) THEN
                     MATRIX_NAME = 'LOAD MATRIX PFYS = KFSe*YSe (portion of F-set loads due to enforced displs)'
                     CALL WRITE_SPARSE_CRS ( MATRIX_NAME, 'F ', '  ', NTERM_PFYS, NDOFF, I_PFYS, J_PFYS, PFYS )
                  ENDIF

               ENDIF

            ENDIF 

            WRITE(SC1, * ) '     DEALLOCATE SOME ARRAYS'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate PFYS', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'PFYS' )
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KFSe', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KFSe' )

         ENDIF

      ELSE IF (MATSPARS == 'N') THEN
                                                           ! Allocate and set PF_FULL to sparse PF
         CALL ALLOCATE_FULL_MAT ( 'PF_FULL', NDOFF, NSUB, SUBR_NAME )

         IF (NTERM_PN > 0) THEN
            CALL SPARSE_CRS_TO_FULL ( 'PF', NTERM_Pf, NDOFf, NSUB, SYM_Pf, I_Pf, J_Pf, Pf, Pf_FULL )
         ENDIF

         IF ((NDOFF > 0) .AND. (NDOFSE > 0)) THEN          ! Do reduction only if there is an SE-set and an F-set

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFSE, NSUB, SUBR_NAME )
            DO I=1,NDOFSE
               DO J=1,NSUB
                  DUM1(I,J) = YSe(I)
               ENDDO 
            ENDDO 
            CALL ALLOCATE_FULL_MAT ( 'KFSe_FULL', NDOFF, NDOFSE, SUBR_NAME )
            CALL ALLOCATE_FULL_MAT ( 'PFYS_FULL', NDOFF, NSUB, SUBR_NAME )
            CALL MATMULT_FFF (KFSe_FULL, DUM1, NDOFF, NDOFSE, NSUB, PFYS_FULL )
            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )

            IF (NTERM_PF > 0) THEN                         ! Step (3a), NTERM_PF > 0 so reduced PF = PF(bar) - PFYS

               IF (NTERM_PFYS > 0) THEN                    ! Only do step (3a) if there are nonzero's in PFYS, otherwise PF=PF(bar)

                  CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFF, NSUB, SUBR_NAME )

                  DO I=1,NDOFF
                     DO J=1,NSUB
                        DUM1(I,J) = PF_FULL(I,J)
                     ENDDO 
                  ENDDO 

                  ALPHA =  ONE
                  BETA  = -ONE
                  CALL MATADD_FFF ( DUM1, PFYS_FULL, NDOFF, NSUB, ALPHA, BETA, ITRNSPB, PF_FULL)

                  CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
         
               ENDIF

            ELSE                                           ! Step (3b), NTERM_PF = 0 so reduced PF = PF(bar)

               IF (NTERM_PFYS > 0) THEN                    ! Only do step (3b) if there are nonzero's in PFYS, otherwise PF = 0

                  DO I=1,NDOFF
                     DO J=1,NSUB
                        PF_FULL(I,J) = -PFYS_FULL(I,J)
                     ENDDO 
                  ENDDO 

               ENDIF

            ENDIF 

            CALL DEALLOCATE_FULL_MAT ( 'PFYS_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'KFSe_FULL' )

         ENDIF

         IF (NTERM_PF > 0) THEN                            ! Create new sparse arrays from PF_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'PF_FULL   ', NDOFF, NSUB , PF_FULL,  NTERM_PF,  SYM_PF,  I_PF,  J_PF,  PF  )
            CALL DEALLOCATE_FULL_MAT ( 'PF_FULL' )      
         ENDIF

      ELSE

         WRITE(ERR,911) SUBR_NAME,MATSPARS
         WRITE(F06,911) SUBR_NAME,MATSPARS
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

! Write PS to L2D for constraint force recovery in LINK9.
  
      IF (NTERM_PS > 0) THEN
         CLOSE_IT   = 'Y'
         CLOSE_STAT = 'KEEP'
         CALL WRITE_MATRIX_1 ( LINK2D, L2D, CLOSE_IT, CLOSE_STAT, L2D_MSG, 'PS ', NTERM_PS , NDOFS, I_PS , J_PS , PS  )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  911 FORMAT(' *ERROR   911: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER MATSPARS MUST BE EITHER ','Y',' OR ','N',' BUT VALUE IS ',A)


12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_PN_TO_PF

