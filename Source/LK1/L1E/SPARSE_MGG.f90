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
 
      SUBROUTINE SPARSE_MGG
 
! Add sparse arrays for concentrated masses (array MGGC), scalar masses (array MGGS) and element mass (array EMS) to get the final
! sparse G-set mass matrix, MGG. Rows are sorted to be in numerical G-set DOF order and the final MGG is written to file LINK1R

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1R, L1R_MSG, LINK1R, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCMASS, NDOFG, NGRID, NTERM_MGG, NTERM_MGGC, NTERM_MGGE,         &
                                         NTERM_MGGS, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_MGG_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DOF_TABLES,ONLY             :  TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  GRID_ID
      USE PARAMS, ONLY                :  EPSIL, PRTMASS, SUPINFO, WTMASS
      USE EMS_ARRAYS, ONLY            :  EMS, EMSCOL, EMSKEY, EMSPNT
      USE SPARSE_MATRICES, ONLY       :  I2_MGG, I_MGG, J_MGG, MGG, I_MGGC, J_MGGC, MGGC, I_MGGE, J_MGGE, MGGE,                    &
                                         I_MGGS, J_MGGS, MGGS,  SYM_MGGC, SYM_MGGE, SYM_MGGS
      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1 
 
      USE SPARSE_MGG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_MGG'
      CHARACTER(  1*BYTE)             :: FOUND             ! 'Y' if there is a mass matrix for this grid and 'N' otherwise
      CHARACTER(LEN=LEN(SYM_MGGE))    :: SYM_CRS1          ! 'Y'/'N' Symmetry indicator for scratch matrix CRS1
 
      INTEGER(LONG)                   :: GRID_NUM          ! An actual grid ID
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IERR              ! Local error count
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IK                ! Index for array I_MGGE
      INTEGER(LONG)                   :: IS                ! Index into arrays EMSPNT, EMSLIS, EMSCOL
      INTEGER(LONG)                   :: KTERM_MGGE        ! Count of terms written to MGG file LINK1R to compare with NTERM_MGGE
      INTEGER(LONG)                   :: MAX_NUM_IN_ROW    ! largest number of terms in any row of the MGG mass matrix
      INTEGER(LONG)                   :: NTERM_CRS1        ! Count of nonzero terms in matrix CRS1
      INTEGER(LONG)                   :: NUM               ! Count of the actual number of nonzero terms in a row of MGG
      INTEGER(LONG)                   :: NUM_IN_ROW_I      ! Number of nonzero terms in a row of MGG
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG)                   :: NZERO   = 0       ! Count on zero terms in array EMS
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: RJ(NDOFG)         ! Column numbers corresponding to the terms in REMS(I).
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_MGG_BEGEND
 
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: GRID_MGG(6,6)     ! 6 x 6 mass matrix for a grid
      REAL(DOUBLE)                    :: REMS(NDOFG)       ! 1D array of the terms from EMS(I) pertaining to one row of the G-set
!                                                            mass matrix. Initially, the cols are not in increasing global DOF
!                                                            order. REMS is sorted, prior to writing the G-set mass matrix
!                                                            to file LINK1R, so that the cols are in increasing DOF order.
 
      INTRINSIC                       :: DABS
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)
! Pass # 1: Determine final NTERM_MGGE (may be less due to zero terms)

      NZERO = 0
i_do0:DO I = 1,NDOFG                                       ! Start conversion.

         IS = EMSKEY(I)
         IF (IS == 0) CYCLE i_do0                          ! Check for null row in mass matrix and CYCLE if it is

         NUM   = 0                                         ! Count zero terms so we can debit NTERM_MGGE before writing it to file
j_do0:   DO J = 1,NDOFG
            IF (DABS(EMS(IS)) < EPS1) THEN
               NZERO = NZERO + 1
            ELSE
               NUM = NUM + 1
            ENDIF
            IS = EMSPNT(IS)
            IF (IS == 0) THEN
               EXIT j_do0
            ENDIF
         ENDDO j_do0
         IF (IS /= 0) THEN
            WRITE(ERR,1626) SUBR_NAME,I
            WRITE(F06,1626) SUBR_NAME,I
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                         ! Coding error, so quit
         ENDIF

      ENDDO i_do0

      NTERM_MGGE = NTERM_MGGE - NZERO

      WRITE(ERR,146) NTERM_MGGE
      IF (SUPINFO == 'N') THEN
         WRITE(F06,146) NTERM_MGGE
      ENDIF

! **********************************************************************************************************************************
! Pass # 2: Reformulate rows and write to file LINK1R

! Open L1R to write mass.
  
      OUNT(1) = ERR
      OUNT(2) = F06
      CALL FILE_OPEN ( L1R, LINK1R, OUNT, 'REPLACE', L1R_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

      KTERM_MGGE = 0
      I_MGGE(1) = 1
      WRITE(SC1, * )
i_do: DO I = 1,NGRID

         WRITE(SC1,12345,ADVANCE='NO') I, NGRID, CR13

         GRID_NUM = GRID_ID(I)

!xx      CALL CALC_TDOF_ROW_NUM ( GRID_NUM, IROW_START, 'N' )
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_NUM, IGRID )
         ROW_NUM_START = TDOF_ROW_START(IGRID)
         CALL GET_GRID_NUM_COMPS ( GRID_NUM, NUM_COMPS, SUBR_NAME )
k_do:    DO K=1,NUM_COMPS

            IK = ROW_NUM_START + K - 1
            IS = EMSKEY(IK)

            IF (IS == 0) THEN                              ! Check for null row in mass matrix
               I_MGGE(IK+1) = I_MGGE(IK)
               CYCLE k_do
            ENDIF

            NUM = 0
j_do1:      DO J=1,NDOFG
               IF (DABS(EMS(IS)) >= EPS1) THEN
                  NUM = NUM + 1
                  REMS(NUM) = EMS(IS)
                  RJ(NUM)   = EMSCOL(IS)
               ENDIF
               IS = EMSPNT(IS)
               IF (IS == 0) THEN
                  EXIT j_do1
               ENDIF
            ENDDO j_do1

            I_MGGE(IK+1) = I_MGGE(IK) + NUM
 
            IF (IS /= 0) THEN
               WRITE(ERR,1626) SUBR_NAME,I
               WRITE(F06,1626) SUBR_NAME,I
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                      ! Coding error, so quit
            ENDIF

            IF (NUM /= 1) THEN                             ! Sort row by the shell method so that RJ is in numerical order
               CALL SORT_INT1_REAL1 ( SUBR_NAME, 'RJ, REMS', NUM, RJ, REMS )
            ENDIF   

j_do3:      DO J = 1,NUM
               KTERM_MGGE = KTERM_MGGE + 1                 ! KTERM_MGGE is a count on the number of records
               IF (KTERM_MGGE > NTERM_MGGE) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_MGGE, 'MGGE' )
                  J_MGGE(KTERM_MGGE) = RJ(J)
                    MGGE(KTERM_MGGE) = REMS(J)
            ENDDO j_do3

         ENDDO k_do
      ENDDO i_do

      WRITE(SC1,*) CR13


      IF (KTERM_MGGE /= NTERM_MGGE) THEN                   ! Check KTERM_MGGE = NTERM_MGGE
         WRITE(ERR,1614) SUBR_NAME,LINK1R,KTERM_MGGE,NTERM_MGGE
         WRITE(F06,1614) SUBR_NAME,LINK1R,KTERM_MGGE,NTERM_MGGE
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
      ENDIF

! *********************************************************************************************************************************
! Call subr to calc MGGS matrix of scalar masses

      IF (NCMASS > 0) THEN
         CALL MGGS_MASS_MATRIX
      ENDIF

! Add MGGC, MGGE and MGGS to get MGG. This is done in 2 steps: add MGGC and MGGE to get temporary CRS1 then add CRS1 to MGGS 

!  (1) add MGGC and MGGE to get CRS1 (do not mult by WTMASS here)
!  --------------------------------------------------------------

      CALL MATADD_SSS_NTERM ( NDOFG, 'MGGC', NTERM_MGGC, I_MGGC, J_MGGC, SYM_MGGC, 'MGGE', NTERM_MGGE, I_MGGE, J_MGGE, SYM_MGGE,&
                                     'CRS1' , NTERM_CRS1 )

      CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFG, NTERM_CRS1, SUBR_NAME )

      CALL MATADD_SSS ( NDOFG, 'MGGC', NTERM_MGGC, I_MGGC, J_MGGC, MGGC, ONE, 'MGGE', NTERM_MGGE, I_MGGE, J_MGGE, MGGE,            &
                        ONE, 'CRS1', NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )


!  (2) add CRS1 = MGGC + MGGE and MGGS to get CMGG (mult by WTMASS here)
!  ---------------------------------------------------------------------

      IF (NTERM_MGGS > 0) THEN

         SYM_CRS1 = SYM_MGGS
         CALL MATADD_SSS_NTERM ( NDOFG, 'CRS1', NTERM_CRS1, I_CRS1, J_CRS1, SYM_CRS1, 'MGGS', NTERM_MGGS, I_MGGS, J_MGGS, SYM_MGGS,&
                                        'MGG' , NTERM_MGG )

         CALL ALLOCATE_L1_MGG ( 'I2_MGG', SUBR_NAME )
         CALL ALLOCATE_SPARSE_MAT ( 'MGG', NDOFG, NDOFG, SUBR_NAME )

         CALL MATADD_SSS ( NDOFG, 'CRS1', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, WTMASS, 'MGGS', NTERM_MGGS, I_MGGS, J_MGGS, MGGS,      &
                           WTMASS, 'MGG', NTERM_MGG, I_MGG, J_MGG, MGG )

      ELSE

         NTERM_MGG = NTERM_CRS1
         CALL ALLOCATE_L1_MGG ( 'I2_MGG', SUBR_NAME )
         CALL ALLOCATE_SPARSE_MAT ( 'MGG', NDOFG, NTERM_MGG, SUBR_NAME )
         DO I=1,NDOFG+1
            I_MGG(I) = I_CRS1(I)
         ENDDO
         DO I=1,NTERM_MGG
            J_MGG(I) = J_CRS1(I)
              MGG(I) = WTMASS*CRS1(I)
         ENDDO
         

      ENDIF

! Deallocate CRS1

      CALL DEALLOCATE_SCR_MAT ( 'CRS1' )

      IF (PRTMASS(1) >= 2) THEN
         IF (NTERM_MGG > 0) THEN
            IF (ALLOCATED(MGGC)) THEN
               CALL WRITE_SPARSE_CRS (  'Conc mass matrix, MGGC', 'G ', 'G ', NTERM_MGGC, NDOFG, I_MGGC, J_MGGC, MGGC )
            ENDIF
            IF (ALLOCATED(MGGE)) THEN
               CALL WRITE_SPARSE_CRS (  'Elem mass matrix, MGGE', 'G ', 'G ', NTERM_MGGE, NDOFG, I_MGGE, J_MGGE, MGGE )
            ENDIF
            IF (ALLOCATED(MGGS)) THEN
               CALL WRITE_SPARSE_CRS ('Scalar mass matrix, MGGS', 'G ', 'G ', NTERM_MGGS, NDOFG, I_MGGS, J_MGGS, MGGS )
            ENDIF
         ENDIF
      ENDIF

! *********************************************************************************************************************************
! Write row, col, value to L1R for matrix MGG

      K = 0
      WRITE(L1R) NTERM_MGG
      IF (NTERM_MGG > 0) THEN
         DO I=1,NDOFG
            NUM_IN_ROW_I = I_MGG(I+1) - I_MGG(I)
            DO J=1,NUM_IN_ROW_I
               K = K + 1
               IF (K > NTERM_MGG)  CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, K, 'MGG' )
               I2_MGG(K) = I
               WRITE(L1R) I2_MGG(K), J_MGG(K), MGG(K)
            ENDDO
         ENDDO
      ENDIF

      CALL FILE_CLOSE ( L1R, LINK1R, 'KEEP', 'Y' )

! Get stats on MGG to write to F06

      IF (NTERM_MGG > 0) THEN

         MAX_NUM_IN_ROW = 0
         DO I=1,NDOFG
            IK = I_MGG(I+1) - I_MGG(I)
            IF (IK > MAX_NUM_IN_ROW) THEN
               MAX_NUM_IN_ROW = IK
            ENDIF
         ENDDO
         
         WRITE(ERR,147) NTERM_MGG
         WRITE(ERR,101) MAX_NUM_IN_ROW
         IF (SUPINFO == 'N') THEN
            WRITE(F06,147) NTERM_MGG
            WRITE(F06,101) MAX_NUM_IN_ROW
         ENDIF

      ENDIF

! Debug output (print grid 6x6 mass for every grid) 

      IERR = 0
      IF (DEBUG(36) > 0) THEN
         WRITE(F06,1101)
         DO K=1,NGRID
            CALL GET_GRID_NUM_COMPS ( GRID_ID(K), NUM_COMPS, SUBR_NAME )
            IF (NUM_COMPS == 6) THEN                       ! Only do output for actual grids, not SPOINT's
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(K), IGRID )
               IF (IGRID == -1) THEN
                  IERR      = IERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1314) 'GRID ', GRID_ID(K), ' CANNOT PROCESS GRID FOR DEBUG(36) OUTPUT'
                  WRITE(F06,1314) 'GRID ', GRID_ID(K), ' CANNOT PROCESS GRID FOR DEBUG(36) OUTPUT'
               ENDIF
               CALL GET_GRID_6X6_MASS (  GRID_ID(K), IGRID, FOUND, GRID_MGG )
               WRITE(F06,1102) GRID_ID(K)
               DO I=1,3
                  WRITE(F06,1103) (GRID_MGG(I,J),J=1,6)
               ENDDO
               WRITE(F06,*)
               DO I=4,6
                  WRITE(F06,1103) (GRID_MGG(I,J),J=1,6)
               ENDDO
               WRITE(F06,*)
            ENDIF
         ENDDO
         WRITE(F06,1104)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  146 FORMAT(' *INFORMATION: NUMBER OF NONZERO TERMS IN THE MGGE MASS MATRIX (ELEMS) IS             = ',I12,/)

  147 FORMAT(' *INFORMATION: NUMBER OF NONZERO TERMS IN THE MGG MASS MATRIX (ELEMS + CONM) IS       = ',I12,/)

  101 FORMAT(' *INFORMATION: MAX NUMBER OF NONZERO TERMS IN A ROW OF THE G-SET MASS MATRIX          = ',I12,/)

 1101 FORMAT(' ___________________________________________________________________________________________________________________'&
            ,'________________'                                                                                                ,//,&
             ' ::::::::::::::::::::::::::::::::::::::::START DEBUG(36) OUTPUT FROM SUBROUTINE SPARSE_MGG:::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1102 FORMAT('6 x 6 mass matrix for grid ',I8,/,'-----------------------------------')

 1103 FORMAT(3(1ES14.6),2X,3(1ES14.6))

 1104 FORMAT(' :::::::::::::::::::::::::::::::::::::::::END DEBUG(36) OUTPUT FROM SUBROUTINE SPARSE_MGG::::::::::::::::::::::::::',&
             ':::::::::::::::::'                                                                                                ,/,&
             ' ___________________________________________________________________________________________________________________'&
            ,'________________',/)

 1314 FORMAT(' *ERROR  1314: UNDEFINED ',A,I8,A)

 1626 FORMAT(' *ERROR  1626: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' EMSPNT ARRAY INDICATES THERE IS MORE DATA IN ARRAY EMS FOR ROW ',I12,' OF THE MGG STIFF MATRIX.'      &
                    ,/,14X,' ALTHOUGH THE DOF COUNT IS AT THE END OF THE ROW')

 1614 FORMAT(' *ERROR  1614: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF G-SET MASS MATRIX RECORDS WRITTEN TO FILE:'                                             &
                    ,/,15X,A                                                                                                       &
                    ,/,14X,' WAS KTERM_MGGE = ',I12,'. IT SHOULD HAVE BEEN NTERM_MGGE = ',I12)

12345 FORMAT(5X,'Working on grid ',24X,I8,' of ',I8, A)







! **********************************************************************************************************************************

      END SUBROUTINE SPARSE_MGG
