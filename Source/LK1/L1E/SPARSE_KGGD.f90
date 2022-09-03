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
 
      SUBROUTINE SPARSE_KGGD
 
! Converts the system KGGD differential stiff matrix from a sparse linked list format to a sparse row, col, val format. It sorts
! each row to be in G-set DOF numerical order.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, SPCFIL, SPC, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NGRID, NIND_GRDS_MPCS,                                    &
                                         NTERM_KGGD, NUM_PCHD_SPC1, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_KGGD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_RAT, EPSIL, PRTSTIFF, SPC1QUIT, SUPINFO, SUPWARN
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID, GRID_SEQ, MPC_IND_GRIDS, INV_GRID_SEQ
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START, TDOFI, TSET
      USE STF_ARRAYS, ONLY            :  STFKEY, STF3
      USE SPARSE_MATRICES, ONLY       :  I_KGGD, J_KGGD, KGGD
 
      USE SPARSE_KGGD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_KGGD'
 
      INTEGER(LONG)                   :: G_SET_COL          ! Col in TDOF where G-set DOF's are
      INTEGER(LONG)                   :: I,J,K,L,N          ! DO loop indices
      INTEGER(LONG)                   :: IGRID              ! Internal grid ID
      INTEGER(LONG)                   :: IS                 ! Index into array STF3
      INTEGER(LONG)                   :: KGGD_COL_NUM       ! The col num in G-set stiff matrix where stiff for DOF I begins
      INTEGER(LONG)                   :: KGGD_ROW_NUM       ! The row num in G-set stiff matrix where stiff for DOF I begins
      INTEGER(LONG)                   :: KGGD_II_COL_NUM    ! Col number in the 6x6 stiff matrix for 1 grid
      INTEGER(LONG)                   :: KTERM_KGGD         ! Count of terms written to KGGD to compare with NTERM_KGGD
      INTEGER(LONG)                   :: NUM_NONZERO_IN_ROW ! Count of the actual number of nonzero terms in a row of KGGD
      INTEGER(LONG)                   :: NUM_MAX = 0        ! largest number of terms in any row of the KGGD stiffness matrix
      INTEGER(LONG)                   :: NUM_COMPS          ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG)                   :: NZERO   = 0        ! Count on zero terms in array STF
      INTEGER(LONG)                   :: ROW_NUM_START      ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: RJ(NDOFG)          ! Column numbers corresponding to the terms in RSTF(I).
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_KGGD_BEGEND
 
      REAL(DOUBLE)                    :: EPS1               ! A small number to compare real zero
      REAL(DOUBLE)                    :: KGGD_II(6,6)       ! 6 x 6 diagonal stiffness matrices for 1 grid
      REAL(DOUBLE)                    :: RSTF(NDOFG)        ! 1D array of terms from STF(I) pertaining to one row of the G-set
!                                                             stiffness matrix. Initially, the cols are not in increasing global
!                                                             DOF order. RSTF is sorted so that the cols are in incr DOF order.

      INTRINSIC                       :: DABS
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Pass # 1: Determine final NTERM_KGGD (may be less due to terms stripped)

      NZERO = 0
      DO I = 1,NDOFG                                        ! Start conversion.
         IS = STFKEY(I)

         IF (IS == 0) CYCLE                                 ! Check for null row in stiffness matrix and CYCLE if it is

         NUM_NONZERO_IN_ROW = 0                             ! Count zero terms so we can debit NTERM_KGGD before writing it to file
         DO J = 1,NDOFG
            IF (DABS(STF3(IS)%Col_3) < EPS1) THEN
               NZERO = NZERO + 1
            ELSE
               NUM_NONZERO_IN_ROW = NUM_NONZERO_IN_ROW + 1
            ENDIF
            IS = STF3(IS)%Col_2
            IF (IS == 0) THEN
               EXIT
            ENDIF
         ENDDO
         IF (NUM_NONZERO_IN_ROW > NUM_MAX) THEN
            NUM_MAX = NUM_NONZERO_IN_ROW
         ENDIF   
         IF (IS /= 0) THEN
            WRITE(ERR,1625) SUBR_NAME,I
            WRITE(F06,1625) SUBR_NAME,I
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                          ! Coding error, so quit
         ENDIF

      ENDDO    


      NTERM_KGGD = NTERM_KGGD - NZERO

      WRITE(ERR,146) NTERM_KGGD
      IF (SUPINFO == 'N') THEN
         WRITE(F06,146) NTERM_KGGD
      ENDIF

      CALL ALLOCATE_SPARSE_MAT ( 'KGGD', NDOFG, NTERM_KGGD, SUBR_NAME )

! **********************************************************************************************************************************
! Pass # 2: Reformulate rows


      IF (NTERM_KGGD <= 0) THEN
         WRITE(ERR,1611) NTERM_KGGD
         WRITE(F06,1611) NTERM_KGGD
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                             ! Quit if the no. nonzero terms in KGGD is <= 0
      ENDIF

      KTERM_KGGD = 0                                        ! I runs over the number of rows (or grids)
      CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
      KGGD_ROW_NUM = 0
      I_KGGD(1) = 1

!xx   WRITE(SC1, * )
i_do: DO I = 1,NGRID

         WRITE(SC1,12345,ADVANCE='NO') I, NGRID, CR13

         DO K=1,6                                          ! Make KGGD_II 6x6 even though for SPOINT's we only use 1-1 term
            DO L=1,6
               KGGD_II = ZERO
            ENDDO
         ENDDO 

!xx      CALL CALC_TDOF_ROW_NUM ( GRID_ID(INV_GRID_SEQ(I)), IROW_START, 'N' )
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(INV_GRID_SEQ(I)), IGRID )
         ROW_NUM_START = TDOF_ROW_START(IGRID)
         KGGD_COL_NUM = TDOF(ROW_NUM_START,G_SET_COL)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
k_do:    DO K=1,NUM_COMPS

            KGGD_ROW_NUM = KGGD_ROW_NUM + 1
            IS = STFKEY(KGGD_ROW_NUM)

            IF (IS == 0) THEN                              ! Check for null row in stiffness matrix
               I_KGGD(KGGD_ROW_NUM+1) = I_KGGD(KGGD_ROW_NUM)
               CYCLE k_do
            ENDIF

            NUM_NONZERO_IN_ROW = 0                         ! Form row of non-zero's in arrays RJ, RSTF
j_do1:      DO J=1,NDOFG
               IF (DABS(STF3(IS)%Col_3) >= EPS1) THEN
                  NUM_NONZERO_IN_ROW = NUM_NONZERO_IN_ROW + 1
                  RSTF(NUM_NONZERO_IN_ROW) = STF3(IS)%Col_3
                  RJ(NUM_NONZERO_IN_ROW)   = STF3(IS)%Col_1
               ENDIF
               IS = STF3(IS)%Col_2
               IF (IS == 0) THEN
                  EXIT j_do1
               ENDIF
            ENDDO j_do1

            IF (NUM_NONZERO_IN_ROW > NUM_MAX) THEN
               NUM_MAX = NUM_NONZERO_IN_ROW
            ENDIF   
            IF (IS /= 0) THEN
               WRITE(ERR,1625) SUBR_NAME,I
               WRITE(F06,1625) SUBR_NAME,I
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                       ! Coding error, so quit
            ENDIF
 
            IF (NUM_NONZERO_IN_ROW /= 1) THEN               ! Sort row by the shell method so that RJ is in numerical order
               CALL SORT_INT1_REAL1 ( SUBR_NAME, 'RJ, RSTF', NUM_NONZERO_IN_ROW, RJ, RSTF )
            ENDIF   


n_do:       DO N=1,NUM_NONZERO_IN_ROW                      ! Formulate the K-th row of KGGD_II
               IF ((RJ(N) >= KGGD_COL_NUM) .AND. (RJ(N) <= KGGD_COL_NUM+NUM_COMPS-1)) THEN
                  KGGD_II_COL_NUM = RJ(N) - (KGGD_COL_NUM - 1)
                  KGGD_II(K,KGGD_II_COL_NUM) = RSTF(N)
               ENDIF
            ENDDO n_do
            
j_do3:      DO J=1,NUM_NONZERO_IN_ROW
               KTERM_KGGD = KTERM_KGGD + 1                    ! KTERM_KGGD is a count on the no. records written
               J_KGGD(KTERM_KGGD) = RJ(J)
                 KGGD(KTERM_KGGD) = RSTF(J)
            ENDDO j_do3

            I_KGGD(KGGD_ROW_NUM+1) = I_KGGD(KGGD_ROW_NUM) + NUM_NONZERO_IN_ROW

         ENDDO k_do

         DO K=1,6                                           ! Set lower portion of KGGD_II to be symmetric
            DO J=1,K-1
               KGGD_II(K,J) = KGGD_II(J,K)
            ENDDO
         ENDDO 


      ENDDO i_do

      WRITE(SC1,*) CR13

      IF (PRTSTIFF(1) >= 1) THEN
         CALL WRITE_SPARSE_CRS ( 'STIFFNESS MATRIX KGGD', 'G ', 'G ', NTERM_KGGD, NDOFG, I_KGGD, J_KGGD, KGGD )
      ENDIF

      WRITE(ERR,101) NUM_MAX
      IF (SUPINFO == 'N') THEN
         WRITE(F06,101) NUM_MAX
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *INFORMATION: MAX NUMBER OF NONZERO TERMS IN A ROW OF THE G-SET STIFFNESS MATRIX     = ',I12,/)

  146 FORMAT(' *INFORMATION: NUMBER OF NONZERO TERMS IN THE KGGD STIFFNESS MATRIX IS                 = ',I12,/)

 1611 FORMAT(' *ERROR  1611: THE G-SET DIFFERENTIAL STIFF MATRIX, KGGD, MUST HAVE SOME NONZERO TERMS. HOWEVER IT HAS ',I12,' TERMS')

 1625 FORMAT(' *ERROR  1625: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' 1ST COL OF ARRAY STF3 INDICATES THERE IS MORE DATA IN ARRAY STF3 FOR ROW ',I12,' OF THE KGGD STIFF'   &
                    ,/,14X,' MATRIX ALTHOUGH THE DOF COUNT IS AT THE END OF THE ROW')

12345 FORMAT(5X,'Working on grid ',24X,I8,' of ',I8, A)

! **********************************************************************************************************************************

      END SUBROUTINE SPARSE_KGGD
