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
 
      SUBROUTINE SPARSE_KGG
 
! (1) Converts the system KGG matrix from a sparse linked list format to a row, col, val format. It sorts each row to be
!     in G-set DOF numerical order. The transformed matrix is written out to file LINK1L row by row.

! (2) Call KGG_SINGULARITY_PROC to check 6x6 diagonal stiffness partitions for singularities (which resets TSET table if there are
!     any grid point singularities)

! (3) Call TDOF_PROC to regenerate TDOF, TDOFI tables if KGG_SINGULARITY_PROC found singularities

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1L, L1L_MSG, LINK1L, SC1, SPCFIL, SPC, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NGRID, NIND_GRDS_MPCS,                                    &
                                         NTERM_KGG, NUM_PCHD_SPC1, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_KGG_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_RAT, EPSIL, PRTTSET, PRTSTIFF, SPC1QUIT, SUPINFO, SUPWARN
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID, GRID_SEQ, MPC_IND_GRIDS, INV_GRID_SEQ
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START, TDOFI, TSET
      USE STF_ARRAYS, ONLY            :  STFKEY, STF3
      USE SPARSE_MATRICES, ONLY       :  I_KGG, J_KGG, KGG
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE SPARSE_KGG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_KGG'
      CHARACTER(  7*BYTE)             :: ASPC_SUM_MSG1      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(100*BYTE)             :: ASPC_SUM_MSG2      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER( 13*BYTE)             :: ASPC_SUM_MSG3      ! Message to be printed out in the AUTOSPC summary table
      CHARACTER(132*BYTE)             :: TDOF_MSG           ! Message to be printed out regarding at what pt in the run the TDOF,I
!                                                             tables are printed out
      CHARACTER( 1*BYTE)              :: SKIPIT      = 'N'  ! Indicator of whether to skip KGG sing proc (for MPC indep grids)
 
      INTEGER(LONG)                   :: AGRIDI             ! Actual grid ID
      INTEGER(LONG)                   :: G_SET_COL          ! Col in TDOF where G-set DOF's are
      INTEGER(LONG)                   :: I,J,K,L,N          ! DO loop indices
      INTEGER(LONG)                   :: IGRID              ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK              ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: IS                 ! Index into array STF3
      INTEGER(LONG)                   :: KGG_COL_NUM        ! The col num in G-set stiff matrix where stiff for DOF I begins
      INTEGER(LONG)                   :: KGG_ROW_NUM        ! The row num in G-set stiff matrix where stiff for DOF I begins
      INTEGER(LONG)                   :: KGG_II_COL_NUM     ! Col number in the 6x6 stiff matrix for 1 grid
      INTEGER(LONG)                   :: KGG_NUM_ASPC       ! Sum of NUM_ASPC_BY_COMP(6) (this is also NDOFSA but we need to test
      INTEGER(LONG)                   :: KTERM_KGG          ! Count of terms written to KGG file LINK1L to compare with NTERM_KGG
      INTEGER(LONG)                   :: NUM_NONZERO_IN_ROW ! Count of the actual number of nonzero terms in a row of KGG
      INTEGER(LONG)                   :: NUM_ASPC_BY_COMP(6)! The number of SPC1's for each displ component
      INTEGER(LONG)                   :: NUM_MAX = 0        ! largest number of terms in any row of the KGG stiffness matrix
      INTEGER(LONG)                   :: NUM_COMPS          ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG)                   :: NZERO   = 0        ! Count on zero terms in array STF
      INTEGER(LONG)                   :: OUNT(2)            ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: ROW_NUM_START      ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: RJ(NDOFG)          ! Column numbers corresponding to the terms in RSTF(I).
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_KGG_BEGEND
 
      REAL(DOUBLE)                    :: EPS1               ! A small number to compare real zero
      REAL(DOUBLE)                    :: KGG_II(6,6)        ! 6 x 6 diagonal stiffness matrices for 1 grid
      REAL(DOUBLE)                    :: RSTF(NDOFG)        ! 1D array of terms from STF(I) pertaining to one row of the G-set
!                                                             stiffness matrix. Initially, the cols are not in increasing global
!                                                             DOF order. RSTF is sorted, prior to writing the G-set stiff matrix
!                                                             to file LINK1L, so that the cols are in increasing DOF order.


      INTRINSIC                       :: DABS
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Pass # 1: Determine final NTERM_KGG (may be less due to terms stripped)

      NZERO = 0
      DO I = 1,NDOFG                                        ! Start conversion.
         IS = STFKEY(I)

         IF (IS == 0) CYCLE                                 ! Check for null row in stiffness matrix and CYCLE if it is

         NUM_NONZERO_IN_ROW = 0                             ! Count zero terms so we can debit NTERM_KGG before writing it to file
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


      NTERM_KGG = NTERM_KGG - NZERO

      WRITE(ERR,146) NTERM_KGG
      IF (SUPINFO == 'N') THEN
         WRITE(F06,146) NTERM_KGG
      ENDIF

      CALL ALLOCATE_SPARSE_MAT ( 'KGG', NDOFG, NTERM_KGG, SUBR_NAME )

! **********************************************************************************************************************************
! Pass # 2: Reformulate rows and write to file LINK1L. Call KGG_SINGULARITY_PROC


      IF (NTERM_KGG <= 0) THEN
         WRITE(ERR,1611) NTERM_KGG
         WRITE(F06,1611) NTERM_KGG
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                             ! Quit if the no. nonzero terms in KGG is <= 0
      ENDIF

! Open L1L to write stiffness.
  
      OUNT(1) = ERR
      OUNT(2) = F06
      CALL FILE_OPEN ( L1L, LINK1L, OUNT, 'REPLACE', L1L_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      WRITE(L1L) NTERM_KGG

! Open SPC to write SPC1 records if KGG_SINGULARITY_PROC finds singularities

      OPEN (SPC,FILE=SPCFIL,STATUS='REPLACE',IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN
         CALL OPNERR ( IOCHK, SPCFIL, OUNT, 'Y')
         CALL FILERR ( OUNT, 'Y' )
         CALL OUTA_HERE ( 'Y' )
      ENDIF
 
      DO I=1,6                                             ! Initialize NUM_ASPC_BY_COMP. It will get accumulated in KGG_SING_PROC
         NUM_ASPC_BY_COMP(I) = 0
      ENDDO

      IF (DEBUG(17) > 0) THEN                              ! Write leading seperator for DEBUG output
         WRITE(F06,9901) AUTOSPC_RAT
      ENDIF
!xx   WRITE(SC1,*)
      KTERM_KGG = 0                                        ! I runs over the number of rows (or grids)
      CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
      KGG_ROW_NUM = 0
      I_KGG(1) = 1

i_do: DO I = 1,NGRID

         WRITE(SC1,12345,ADVANCE='NO') I, NGRID, CR13

         SKIPIT = 'N'

         DO K=1,6                                          ! Make KGG_II 6x6 even though for SPOINT's we only use 1-1 term
            DO L=1,6
               KGG_II = ZERO
            ENDDO
         ENDDO 

!xx      CALL CALC_TDOF_ROW_NUM ( GRID_ID(INV_GRID_SEQ(I)), IROW_START, 'N' )
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(INV_GRID_SEQ(I)), IGRID )
         ROW_NUM_START = TDOF_ROW_START(IGRID)
         KGG_COL_NUM = TDOF(ROW_NUM_START,G_SET_COL)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
k_do:    DO K=1,NUM_COMPS

            KGG_ROW_NUM = KGG_ROW_NUM + 1
            IS = STFKEY(KGG_ROW_NUM)

            IF (IS == 0) THEN                              ! Check for null row in stiffness matrix
               I_KGG(KGG_ROW_NUM+1) = I_KGG(KGG_ROW_NUM)
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


n_do:       DO N=1,NUM_NONZERO_IN_ROW                      ! Formulate the K-th row of KGG_II
               IF ((RJ(N) >= KGG_COL_NUM) .AND. (RJ(N) <= KGG_COL_NUM+NUM_COMPS-1)) THEN
                  KGG_II_COL_NUM = RJ(N) - (KGG_COL_NUM - 1)
                  KGG_II(K,KGG_II_COL_NUM) = RSTF(N)
               ENDIF
            ENDDO n_do
            
j_do3:      DO J=1,NUM_NONZERO_IN_ROW
               KTERM_KGG = KTERM_KGG + 1                    ! KTERM_KGG is a count on the no. records written
               WRITE(L1L) KGG_ROW_NUM, RJ(J), RSTF(J)
               J_KGG(KTERM_KGG) = RJ(J)
                 KGG(KTERM_KGG) = RSTF(J)
            ENDDO j_do3

            I_KGG(KGG_ROW_NUM+1) = I_KGG(KGG_ROW_NUM) + NUM_NONZERO_IN_ROW

         ENDDO k_do

         DO K=1,6                                           ! Set lower portion of KGG_II to be symmetric
            DO J=1,K-1
               KGG_II(K,J) = KGG_II(J,K)
            ENDDO
         ENDDO 


         AGRIDI = GRID_ID(INV_GRID_SEQ(I))               ! We don't want to call KGG_SINGULARITY_PROC for grids that are indep
j_do4:   DO J=1,NIND_GRDS_MPCS                           ! on MPC's since they may have zero stiff at this point and get stiff
            IF (AGRIDI == MPC_IND_GRIDS(J)) THEN         ! later when MPC's are eliminated. These grids will get SKIPIT = 'Y'
               SKIPIT = 'Y'
               EXIT j_do4
            ENDIF
         ENDDO j_do4
         IF (SKIPIT == 'N') THEN
            CALL KGG_SINGULARITY_PROC ( AGRIDI, KGG_II, NUM_ASPC_BY_COMP )
         ENDIF

      ENDDO i_do
      IF (DEBUG(17) > 0) THEN                              ! Write trailing seperator for DEBUG output
         WRITE(F06,9902)
      ENDIF
      WRITE(F06,*)

      WRITE(SC1,*) CR13

      IF (PRTSTIFF(1) >= 1) THEN
         CALL WRITE_SPARSE_CRS ( 'STIFFNESS MATRIX KGG' , 'G ', 'G ', NTERM_KGG, NDOFG, I_KGG, J_KGG, KGG )
      ENDIF

! If AUTOSPC = Y reprint TSET table, and reset and reprint TDOF tables if we have AUTOSPC'd any DOF's

      IF (AUTOSPC == 'Y') THEN

         KGG_NUM_ASPC = 0
         DO I=1,6
            KGG_NUM_ASPC = KGG_NUM_ASPC + NUM_ASPC_BY_COMP(I)
         ENDDO

         IF (KGG_NUM_ASPC > 0) THEN

            IF (PRTTSET > 0) THEN
               WRITE(F06,56)
               WRITE(F06,57)
               DO J = 1,NGRID
                  WRITE(F06,58) GRID(J,1), GRID_SEQ(J), (TSET(J,K),K = 1,6)
               ENDDO   
               WRITE(F06,'(//)')
            ENDIF

            ASPC_SUM_MSG1(1:) = 'Stage 1:'
            ASPC_SUM_MSG2(1:) = 'after identification of AUTOSPC''s at the grid level'
            ASPC_SUM_MSG3(1:) = 'in this stage'
            CALL AUTOSPC_SUMMARY_MSGS ( ASPC_SUM_MSG1, ASPC_SUM_MSG2, ASPC_SUM_MSG3, 'Y', NUM_ASPC_BY_COMP )

            TDOF_MSG(1:)  = ' '
            TDOF_MSG(39:) = ASPC_SUM_MSG2(1:)
            CALL TDOF_PROC ( TDOF_MSG )

         ENDIF

      ENDIF

      IF (NUM_PCHD_SPC1 > 0) THEN                       ! Close SPC file and, if any records were written to it, save it
         CALL FILE_CLOSE ( SPC, SPCFIL, 'KEEP', 'Y' )
         IF (SPC1QUIT == 'Y') THEN
            WRITE(ERR,9991) SUBR_NAME, SPC1QUIT
            WRITE(F06,9991) SUBR_NAME, SPC1QUIT
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ELSE
         CALL FILE_CLOSE ( SPC, SPCFIL, 'DELETE', 'Y' )
      ENDIF

      IF (KTERM_KGG /= NTERM_KGG) THEN                      ! Check KTERM_KGG = NTERM_KGG
         WRITE(ERR,1623) SUBR_NAME, LINK1L, KTERM_KGG, NTERM_KGG
         WRITE(F06,1623) SUBR_NAME, LINK1L, KTERM_KGG, NTERM_KGG
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                             ! Coding error, so quit
      ENDIF

      CALL FILE_CLOSE ( L1L, LINK1L, 'KEEP', 'Y' )
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
   56 FORMAT(64X,'DEGREE OF FREEDOM SET TABLE (TSET)')

   57 FORMAT(33x,'     GRID SEQUENCE       T1       T2       T3       R1       R2       R3',/)

   58 FORMAT(33x,2(1X,I8),6(7X,A2))

  146 FORMAT(' *INFORMATION: NUMBER OF NONZERO TERMS IN THE KGG STIFFNESS MATRIX IS                 = ',I12,/)

  101 FORMAT(' *INFORMATION: MAX NUMBER OF NONZERO TERMS IN A ROW OF THE G-SET STIFFNESS MATRIX     = ',I12,/)

 1625 FORMAT(' *ERROR  1625: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' 1ST COL OF ARRAY STF3 INDICATES THERE IS MORE DATA IN ARRAY STF3 FOR ROW ',I12,' OF THE KGG STIFF'    &
                    ,/,14X,' MATRIX ALTHOUGH THE DOF COUNT IS AT THE END OF THE ROW')

 1611 FORMAT(' *ERROR  1611: THE G-SET STIFFNESS MATRIX, KGG, MUST HAVE SOME NONZERO TERMS. HOWEVER IT HAS ',I12,' TERMS')

 1623 FORMAT(' *ERROR  1623: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF G-SET STIFFNESS MATRIX RECORDS WRITTEN TO FILE:'                                        &
                    ,/,15X,A                                                                                                       &
                    ,/,14X,' WAS KTERM_KGG = ',I12,'. IT SHOULD HAVE BEEN NTERM_KGG = ',I12)

 9901 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::START DEBUG(17) OUTPUT FROM SUBROUTINE KGG_SINGULARITY_PROC:::::::::::::::::::',&
             ':::::::::::::::::',//,54X,'AUTOSPC_RAT = ',1ES13.6,/)

 9902 FORMAT(' :::::::::::::::::::::::::::::::::::::END DEBUG(17) OUTPUT FROM SUBROUTINE KGG_SINGULARITY_PROC::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

 9991 FORMAT(' PROCESSING ABORTED IN SUBR ',A,' BASED ON PARAMETER SPC1QUIT = ',A)

12345 FORMAT(5X,'Working on grid ',24X,I8,' of ',I8, A)




99111 format('         I      GRID     COMPS   KGG_COL    II_ROW     II_COL        N           KGG_II',/,                          &
             '         -      ----     -----   -------    ------     ------        -        ------------')
99121 format(7i10,1es20.6)

! **********************************************************************************************************************************

      END SUBROUTINE SPARSE_KGG
