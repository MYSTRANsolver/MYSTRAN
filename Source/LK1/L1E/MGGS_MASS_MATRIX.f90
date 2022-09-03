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
 
      SUBROUTINE MGGS_MASS_MATRIX
 
! Forms the sparse scalar mass matrix, MGGS, (for masses defined on Bulk Data CMASS)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, NCMASS, NDOFG, NGRID, NPMASS, NTERM_MGGS, BLNK_SUB_NAM
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  SPARSTOR, WTMASS
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOF
      USE MODEL_STUF, ONLY            :  CMASS, GRID_ID, PMASS, RPMASS
      USE SPARSE_MATRICES, ONLY       :  I_MGGS, J_MGGS, MGGS
      USE SUBR_BEGEND_LEVELS, ONLY    :  MGGS_MASS_MATRIX_BEGEND
 
      USE MGGS_MASS_MATRIX_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MGGS_MASS_MATRIX'
      CHARACTER( 1*BYTE)              :: FOUND             ! 'Y'/'N' indicator of whether we found something

      INTEGER(LONG)                   :: G_SET_DOF(NGRID)  ! G-set array with grid actual ID's for the grids that have scalar mass
      INTEGER(LONG)                   :: G_SET_COL         ! Col in TDOF where G-set exists
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters   
      INTEGER(LONG)                   :: IERROR            ! Local error count
      INTEGER(LONG)                   :: IDOF(NCMASS)      ! G-set DOF number   
      INTEGER(LONG)                   :: KTERM_MGGS        ! Count of number of terma going into MGGS
      INTEGER(LONG)                   :: ROW_NUM           ! Row number in TDOF where data begins for IGRID
      INTEGER(LONG)                   :: SGRID(NCMASS)     ! Grid number for a scalar mass (from array CMASS)
      INTEGER(LONG)                   :: PMASS_ID(NCMASS)  ! Prop ID for the CMASS that is attached to SGRID(I) (from array PMASS)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MGGS_MASS_MATRIX_BEGEND

      REAL(DOUBLE)                    :: PMASS_VAL(NCMASS) ! Value for the mass attached to SGRID(I)

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      IERROR = 0

      NTERM_MGGS = NDOFG+1                                 ! Max possible NTERM_MGGS. Will be set to actual later by nonzero count
      CALL ALLOCATE_L1_MGG ( 'MGGS', SUBR_NAME )

      CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
      K = 0
      DO I=1,NDOFG
         IF (TDOF(I,2) == 1) THEN
            K = K + 1
            G_SET_DOF(K)  = TDOF(I,G_SET_COL)
         ENDIF
      ENDDO

      DO I=1,NCMASS

         IF (CMASS(I,4) /= 0) THEN                         ! The scalar point is in either col 4 or 6 in CMASS (checked in BD read)
            SGRID(I) = CMASS(I,4)
         ELSE
            SGRID(I) = CMASS(I,6)
         ENDIF
         PMASS_ID(I) = CMASS(I,3)

         ROW_NUM = -1
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, SGRID(I), ROW_NUM )
         IF (ROW_NUM /= -1) THEN
            IDOF(I) = G_SET_DOF(ROW_NUM)
         ELSE
            IERROR    = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1361) 'GRID OR SPOINT', SGRID, 'BULK DATA CMASS ENTRY'
            WRITE(F06,1361) 'GRID OR SPOINT', SGRID, 'BULK DATA CMASS ENTRY'
         ENDIF

      ENDDO

! Get mass value at each SGRID (PMASS_VAL array) 

i_do1:DO I=1,NCMASS
         FOUND = 'N'
j_do1:   DO J=1,NPMASS
            IF (PMASS_ID(I) == PMASS(J,1)) THEN
               PMASS_VAL(I) = RPMASS(J,1)
               FOUND = 'Y'
               EXIT j_do1
            ENDIF
         ENDDO j_do1
         IF (FOUND == 'N') THEN
            IERROR    = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1601) PMASS_ID(I), CMASS(I,1) 
            WRITE(ERR,1601) PMASS_ID(I), CMASS(I,1) 
         ENDIF
      ENDDO i_do1
         
      IF (DEBUG(182) > 0) CALL DEB_MGGS ( 1 )

! Quit if IERROR > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,9999) IERROR
         WRITE(F06,9999) IERROR
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Sort arrays IDOF, SGRID, PMASS_VAL so that IDOF is in numerical order (G-set DOF order)

      CALL SORT_INT2_REAL1 ( SUBR_NAME, 'IDOF, SGRI, PMASS_VAL', NCMASS, IDOF, SGRID, PMASS_VAL )
      IF (DEBUG(182) > 0) CALL DEB_MGGS ( 2 )

! Formulate sparse matrix MGGS. Note that the MGGS matrix is diagonal since CMASS is attached to only 1 grid/scalar point

      KTERM_MGGS = 0
      I_MGGS(1) = 1
i_do2:DO I=1,NDOFG
j_do2:   DO J=1,NCMASS
            IF (IDOF(J) == I) THEN
               I_MGGS(I+1)        = I_MGGS(I) + 1
               KTERM_MGGS         = KTERM_MGGS + 1
               IF (KTERM_MGGS > NTERM_MGGS) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_MGGS, 'MGGS' )
               J_MGGS(KTERM_MGGS) = I                      ! Since MGGS is diagonal
               MGGS(KTERM_MGGS)   = PMASS_VAL(J)
               EXIT j_do2
            ELSE
               I_MGGS(I+1) = I_MGGS(I)
            ENDIF
         ENDDO j_do2
      ENDDO i_do2

      IF (DEBUG(182) > 0) CALL DEB_MGGS ( 3 )
      
! Reset NTERM_MGGS to what was counted above

      NTERM_MGGS = KTERM_MGGS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1361 FORMAT(' *ERROR  1361: UNDEFINED ',A,I8,' ON ',A)

 1601 FORMAT(' *ERROR  1601: UNDEFINED SCALAR MASS PROPERTY ID = ',I8,' ON CMASS ID ',I8)

 9999 FORMAT(' PROCESSING TERMINATED DUE TO ABOVE ',I8,' ERRORS')


! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEB_MGGS ( WHAT )

      USE PENTIUM_II_KIND, ONLY       :  LONG

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: WHAT              ! What to print on this call
      INTEGER(LONG)                   :: II                ! DO loop index

! **********************************************************************************************************************************
      IF (WHAT == 1) THEN
         WRITE(F06,1001)
         WRITE(F06,*) 'I, IDOF, SGRID, PMASS_ID before sorting IDOF, SGRID, PMASS_VAL on IDOF'
         DO II=1,NCMASS
            WRITE(F06,1002) II, IDOF(II), SGRID(II), PMASS_ID(II)
         ENDDO
         WRITE(F06,*)
      ENDIF

      IF (WHAT == 2) THEN
         WRITE(F06,*) 'I, IDOF(I), SGRID(I), PMASS_VAL(I) after  sort on IDOF'
         DO II=1,NCMASS
            WRITE(F06,2001) II, IDOF(II), SGRID(II), PMASS_VAL(II)
         ENDDO
         WRITE(F06,*)
      ENDIF

      IF (WHAT == 3) THEN
         WRITE(F06,*) ' Sparse arrays for MGGS'
         DO II=1,NDOFG+1
            WRITE(F06,3001) II, I_MGGS(II)
         ENDDO
         WRITE(F06,*)
         DO II=1,KTERM_MGGS
            WRITE(F06,3002) II, J_MGGS(II), MGGS(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,3999)
      ENDIF

! **********************************************************************************************************************************
 1001 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::START DEBUG(182) OUTPUT FROM SUBROUTINE MGGS_MASS_MATRIX::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1002 format(' In MGGS_MASS_MATRIX: I, IDOF(I), SGRID(I), PMASS_ID(I)  = ',4i8)

 2001 format(' In MGGS_MASS_MATRIX: I, IDOF(I), SGRID(I), PMASS_VAL(I) = ',3i8,1es14.6)

 3001 format(' In MGGS_MASS_MATRIX: I, I_MGGS(I)                       = ',2i8)

 3002 format(' In MGGS_MASS_MATRIX: I, J_MGGS(I), MGGS(I)              = ',2i8,1es14.6)

 3999 FORMAT(' ::::::::::::::::::::::::::::::::::::::::END DEBUG(15) OUTPUT FROM SUBROUTINE CONM2_PROC_1:::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE DEB_MGGS

      END SUBROUTINE MGGS_MASS_MATRIX
