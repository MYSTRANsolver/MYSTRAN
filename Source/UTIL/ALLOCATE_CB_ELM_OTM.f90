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

      SUBROUTINE ALLOCATE_CB_ELM_OTM ( NAME_IN )

! Calculates how many rows/cols are going to be needed for element related OTM's (Output Transformation Matrices) for Craig-Bampton
! model generation runs and allocates memory to the arrays

      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR,                                                                  &
                                         ELOUT_ELFE_BIT, ELOUT_ELFN_BIT, ELOUT_STRE_BIT, ELOUT_STRN_BIT,                           &
                                         IBIT, NELE, NUM_CB_DOFS,                                                                  &
                                         NROWS_OTM_ELFE, NROWS_OTM_ELFN, NROWS_OTM_STRE, NROWS_OTM_STRN,                           &
                                         NROWS_TXT_ELFE, NROWS_TXT_ELFN, NROWS_TXT_STRE, NROWS_TXT_STRN, TOT_MB_MEM_ALLOC


      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE MODEL_STUF, ONLY            :  ELGP, ELOUT, ELMTYP, ETYPE, METYPE, NELGP, TYPE
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRN_LOC, STRE_LOC
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ELFE, OTM_ELFN, OTM_STRE, OTM_STRN, TXT_ELFE, TXT_ELFN, TXT_STRE, TXT_STRN
      USE PARAMS, ONLY                :  OTMSKIP
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_CB_ELM_OTM_BEGEND

      USE ALLOCATE_CB_ELM_OTM_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATE_CB_ELM_OTM'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name of the matrix to be allocated
      CHARACTER(LEN(NAME_IN))         :: NAME              ! Name for output error purposes

      INTEGER(LONG)                   :: ELOUT_ELFE        ! If > 0, there are ELFORCE(ENGR) requests for some elems                
      INTEGER(LONG)                   :: ELOUT_ELFN        ! If > 0, there are ELFORCE(NODE) requests for some elems                
      INTEGER(LONG)                   :: ELOUT_STRE        ! If > 0, there are STRESS   requests for some elems                
      INTEGER(LONG)                   :: ELOUT_STRN        ! If > 0, there are STRAIN   requests for some elems                
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERR              ! STAT from DEALLOCATE
      INTEGER(LONG)                   :: JERR              ! Local error indicator
      INTEGER(LONG)                   :: NCOLS             ! Number of cols in OTM matrix
      INTEGER(LONG)                   :: NROWS_MAT         ! Number of rows in OTM matrix
      INTEGER(LONG)                   :: NROWS_TXT         ! Number of rows in TXT mmatrix
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_CB_ELM_OTM_BEGEND

      REAL(DOUBLE)                    :: CUR_MB_ALLOCATED  ! MB of memory that is currently allocated to ARRAY_NAME when subr
!                                                            ALLOCATED_MEMORY is called (before entering MB_ALLOCATED into array
!                                                            ALLOCATED_ARRAY_MEM
      REAL(DOUBLE)                    :: MB_ALLOCATED      ! Megabytes of mmemory allocated for the arrays to put into array
!                                                            ALLOCATED_ARRAY_MEM when subr ALLOCATED_MEMORY is called

      INTRINSIC                       :: IAND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      JERR = 0

! ----------------------------------------------------------------------------------------------------------------------------------
      IF (NAME_IN == 'OTM_ELFE') THEN                      ! Determine size of OTM_ELFE and allocate it and TXT_ELFE                

         NROWS_MAT = 0
         NROWS_TXT = 0
         DO I=1,METYPE

             DO J = 1,NELE

               TYPE  = ETYPE(J)

               IF      (ETYPE(J)(1:4) == 'ELAS') THEN
                  IF (ETYPE(J) == ELMTYP(I)) THEN
                     ELOUT_ELFE = IAND(ELOUT(J,1),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        NROWS_MAT = NROWS_MAT + 1
                        NROWS_TXT = NROWS_TXT + 1 + OTMSKIP 
                     ENDIF
                  ENDIF

               ELSE IF (ETYPE(J)(1:4) == 'BUSH') THEN
                  IF (ETYPE(J) == ELMTYP(I)) THEN
                     ELOUT_ELFE = IAND(ELOUT(J,1),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        NROWS_MAT = NROWS_MAT + 6
                        NROWS_TXT = NROWS_TXT + 6 + OTMSKIP 
                     ENDIF
                  ENDIF

               ELSE IF (ETYPE(J)(1:3) == 'ROD') THEN
                  IF (ETYPE(J) == ELMTYP(I)) THEN
                     ELOUT_ELFE = IAND(ELOUT(J,1),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        NROWS_MAT = NROWS_MAT + 2
                        NROWS_TXT = NROWS_TXT + 2 + OTMSKIP 
                     ENDIF
                  ENDIF

               ELSE IF (ETYPE(J)(1:3) == 'BAR') THEN
                  IF (ETYPE(J) == ELMTYP(I)) THEN
                     ELOUT_ELFE = IAND(ELOUT(J,1),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        NROWS_MAT = NROWS_MAT + 8
                        NROWS_TXT = NROWS_TXT + 8 + OTMSKIP
                     ENDIF
                  ENDIF

               ELSE IF((ETYPE(J)(1:5) == 'TRIA3') .OR. (ETYPE(J)(1:5) == 'QUAD4') .OR. (ETYPE(J)(1:5) == 'SHEAR') .OR.             &
                       (ETYPE(J)(1:6) == 'USERIN')) THEN
                  IF (ETYPE(J) == ELMTYP(I)) THEN
                     ELOUT_ELFE = IAND(ELOUT(J,1),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        NROWS_MAT = NROWS_MAT + 8
                        NROWS_TXT = NROWS_TXT + 8 + OTMSKIP
                     ENDIF
                   ENDIF

               ELSE IF((ETYPE(J)(1:4) == 'HEXA'  ) .OR. (ETYPE(J)(1:5) == 'PENTA' ) .OR. (ETYPE(J)(1:5) == 'TETRA' ) .OR.          &
                       (ETYPE(J)(1:5) == 'USER1' ) .OR. (ETYPE(J)(1:6) == 'PLOTEL')) THEN
                  CONTINUE                                 ! No element forces for these elements

               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,963) SUBR_NAME, TYPE
                  WRITE(F06,963) SUBR_NAME, TYPE
                  CALL OUTA_HERE ( 'Y' )

               ENDIF

            ENDDO

         ENDDO
 
         NROWS_OTM_ELFE = NROWS_MAT
         NROWS_TXT_ELFE = NROWS_TXT
         NCOLS          = NUM_CB_DOFS

         NAME = 'OTM_ELFE'
         IF (ALLOCATED(OTM_ELFE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_ELFE(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_ELFE(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_ELFE'
         IF (ALLOCATED(TXT_ELFE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_ELFE(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_ELFE(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (NAME_IN == 'OTM_ELFN') THEN                 ! Determine size of OTM_ELFN and allocate it and TXT_ELFN

         NROWS_MAT = 0
         NROWS_TXT = 0
         DO I=1,METYPE
            DO J = 1,NELE
               TYPE  = ETYPE(J)
               IF (ETYPE(J) == ELMTYP(I)) THEN
                  ELOUT_ELFN   = IAND(ELOUT(J,1), IBIT(ELOUT_ELFN_BIT))
                  IF (ELOUT_ELFN > 0) THEN
                     NROWS_MAT = NROWS_MAT + 6*NELGP(I)
                     NROWS_TXT = NROWS_TXT + 6*NELGP(I) + OTMSKIP
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         NROWS_OTM_ELFN = NROWS_MAT
         NROWS_TXT_ELFN = NROWS_TXT
         NCOLS          = NUM_CB_DOFS
      
         NAME = 'OTM_ELFN'
         IF (ALLOCATED(OTM_ELFN)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_ELFN(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_ELFN(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_ELFN'
         IF (ALLOCATED(TXT_ELFN)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_ELFN(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_ELFN(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (NAME_IN == 'OTM_STRE') THEN                 ! Determine size of OTM_STRE and allocate it and TXT_STRE

         NROWS_MAT = 0
         NROWS_TXT = 0
         DO I=1,METYPE
            DO J = 1,NELE
               TYPE  = ETYPE(J)
               IF (ETYPE(J) == ELMTYP(I)) THEN
                  ELOUT_STRE = IAND(ELOUT(J,1),IBIT(ELOUT_STRE_BIT))
                  IF (ELOUT_STRE > 0) THEN
                     IF (TYPE(1:4) == 'ELAS') THEN
                        NROWS_MAT = NROWS_MAT + 1
                        NROWS_TXT = NROWS_TXT + 1 + OTMSKIP
                     ELSE IF (TYPE == 'ROD     ') THEN
                        NROWS_MAT = NROWS_MAT + 4
                        NROWS_TXT = NROWS_TXT + 4 + OTMSKIP
                     ELSE IF (TYPE == 'BAR     ') THEN
                        NROWS_MAT = NROWS_MAT + 18
                        NROWS_TXT = NROWS_TXT + 18 + OTMSKIP
                     ELSE IF (TYPE(1:5) == 'QUAD4') THEN
                        IF ((STRE_LOC == 'CORNER  ') .OR. (STRE_LOC == 'GAUSS   ')) THEN
                           NROWS_MAT = NROWS_MAT + 100     ! 10 stresses each for -Z1, Z1 mult by 5 (CENTER + 4 grids)
                           NROWS_TXT = NROWS_TXT + 100 + OTMSKIP
                        ELSE
                           NROWS_MAT = NROWS_MAT + 20
                           NROWS_TXT = NROWS_TXT + 20 + OTMSKIP
                        ENDIF
                     ELSE IF (TYPE(1:5) == 'TRIA3') THEN
                        NROWS_MAT = NROWS_MAT + 20
                        NROWS_TXT = NROWS_TXT + 20 + OTMSKIP
                     ELSE IF (TYPE == 'USERIN  ') THEN
                        NROWS_MAT = NROWS_MAT + 9
                        NROWS_TXT = NROWS_TXT + 9 + OTMSKIP
                     ELSE IF ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN
                        NROWS_MAT = NROWS_MAT + 6
                        NROWS_TXT = NROWS_TXT + 6 + OTMSKIP
                     ENDIF   
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         NROWS_OTM_STRE = NROWS_MAT
         NROWS_TXT_STRE = NROWS_TXT
         NCOLS          = NUM_CB_DOFS
 
         NAME = 'OTM_STRE'
         IF (ALLOCATED(OTM_STRE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_STRE(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_STRE(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_STRE'
         IF (ALLOCATED(TXT_STRE)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_STRE(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_STRE(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (NAME_IN == 'OTM_STRN') THEN                 ! Determine size of OTM_STRN and allocate it and TXT_STRN

         NROWS_MAT = 0
         NROWS_TXT = 0
         DO I=1,METYPE
            DO J = 1,NELE
               TYPE  = ETYPE(J)
               IF (ETYPE(J) == ELMTYP(I)) THEN
                  ELOUT_STRN = IAND(ELOUT(J,1),IBIT(ELOUT_STRN_BIT))
                  IF (ELOUT_STRN > 0) THEN
                     IF      (TYPE(1:5) == 'QUAD4') THEN
                        IF ((STRN_LOC == 'CORNER  ') .OR. (STRN_LOC == 'GAUSS   ')) THEN
                           NROWS_MAT = NROWS_MAT + 100     ! 10 strains each for -Z1, Z1 mult by 5 (CENTER + 4 grids)
                           NROWS_TXT = NROWS_TXT + 100 + OTMSKIP
                        ELSE
                           NROWS_MAT = NROWS_MAT + 20
                           NROWS_TXT = NROWS_TXT + 20 + OTMSKIP
                        ENDIF
                     ELSE IF (TYPE(1:5) == 'TRIA3') THEN
                        NROWS_MAT = NROWS_MAT + 20
                        NROWS_TXT = NROWS_TXT + 20 + OTMSKIP
                     ELSE IF (TYPE == 'USERIN  ') THEN
                        NROWS_MAT = NROWS_MAT + 9
                        NROWS_TXT = NROWS_TXT + 9 + OTMSKIP
                     ELSE IF ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN
                        NROWS_MAT = NROWS_MAT + 6
                        NROWS_TXT = NROWS_TXT + 6 + OTMSKIP
                     ENDIF   
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         NROWS_OTM_STRN = NROWS_MAT
         NROWS_TXT_STRN = NROWS_TXT
         NCOLS          = NUM_CB_DOFS
 
         NAME = 'OTM_STRN'
         IF (ALLOCATED(OTM_STRN)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (OTM_STRN(NROWS_MAT,NCOLS),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(DOUBLE)*REAL(NROWS_MAT)*REAL(NCOLS)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_MAT, NCOLS, SUBR_BEGEND )
               DO I=1,NROWS_MAT
                  DO J=1,NCOLS
                     OTM_STRN(I,J) = ZERO
                  ENDDO
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

         NAME = 'TXT_STRN'
         IF (ALLOCATED(TXT_STRN)) THEN
            WRITE(ERR,990) SUBR_NAME, NAME
            WRITE(F06,990) SUBR_NAME, NAME
            FATAL_ERR = FATAL_ERR + 1
            JERR = JERR + 1
         ELSE
            ALLOCATE (TXT_STRN(NROWS_TXT),STAT=IERR)
            IF (IERR == 0) THEN
               MB_ALLOCATED = REAL(NROWS_TXT)/ONEPP6
               CALL ALLOCATED_MEMORY ( NAME, MB_ALLOCATED, 'ALLOC', 'Y', CUR_MB_ALLOCATED, SUBR_NAME )
               CALL WRITE_MEM_SUM_TO_F04 ( NAME, 'ALLOC', MB_ALLOCATED, NROWS_TXT, 1, SUBR_BEGEND )
               DO I=1,NROWS_TXT
                  TXT_STRN(I)(1:) = ' '
               ENDDO
            ELSE
               WRITE(ERR,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               WRITE(F06,991) MB_ALLOCATED, NAME, SUBR_NAME, IERR
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
            ENDIF
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
      ELSE                                                 ! NAME not recognized, so coding error

         WRITE(ERR,915) SUBR_NAME, 'ALLOCATED', NAME_IN 
         WRITE(F06,915) SUBR_NAME, 'ALLOCATED', NAME_IN
         FATAL_ERR = FATAL_ERR + JERR
         JERR = JERR + 1

      ENDIF
 
! ---------------------------------------------------------------------------------------------------------------------------------
! Quit if there were errors

      IF (JERR /= 0) THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME, TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  915 FORMAT(' *ERROR   915: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NAME OF ARRAY TO BE ',A,' IS INCORRECT. INPUT NAME WAS ',A)

  963 FORMAT(' *ERROR   946: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NO CODE FOR ELEMENT TYPE "',A,'"')

  990 FORMAT(' *ERROR   990: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT ALLOCATE MEMORY TO ARRAY ',A,'. IT IS ALREADY ALLOCATED')

  991 FORMAT(' *ERROR   991: CANNOT ALLOCATE ',F10.3,' MB OF MEMORY TO ARRAY ',A,' IN SUBROUTINE ',A                               &
                    ,/,14X,' ALLOCATION STAT = ',I8)

! **********************************************************************************************************************************

      END SUBROUTINE ALLOCATE_CB_ELM_OTM

