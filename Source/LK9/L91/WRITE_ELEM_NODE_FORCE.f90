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
  
      SUBROUTINE WRITE_ELEM_NODE_FORCE ( JSUB, NUM_ELGP, NUM, IHDR )
  
! Writes blocks of elem nodal force output for one elem type, one subcase. All elements can have node force output
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, INT_SC_NUM, NDOFR, NUM_CB_DOFS, MOGEL, NVEC, SOL_NAME
      USE PARAMS, ONLY                :  ELFORCEN
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ELEM_NODE_FORCE_BEGEND
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, EID_OUT_ARRAY, MAXREQ, OGEL
      USE MODEL_STUF, ONLY            :  ELEM_ONAME, LABEL, SCNUM, STITLE, TITLE
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  
  
      USE WRITE_ELEM_NODE_FORCE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ELEM_NODE_FORCE'
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write output header
      CHARACTER(14*BYTE)              :: ABS_ANS_CHAR(6)   ! Character variable that contains the 6 grid abs  outputs
      CHARACTER(14*BYTE)              :: MAX_ANS_CHAR(6)   ! Character variable that contains the 6 grid max  outputs
      CHARACTER(14*BYTE)              :: MIN_ANS_CHAR(6)   ! Character variable that contains the 6 grid min  outputs
      CHARACTER(11*BYTE)              :: FORCE_COORD_SYS   ! Indicator of whether output is in global or basic coord system
      CHARACTER(14*BYTE)              :: OGEL_CHAR(MOGEL)  ! Char representation of 1 row of OGEL outputs
      CHARACTER(LEN=LEN(ELEM_ONAME))  :: ONAME             ! Element name to write out in F06 file
  
      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), INTENT(IN)       :: NUM_ELGP          ! The number of grid points for the elem being processed
      INTEGER(LONG)                   :: BDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_DOF_NUM       ! DOF number for BDY_GRID/BDY_COMP
      INTEGER(LONG)                   :: I,J,K,M           ! DO loop indices
      INTEGER(LONG)                   :: L                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ELEM_NODE_FORCE_BEGEND
  
      REAL(DOUBLE)                    :: ABS_ANS(6)        ! Max Abs for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(6)        ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(6)        ! Min for all grids output for each of the 6 disp components

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Get element output name
 
      ONAME(1:) = ' '
      CALL GET_ELEM_ONAME ( ONAME )
 
! Write output headers if this is not the first use of this subr.

      IF (IHDR == 'Y') THEN

         WRITE(F06,*)
         WRITE(F06,*)
         IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

            WRITE(F06,9101) SCNUM(JSUB)

         ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

            WRITE(F06,9102) JSUB

         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

            IF ((JSUB <= NDOFR) .OR. (JSUB >= NDOFR+NVEC)) THEN 
               IF (JSUB <= NDOFR) THEN
                  BDY_DOF_NUM = JSUB
               ELSE
                  BDY_DOF_NUM = JSUB-(NDOFR+NVEC)
               ENDIF
               CALL GET_GRID_AND_COMP ( 'R ', BDY_DOF_NUM, BDY_GRID, BDY_COMP  )
            ENDIF

            IF       (JSUB <= NDOFR) THEN
               WRITE(F06,9103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
            ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
               WRITE(F06,9105) JSUB, NUM_CB_DOFS, JSUB-NDOFR
            ELSE
               WRITE(F06,9103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
            ENDIF

         ENDIF

         IF (TITLE(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,9113) TITLE(INT_SC_NUM)
         ENDIF

         IF (STITLE(INT_SC_NUM)(1:) /= ' ') THEN
            WRITE(F06,9113) STITLE(INT_SC_NUM)
         ENDIF

         IF (LABEL(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,9113) LABEL(INT_SC_NUM)
         ENDIF

         WRITE(F06,*)

         IF      (ELFORCEN == 'LOCAL') THEN
            FORCE_COORD_SYS = 'L O C A L'
         ELSE IF (ELFORCEN == 'GLOBAL') THEN
            FORCE_COORD_SYS = 'G L O B A L'
         ELSE IF (ELFORCEN == 'BASIC' ) THEN
            FORCE_COORD_SYS = 'B A S I C  '
         ENDIF

         IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
            WRITE(F06,202) FORCE_COORD_SYS
         ELSE
            WRITE(F06,201) FORCE_COORD_SYS
         ENDIF

         WRITE(F06,212) ONAME
         WRITE(F06,213)

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,*)
            IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

               WRITE(ANS,9101) SCNUM(JSUB)

            ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

               WRITE(ANS,9102) JSUB

            ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

               IF ((JSUB <= NDOFR) .OR. (JSUB >= NDOFR+NVEC)) THEN 
                  IF (JSUB <= NDOFR) THEN
                     BDY_DOF_NUM = JSUB
                  ELSE
                     BDY_DOF_NUM = JSUB-(NDOFR+NVEC)
                  ENDIF
                  CALL GET_GRID_AND_COMP ( 'R ', BDY_DOF_NUM, BDY_GRID, BDY_COMP  )
               ENDIF

               IF       (JSUB <= NDOFR) THEN
                  WRITE(ANS,9103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
               ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
                  WRITE(ANS,9105) JSUB, NUM_CB_DOFS, JSUB-NDOFR
               ELSE
                  WRITE(ANS,9103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
               ENDIF

            ENDIF

            WRITE(ANS,*)

            IF      (ELFORCEN == 'LOCAL') THEN
               FORCE_COORD_SYS = 'L O C A L'
            ELSE IF (ELFORCEN == 'GLOBAL') THEN
               FORCE_COORD_SYS = 'G L O B A L'
            ELSE IF (ELFORCEN == 'BASIC' ) THEN
               FORCE_COORD_SYS = 'B A S I C  '
            ENDIF
            WRITE(ANS,201) FORCE_COORD_SYS
            WRITE(ANS,212) ONAME
            WRITE(ANS,213)
         ENDIF

      ENDIF
 
! Get MAX, MIN, ABS values

      DO J=1,6
         MAX_ANS(J) = -MACH_LARGE_NUM 
      ENDDO 

      L = 0
      DO I=1,NUM
         DO J=1,NUM_ELGP
            L = L + 1
            DO M=1,6
               IF (OGEL(L,M) > MAX_ANS(M)) THEN
                  MAX_ANS(M) = OGEL(L,M)
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      DO J=1,6
         MIN_ANS(J) = MAX_ANS(J)
      ENDDO

      L = 0
      DO I=1,NUM
         DO J=1,NUM_ELGP
            L = L + 1
            DO M=1,6
               IF (OGEL(L,M) < MIN_ANS(M)) THEN
                  MIN_ANS(M) = OGEL(L,M)
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      DO I=1,6
         ABS_ANS(I) = MAX( DABS(MAX_ANS(I)), DABS(MIN_ANS(I)) )
      ENDDO

      DO I=1,6

         IF (ABS_ANS(I) == 0.0) THEN
            WRITE(ABS_ANS_CHAR(I),'(A)') '  0.0         '
         ELSE
            WRITE(ABS_ANS_CHAR(I),'(1ES14.6)') ABS_ANS(I)
         ENDIF

         IF (MAX_ANS(I) == 0.0) THEN
            WRITE(MAX_ANS_CHAR(I),'(A)') '  0.0         '
         ELSE
            WRITE(MAX_ANS_CHAR(I),'(1ES14.6)') MAX_ANS(I)
         ENDIF

         IF (MIN_ANS(I) == 0.0) THEN
            WRITE(MIN_ANS_CHAR(I),'(A)') '  0.0         '
         ELSE
            WRITE(MIN_ANS_CHAR(I),'(1ES14.6)') MIN_ANS(I)
         ENDIF

      ENDDO

! Write the elem force output
  
      L = 0
      DO I=1,NUM
  
         DO J=1,NUM_ELGP

            L = L + 1

            CALL WRT_REAL_TO_CHAR_VAR ( OGEL, MAXREQ, MOGEL, L, OGEL_CHAR )

            IF (J == 1) THEN
               WRITE(F06,221) EID_OUT_ARRAY(I,1),GID_OUT_ARRAY(I,J),(OGEL_CHAR(K),K=1,6)
               IF (DEBUG(200) > 0) THEN
                  WRITE(ANS,291) EID_OUT_ARRAY(I,1),GID_OUT_ARRAY(I,J),(OGEL(L,K),K=1,6)
               ENDIF
            ELSE
               WRITE(F06,222) GID_OUT_ARRAY(I,J),(OGEL_CHAR(K),K=1,6)        
               IF (DEBUG(200) > 0) THEN
                  WRITE(ANS,292) GID_OUT_ARRAY(I,J),(OGEL(L,K),K=1,6)        
               ENDIF
            ENDIF

         ENDDO
 
         WRITE(F06,*)
         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
         ENDIF

      ENDDO 
  
      DO I=1,6
         ABS_ANS(I) = MAX( DABS(MAX_ANS(I)), DABS(MIN_ANS(I)) )
      ENDDO

      WRITE(F06,9111) (MAX_ANS_CHAR(J),J=1,6),(MIN_ANS_CHAR(J),J=1,6),(ABS_ANS_CHAR(J),J=1,6)
      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,9191) (MAX_ANS(J),J=1,6),(MIN_ANS(J),J=1,6),(ABS_ANS (J),J=1,6)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  201 FORMAT(34X,'E L E M   N O D A L   F O R C E S   I N   ',A,'   C O O R D S')

  202 FORMAT(27X,'C B   E L E M   N O D A L   F O R C E   O T M   I N   ',A,'   C O O R D S')

  212 FORMAT(47X,'F O R   E L E M E N T   T Y P E   ',A11)

  213 FORMAT(6X,'Element     Grid         T1            T2            T3            R1            R2            R3'  &
           ,/,'         ID      Point')

  221 FORMAT(6X,2(1X,I8),6A)

  222 FORMAT(16X,I8,6A)

  291 FORMAT(6X,2(1X,I8),6(1ES14.6))

  292 FORMAT(16X,I8,6(1ES14.6))

 9101 FORMAT(' OUTPUT FOR SUBCASE ',I8)

 9102 FORMAT(' OUTPUT FOR EIGENVECTOR ',I8)

 9103 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (boundary ',A,' for grid',I8,' component',I2,')')

 9105 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (modal acceleration for mode ',I8,')')

 9113 FORMAT(1X,A)

 9111 FORMAT(12X,'             ------------- ------------- ------------- ------------- ------------- -------------',/,&
             16X,'MAX* :  ',6A,/,                                                                                                  &
             16X,'MIN* :  ',6A,//,                                                                                                 &
             16X,'ABS* :  ',6A,/                                                                                                   &
             16X,'* for output set')

 9191 FORMAT(12X,'             ------------- ------------- ------------- ------------- ------------- -------------',/,&
             1X,'MAX (for output set):  ',6(1ES14.6),/,                                                                            &
             1X,'MIN (for output set):  ',6(1ES14.6),//,                                                                           &
             1X,'ABS (for output set):  ',6(1ES14.6))

 8001 FORMAT(A1)
  
! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_ELEM_NODE_FORCE
