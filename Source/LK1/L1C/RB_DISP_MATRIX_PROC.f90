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

      SUBROUTINE RB_DISP_MATRIX_PROC ( REF_PT_TXT, REF_PT )

! Generates a 6 x 6 rigid body displacement matrix in global coords for one grid point via the following procedure:
!    1) for each grid generate a 6 x 6 rigid body displ matrix in basic coords relative to the reference grid which can be:
!         a) a grid defined by param (REF_PT_TXT = EQCHK_REF_GRID)
!         b) basic systen origin (REF_PT_TXT = 'BASIC_ORIGIN')
!         c) model CG (REF_PT_TXT = 'CG')
!         d) an arbitrary grid (REF_PT_TXT = 'GRID' and REF_PT = the grid ID)
!    2) transform to global coords at the grid
!    3) assemble the 6 x 6 rigid body displ matrices, in global coords, for all grids
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCORD, NGRID, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI, TDOF_ROW_START
      USE PARAMS, ONLY                :  EQCHK_REF_GRID, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  RB_DISP_MATRIX_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CORD, GRID, RGRID, GRID_ID, INV_GRID_SEQ, MODEL_XCG, MODEL_YCG, MODEL_ZCG 
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE RB_DISP_MATRIX_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RB_DISP_MATRIX_PROC'
      CHARACTER(LEN=*), INTENT(IN)    :: REF_PT_TXT        ! Reference point used in calculating the 6 rigid body displ vectors
      CHARACTER( 2*BYTE)              :: COMP(6)           ! Text reference to the 6 components of displ (T1, T2, etc)

      INTEGER(LONG), INTENT(IN)       :: REF_PT            ! An actual grid ID (only used if REF_PT_TXT = 'GRID')
      INTEGER(LONG)                   :: ECORD_K     = 0   ! Global coord ID (actual) for grid AGRID_I
      INTEGER(LONG)                   :: ECORD_R     = 0   ! Global coord ID (actual) for grid AGRID_R
      INTEGER(LONG)                   :: AGRID             ! An actual grid number
      INTEGER(LONG)                   :: AGRID_K           ! Grid ID (actual) for a grid whose RB matrix is being generated
      INTEGER(LONG)                   :: AGRID_R           ! Grid ID (actual) for the reference grid (EQCHK_REF_GRID)

      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_R ! Row number in array GRID_ID where AGRID_R is found
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_K ! Row number in array GRID_ID where AGRID_K is found
      INTEGER(LONG)                   :: G_SET_COL         ! Col no. in array TDOF where the  G-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: G_SET_DOF_NUM     ! DOF number from TDOF for a G-set DOF
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices or counters
      INTEGER(LONG)                   :: ICORD_K           ! Internal coord ID corresponding to ECORD_I
      INTEGER(LONG)                   :: ICORD_R           ! Internal coord ID corresponding to ECORD_R
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: NUM_COMPS         ! 6 if GRID_NUM is an physical grid, 1 if an SPOINT
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RB_DISP_MATRIX_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: DUM1(6,6)         ! Intermediate result in obtaining RB_GRID_GLOBL
      REAL(DOUBLE)                    :: DX0               ! X coord difference between grid I and ref grid
      REAL(DOUBLE)                    :: DY0               ! Y coord difference between grid I and ref grid
      REAL(DOUBLE)                    :: DZ0               ! Z coord difference between grid I and ref grid
      REAL(DOUBLE)                    :: PHID, THETAD      ! Angles output from subr GEN_T0L, called herein but not needed here
      REAL(DOUBLE)                    :: RB_GRID_BASIC(6,6)! Rigid body displ matrix for grid AGRID_I in basic  coords
      REAL(DOUBLE)                    :: RB_GRID_GLOBL(6,6)! Rigid body displ matrix for grid AGRID_I in global coords
      REAL(DOUBLE)                    :: T0G_K(3,3)        ! Transforms a vector in basic coords to global coords for grid AGRID_I
      REAL(DOUBLE)                    :: T0G_R(3,3)        ! Transforms a vector in basic coords to global coords for grid AGRID_R
      REAL(DOUBLE)                    :: TTR_K(6,6)        ! 6x6 matrix with 2 TOG_K(3,3) matrices (one for trans, one for rot)
      REAL(DOUBLE)                    :: TTR_R(6,6)        ! 6x6 matrix with 2 TOG_R(3,3) matrices (one for trans, one for rot)
      REAL(DOUBLE)                    :: X0_R              ! Basic X coord of AGRID_R (ref grid)
      REAL(DOUBLE)                    :: Y0_R              ! Basic Y coord of AGRID_R (ref grid)
      REAL(DOUBLE)                    :: Z0_R              ! Basic Z coord of AGRID_R (ref grid)
      REAL(DOUBLE)                    :: X0_K              ! Basic X coord of AGRID_I
      REAL(DOUBLE)                    :: Y0_K              ! Basic Y coord of AGRID_I
      REAL(DOUBLE)                    :: Z0_K              ! Basic Z coord of AGRID_I
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CALL TDOF_COL_NUM ( 'G ',  G_SET_COL )

! Initialize

      DO I=1,6
         DO J=1,6
            TTR_R(I,J) = ZERO
         ENDDO
      ENDDO

! Get basic coords of the ref grid

      IF ((REF_PT_TXT == 'EQCHK REF GRID') .OR. (REF_PT_TXT == 'GRID')) THEN

         IF (REF_PT_TXT == 'EQCHK REF GRID') THEN
            AGRID_R = EQCHK_REF_GRID
         ELSE
            AGRID_R = REF_PT
         ENDIF

         IF (AGRID_R == 0) THEN
            ECORD_R = 0
            X0_R = ZERO
            Y0_R = ZERO
            Z0_R = ZERO
            DO I=1,6
               TTR_R(I,I) = ONE
            ENDDO
         ELSE
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_R, GRID_ID_ROW_NUM_R )
            IF (GRID_ID_ROW_NUM_R == -1) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1408) SUBR_NAME, AGRID_R, 'GRID_ID'
               WRITE(F06,1408) SUBR_NAME, AGRID_R, 'GRID_ID'
               CALL OUTA_HERE ( 'Y' )
            ENDIF
            CALL GET_GRID_NUM_COMPS ( AGRID_R, NUM_COMPS, SUBR_NAME )
            IF (NUM_COMPS == 6) THEN                       ! AGRID_R is a physical grid
               X0_R = RGRID(GRID_ID_ROW_NUM_R,1)
               Y0_R = RGRID(GRID_ID_ROW_NUM_R,2)
               Z0_R = RGRID(GRID_ID_ROW_NUM_R,3)
               ECORD_R = GRID(GRID_ID_ROW_NUM_R,3)
               IF (ECORD_R /= 0) THEN
                  DO I=1,NCORD                             ! We know that if ECORD_R > 0, ICORD_R is defined (chk in CORD_PROC)
                     IF (ECORD_R == CORD(I,2)) THEN
                        ICORD_R = I
                        EXIT
                     ENDIF
                  ENDDO   
                  CALL GEN_T0L ( GRID_ID_ROW_NUM_R, ICORD_R, THETAD, PHID, T0G_R )
                  DO I=1,3
                     DO J=1,3
                        TTR_R(I  ,J  ) = T0G_R(I,J)
                        TTR_R(I+3,J+3) = T0G_R(I,J)
                     ENDDO
                  ENDDO
               ELSE
                  DO I=1,6
                     TTR_R(I,I) = ONE
                  ENDDO   
               ENDIF
            ELSE
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,1001) AGRID_R
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,1001) AGRID_R
               ENDIF
               ECORD_R = 0
               X0_R = ZERO
               Y0_R = ZERO
               Z0_R = ZERO
               DO I=1,6
                  TTR_R(I,I) = ONE
               ENDDO
            ENDIF
         ENDIF

      ELSE IF (REF_PT_TXT == 'BASIC ORIGIN')  THEN

         X0_R = ZERO
         Y0_R = ZERO
         Z0_R = ZERO

         DO I=1,6
            TTR_R(I,I) = ONE
         ENDDO   

      ELSE IF (REF_PT_TXT == 'CG') THEN
      
         X0_R = MODEL_XCG
         Y0_R = MODEL_YCG
         Z0_R = MODEL_ZCG

         DO I=1,6
            TTR_R(I,I) = ONE
         ENDDO   

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1409) SUBR_NAME, REF_PT_TXT
         WRITE(F06,1409) SUBR_NAME, REF_PT_TXT
         CALL OUTA_HERE ( 'Y' )

      ENDIF

! Get the 6x6 RB matrices for each grid and transform to global

      DO K=1,NGRID

         AGRID_K = GRID_ID(INV_GRID_SEQ(K))
         CALL GET_GRID_NUM_COMPS ( AGRID_K, NUM_COMPS, SUBR_NAME )

         IF (NUM_COMPS == 6) THEN                          ! Only process physical grids. Let rows of RBGLOBAL = 0 otherwise

            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_K, GRID_ID_ROW_NUM_K )

            DO I=1,6
               DO  J=1,6
                  TTR_K(I,J) = ZERO
               ENDDO
            ENDDO

            X0_K = RGRID(GRID_ID_ROW_NUM_K,1)
            Y0_K = RGRID(GRID_ID_ROW_NUM_K,2)
            Z0_K = RGRID(GRID_ID_ROW_NUM_K,3)

            DX0  = X0_K - X0_R         
            DY0  = Y0_K - Y0_R         
            DZ0  = Z0_K - Z0_R 
            
            DO I=1,6
               DO J=1,6
                  RB_GRID_BASIC(I,J) = ZERO
               ENDDO
               RB_GRID_BASIC(I,I) = ONE
            ENDDO
            RB_GRID_BASIC(1,5) =  DZ0
            RB_GRID_BASIC(1,6) = -DY0
            RB_GRID_BASIC(2,4) = -DZ0
            RB_GRID_BASIC(2,6) =  DX0
            RB_GRID_BASIC(3,4) =  DY0
            RB_GRID_BASIC(3,5) = -DX0


            ECORD_K = GRID(GRID_ID_ROW_NUM_K,3)
            IF (ECORD_K /= 0) THEN
               DO I=1,NCORD
                  IF (ECORD_K == CORD(I,2)) THEN
                     ICORD_K = I
                     EXIT
                  ENDIF
               ENDDO   
               CALL GEN_T0L ( GRID_ID_ROW_NUM_K, ICORD_K, THETAD, PHID, T0G_K )
               DO I=1,3
                  DO J=1,3
                     TTR_K(I  ,J  ) = T0G_K(I,J)
                     TTR_K(I+3,J+3) = T0G_K(I,J)
                  ENDDO
               ENDDO
            ELSE
               DO I=1,6
                  TTR_K(I,I) = ONE
               ENDDO   
            ENDIF

            IF (ECORD_R == 0) THEN
               DO I=1,6
                  DO J=1,6
                     DUM1(I,J) = RB_GRID_BASIC(I,J)
                  ENDDO
               ENDDO
            ELSE
               CALL MATMULT_FFF   ( RB_GRID_BASIC, TTR_R, 6, 6, 6, DUM1 )
            ENDIF
            IF (ECORD_K == 0) THEN
               DO I=1,6
                  DO J=1,6
                     RB_GRID_GLOBL(I,J) = DUM1(I,J)
                  ENDDO
               ENDDO
            ELSE
               CALL MATMULT_FFF_T ( TTR_K, DUM1, 6, 6, 6, RB_GRID_GLOBL  )
            ENDIF

            IF ((DEBUG(11) == 1) .OR. (DEBUG(11) == 3)) THEN
               WRITE(F06,111) AGRID_K
               DO I=1,6
                  WRITE(F06,112) (RB_GRID_GLOBL(I,J),J=1,6)
               ENDDO
               WRITE(F06,*)
            ENDIF

            AGRID = GRID_ID(INV_GRID_SEQ(K))
!xx         CALL CALC_TDOF_ROW_NUM ( AGRID, ROW_NUM_START, 'N' )
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, IGRID )
            ROW_NUM_START = TDOF_ROW_START(IGRID)
            G_SET_DOF_NUM = TDOF(ROW_NUM_START,G_SET_COL)
            DO I=1,6
               DO J=1,6
                  RBGLOBAL_GSET(G_SET_DOF_NUM+I-1,J) = RB_GRID_GLOBL(I,J)
               ENDDO
            ENDDO

         ENDIF

      ENDDO

      IF ((DEBUG(11) == 2) .OR. (DEBUG(11) == 3)) THEN
         WRITE(F06,121)
         IF      (REF_PT_TXT == 'CG') THEN
            WRITE(F06,122)
         ELSE IF (REF_PT_TXT == 'BASIC ORIGIN'  ) THEN
            WRITE(F06,123)
         ELSE IF ((REF_PT_TXT == 'EQCHK REF GRID') .OR. (REF_PT_TXT == 'GRID')) THEN
            IF (AGRID_R == 0) THEN
               WRITE(F06,123)
            ELSE
               WRITE(F06,124) AGRID_R, ECORD_R
            ENDIF
         ENDIF
         WRITE(F06,125)
         L = 0
         COMP(1) = 'T1'
         COMP(2) = 'T2'
         COMP(3) = 'T3'
         COMP(4) = 'R1'
         COMP(5) = 'R2'
         COMP(6) = 'R3'
         DO I=1,NGRID
            AGRID_K = GRID_ID(INV_GRID_SEQ(I))
            AGRID = GRID_ID(INV_GRID_SEQ(I))
            CALL GET_GRID_NUM_COMPS ( AGRID, NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               L = L + 1
               IF (J == 1) THEN
                  WRITE(F06,141) AGRID_K, COMP(J), (RBGLOBAL_GSET(L,K),K=1,6)
               ELSE
                  WRITE(F06,142)          COMP(J), (RBGLOBAL_GSET(L,K),K=1,6)
               ENDIF
            ENDDO
            WRITE(F06,*)
         ENDDO
         WRITE(F06,*)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  111 FORMAT(' RIGID BODY DISPL MATRIX IN GLOBAL COORDS FOR GRID ',I8) 

  112 FORMAT(1X,6(1ES15.6))

  121 FORMAT('                                RBGLOBAL RIGID BODY MATRIX - G-SET RIGID BODY DISPLS IN GLOBAL COORDS')

  122 FORMAT('                                COLUMNS ARE DUE TO UNIT DISPLACEMENTS OF THE MODEL CG IN BASIC COORDS',/)

  123 FORMAT('                           COLUMNS ARE DUE TO UNIT DISPLACEMENTS AT THE BASIC SYSTEM ORIGIN IN BASIC COORDS',/)

  124 FORMAT('                        COLUMNS ARE DUE TO UNIT DISPLCEMENTS AT GRID ',I8,' IN GLOBAL COORD SYSTEM ',I8,/)

  125 FORMAT(13X,'GRID COMP       T1             T2             T3             R1             R2             R3')

  141 FORMAT(9X,I8,2X,A2,6(1ES15.6))

  142 FORMAT(19X,A2,6(1ES15.6))

 1001 FORMAT(' *WARNING    : REFERENCE GRID = ',I8,' FOR CALCULATING RIGID BODY RBGLOBAL MATRIX MUST BE A PHYSICAL GRID NOT A',    &
                           ' SCALAR POINT. BASIC ORIGIN WILL BE USED INSTEAD')

 1408 FORMAT(' *ERROR  1408: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' GRID NUMBER ',I8,' DOES NOT EXIST IN ARRAY ',A)

 1409 FORMAT(' *ERROR  1409: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INCORRECT INDICATOR OF REF PT = ',A,' FOR CALC OF RIGID BODY DISPL VECTORS')

29381 format(' In RB_DISP_MATRIX_PROC: Act grid = ',i8,' is at row ',i8,' in RGRID')


! **********************************************************************************************************************************
 
      END SUBROUTINE RB_DISP_MATRIX_PROC

