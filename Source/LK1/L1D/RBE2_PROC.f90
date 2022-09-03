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

      SUBROUTINE RBE2_PROC ( RTYPE, REC_NO, IERR )

! Processes a single RBE2 rigid element, per call, to get terms for the RMG constraint matrix. When the Bulk data was read, the
! RBE2 input data was written to file LINK1F. In this subr, file LINK1F is read and RBE2 terms for array RMG are calculated and
! written to file LINK1J.  Later, in subr SPARSE_RMG, LINK1J will be read to create the sparse array RMG (of all rigid element and
! MPC coefficients) which will be used in LINK2 to reduce the G-set mass, stiffness and load matrices to the N-set. 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F, LINK1F, L1F_MSG, L1J
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCORD, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  GRID, RGRID, GRID_ID, CORD
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE RBE2_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RBE2_PROC'
      CHARACTER( 8*BYTE), INTENT(IN)  :: RTYPE             ! The type of rigid element being processed (RBE2)
      CHARACTER( 1*BYTE)              :: CDOF(6)           ! An output from subr RDOF (= 1 if a digit 1-6 is in DDOF)

      INTEGER(LONG), INTENT(INOUT)    :: IERR              ! Count of errors in RIGID_ELEM_PROC
      INTEGER(LONG), INTENT(INOUT)    :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: AGRID_D           ! Dep   grid ID (actual) read from a record of file LINK1F
      INTEGER(LONG)                   :: AGRID_I           ! Indep grid ID (actual) read from a record of file LINK1F
      INTEGER(LONG)                   :: DDOF              ! Dep DOF's for AGRID_D
      INTEGER(LONG)                   :: ECORD_D           ! Global coord ID (actual) for grid AGRID_D
      INTEGER(LONG)                   :: ECORD_I           ! Global coord ID (actual) for grid AGRID_I
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_D ! Row number in array GRID_ID where AGRID_D is found
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_I ! Row number in array GRID_ID where AGRID_I is found
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no., in TDOF array, of the G-set DOF list
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ICORD_D           ! Internal coord ID corresponding to ECORD_D
      INTEGER(LONG)                   :: ICORD_I           ! Internal coord ID corresponding to ECORD_I
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: IROW              ! A row number in matrix RDI_GLOBAL
      INTEGER(LONG)                   :: JERR              ! Local error count
      INTEGER(LONG)                   :: M_SET_COL_NUM     ! Col no., in TDOF array, of the M-set DOF list
      INTEGER(LONG)                   :: NUM_COMPS_D       ! 6 if AGRID_D is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: NUM_COMPS_I       ! 6 if AGRID_I is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REID              ! RBE2 elem ID read from file LINK1F
      INTEGER(LONG)                   :: RMG_COL_NUM       ! Col no. of a term in array RMG
      INTEGER(LONG)                   :: RMG_ROW_NUM       ! Row no. of a term in array RMG
      INTEGER(LONG)                   :: ROW_NUM           ! A row number in array TDOF
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: DELTA_0(3,3)      ! 3 x 3 matrix of diffs in coords bet dep & indep grids in basic coords
      REAL(DOUBLE)                    :: DUM1(3,3)         ! Intermediate result in obtaining RDI_GLOBAL
      REAL(DOUBLE)                    :: DUM2(3,3)         ! Intermediate result in obtaining RDI_GLOBAL
      REAL(DOUBLE)                    :: DUM3(3,3)         ! Intermediate result in obtaining RDI_GLOBAL
      REAL(DOUBLE)                    :: PHID, THETAD      ! Angles output from subr GEN_T0L, called herein but not needed here
      REAL(DOUBLE)                    :: RDI_GLOBAL(6,6)   ! RDI matrix (see explanation below)
      REAL(DOUBLE)                    :: T0G_D(3,3)        ! Transforms a dep   DOF vector in basic coords to global coords
      REAL(DOUBLE)                    :: T0G_I(3,3)        ! Transforms a indep DOF vector in basic coords to global coords
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06

      JERR = 0 

      READ(L1F,IOSTAT=IOCHK) REID, AGRID_D, DDOF, AGRID_I   
      REC_NO = REC_NO + 1
      IF (IOCHK == 0) THEN
         CALL GET_GRID_NUM_COMPS ( AGRID_D, NUM_COMPS_D, SUBR_NAME )
         IF (NUM_COMPS_D /= 6) THEN
            IERR = IERR + 1
            JERR = JERR + 1
            WRITE(ERR,1951) 'RBE2', REID, NUM_COMPS_D
            WRITE(F06,1951) 'RBE2', REID, NUM_COMPS_D
         ENDIF
         CALL GET_GRID_NUM_COMPS ( AGRID_I, NUM_COMPS_I, SUBR_NAME )
         IF (NUM_COMPS_I /= 6) THEN
            IERR = IERR + 1
            JERR = JERR + 1
            WRITE(ERR,1951) 'RBE2', REID, NUM_COMPS_I
            WRITE(F06,1951) 'RBE2', REID, NUM_COMPS_I
         ENDIF
      ELSE
         CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
         IERR = IERR + 1
         JERR = JERR + 1
      ENDIF

! Return if local error count > 0

      IF (JERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF      

! We know that the indep and dep grids (AGRID_I and AGRID_D) exist. This was checked in subr DOF_PROC.
! Get the basic-to-global trensformation matrices for AGRID_D and AGRID_I

      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, GRID_ID_ROW_NUM_D )
      ECORD_D = GRID(GRID_ID_ROW_NUM_D,3)
      IF (ECORD_D /= 0) THEN
         DO I=1,NCORD
            IF (ECORD_D == CORD(I,2)) THEN
               ICORD_D = I
               EXIT
            ENDIF
         ENDDO   
         CALL GEN_T0L ( GRID_ID_ROW_NUM_D, ICORD_D, THETAD, PHID, T0G_D )
      ELSE
         DO I=1,3
            DO J=1,3
               T0G_D(I,J) = ZERO
            ENDDO   
            T0G_D(I,I) = ONE
         ENDDO   
      ENDIF
 
      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I, GRID_ID_ROW_NUM_I )
      ECORD_I = GRID(GRID_ID_ROW_NUM_I,3)
      IF (ECORD_I /= 0) THEN
         DO I=1,NCORD
            IF (ECORD_I == CORD(I,2)) THEN
               ICORD_I = I
               EXIT
            ENDIF
         ENDDO   
         CALL GEN_T0L ( GRID_ID_ROW_NUM_I, ICORD_I, THETAD, PHID, T0G_I )
      ELSE
         DO I=1,3
            DO J=1,3
               T0G_I(I,J) = ZERO
            ENDDO   
            T0G_I(I,I) = ONE
         ENDDO   
      ENDIF
 
! Generate DELTA_0

      DO I=1,3
         DO J=1,3
            DELTA_0(I,J) = ZERO
         ENDDO
      ENDDO
      DELTA_0(1,2) =  (RGRID(GRID_ID_ROW_NUM_D,3) - RGRID(GRID_ID_ROW_NUM_I,3))
      DELTA_0(1,3) = -(RGRID(GRID_ID_ROW_NUM_D,2) - RGRID(GRID_ID_ROW_NUM_I,2)) 
      DELTA_0(2,1) = -DELTA_0(1,2)
      DELTA_0(2,3) =  (RGRID(GRID_ID_ROW_NUM_D,1) - RGRID(GRID_ID_ROW_NUM_I,1)) 
      DELTA_0(3,1) = -DELTA_0(1,3)
      DELTA_0(3,2) = -DELTA_0(2,3)

! The global constraint equations are
!                                           | UN | 
!                     RMG x UG = [RMN | RMM]|    | = 0
!                                           | UM | 

! where UM are all of the dependent DOF's and UN are all indep. DOF's

! However, we first have to cast them, for each rigid element, as:

!                                           | UD |
!                     RMG x UG = [ I  | RDI]|    | = 0
!                                           | UI | 

! where UD are all dependent (or M-set) DOF's, but UI may contain M-set, as well as N-set, DOF's. 

! Generate the 6 x 6 matrix of RDI coeffs for this rigid elem. The 6 x 6 consists of three nonzero 3 x 3 matrices:
!   1) The upper left  3 x 3 partition is (T0G_D)(transpose) x (T0G_I)
!   2) The upper right 3 x 3 partition is (T0G_D)(transpose) x (DELTA_0) x (T0G_I)
!   3) The lower left  3 x 3 partition is null
!   4) The lower right 3 x 3 partition is the same as the upper left 3 x 3 partition

! Multiply (T0G_D)(trans) x (T0G_I) to get DUM1 (upper left and lower right 3 x 3 partitions of the 6 x 6 RDI matrix
! of coeffs for this rigid elem)

      CALL MATMULT_FFF_T ( T0G_D, T0G_I, 3, 3, 3, DUM1 ) 

! Now get DUM3 (upper right 3 x 3 partition of the 6 x 6 RDI matrix of coefficients for this rigid element) 

      CALL MATMULT_FFF   ( DELTA_0, T0G_I, 3, 3, 3, DUM2 ) ! Multiply DELTA_0 x (T0G_I)
      CALL MATMULT_FFF_T ( T0G_D  , DUM2 , 3, 3, 3, DUM3 ) ! Multiply (T0G_D)(transpose) x DUM2

! RDI_GLOBAL is the 6 x 6 matrix of RDI coeffs for this element

      DO I=1,3
         DO J=1,3
            RDI_GLOBAL(I  ,J  ) = -DUM1(I,J)
            RDI_GLOBAL(I  ,J+3) = -DUM3(I,J)
            RDI_GLOBAL(I+3,J  ) =  ZERO
            RDI_GLOBAL(I+3,J+3) = -DUM1(I,J)
         ENDDO
      ENDDO

! Now put all coeffs (I as well as RDI) into the RMG file (L1J). Due to the coord transformation (made above) from
! basic to global, we have to assume that all 6 columns in RDI_GLOBAL are not null. However, we only need the rows
! defined by the M-set DOF's in DDOF

      CALL TDOF_COL_NUM ( 'G ', G_SET_COL_NUM )
      CALL TDOF_COL_NUM ( 'M ', M_SET_COL_NUM )
      CALL RDOF ( DDOF, CDOF )

      DO I=1,6

         IF (CDOF(I) == '1') THEN                          ! The I-th component is in DDOF so write this row to RMG

            IROW = I
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, IGRID )
            ROW_NUM_START = TDOF_ROW_START(IGRID)
            ROW_NUM = ROW_NUM_START + I - 1
            RMG_ROW_NUM = TDOF(ROW_NUM,M_SET_COL_NUM)
            RMG_COL_NUM = TDOF(ROW_NUM,G_SET_COL_NUM)

            IF ((RMG_ROW_NUM > 0) .AND. (RMG_COL_NUM > 0)) THEN
               WRITE(L1J) RMG_ROW_NUM,RMG_COL_NUM,ONE
            ELSE
               IF (RMG_ROW_NUM  == 0) THEN
                  WRITE(ERR,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  WRITE(F06,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  FATAL_ERR = FATAL_ERR + 1
                  IERR = IERR + 1
                  JERR = JERR + 1
               ENDIF
               IF (RMG_COL_NUM == 0) THEN
                  WRITE(ERR,1510) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  WRITE(F06,1510) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  FATAL_ERR = FATAL_ERR + 1
                  IERR = IERR + 1
                  JERR = JERR + 1
               ENDIF
            ENDIF

            DO J=1,6                                       ! Now write RDI coefficients

               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I, IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
               ROW_NUM = ROW_NUM_START + J - 1
               RMG_COL_NUM = TDOF(ROW_NUM,G_SET_COL_NUM)

               IF ((RMG_ROW_NUM > 0) .AND. (RMG_COL_NUM > 0)) THEN
                  WRITE(L1J) RMG_ROW_NUM,RMG_COL_NUM,RDI_GLOBAL(IROW,J)
               ELSE
                  IF (RMG_COL_NUM == 0) THEN
                     WRITE(ERR,1510) SUBR_NAME,RTYPE,REID,AGRID_I,J
                     WRITE(F06,1510) SUBR_NAME,RTYPE,REID,AGRID_I,J
                     FATAL_ERR = FATAL_ERR + 1
                     IERR = IERR + 1
                     JERR = JERR + 1
                  ENDIF
               ENDIF

            ENDDO

         ENDIF

      ENDDO

! Return if JERR > 0

      IF (JERR > 0) THEN
         RETURN
      ENDIF

! If DEBUG(14) = 1, print results 

      IF (DEBUG(14) == 1) THEN
         WRITE(F06,3001) RTYPE,REID,AGRID_D,ECORD_D,AGRID_I,ECORD_I
         WRITE(F06,*)
         WRITE(F06,3002)
         DO I=1,3
            WRITE(F06,3003) (DELTA_0(I,J),J=1,3)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,3004)
         DO I=1,6
            WRITE(F06,3005) (RDI_GLOBAL(I,J),J=1,6)
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
 1509 FORMAT(' *ERROR  1509: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A8,' RIGID ELEMENT NUMBER ',I8,', DEPENDENT GRID NUMBER ',I8,', COMPONENT ',I2                          &
                    ,/,14X,' IS NOT A M-SET DOF IN TABLE TDOFI')

 1510 FORMAT(' *ERROR  1510: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A8,' RIGID ELEMENT NUMBER ',I8,', INDEPENDENT GRID NUMBER ',I8,', COMPONENT ',I2                        &
                    ,/,14X,' IS NOT A G-SET DOF IN TABLE TDOFI')

 1951 FORMAT(' *ERROR  1951: ',A,I8,' USES GRID ',I8,' WHICH IS A SCALAR POINT. SCALAR POINTS NOT ALLOWED FOR THIS ELEM TYPE')

 3001 FORMAT(/,' OUTPUT FROM SUBROUTINE RBE2_PROC FOR ',A8,' RIGID ELEMENT NUMBER ',I8                                             &
            ,/,' DEPENDENT   GRID IS NUMBER ',I8,' WITH GLOBAL COORD SYSTEM NUMBER ',I8                                            &
            ,/,' INDEPENDENT GRID IS NUMBER ',I8,' WITH GLOBAL COORD SYSTEM NUMBER ',I8)

 3002 FORMAT(1X,' 3 x 3 MATRIX DELTA_0 (DIFFERENCES OF COORDS OF DEP/INDEP GRIDS IN BASIC COORDS)')

 3003 FORMAT(1X,3(1ES15.6))

 3004 FORMAT(1X,' 6 x 6 MATRIX OF RDI MATRIX COEFFS IN GLOBAL COORDS FOR THIS RIGID ELEMENT')

 3005 FORMAT(1X,6(1ES15.6))

 3011 FORMAT(/,' In RIGID_ELEM_PROC: AGRID_D, GRID_ID_ROW_NUM_D, M_ROW, M_SET_COL_NUM, RMG_ROW_NUM                     = ',5(1X,I8))

 3012 FORMAT(/,' In RIGID_ELEM_PROC: AGRID_I, GRID_ID_ROW_NUM_I, N_ROW, N_SET_COL_NUM, RMG_COL_NUM, IROW, J RDI_GLOBAL = ',7I8,    &
                 1ES15.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE RBE2_PROC

