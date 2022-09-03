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

      SUBROUTINE RSPLINE_PROC ( RTYPE, REC_NO, IERR )

! Processes a single RSPLINE rigid element, per call, to get terms for the RMG constraint matrix
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F, L1F_MSG, LINK1F, L1J
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MRSPLINE, NCORD, NGRID, NTERM_RMG
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  CORD, GRID, RGRID, GRID_ID, CORD
      USE PARAMS, ONLY                :  EPSIL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE RSPLINE_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RSPLINE_PROC'
      CHARACTER( 8*BYTE), INTENT(IN)  :: RTYPE             ! The type of rigid element being processed (RSPLINE)
      CHARACTER( 1*BYTE)              :: CDOF(6)           ! An output from subr RDOF (= 1 if a digit 1-6 is in DDOF)

      INTEGER(LONG), INTENT(INOUT)    :: IERR              ! Count of errors in RIGID_ELEM_PROC
      INTEGER(LONG), INTENT(INOUT)    :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: AGRID_D           ! Dep   grid ID (actual) read for the RSPLINE
      INTEGER(LONG)                   :: AGRID_I1          ! Indep grid ID (actual) read for the RSPLINE
      INTEGER(LONG)                   :: AGRID_I2          ! Indep grid ID (actual) read for the RSPLINE
      INTEGER(LONG)                   :: COMPS_D           ! Components for dependent grids, AGRID_D(i)
      INTEGER(LONG)                   :: ECORD_D           ! Global coord ID (actual) for   dep grid AGRID_D
      INTEGER(LONG)                   :: ECORD_I1          ! Global coord ID (actual) for indep grid AGRID_I1
      INTEGER(LONG)                   :: ECORD_I2          ! Global coord ID (actual) for indep grid AGRID_I2
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_D ! Row number in array GRID_ID where AGRID_D is found
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_I1! Row number in array GRID_ID where AGRID_I is found
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_I2! Row number in array GRID_ID where AGRID_I is found
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no., in TDOF array, of the G-set DOF list
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: ICORD             ! Internal coord ID
      INTEGER(LONG)                   :: INDEX             ! Index of records read from file LINK1F
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: IROW              ! A row number in matrix RDI_GLOBAL
      INTEGER(LONG)                   :: JERR              ! Local error count
      INTEGER(LONG)                   :: M_SET_COL_NUM     ! Col no., in TDOF array, of the M-set DOF list
      INTEGER(LONG)                   :: NUM_COMPS_D       ! 6 if AGRID_D  is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: NUM_COMPS_I1      ! 6 if AGRID_I1 is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: NUM_COMPS_I2      ! 6 if AGRID_I2 is a physical grid, 1 if a scalar point
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REID              ! RBE2 elem ID read from file LINK1F
      INTEGER(LONG)                   :: RMG_COL_NUM       ! Col no. of a term in array RMG
      INTEGER(LONG)                   :: RMG_ROW_NUM       ! Row no. of a term in array RMG
      INTEGER(LONG)                   :: ROW_NUM           ! A row number in array TDOF
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: TOTAL_NUM         ! Total number of records read for a single rigid element
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: DL_RAT            ! D/L ratio from the B.D. RSPLINE entry

      REAL(DOUBLE)                    :: FR(6,12)          ! Matrix of spline coefficients relating the 6 components of displ at the
!                                                            dependent grid to the 6 comps of displ at the 2 independent grids

      REAL(DOUBLE)                    :: FR11(3,3)         ! Partition of FR in 1st 3 rows and 1st 3 cols
      REAL(DOUBLE)                    :: FR12(3,3)         ! Partition of FR in 1st 3 rows and 2nd 3 cols
      REAL(DOUBLE)                    :: FR13(3,3)         ! Partition of FR in 1st 3 rows and 3rd 3 cols
      REAL(DOUBLE)                    :: FR14(3,3)         ! Partition of FR in 1st 3 rows and 4th 3 cols

      REAL(DOUBLE)                    :: FR21(3,3)         ! Partition of FR in 2nd 3 rows and 1st 3 cols
      REAL(DOUBLE)                    :: FR22(3,3)         ! Partition of FR in 2nd 3 rows and 2nd 3 cols
      REAL(DOUBLE)                    :: FR23(3,3)         ! Partition of FR in 2nd 3 rows and 3rd 3 cols
      REAL(DOUBLE)                    :: FR24(3,3)         ! Partition of FR in 2nd 3 rows and 4th 3 cols

      REAL(DOUBLE)                    :: L12               ! Length of RSPLINE between grids AGRID_I1 and AGRID_I2
      REAL(DOUBLE)                    :: L1D               ! Length of RSPLINE between grids AGRID_I1 and AGRID_D
      REAL(DOUBLE)                    :: PHID, THETAD      ! Angles output from subr GEN_T0L, called herein but not needed here
      REAL(DOUBLE)                    :: T0G_D(3,3)        ! Transforms a dep   DOF vector in basic coords to global coords
      REAL(DOUBLE)                    :: T0G_I1(3,3)       ! Transforms a indep DOF vector in basic coords to global coords
      REAL(DOUBLE)                    :: T0G_I2(3,3)       ! Transforms a indep DOF vector in basic coords to global coords

      REAL(DOUBLE)                    :: TRSPLINE(3,3)     ! Coord transformation matrix for RSPLINE that transforms a vector in
!                                                            basic coords to a vector in RSPLINE coords
 
      REAL(DOUBLE)                    :: V01(3)            ! Vector in basic coords from basic origin to AGRID_I1
      REAL(DOUBLE)                    :: V02(3)            ! Vector in basic coords from basic origin to AGRID_I2
      REAL(DOUBLE)                    :: V0D(3)            ! Vector in basic coords from basic origin to AGRID_D
      REAL(DOUBLE)                    :: V012(3)           ! Vector in basic coords from AGRID_I1 to AGRID_I2
      REAL(DOUBLE)                    :: V01D(3)           ! Vector in basic coords from AGRID_I1 to AGRID_D
      REAL(DOUBLE)                    :: ZETA              ! Nondimensional distance from AGRID_I1 to AGRID_D

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! File LINK1F contains data from the logical RSPLINE cards in the input B.D. deck. For each logical RSPLINE card, LINK1F has:

! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
      JERR = 0 

! Make sure that the grids are all 6 components (i.e. no SPOINTS)

      READ(L1F,IOSTAT=IOCHK) REID, INDEX, TOTAL_NUM, AGRID_I1, AGRID_I2, AGRID_D, COMPS_D, DL_RAT   

      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 1' )

      REC_NO = REC_NO + 1
      IF (IOCHK == 0) THEN
         CALL GET_GRID_NUM_COMPS ( AGRID_I1, NUM_COMPS_I1, SUBR_NAME )
         IF (NUM_COMPS_I1 /= 6) THEN
            IERR  = IERR + 1
            JERR = JERR + 1
            WRITE(ERR,1951) 'RSPLINE', REID, NUM_COMPS_I1
            WRITE(F06,1951) 'RSPLINE', REID, NUM_COMPS_I1
         ENDIF
         CALL GET_GRID_NUM_COMPS ( AGRID_I2, NUM_COMPS_I2, SUBR_NAME )
         IF (NUM_COMPS_I2 /= 6) THEN
            IERR  = IERR + 1
            JERR = JERR + 1
            WRITE(ERR,1951) 'RSPLINE', REID, NUM_COMPS_I2
            WRITE(F06,1951) 'RSPLINE', REID, NUM_COMPS_I2
         ENDIF
         CALL GET_GRID_NUM_COMPS ( AGRID_D, NUM_COMPS_D, SUBR_NAME )
         IF (NUM_COMPS_D /= 6) THEN
            IERR  = IERR + 1
            JERR = JERR + 1
            WRITE(ERR,1951) 'RSPLINE', REID, NUM_COMPS_D
            WRITE(F06,1951) 'RSPLINE', REID, NUM_COMPS_D
         ENDIF
      ELSE
         CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
         IERR = IERR + 1
         JERR = JERR + 1
      ENDIF

! Return if local error count > 0

      IF (JERR > 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF

! Get the internal grid ID's for AGRID I1, AGRID_I2 and AGRID_D. We know they exist 

      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I1, GRID_ID_ROW_NUM_I1 )
      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I2, GRID_ID_ROW_NUM_I2 )
      CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D , GRID_ID_ROW_NUM_D )

! Get the distances from AGRID_I1 to AGRID_I2 (total length of the RSPLINE) and AGRID_I1 to AGRID_D

      DO I=1,3
         V01(I)  = RGRID(GRID_ID_ROW_NUM_I1,I)
         V02(I)  = RGRID(GRID_ID_ROW_NUM_I2,I)
         V0D(I)  = RGRID(GRID_ID_ROW_NUM_D ,I)
         V012(I) = V02(I) - V01(I)
         V01D(I) = V0D(I) - V01(I)
      ENDDO

      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 2' )

! Get the element-to-basic trensformation matrices for AGRID_D and AGRID_I1, AGRID_I2

      CALL RSPLINE_GEOM ( REID, AGRID_I1, AGRID_I2, V012, L12, TRSPLINE )
      IF (IERR > 0) THEN
         RETURN
      ENDIF
      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 3' )

      L1D  = DSQRT( V01D(1)*V01D(1) + V01D(2)*V01D(2) + V01D(3)*V01D(3) )
      ZETA = L1D/L12
      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 4' )

! Get the spline functions in element coords

      CALL RSPLINE_FUNCTIONS ( ZETA, L12, FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24 )
      CALL ASSEMBLE_FR ( FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24, FR  )
      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 5' )

! Transform FR from element coords (x axis along line between the 2 indep grids) to basic coords

      CALL TRANSFORM_FR_E0 ( TRSPLINE, FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24 )
      CALL ASSEMBLE_FR ( FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24, FR  )
      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 6' )

! Get the basic to global transformation matrices for the 2 indep grids and the dep grid

      ECORD_I1 = GRID(GRID_ID_ROW_NUM_I1,3)                ! Indep grid, AGRID_I1, basic to global transformation is T0G_I1
      IF (ECORD_I1 /= 0) THEN
         DO I=1,NCORD
            IF (ECORD_I1 == CORD(I,2)) THEN
               ICORD = I
               EXIT
            ENDIF
         ENDDO   
         CALL GEN_T0L ( GRID_ID_ROW_NUM_I1, ICORD, THETAD, PHID, T0G_I1 )
      ELSE
         DO I=1,3
            DO J=1,3
               T0G_I1(I,J) = ZERO
            ENDDO   
            T0G_I1(I,I) = ONE
         ENDDO   
      ENDIF
 
      ECORD_I2 = GRID(GRID_ID_ROW_NUM_I2,3)                ! Indep grid, AGRID_I2, basic to global transformation is T0G_I2
      IF (ECORD_I2 /= 0) THEN
         DO I=1,NCORD
            IF (ECORD_I2 == CORD(I,2)) THEN
               ICORD = I
               EXIT
            ENDIF
         ENDDO   
         CALL GEN_T0L ( GRID_ID_ROW_NUM_I2, ICORD, THETAD, PHID, T0G_I2 )
      ELSE
         DO I=1,3
            DO J=1,3
               T0G_I2(I,J) = ZERO
            ENDDO   
            T0G_I2(I,I) = ONE
         ENDDO   
      ENDIF
 
      ECORD_D = GRID(GRID_ID_ROW_NUM_D,3)                  ! Dep grid, AGRID_D, basic to global transformation is T0G_D
      IF (ECORD_D /= 0) THEN
         DO I=1,NCORD
            IF (ECORD_D == CORD(I,2)) THEN
               ICORD = I
               EXIT
            ENDIF
         ENDDO   
         CALL GEN_T0L ( GRID_ID_ROW_NUM_D, ICORD, THETAD, PHID, T0G_D )
      ELSE
         DO I=1,3
            DO J=1,3
               T0G_D(I,J) = ZERO
            ENDDO   
            T0G_D(I,I) = ONE
         ENDDO   
      ENDIF
 
      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 7' )

! Transform FR from basic coords to global coords at the 2 indep and 1 dep grid

      CALL TRANSFORM_FR_0G ( T0G_I1, T0G_I2, T0G_D, FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24 )      
      CALL ASSEMBLE_FR ( FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24, FR  )
      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( ' 8' )

! Write coefficients for the RMG constraint matrix to file L1J

      CALL TDOF_COL_NUM ( 'G ', G_SET_COL_NUM )
      CALL TDOF_COL_NUM ( 'M ', M_SET_COL_NUM )
      CALL RDOF ( COMPS_D, CDOF )

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
               NTERM_RMG = NTERM_RMG + 1
            ELSE IF (RMG_ROW_NUM  == 0) THEN
               WRITE(ERR,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
               WRITE(F06,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ELSE IF (RMG_COL_NUM == 0) THEN
               WRITE(ERR,1510) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
               WRITE(F06,1510) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF

            DO J=1,6                                       ! Now write FR coefficients for AGRID_I1
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I1, IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
               ROW_NUM = ROW_NUM_START + J - 1
               RMG_COL_NUM = TDOF(ROW_NUM,G_SET_COL_NUM)
               IF ((RMG_ROW_NUM > 0) .AND. (RMG_COL_NUM > 0)) THEN
                  WRITE(L1J) RMG_ROW_NUM,RMG_COL_NUM,-FR(IROW,J)
                  NTERM_RMG = NTERM_RMG + 1
               ELSE IF (RMG_ROW_NUM  == 0) THEN
                  WRITE(ERR,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  WRITE(F06,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ELSE IF (RMG_COL_NUM == 0) THEN
                  WRITE(ERR,1510) SUBR_NAME,RTYPE,REID,AGRID_I1,J
                  WRITE(F06,1510) SUBR_NAME,RTYPE,REID,AGRID_I1,J
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
            ENDDO

            DO J=1,6                                       ! Now write FR coefficients for AGRID_I2
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I2, IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
               ROW_NUM = ROW_NUM_START + J - 1
               RMG_COL_NUM = TDOF(ROW_NUM,G_SET_COL_NUM)
               IF ((RMG_ROW_NUM > 0) .AND. (RMG_COL_NUM > 0)) THEN
                  WRITE(L1J) RMG_ROW_NUM,RMG_COL_NUM,-FR(IROW,J+6)
               ELSE IF (RMG_ROW_NUM  == 0) THEN
                  WRITE(ERR,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  WRITE(F06,1509) SUBR_NAME,RTYPE,REID,AGRID_D,IROW
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ELSE IF (RMG_COL_NUM == 0) THEN
                  WRITE(ERR,1510) SUBR_NAME,RTYPE,REID,AGRID_I2,J
                  WRITE(F06,1510) SUBR_NAME,RTYPE,REID,AGRID_I2,J
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
            ENDDO

         ENDIF

      ENDDO

! Write lines for end of debug output

      IF (DEBUG(111) > 0) CALL DEB_RSPLINE_PROC ( '99' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************





 9999 FORMAT(' PROCESSING TERMINATED DUE TO ABOVE ERRORS')

 1509 FORMAT(' *ERROR  1509: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A8,' RIGID ELEMENT NUMBER ',I8,', DEPENDENT GRID NUMBER ',I8,', COMPONENT ',I2                          &
                    ,/,14X,' IS NOT A M-SET DOF IN TABLE TDOFI')

 1510 FORMAT(' *ERROR  1510: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A8,' RIGID ELEMENT NUMBER ',I8,', INDEPENDENT GRID NUMBER ',I8,', COMPONENT ',I2                        &
                    ,/,14X,' IS NOT A G-SET DOF IN TABLE TDOFI')

 1951 FORMAT(' *ERROR  1951: ',A,I8,' USES GRID ',I8,' WHICH IS A SCALAR POINT. SCALAR POINTS NOT ALLOWED FOR THIS ELEM TYPE')

 3001 FORMAT(/,' OUTPUT FROM SUBROUTINE RSPLINE_PROC FOR ',A8,' RIGID ELEMENT NUMBER ',I8                                          &
            ,/,' DEPENDENT   GRID IS NUMBER ',I8,' WITH GLOBAL COORD SYSTEM NUMBER ',I8                                            &
            ,/,' INDEPENDENT GRID IS NUMBER ',I8,' WITH GLOBAL COORD SYSTEM NUMBER ',I8)

 3002 FORMAT(1X,' 3 x 3 MATRIX DELTA_0 (DIFFERENCES OF COORDS OF DEP/INDEP GRIDS IN BASIC COORDS)')

 3003 FORMAT(1X,3(1ES15.6))

 3004 FORMAT(1X,' 6 x 6 MATRIX OF RDI MATRIX COEFFS IN GLOBAL COORDS FOR THIS RIGID ELEMENT')

 3005 FORMAT(1X,6(1ES15.6))

 3011 FORMAT(/,' In RIGID_ELEM_PROC: AGRID_D, GRID_ID_ROW_NUM_D, M_ROW, M_SET_COL_NUM, RMG_ROW_NUM                     = ',5(1X,I8))

 3012 FORMAT(/,' In RIGID_ELEM_PROC: AGRID_I, GRID_ID_ROW_NUM_I, N_ROW, N_SET_COL_NUM, RMG_COL_NUM, IROW, J RDI_GLOBAL = ',7I8,    &
                 1ES15.6)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################
 
      SUBROUTINE RSPLINE_GEOM ( REID, AGRID1, AGRID2, V012, LENGTH, TRSPLINE )

! Calculate an RSPLINE coord transformation matriox that transforms a vector in basic coords to one in element coords (x axis along
! the line between the 2 RSPLINE indep grids)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RSPLINE_GEOM'
      CHARACTER( 5*BYTE)              :: SORT_ORDER        ! Order in which the VX(i) have been sorted in subr CALC_VEC_SORT_ORDER

      INTEGER(LONG), INTENT(IN)       :: AGRID1            ! Indep grid number 1 on the RSPLINE
      INTEGER(LONG), INTENT(IN)       :: AGRID2            ! Indep grid number 2 on the RSPLINE
      INTEGER(LONG), INTENT(IN)       :: REID              ! Element ID
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I3_IN(3)          ! Integer array used in sorting VX. 
      INTEGER(LONG)                   :: I3_OUT(3)         ! Integer array in sort order of VX_SORT. If VX is sorted sp that
!                                                             comp 2 is smallest then comp 3 then comp 1 then I3_OUT is 2, 3, 1 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND + 1

      REAL(DOUBLE) , INTENT(IN)       :: V012(3)           ! Vector in basic coords from 1st to 2nd indep grids on the RSPLINE

      REAL(DOUBLE) , INTENT(OUT)      :: TRSPLINE(3,3)     ! Coord transformation matrix for RSPLINE that transforms a vector in
!                                                            basic coords to a vector in RSPLINE coords
      REAL(DOUBLE) , INTENT(OUT)      :: LENGTH            ! Length of RSPLINE between the 2 independent grids
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare to real zero
      REAL(DOUBLE)                    :: MAGY              ! Magnitude of vector VY
      REAL(DOUBLE)                    :: MAGZ              ! Magnitude of vector VZ
      REAL(DOUBLE)                    :: VX(3)             ! A vector in the elem x dir
      REAL(DOUBLE)                    :: VY(3)             ! A vector in the elem y dir
      REAL(DOUBLE)                    :: VZ(3)             ! A vector in the elem z dir

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Initialize

      DO I=1,3
        DO J=1,3
           TRSPLINE(I,J) = ZERO
        ENDDO 
      ENDDO 

      DO I=1,3
         VX(I) = ZERO
      ENDDO

! Calculate a vector between ends of the element in basic coords

      VX(1) = V012(1)
      VX(2) = V012(2)
      VX(3) = V012(3)

! Length of element between ends is:

      LENGTH = DSQRT( VX(1)*VX(1) + VX(2)*VX(2) + VX(3)*VX(3) )

      IF (LENGTH < EPS1) THEN
         IERR = IERR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1504) RTYPE, REID, AGRID1, AGRID2, LENGTH
         WRITE(F06,1504) RTYPE, REID, AGRID1, AGRID2, LENGTH
         RETURN
      ENDIF

! Unit vector in element X direction except for BUSH element with CID >= 0 (i.e. when BUSH does not have a V vector)

      DO I=1,3
         TRSPLINE(1,I) = VX(I)/LENGTH
      ENDDO

      DO I=1,3
         I3_IN(I)   = I
         I3_OUT(I)  = I3_IN(I)
      ENDDO
      CALL CALC_VEC_SORT_ORDER ( VX, SORT_ORDER, I3_OUT)! Use this rather than SORT_INT1_REAL1 - didn't work for vec 10., 0., 0.
      IF (SORT_ORDER == '     ') THEN                   ! Subr CALC_VEC_SORT_ORDER did not find a sort order
         FATAL_ERR = FATAL_ERR + 1
         IERR = IERR + 1
         WRITE(ERR,1944) SUBR_NAME, 'RSPLINE', REID
         WRITE(F06,1944) SUBR_NAME, 'RSPLINE', REID
         RETURN
      ENDIF
                                                        ! See notes on "Some Basic Vector Operations In IDL" on web site:
                                                        ! http://fermi.jhuapl.edu/s1r/idl/s1rlib/vectors/v_basic.html
      VY(I3_OUT(1)) =  ZERO                             !  (a) Component of VY in direction of min VX is set to zero
      VY(I3_OUT(2)) =  VX(I3_OUT(3))                    !  (b) Other 2 VY(i) are corresponding VX(i) switched with one x(-1)
      VY(I3_OUT(3)) = -VX(I3_OUT(2))
      MAGY  = DSQRT(VY(1)*VY(1) + VY(2)*VY(2) + VY(3)*VY(3))

      IF (DABS(MAGY) < EPS1) THEN
         FATAL_ERR = FATAL_ERR + 1
         IERR = IERR + 1
         WRITE(ERR,1938) SUBR_NAME, 'Y', 'RSPLINE', REID, (VY(I),I=1,3)
         WRITE(F06,1938) SUBR_NAME, 'Y', 'RSPLINE', REID, (VY(I),I=1,3)
         RETURN
      ENDIF

      DO I=1,3
         TRSPLINE(2,I) = VY(I)/MAGY
      ENDDO

      CALL CROSS ( VX, VY, VZ )

      MAGZ  = DSQRT(VZ(1)*VZ(1) + VZ(2)*VZ(2) + VZ(3)*VZ(3))

      IF (DABS(MAGZ) < EPS1) THEN
         FATAL_ERR = FATAL_ERR + 1
         IERR = IERR + 1
         WRITE(ERR,1938) SUBR_NAME, 'Z', 'RSPLINE', REID, (VZ(I),I=1,3)
         WRITE(F06,1938) SUBR_NAME, 'Z', 'RSPLINE', REID, (VZ(I),I=1,3)
         RETURN
      ENDIF

      DO I=1,3
         TRSPLINE(3,I) = VZ(I)/MAGZ
      ENDDO 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1504 FORMAT(' *ERROR  1504: ',A, I8,' HAS LENGTH BETWEEN INDEPENDENT GRIDS ',I8,' AND ',I8,' = ',1ES10.2,' MUST BE > 0')

 1938 FORMAT(' *ERROR  1938: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT CALCULATE PROPER VECTOR IN ELEMENT LOCAL COORDINATE DIRECTION ',A,' FOR ',A,' ELEMENT ',I8,'.' &
                    ,/,14X,' THE VECTOR COMPONENTS CALCULATED WERE ',3(1ES14.6))

 1944 FORMAT(' *ERROR  1944: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE VX VECTOR FOR ',A,' ELEMENT ',I8,' WAS LEFT UNSORTED. IT MUST BE SORTED TO DETERMINE VY, VZ') 

! **********************************************************************************************************************************

      END SUBROUTINE RSPLINE_GEOM

! ##################################################################################################################################

      SUBROUTINE RSPLINE_FUNCTIONS ( Z, L12, FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24  )

! Calculate thespline functions for an RSPLINE element in element coords (x axis along the line between the 2 indep grids)

      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE CONSTANTS_1, ONLY           :  ONE, TWO, THREE, FOUR, SIX
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RSPLINE_FUNCTIONS'

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND + 1

      REAL(DOUBLE) , INTENT(IN)       :: L12               ! Length of RSPLINE between the 2 independent grids
      REAL(DOUBLE) , INTENT(IN)       :: Z                 ! Nondim distance to the RSPLINE dependent grid from the 1st indep grid

      REAL(DOUBLE) , INTENT(OUT)      :: FR11(3,3)         ! Partition of FR in 1st 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(OUT)      :: FR12(3,3)         ! Partition of FR in 1st 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(OUT)      :: FR13(3,3)         ! Partition of FR in 1st 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(OUT)      :: FR14(3,3)         ! Partition of FR in 1st 3 rows and 4th 3 cols

      REAL(DOUBLE) , INTENT(OUT)      :: FR21(3,3)         ! Partition of FR in 2nd 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(OUT)      :: FR22(3,3)         ! Partition of FR in 2nd 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(OUT)      :: FR23(3,3)         ! Partition of FR in 2nd 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(OUT)      :: FR24(3,3)         ! Partition of FR in 2nd 3 rows and 4th 3 cols
      REAL(DOUBLE)                    :: Z_2               ! Z squared
      REAL(DOUBLE)                    :: Z_3               ! Z cubed

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=1,3
         DO J=1,3
            FR11(I,J) = ZERO   ;   FR12(I,J) = ZERO   ;   FR13(I,J) = ZERO   ;   FR14(I,J) = ZERO
            FR21(I,J) = ZERO   ;   FR22(I,J) = ZERO   ;   FR23(I,J) = ZERO   ;   FR24(I,J) = ZERO
         ENDDO
      ENDDO
      
      Z_2 = Z*Z
      Z_3 = Z*Z_2

! 1st 3 rows of FR

      FR11(1,1) = ONE - Z
      FR11(2,2) = ONE - THREE*Z_2 + TWO*Z_3
      FR11(3,3) = FR11(2,2)

      FR12(2,3) =  (Z - TWO*Z_2 + Z_3)*L12
      FR12(3,2) = -FR12(2,3)

      FR13(1,1) = Z
      FR13(2,2) = THREE*Z_2 - TWO*Z_3
      FR13(3,3) = FR13(2,2)

      FR14(2,3) = (Z_3 - Z_2)*L12
      FR14(3,2) = -FR14(2,3)

! 2nd 3 rows of FR

      FR21(2,3) = -(Z_2 - Z)*SIX/L12
      FR21(3,2) = -FR21(2,3)

      FR22(1,1) = ONE - Z
      FR22(2,2) = ONE - FOUR*Z +THREE*Z_2
      FR22(3,3) = FR22(2,2)

      FR23(2,3) = (Z_2 - Z)*SIX/L12 
      FR23(3,2) = -FR23(2,3) 

      FR24(1,1) = Z
      FR24(2,2) = THREE*Z_2 - TWO*Z
      FR24(3,3) = FR24(2,2)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE RSPLINE_FUNCTIONS

! ##################################################################################################################################

      SUBROUTINE ASSEMBLE_FR ( FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24, FR  )

! Assemble the 8 3x3 FRij matrices into the 6x12 matrix FR

      IMPLICIT NONE

      INTEGER(LONG)                   :: I,J               ! DO loop indices

      REAL(DOUBLE) , INTENT(OUT)      :: FR(6,12)          ! Matrix of spline coefficients relating the 6 components of displ at the
!                                                            dependent grid to the 6 comps of displ at the 2 independent grids

      REAL(DOUBLE) , INTENT(IN)       :: FR11(3,3)         ! Partition of FR in 1st 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(IN)       :: FR12(3,3)         ! Partition of FR in 1st 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(IN)       :: FR13(3,3)         ! Partition of FR in 1st 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(IN)       :: FR14(3,3)         ! Partition of FR in 1st 3 rows and 4th 3 cols

      REAL(DOUBLE) , INTENT(IN)       :: FR21(3,3)         ! Partition of FR in 2nd 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(IN)       :: FR22(3,3)         ! Partition of FR in 2nd 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(IN)       :: FR23(3,3)         ! Partition of FR in 2nd 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(IN)       :: FR24(3,3)         ! Partition of FR in 2nd 3 rows and 4th 3 cols

! **********************************************************************************************************************************
! Put 3x3 matrices Fij into 6x12 matrix FR

      DO I=1,3
         DO J=1,3
            FR(I  ,J  ) = FR11(I,J)
            FR(I  ,J+3) = FR12(I,J)
            FR(I  ,J+6) = FR13(I,J)
            FR(I  ,J+9) = FR14(I,J)
         ENDDO
      ENDDO

      DO I=1,3
         DO J=1,3
            FR(I+3,J  ) = FR21(I,J)
            FR(I+3,J+3) = FR22(I,J)
            FR(I+3,J+6) = FR23(I,J)
            FR(I+3,J+9) = FR24(I,J)
         ENDDO
      ENDDO

! **********************************************************************************************************************************

      END SUBROUTINE ASSEMBLE_FR

! ##################################################################################################################################

      SUBROUTINE TRANSFORM_FR_E0 ( TR, FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24 )

! Transform FR matrix for an RSPLINE from element coords (x axis along lin e between the 2 indep grids) to basic coords

      IMPLICIT NONE

      REAL(DOUBLE) , INTENT(IN)       :: TR(3,3)           ! Coord transformation matrix for RSPLINE that transforms a vector in
!                                                            basic coords to a vector in RSPLINE coords

      REAL(DOUBLE) , INTENT(INOUT)    :: FR11(3,3)         ! Partition of FR in 1st 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR12(3,3)         ! Partition of FR in 1st 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR13(3,3)         ! Partition of FR in 1st 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR14(3,3)         ! Partition of FR in 1st 3 rows and 4th 3 cols

      REAL(DOUBLE) , INTENT(INOUT)    :: FR21(3,3)         ! Partition of FR in 2nd 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR22(3,3)         ! Partition of FR in 2nd 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR23(3,3)         ! Partition of FR in 2nd 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR24(3,3)         ! Partition of FR in 2nd 3 rows and 4th 3 cols

      REAL(DOUBLE)                    :: DUM1(3,3)         ! Intermediate matrix

! **********************************************************************************************************************************
      CALL MATMULT_FFF   ( FR11, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR11 )

      CALL MATMULT_FFF   ( FR12, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR12 )

      CALL MATMULT_FFF   ( FR13, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR13 )

      CALL MATMULT_FFF   ( FR14, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR14 )

      CALL MATMULT_FFF   ( FR21, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR21 )

      CALL MATMULT_FFF   ( FR22, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR22 )

      CALL MATMULT_FFF   ( FR23, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR23 )

      CALL MATMULT_FFF   ( FR24, TR, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR, DUM1, 3, 3, 3, FR24 )

! **********************************************************************************************************************************

      END SUBROUTINE TRANSFORM_FR_E0

! ##################################################################################################################################

      SUBROUTINE TRANSFORM_FR_0G ( TR_I1, TR_I2, TR_D, FR11, FR12, FR13, FR14, FR21, FR22, FR23, FR24 )

! Transform FR matrix for an RSPLINE from basic to global coords

      IMPLICIT NONE

      REAL(DOUBLE) , INTENT(IN)       :: TR_I1(3,3)        ! Matrix that transforms a vector in global to basic for 1st indep grid 
      REAL(DOUBLE) , INTENT(IN)       :: TR_I2(3,3)        ! Matrix that transforms a vector in global to basic for 2nd indep grid 
      REAL(DOUBLE) , INTENT(IN)       :: TR_D(3,3)         ! Matrix that transforms a vector in global to basic for dep grid 

      REAL(DOUBLE) , INTENT(INOUT)    :: FR11(3,3)         ! Partition of FR in 1st 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR12(3,3)         ! Partition of FR in 1st 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR13(3,3)         ! Partition of FR in 1st 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR14(3,3)         ! Partition of FR in 1st 3 rows and 4th 3 cols

      REAL(DOUBLE) , INTENT(INOUT)    :: FR21(3,3)         ! Partition of FR in 2nd 3 rows and 1st 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR22(3,3)         ! Partition of FR in 2nd 3 rows and 2nd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR23(3,3)         ! Partition of FR in 2nd 3 rows and 3rd 3 cols
      REAL(DOUBLE) , INTENT(INOUT)    :: FR24(3,3)         ! Partition of FR in 2nd 3 rows and 4th 3 cols

      REAL(DOUBLE)                    :: DUM1(3,3)         ! Intermediate matrix

! **********************************************************************************************************************************
      CALL MATMULT_FFF   ( FR11, TR_I1, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR11 )

      CALL MATMULT_FFF   ( FR12, TR_I1, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR12 )

      CALL MATMULT_FFF   ( FR13, TR_I1, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR13 )

      CALL MATMULT_FFF   ( FR14, TR_I1, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR14 )

      CALL MATMULT_FFF   ( FR21, TR_I2, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR21 )

      CALL MATMULT_FFF   ( FR22, TR_I2, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR22 )

      CALL MATMULT_FFF   ( FR23, TR_I2, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR23 )

      CALL MATMULT_FFF   ( FR24, TR_I2, 3, 3, 3, DUM1 )
      CALL MATMULT_FFF_T ( TR_D, DUM1 , 3, 3, 3, FR24 )

! **********************************************************************************************************************************

      END SUBROUTINE TRANSFORM_FR_0G

! ##################################################################################################################################

      SUBROUTINE DEB_RSPLINE_PROC ( WHAT )

! Write debug info for RSPLINE rigid element code

      IMPLICIT NONE

      CHARACTER( 2*BYTE)              :: WHAT              ! Indicator of what to write out

      INTEGER(LONG)                   :: II,JJ             ! DO loop indices

! **********************************************************************************************************************************
      IF      (WHAT == ' 1' ) THEN
         WRITE(F06,1001)
         WRITE(F06,1002) RTYPE, REID, AGRID_D, AGRID_I1, AGRID_I2
         WRITE(F06,*)

      ELSE IF (WHAT == ' 2' ) THEN
         WRITE(F06,2001) 'indep', AGRID_I1, (V01(II) ,II=1,3)
         WRITE(F06,2001) 'indep', AGRID_I2, (V02(II) ,II=1,3)
         WRITE(F06,2001) '  dep', AGRID_D , (V0D(II) ,II=1,3)
         WRITE(F06,*)
         WRITE(F06,2002) AGRID_I1,'indep', AGRID_I2,(V012(II),II=1,3)
         WRITE(F06,2002) AGRID_I1,'  dep', AGRID_D ,(V01D(II),II=1,3)
         WRITE(F06,*)

      ELSE IF (WHAT == ' 3' ) THEN
         WRITE(F06,3001)
         DO II=1,3
            WRITE(F06,3002) (TRSPLINE(II,JJ),JJ=1,3)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHAT == ' 4' ) THEN
         WRITE(F06,4001) L12, ZETA
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHAT == ' 5' ) THEN
         WRITE(F06,5001)
         WRITE(F06,5002)
         DO II=1,3
            WRITE(F06,5003) (FR(II,JJ),JJ=1,3), (FR(II,JJ),JJ=4,6), (FR(II,JJ),JJ=7,9), (FR(II,JJ),JJ=10,12)
         ENDDO
         WRITE(F06,*)
         DO II=4,6
            WRITE(F06,5003) (FR(II,JJ),JJ=1,3), (FR(II,JJ),JJ=4,6), (FR(II,JJ),JJ=7,9), (FR(II,JJ),JJ=10,12)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHAT == ' 6' ) THEN
         WRITE(F06,5001)
         WRITE(F06,6002)
         DO II=1,3
            WRITE(F06,5003) (FR(II,JJ),JJ=1,3), (FR(II,JJ),JJ=4,6), (FR(II,JJ),JJ=7,9), (FR(II,JJ),JJ=10,12)
         ENDDO
         WRITE(F06,*)
         DO II=4,6
            WRITE(F06,5003) (FR(II,JJ),JJ=1,3), (FR(II,JJ),JJ=4,6), (FR(II,JJ),JJ=7,9), (FR(II,JJ),JJ=10,12)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHAT == ' 7' ) THEN
         WRITE(F06,7001) AGRID_I1, ECORD_I1, AGRID_I2, ECORD_I2, AGRID_D, ECORD_D 
         DO II=1,3
            WRITE(F06,7002) (T0G_I1(II,JJ),JJ=1,3), (T0G_I2(II,JJ),JJ=1,3), (T0G_D(II,JJ),JJ=1,3)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHAT == ' 8' ) THEN
         WRITE(F06,5001)
         WRITE(F06,8002)
         DO II=1,3
            WRITE(F06,5003) (FR(II,JJ),JJ=1,3), (FR(II,JJ),JJ=4,6), (FR(II,JJ),JJ=7,9), (FR(II,JJ),JJ=10,12)
         ENDDO
         WRITE(F06,*)
         DO II=4,6
            WRITE(F06,5003) (FR(II,JJ),JJ=1,3), (FR(II,JJ),JJ=4,6), (FR(II,JJ),JJ=7,9), (FR(II,JJ),JJ=10,12)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHAT == '99' ) THEN
         WRITE(f06,9001)

      ENDIF

! **********************************************************************************************************************************
 1001 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::::START DEBUG(111) OUTPUT FROM SUBROUTINE RSPLINE_PROC::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1002 FORMAT(14X,A,I9,' with dependent grid ',I8,' connects between independent grids ',I9,' and ',I8)

 2001 FORMAT(1X,'Vector in basic coords from basic system origin to ',A,' grid ',I8,':',3(1ES14.6))

 2002 FORMAT(1X,'Vector in basic coords from indep grid ',I8,' to ',A,' grid ',I8,':',3(1ES14.6))

 3001 FORMAT(19X,'Coord transformation matrix TRSPLINE (transforms a vector to element coords from basic coords):',/)

 3002 FORMAT(45X,3(1ES14.6))

 4001 FORMAT(1X,'Length of the RSPLINE (distance between the 2 independent grids)      :',1ES14.6,/,                               &
             1x,'Nondimensional distance from the 1st indep grid to the dep grid       :',1ES14.6)

 5001 FORMAT(38X,'                       Matrix FR of RSPLINE coefficients for the dependent grid',/,                              &
             38x,'Ud = FR*Ui where Ud are the 6 DOF''s at the dep grid and Ui are 12 DOF''s; 6 at each of the 2 indep grids')

 5002 FORMAT(56X,'(coord system is elem coords with x axis between the 2 indep grids)',/)

 5003 FORMAT(3(1ES14.6),'  |', 3(1ES14.6),'  |', 3(1ES14.6),'  |', 3(1ES14.6)) 

 6002 FORMAT(56X,'                      (coord system is basic)',/)

 7001 FORMAT(21X,'Coord transformation matrices that transform a vector to basic coords from global coords for:',/,                &
             21X,'--------------------------------------------------------------------------------------------' ,/,                &
             3X,'Indep grid',I8,', global CID',I8,7X,'Indep grid',I8,', global CID',I8,8X,'Dep grid',I8,', global CID',I8,/,       &
             3X,'    (transformation matrix T0G_I1)',15X,'(transformation matrix T0G_I2)',15X,'(transformation matrix T0G_D)',/)

 7002 FORMAT(3(3(1ES14.6),3X))

 8002 FORMAT(56X,'                      (coord system is global)',/)

 9001 FORMAT(' :::::::::::::::::::::::::::::::::::::::END DEBUG(111) OUTPUT FROM SUBROUTINE RSPLINE_PROC:::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/) 

! **********************************************************************************************************************************

      END SUBROUTINE DEB_RSPLINE_PROC

      END SUBROUTINE RSPLINE_PROC

