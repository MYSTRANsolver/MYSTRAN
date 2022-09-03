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
 
      SUBROUTINE ELMDIS
 
! Get displs for one element, one subcase from list of all displ's (in UG_COL). Transform them to local elem coords.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, INT_SC_NUM, meldof, MELGP, NCORD, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMDIS_BEGEND
      USE MODEL_STUF, ONLY            :  AGRID, CAN_ELEM_TYPE_OFFSET, GRID, CORD, BGRID, ELGP, ELDOF, GRID_ID, OFFSET, OFFDIS,     &
                                         SCNUM, TE, TYPE, UEB, UEG, UEL, UGG
      USE COL_VECS, ONLY              :  UG_COL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  EID, AGRID
 
      USE ELMDIS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMDIS'

      INTEGER(LONG)                   :: G_SET_COL         ! Col number in TDOF where the G-set DOF's exist
      INTEGER(LONG)                   :: GDOF              ! G-set DOF number
      INTEGER(LONG)                   :: GLOBAL_CID        ! Global coord. sys. ID for a grid (BGRID(i)) of the element
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I1,I2             ! Calculated indices in arrays
      INTEGER(LONG)                   :: ICORD             ! Internal coord. system corresponding to GLOBAL_CID
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG), PARAMETER        :: NCOLA     = 3     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NCOLB     = 1     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NROWA     = 3     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NROW      = 3     ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG), PARAMETER        :: NCOL      = 1     ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG)                   :: NUM_COMPS         ! Either 6 or 1 depending on whether grid is a physical grid or a SPOINT
      INTEGER(LONG)                   :: PROW              ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG), PARAMETER        :: PCOL      = 1     ! An input to subr MATPUT, MATGET called herein 
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: TDOF_ROW          ! Row no. in array TDOF to find GDOF DOF number
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMDIS_BEGEND
 
      REAL(DOUBLE)                    :: DXI               ! An offset distance in direction 1
      REAL(DOUBLE)                    :: DYI               ! An offset distance in direction 2
      REAL(DOUBLE)                    :: DZI               ! An offset distance in direction 3
      REAL(DOUBLE)                    :: T0G(3,3)          ! Coord transformation matrix - basic to global 
      REAL(DOUBLE)                    :: DUM1(3),DUM2(3)   ! Dummy arrays needed in transforming from global to basic coords
      REAL(DOUBLE)                    :: THETAD,PHID       ! Returns from subr GEN_T0L (not used here)
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF
! **********************************************************************************************************************************
! Initialize

      DO I=1,MELDOF
         UEL(I) = ZERO
         UEB(I) = ZERO
         UEG(I) = ZERO
         UGG(I) = ZERO
      ENDDO

! Get displs for the grid points for this elem. AGRID(i) contains the actual grid point no's and BGRID(i) contains
! the internal grid point no's for this elem. These are generated in subroutine ELMDAT called by EMG
! called by OFP3 prior to this call. The grid point displ's in global coord's are in array UG_COL created in LINK9.
! This elems grid point displ's in global coord's, UGG, are gotten from UG_COL.
 
      I2 = 0
      DO I=1,ELGP
!        CALL CALC_TDOF_ROW_NUM ( AGRID(I), ROW_NUM_START, 'N' )
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(I), IGRID )
         ROW_NUM_START = TDOF_ROW_START(IGRID)
         CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
            TDOF_ROW = ROW_NUM_START + J - 1               ! TDOF has rows in grid point numerical order
            GDOF = TDOF(TDOF_ROW,G_SET_COL)                ! This is the G-set DOF for BGRID(I), component J
            I2 = I2 + 1
            UGG(I2) = UG_COL(GDOF)
         ENDDO
      ENDDO

      IF (DEBUG(56) > 0) THEN
         WRITE(F06,5000)
         WRITE(F06,5001) TRIM(TYPE), EID, SCNUM(INT_SC_NUM)
         WRITE(F06,*)
         WRITE(F06,5101)
         WRITE(F06,5002) ' UGG FOR G.P. ', AGRID(1), ':       ', (UGG(I),I=1, 6)
         WRITE(F06,5002) ' UGG FOR G.P. ', AGRID(2), ':       ', (UGG(I),I=7,12)
         WRITE(F06,*)
      ENDIF
                                                           ! For all but ELAS, USERIN there is a 3 step process to get UEL from UGG 
      IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  ')) THEN

!        ---------------------------------------------------------------------------------------------------------------------------
         I2 = 0                                            ! (1) Transform global displs at grids to global displs at elem nodes
         DO I=1,ELGP                                          ! First assume no offset at this node
            CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               I2 = I2 + 1
               UEG(I2) = UGG(I2)
            ENDDO   
            IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
               IF (TYPE /= 'BUSH    ') THEN                !     BUSH local axes are et the grids, not at the BUSH location
                  IF (OFFSET(I) == 'Y') THEN               !     Elem is offset at this node so transform UGG to UEG
                     DXI = OFFDIS(I,1)
                     DYI = OFFDIS(I,2)
                     DZI = OFFDIS(I,3)
                     PROW = 6*(I-1) + 1
                     UEG(PROW)   = UGG(PROW)                     + DZI*UGG(PROW+4) - DYI*UGG(PROW+5)
                     UEG(PROW+1) = UGG(PROW+1) - DZI*UGG(PROW+3)                   + DXI*UGG(PROW+5)
                     UEG(PROW+2) = UGG(PROW+2) + DYI*UGG(PROW+3) - DXI*UGG(PROW+4) 
                     UEG(PROW+3) = UGG(PROW+3)
                     UEG(PROW+4) = UGG(PROW+4)
                     UEG(PROW+5) = UGG(PROW+5)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO 

         IF (DEBUG(56) > 0) THEN
            WRITE(F06,5102)
            WRITE(F06,5002) ' UEG for G.P. ', AGRID(1), ':       ', (UEG(I),I=1, 6)
            WRITE(F06,5002) ' UEG for G.P. ', AGRID(2), ':       ', (UEG(I),I=7,12)
            WRITE(F06,*)
         ENDIF

!        ---------------------------------------------------------------------------------------------------------------------------
         I2 = 0                                            ! (2) Transform from global (UEG) to basic (UEB) coords at elem nodes
         DO I=1,ELGP
            GLOBAL_CID = GRID(BGRID(I),3)                  !     GLOBAL_CID local coord sys exists. It was checked in CORD_PROC
            IF (GLOBAL_CID /= 0) THEN                      !     If global is not basic, do coord transformation
               DO J=1,NCORD
                  IF (CORD(J,2) == GLOBAL_CID) THEN
                     ICORD = J                             !     ICORD is the internal coord. sys. ID corresponding to GLOBAL_CID
                     EXIT
                  ENDIF
               ENDDO   
               CALL GEN_T0L ( BGRID(I), ICORD, THETAD, PHID, T0G )
               DO J=1,2
                  PROW = 6*(I-1) + 1 + 3*(J-1)
                  CALL MATGET ( UEG,  6*MELGP, 1, PROW, PCOL, NROW, NCOL, DUM1 )
                  CALL MATMULT_FFF ( T0G, DUM1, NROWA, NCOLA, NCOLB, DUM2 )
                  CALL MATPUT ( DUM2, 6*MELGP, 1, PROW, PCOL, NROW, NCOL, UEB )
               ENDDO   
            ELSE                                           ! If global is basic, get UEB terms directly from UEG
               CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  I2 = I2 + 1
                  UEB(I2) = UEG(I2)
               ENDDO   
            ENDIF
         ENDDO

         IF (DEBUG(56) > 0) THEN
            WRITE(F06,5103)
            WRITE(F06,5002) ' UEB for G.P. ', AGRID(1), ':       ', (UEB(I),I=1, 6)
            WRITE(F06,5002) ' UEB for G.P. ', AGRID(2), ':       ', (UEB(I),I=7,12)
            WRITE(F06,*)
         ENDIF
 
!        ---------------------------------------------------------------------------------------------------------------------------
         DO I=1,2*ELGP                                     ! (3) Transform from basic (UEB) to local (UEL) elem coords at elem nodes
            I1 = 3*I - 2
            UEL(I1)   = TE(1,1)*UEB(I1)+ TE(1,2)*UEB(I1+1)+ TE(1,3)*UEB(I1+2)
            UEL(I1+1) = TE(2,1)*UEB(I1)+ TE(2,2)*UEB(I1+1)+ TE(2,3)*UEB(I1+2)
            UEL(I1+2) = TE(3,1)*UEB(I1)+ TE(3,2)*UEB(I1+1)+ TE(3,3)*UEB(I1+2)
         ENDDO   

         IF (DEBUG(56) > 0) THEN
            WRITE(F06,5104)
            WRITE(F06,5002) ' UEL for G.P. ', AGRID(1), ':       ', (UEL(I),I=1, 6)
            WRITE(F06,5002) ' UEL for G.P. ', AGRID(2), ':       ', (UEL(I),I=7,12)
            WRITE(F06,*)
            WRITE(F06,5000)
         ENDIF

      ELSE                                                 ! ELAS, USERIN have stiff, etc, in global coords so UEL = UGG

         DO I=1,ELDOF
            UEG(I) = UGG(I)
            UEB(I) = UGG(I)
            UEL(I) = UGG(I)
         ENDDO

      ENDIF

      IF (DEBUG(109) > 0) THEN
         CALL DEBUG_ELMDIS
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

 5000 FORMAT('=============================================================================================================',/)

 5001 FORMAT(23X, 'S U B R O U T I N E   ELMDIS   F O R   ',A,I8,', S/C ',I8)

 5002 FORMAT(A,I3,A,12ES14.6)

 5101 FORMAT(' Displacements at grids in global coords',/,' ---------------------------------------')
 
 5102 FORMAT(' Displacements at elem nodes in global coords',/,' --------------------------------------------')

 5103 FORMAT(' Displacements at elem nodes in basic coords',/,' -------------------------------------------')
 
 5104 FORMAT(' Displacements at elem nodes in local elem coords',/,' ------------------------------------------------')


! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEBUG_ELMDIS

      IMPLICIT NONE

! **********************************************************************************************************************************

      I2 = 0
      DO I=1,ELGP
         ROW_NUM_START = TDOF_ROW_START(IGRID)
         CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
            TDOF_ROW = ROW_NUM_START + J - 1
            GDOF = TDOF(TDOF_ROW,G_SET_COL)
            I2 = I2 + 1
         ENDDO
      ENDDO

      I2 = 0
      DO I=1,ELGP
         CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            I2 = I2 + 1
         ENDDO
      ENDDO

      I2 = 0
      DO I=1,ELGP
         CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            I2 = I2 + 1
         ENDDO
      ENDDO

      I2 = 0
      DO I=1,ELGP
         CALL GET_GRID_NUM_COMPS ( AGRID(I), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            I2 = I2 + 1
         ENDDO
      ENDDO

! **********************************************************************************************************************************








! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_ELMDIS

      END SUBROUTINE ELMDIS
