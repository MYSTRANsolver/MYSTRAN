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
 
      SUBROUTINE TRANSFORM_NODE_FORCES ( COORD_SYS )
 
! Converts node forces for all elements from local to global or local to basic coords. The local to basic transformation is done
! every time this subr is called since that transformation must be done if either basic global is the final system anyway.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELGP, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TRANSFORM_NODE_FORCES_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  CAN_ELEM_TYPE_OFFSET, GRID, CORD, BGRID, ELDOF, ELGP, OFFDIS, OFFSET, PEB, PEG, PEL, TE,  &
                                         TYPE

      USE TRANSFORM_NODE_FORCES_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TRANSFORM_NODE_FORCES'
      CHARACTER(LEN=*), INTENT(IN)    :: COORD_SYS         ! 'B" for basic, 'G' for global
 
      INTEGER(LONG)                   :: GLOBAL_CID        ! Global coord. sys. ID for a grid (BGRID(i)) of the element
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I1                ! Calculated displ component no's for ELAS elems
      INTEGER(LONG)                   :: ICORD             ! Internal coord. system corresponding to GLOBAL_CID
      INTEGER(LONG)                   :: NROWS             ! DO loop limits when calculating elem nodal or engr forces
      INTEGER(LONG), PARAMETER        :: NCOLA     = 3     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NCOLB     = 1     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NROWA     = 3     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NROW      = 3     ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG), PARAMETER        :: NCOL      = 1     ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG)                   :: PROW              ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG), PARAMETER        :: PCOL      = 1     ! An input to subr MATPUT, MATGET called herein 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TRANSFORM_NODE_FORCES_BEGEND
 
      REAL(DOUBLE)                    :: DXI               ! An offset distance in direction 1
      REAL(DOUBLE)                    :: DYI               ! An offset distance in direction 2
      REAL(DOUBLE)                    :: DZI               ! An offset distance in direction 3
      REAL(DOUBLE)                    :: T0G(3,3)          ! Coord transformation matrix - basic to global 
      REAL(DOUBLE)                    :: DUM1(3)           ! Dummy arrays needed in transforming from global to basic coords
      REAL(DOUBLE)                    :: DUM2(3)           ! Dummy arrays needed in transforming from global to basic coords
      REAL(DOUBLE)                    :: DUM3(ELDOF)       ! Dummy arrays needed in transforming from global to basic coords
      REAL(DOUBLE)                    :: THETAD,PHID       ! Returns from subr GEN_T0L (not used here)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NROWS = ELDOF
 
      DO I=1,NROWS
         PEB(I) = ZERO
         PEG(I) = ZERO
      ENDDO 

      IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  ')) THEN
                                                           ! (1) Transform from local to basic. Use TE transpose to get PEB from PEL
         DO I=1,2*ELGP
            I1 = 3*I - 2
            PEB(I1)   = TE(1,1)*PEL(I1)+ TE(2,1)*PEL(I1+1)+ TE(3,1)*PEL(I1+2)
            PEB(I1+1) = TE(1,2)*PEL(I1)+ TE(2,2)*PEL(I1+1)+ TE(3,2)*PEL(I1+2)
            PEB(I1+2) = TE(1,3)*PEL(I1)+ TE(2,3)*PEL(I1+1)+ TE(3,3)*PEL(I1+2)
         ENDDO

         IF (COORD_SYS == 'G') THEN                        ! (2) Transform from basic to global if output is to be in global

            DO I=1,ELGP
               GLOBAL_CID = GRID(BGRID(I),3)               !     GLOBAL_CID local coord system exists. It was checked in CORD_PROC.
               IF (GLOBAL_CID /= 0) THEN                   !     If global is not basic, do coord transformation
                  DO J=1,NCORD
                     IF (CORD(J,2) == GLOBAL_CID) THEN
                        ICORD = J                          !     ICORD is the internal coord. sys. ID corresponding to GLOBAL_CID
                        EXIT
                     ENDIF
                  ENDDO   
                  CALL GEN_T0L ( BGRID(I), ICORD, THETAD, PHID, T0G )
                  DO J=1,2
                     PROW = 6*(I-1) + 1 + 3*(J-1)
                     CALL MATGET ( PEB,  6*MELGP, 1, PROW, PCOL, NROW, NCOL, DUM1 )
                     CALL MATMULT_FFF_T ( T0G, DUM1, NROWA, NCOLA, NCOLB, DUM2 )
                     CALL MATPUT ( DUM2, 6*MELGP, 1, PROW, PCOL, NROW, NCOL, PEG )
                  ENDDO   
               ELSE                                        !     If global is basic, get PEB terms directly from PEG
                  PROW = 6*(I-1) + 1
                  DO J=1,6
                     PEG(PROW+J-1) = PEB(PROW+J-1)
                  ENDDO   
               ENDIF
            ENDDO

         ENDIF

      ELSE

         DO I=1,ELDOF
            PEG(I) = PEL(I)
            PEB(I) = PEL(I)
         ENDDO

      ENDIF

! Transform element loads at element ends to grids for BAR and ROD. Only need to do this if there are any offsets. 
! Still use PEG to denote element forces (but now at grids, not elem ends)

      IF ((TYPE == 'BAR     ') .OR. (TYPE == 'ROD     ')) THEN
         DO I=1,ELGP
            PROW = 6*(I-1) + 1
            DO J=1,6
               DUM3(PROW+J-1) = PEG(PROW+J-1)
            ENDDO   
            IF (CAN_ELEM_TYPE_OFFSET == 'Y') THEN
               IF (OFFSET(I) == 'Y') THEN                     ! Elem is offset at this node so transform PEG (using DUM3)
                  DXI = OFFDIS(I,1)
                  DYI = OFFDIS(I,2)
                  DZI = OFFDIS(I,3)
                  PROW = 6*(I-1) + 1
                  DUM3(PROW)   = PEG(PROW)
                  DUM3(PROW+1) = PEG(PROW+1)
                  DUM3(PROW+2) = PEG(PROW+2)
                  DUM3(PROW+3) = PEG(PROW+3)                   - DZI*PEG(PROW+1) + DYI*PEG(PROW+2)
                  DUM3(PROW+4) = PEG(PROW+4) + DZI*PEG(PROW)                     - DXI*PEG(PROW+2)
                  DUM3(PROW+5) = PEG(PROW+5) - DYI*PEG(PROW)   + DXI*PEG(PROW+1)
               ENDIF
            ENDIF
         ENDDO

         DO I=1,ELDOF                                      ! PEG is now at grids, not elem ends
            PEG(I) = DUM3(I)
         ENDDO

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

! **********************************************************************************************************************************
 
      END SUBROUTINE TRANSFORM_NODE_FORCES

