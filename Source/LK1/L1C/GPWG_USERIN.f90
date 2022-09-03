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
 
      SUBROUTINE GPWG_USERIN ( IEID )
 
! Generates rigid body mass properties for one USERIN element
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NGRID, SOL_NAME, WARN_ERR
      USE PARAMS, ONLY                :  EPSIL, GRDPNT, MEFMGRID, SUPWARN, WTMASS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GPWG_USERIN_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  NUM_EMG_FATAL_ERRS, EID, GRID_ID, ME, PLY_NUM, RGRID, USERIN_RBM0
 

      USE GPWG_USERIN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GPWG_USERIN'
      CHARACTER(1*BYTE)               :: OPT(6)            ! Option flags for what to calculate when subr EMG is called

      INTEGER(LONG), INTENT(IN)       :: IEID              ! Internal element ID for the USERIN element to process
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Local error indicator
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where an actual grid ID is found
      INTEGER(LONG)                   :: INFO        = 0   ! An output from subr GPWG_PMOI, called herein
      INTEGER(LONG)                   :: GRDPNT_DEF        ! Default value of GRDPNT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GPWG_USERIN_BEGEND

      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: M0                ! An intermediate variable used in calc model mass props
      REAL(DOUBLE)                    :: MOI1(3,3)         ! Moments of inertia (several diff interps during exec of this subr)
      REAL(DOUBLE)                    :: MX                ! First moment of MASS about X axis
      REAL(DOUBLE)                    :: MY                ! First moment of MASS about Y axis
      REAL(DOUBLE)                    :: MZ                ! First moment of MASS about Z axis
      REAL(DOUBLE)                    :: Q(3,3)            ! Output from subr GPWG_PMOI, called herein (transform to princ dir's)
      REAL(DOUBLE)                    :: TRANS(3,3)        ! Transfer terms when MOI's about c.g. ard calc'd from MOI's about XREF
      REAL(DOUBLE)                    :: XB(3)             ! Basic coord diffs bet c.g. and XREF in X, Y, Z directions
      REAL(DOUBLE)                    :: XD(3)             ! Basic coord diffs bet a mass (at it's c.m.) and XREF in X, Y, Z dirs
      REAL(DOUBLE)                    :: XREF(3)           ! GRDPNT basic coords (or origin of basic sys if GRDPNT doesn't exist)
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Set defaults in case GRDPNT grid cannot be found or is input as 0 (basic origin)

      GRDPNT_DEF = 0
      XREF(1)    = ZERO
      XREF(2)    = ZERO
      XREF(3)    = ZERO

! Get reference point coordinates in basic system for the reference point
 
      IF (GRDPNT /= -1) THEN
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRDPNT, GRID_ID_ROW_NUM )
         IF (GRID_ID_ROW_NUM /= -1) THEN                   ! GRDPNT is a grid point in the model, so get its basic coords
            XREF(1) = RGRID(GRID_ID_ROW_NUM,1)
            XREF(2) = RGRID(GRID_ID_ROW_NUM,2)
            XREF(3) = RGRID(GRID_ID_ROW_NUM,3)
         ELSE                                              ! GRDPNT was not 0 and is not a grid number in the model
            IF (GRDPNT /= 0) THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,1402) GRDPNT
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,1402) GRDPNT
               ENDIF
            ENDIF
            GRDPNT = GRDPNT_DEF
         ENDIF
      ENDIF
 
! Generate total mass, first and second moments by summing up mass terms. XD(i) are components of vector from
! ref point to a mass point. At this time, mass units are input units without PARAM WTMASS which is what we want for
! the grid point weight generator. Later the mass will be converted by multiplying by WTMASS.
 
      M0  = ZERO
      MX    = ZERO
      MY    = ZERO
      MZ    = ZERO   

      XB(1) = ZERO
      XB(2) = ZERO
      XB(3) = ZERO
 
! Process this USERIN element mass terms
 
      OPT(1) = 'Y'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'N'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'N'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'N'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff

      IERROR  = 0
      PLY_NUM = 0
      CALL EMG ( IEID, OPT, 'N', SUBR_NAME, 'N' )          ! 'N' means do not write to BUG file

      IF (NUM_EMG_FATAL_ERRS == 0) THEN

         M0   = USERIN_RBM0(1,1)
         IF (DABS(M0) > EPS1) THEN
            XD(1) = USERIN_RBM0(2,6)/M0 !- XREF(1)
            XD(2) = USERIN_RBM0(3,4)/M0 !- XREF(2)
            XD(3) = USERIN_RBM0(1,5)/M0 !- XREF(3)
            MX    = MX + M0*XD(1)
            MY    = MY + M0*XD(2)
            MZ    = MZ + M0*XD(3)
         ELSE
            XD(1) = ZERO
            XD(2) = ZERO
            XD(3) = ZERO
         ENDIF


      ENDIF

      OPT(1) = 'N'                                         ! Reset subr EMG option value:

      IF (IERROR > 0) THEN
         WRITE(ERR,9876) IERROR
         WRITE(F06,9876) IERROR
         CALL OUTA_HERE ( 'Y' )                            ! Errors from subr EMG, so quit
      ENDIF

! XB(I) are components of distance from reference point, XREF, to c.g.
 
      IF (DABS(M0) > EPS1) THEN
         XB(1) = MX/M0
         XB(2) = MY/M0
         XB(3) = MZ/M0
      ENDIF
 
! Output results so far
 
      IF (GRDPNT >= 0) THEN
         WRITE(F06,1000) EID
         IF (GRDPNT == 0) THEN
            WRITE(F06,1001)
         ELSE IF (GRDPNT > 0) THEN
            WRITE(F06,1002) GRDPNT
         ENDIF
      ENDIF

      IF (GRDPNT >= 0) THEN

         WRITE(F06,1004) M0

         WRITE(F06,1005) (XB(I),I=1,3)

         WRITE(F06,1021)
         WRITE(F06,1901)
         DO I=1,3
            WRITE(F06,1022) (USERIN_RBM0(I,J),J=1,6)
         ENDDO
         WRITE(F06,1902)
         DO I=4,6
            WRITE(F06,1022) (USERIN_RBM0(I,J),J=1,6)
         ENDDO
         WRITE(F06,1901)
         WRITE(F06,*)
         WRITE(F06,*)

         WRITE(F06,1006)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (USERIN_RBM0(I+3,J+3),J=1,3)
         ENDDO   
         WRITE(F06,1900)
         WRITE(F06,*)
         WRITE(F06,*)

      ENDIF
 
! Generate moments of inertia about c.g. in basic coord. system
 
      TRANS(1,1) =  M0*(XB(2)*XB(2) + XB(3)*XB(3))
      TRANS(2,2) =  M0*(XB(1)*XB(1) + XB(3)*XB(3))
      TRANS(3,3) =  M0*(XB(1)*XB(1) + XB(2)*XB(2))
      TRANS(1,2) = -M0*XB(1)*XB(2)
      TRANS(1,3) = -M0*XB(1)*XB(3)
      TRANS(2,3) = -M0*XB(2)*XB(3)
      TRANS(2,1) =  TRANS(1,2)
      TRANS(3,1) =  TRANS(1,3)
      TRANS(3,2) =  TRANS(2,3)
      DO I=1,3
         DO J=1,3
            MOI1(I,J) = USERIN_RBM0(I+3,J+3) - TRANS(I,J)
         ENDDO
      ENDDO   
 
! Output MOI's about c.g.
 
      IF (GRDPNT >= 0) THEN
         WRITE(F06,1007)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (MOI1(I,J),J=1,3)              ! MOI1 now are MOI's about cg in basic
         ENDDO   
         WRITE(F06,1900)
         WRITE(F06,*)
         WRITE(F06,*)
      ENDIF
 
! Get principal MOI's and transformation matrix (eigenvectors of MOI1)
 
      CALL GPWG_PMOI ( MOI1, Q, INFO )
 
! Write out princ MOI's and coord transf. Otherwise errors were written in subr GPWG_PMOI

      IF ((INFO == 0) .AND. (GRDPNT >= 0)) THEN  
         WRITE(F06,1008)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (MOI1(I,J),J=1,3)              ! MOI1 is now principal MOI matrix
         ENDDO   
         WRITE(F06,1900)
         WRITE(F06,*)
         WRITE(F06,*)
 
         WRITE(F06,1009)
         WRITE(F06,1900)
         DO I=1,3
            WRITE(F06,1101) (Q(I,J),J=1,3)
         ENDDO   
         WRITE(F06,1900)
         WRITE(F06,*)
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
 1000 FORMAT(/,'     O U T P U T   F R O M   T H E   G R I D   P O I N T   W E I G H T   G E N E R A T O R   F O R   U S E R I N', &
               '   E L E M',I8)

 1001 FORMAT(26X,'                   (reference point is basic coord system origin)'                                               &
            ,//)

 1002 FORMAT(26X,'                      (reference point is grid point '    ,I8,')'                                                &
            ,//)

 1004 FORMAT(  26X,'                            Total mass = '    ,1ES13.6                                                         &
            ,//)

 1005 FORMAT(61X,                                    'X             Y             Z'                                               &
          ,/,26X,'             C.G. location :',3(1ES14.6),                                                                        &
           /,26X,'              (relative to reference point in basic coordinate system)'                                          &
          ,//)

 1006 FORMAT(36X,'M.O.I. matrix - about reference point in basic coordinate system')

 1007 FORMAT(34X,'M.O.I. matrix - about above c.g. location in basic coordinate system')

 1008 FORMAT(37X,'M.O.I. matrix - about above c.g. location in principal directions')

 1009 FORMAT(37X,'Transformation from basic coordinates to principal directions')

 1021 FORMAT(29X,'6x6 Rigid body mass matrix - about reference point in basic coordinate system')

 1022 FORMAT(22X,'*',3(1ES14.6),'  *',3(1ES14.6),'  *')

 1901 FORMAT('                      ***                                                                                     ***')

 1902 FORMAT('                      *  ************  ************  ************  *  ************  ************  ************  *')

 1101 FORMAT(45X,'*',3(1ES14.6),'  *')

 1900 FORMAT(45X,'***',40X,'***')

 1402 FORMAT(' *WARNING    : PARAM GRDPNT (OR PARAM MEFMGRID) REFERENCES NONEXISTENT GRID POINT ',I8,'. BASIC ORIGIN WILL BE USED')

 9876 FORMAT(/,' PROCESSING ABORTED DUE TO ABOVE ',I8,' ELEMENT GENERATION ERRORS')

! **********************************************************************************************************************************

      END SUBROUTINE GPWG_USERIN
