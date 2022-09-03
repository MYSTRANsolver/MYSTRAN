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
  
      SUBROUTINE WRITE_ELEM_ENGR_FORCE ( JSUB, NUM, IHDR, ITABLE )
  
! Writes blocks of element engineering force output for one element type, one subcase. Elements that can have engineering force
! output are the ones enumerated below fin the IF(TYPE == ???). 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06, OP2
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, INT_SC_NUM, NDOFR, NUM_CB_DOFS, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, OGEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ELEM_ENGR_FORCE_BEGEND
      USE MODEL_STUF, ONLY            :  ELEM_ONAME, LABEL, SCNUM, STITLE, TITLE, TYPE
      USE WRITE_ELEM_ENGR_FORCE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ELEM_ENGR_FORCE'
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write an output header
      CHARACTER(128*BYTE)             :: FILL              ! Padding for output format
      CHARACTER(LEN=LEN(ELEM_ONAME))  :: ONAME             ! Element name to write out in F06 file
  
      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      INTEGER(LONG)                   :: BDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_DOF_NUM       ! DOF number for BDY_GRID/BDY_COMP
      INTEGER(LONG)                   :: I,J,J1            ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_TERMS         ! Number of terms to write out for shell elems
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ELEM_ENGR_FORCE_BEGEND
  
      REAL(DOUBLE)                    :: ABS_ANS(8)       ! Max ABS for all element output
      REAL(DOUBLE)                    :: MAX_ANS(8)       ! Max for all element output
      REAL(DOUBLE)                    :: MIN_ANS(8)       ! Min for all element output
      
      ! op2 info
      CHARACTER( 8*BYTE)              :: TABLE_NAME             ! the name of the op2 table

      ! table -3 info
      INTEGER(LONG)                   :: ANALYSIS_CODE          ! static/modal/time/etc. flag
      INTEGER(LONG)                   :: ELEMENT_TYPE           ! the OP2 flag for the element
      CHARACTER(LEN=128)              :: TITLEI                 ! the model TITLE
      CHARACTER(LEN=128)              :: STITLEI                ! the subcase SUBTITLE
      CHARACTER(LEN=128)              :: LABELI                 ! the subcase LABEL
      INTEGER(LONG)                   :: FIELD5_INT_MODE
      REAL(DOUBLE)                    :: FIELD6_EIGENVALUE

!     op2 specific flags
      INTEGER(LONG)                   :: DEVICE_CODE  ! PLOT, PRINT, PUNCH flag
      INTEGER(LONG)                   :: NUM_WIDE     ! the number of "words" for an element
      INTEGER(LONG)                   :: NVALUES      ! the number of "words" for all the elments
      INTEGER(LONG)                   :: NTOTAL       ! the number of bytes for all NVALUES
      INTEGER(LONG)                   :: ISUBCASE     ! the subcase ID
      INTEGER(LONG)                   :: NELEMENTS
      ! initialize
      ANALYSIS_CODE = -1

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ! initialize
      DEVICE_CODE = 1  ! PLOT
      ANALYSIS_CODE = -1
      FILL(1:) = ' '

! Get element output name
 
      ONAME(1:) = ' '
      CALL GET_ELEM_ONAME ( ONAME )

      ! Write output headers.
      ANALYSIS_CODE = -1
      FIELD5_INT_MODE = 0
      FIELD6_EIGENVALUE = 0.0

headr:IF (IHDR == 'Y') THEN

         !--- Subcase num, TITLE, SUBT, LABEL:

         WRITE(F06,*)
         WRITE(F06,*)
         ISUBCASE = SCNUM(JSUB)
         IF    (SOL_NAME(1:7) == 'STATICS') THEN
            ANALYSIS_CODE = 1
            FIELD5_INT_MODE = SCNUM(JSUB)
            WRITE(F06,101) SCNUM(JSUB)
         ELSE IF (SOL_NAME(1:8) == 'NLSTATIC') THEN
            ANALYSIS_CODE = 10
            FIELD5_INT_MODE = SCNUM(JSUB)
            WRITE(F06,101) SCNUM(JSUB);   IF (DEBUG(200) > 0) WRITE(ANS,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN
            ANALYSIS_CODE = 1
            FIELD5_INT_MODE = SCNUM(JSUB)
            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
            ANALYSIS_CODE = 7
            FIELD5_INT_MODE = JSUB
            ! FIELD6_EIGENVALUE = ????
            WRITE(F06,102) JSUB

         ELSE IF (SOL_NAME(1:5) == 'MODES') THEN
            ANALYSIS_CODE = 2
            FIELD5_INT_MODE = JSUB
            ! FIELD6_EIGENVALUE = ????
            WRITE(F06,102) JSUB

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
               WRITE(F06,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
            ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
               WRITE(F06,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
            ELSE
               WRITE(F06,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
            ENDIF
         ENDIF

         TITLEI = TITLE(INT_SC_NUM)
         STITLEI = STITLE(INT_SC_NUM)
         LABELI = LABEL(INT_SC_NUM)
         IF (TITLE(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,201) TITLE(INT_SC_NUM)
         ENDIF

         IF (STITLE(INT_SC_NUM)(1:) /= ' ') THEN
            WRITE(F06,201) STITLE(INT_SC_NUM)
         ENDIF

         IF (LABEL(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,201) LABEL(INT_SC_NUM)
         ENDIF

         WRITE(F06,*)

         !--- 1st 2 lines of element specific headers - general info on what type of output:
         IF      (TYPE(1:3) == 'BAR') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:33)
            ELSE
               WRITE(F06,301) FILL(1:39)
            ENDIF
            WRITE(F06,401) FILL(1:45), ONAME

         ELSE IF (TYPE(1:4) == 'BUSH') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:19)
            ELSE
               WRITE(F06,301) FILL(1:24)
            ENDIF
            WRITE(F06,401) FILL(1:29), ONAME

         ELSE IF (TYPE(1:4) == 'ELAS') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:27)
            ELSE
               WRITE(F06,301) FILL(1:33)
            ENDIF
            WRITE(F06,401) FILL(1:37), ONAME

         ELSE IF (TYPE(1:3) == 'ROD') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:27)
            ELSE
               WRITE(F06,301) FILL(1:33)
            ENDIF
            WRITE(F06,401) FILL(1:37), ONAME

         ELSE IF (TYPE(1:5) == 'SHEAR') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:22)
            ELSE
               WRITE(F06,301) FILL(1:29)
            ENDIF
            WRITE(F06,401) FILL(1:32), ONAME

         ELSE IF((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:33)
            ELSE
               WRITE(F06,301) FILL(1:39)
            ENDIF
            WRITE(F06,401) FILL(1:43), ONAME

         ENDIF

         !--- Header lines describing columns of output for an element type:
         IF      (TYPE(1:3) == 'BAR'  ) THEN
            WRITE(F06,1101) FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:4) == 'ELAS') THEN
            WRITE(F06,1201) FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:3) == 'ROD') THEN
            WRITE(F06,1301) FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:5) == 'SHEAR') THEN
            WRITE(F06,1401) FILL(1: 0), FILL(1: 0)

         ELSE IF ((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN 
            WRITE(F06,1501) FILL(1: 0), FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:4) == 'BUSH') THEN
            WRITE(F06,1601) FILL(1: 0), FILL(1: 0)

         ENDIF

         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'Headers' )

      ENDIF headr

      ! Write element force output
      IF      (TYPE == 'BAR     ') THEN
         ELEMENT_TYPE = 34
         NUM_WIDE = 9  ! eid, bm1a, bm2a, bm1b, bm2b, ts1, ts2, af, trq
         NVALUES = NUM_WIDE * NUM
         CALL WRITE_OEF3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, ELEMENT_TYPE, NUM_WIDE, &
                                TITLEI, STITLEI, LABELI, FIELD5_INT_MODE, FIELD6_EIGENVALUE)
         WRITE(OP2) NVALUES
         WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, (REAL(OGEL(I,J), 4), J=1,8), I=1,NUM)


         DO I=1,NUM
            WRITE(F06,1102) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,8)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 8 )
         WRITE(F06,1103) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,8), FILL(1: 0), (MIN_ANS(J),J=1,8), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,8), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'BAR' )

      ELSE IF (TYPE(1:4) == 'ELAS') THEN                   ! Engr force for ELAS was put into OGEL(I,1)
!          CALL GET_SPRING_OP2_ELEMENT_TYPE(ELEMENT_TYPE)
!          NUM_WIDE = 2 ! eid, spring_force
!          NVALUES = NUM_WIDE * NUM
!          CALL WRITE_OEF3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, ELEMENT_TYPE, NUM_WIDE, &
!                                 TITLEI, STITLEI, LABELI, FIELD5_INT_MODE, FIELD6_EIGENVALUE)
!          WRITE(OP2) NVALUES
!          WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, REAL(OGEL(I,1), 4), I=1,NUM)
! 
!          ! TODO: what's going on with this loop having the 1,NUM,5??? and the J=J1,J1+4???
! =======
! 
!xx      WRITE(F06,1202) FILL(1: 0), (EID_OUT_ARRAY(I,1),OGEL(I,1),I=1,NUM)

         J1 = 1
         DO I=1,NUM,5
            IF (J1+4 <= NUM) THEN
               WRITE(F06,1202) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), J=J1,J1+4)
               J1 = J1 + 5
            ELSE
               WRITE(F06,1202) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), J=J1,NUM)
            ENDIF
         ENDDO
         CALL GET_MAX_MIN_ABS ( 1, 1 )
         WRITE(F06,1203) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,1), FILL(1: 0), (MIN_ANS(J),J=1,1), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,1), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'ELAS' )
 
      ELSE IF (TYPE == 'ROD     ') THEN
         !CALL WRITE_OEF_ROD ( ISUBCASE, NUM, FILL(1:1), FILL(1:16), ITABLE, TITLEI, STITLEI, LABELI )
         ELEMENT_TYPE = 1
         NUM_WIDE = 3  ! eid, axial, torsion
         NVALUES = NUM_WIDE * NUM
         CALL WRITE_OEF3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, ELEMENT_TYPE, NUM_WIDE, &
                                TITLEI, STITLEI, LABELI, FIELD5_INT_MODE, FIELD6_EIGENVALUE)

         ! TODO: why does fields 7/8 write out the axial and torsion?
         WRITE(OP2) NVALUES
         WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, REAL(OGEL(I,7), 4), REAL(OGEL(I,8), 4), I=1,NUM)

         J1 = 1
         DO I=1,NUM,3
            IF (J1+2 <= NUM) THEN
               WRITE(F06,1302) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,7), OGEL(J,8), J=J1,J1+2)
               J1 = J1 + 3
            ELSE
               WRITE(F06,1302) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,7), OGEL(J,8), J=J1,NUM)
            ENDIF
         ENDDO
         CALL GET_MAX_MIN_ABS ( 7, 8 )
         WRITE(F06,1303) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=7,8), FILL(1: 0), (MIN_ANS(J),J=7,8), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=7,8), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'ROD' )
  
      ELSE IF (TYPE == 'SHEAR   ') THEN
         !CALL WRITE_SHEAR_OEF()
         ELEMENT_TYPE = 4  ! CSHEAR
         ! eid,[
         !  force41, force21, force12, force32, force23, force43,
         !  force34, force14,
         !  kick_force1, shear12, kick_force2, shear23,
         !  kick_force3, shear34, kick_force4, shear41,
         !
         NUM_WIDE = 17
         !CALL WRITE_OEF3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, ELEMENT_TYPE, NUM_WIDE, &
         !                       TITLEI, STITLEI, LABELI, FIELD5_INT_MODE, FIELD6_EIGENVALUE)
         NVALUES = NUM * NUM_WIDE
         !WRITE(OP2) NVALUES
         ! write the CSHEAR force data
         !WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, REAL(OGEL(I,3), 4), REAL(OGEL(I,3), 4), &
         !                                               NAN, I=1,NUM)
         
         J1 = 1
         DO I=1,NUM,2
            IF      (J1+1 <= NUM) THEN
               WRITE(F06,1402) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), OGEL(J,2), OGEL(J,3), J=J1,J1+1)
               J1 = J1 + 2
            ELSE
               WRITE(F06,1402) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), OGEL(J,2), OGEL(J,3), J=J1,NUM)
            ENDIF
         ENDDO
         CALL GET_MAX_MIN_ABS ( 1, 3 )
         WRITE(F06,1403) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,3), FILL(1: 0), (MIN_ANS(J),J=1,3), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,3), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'SHEAR' )
  
      ELSE IF ((TYPE == 'TRIA3K  ') .OR. (TYPE == 'QUAD4K  ')) THEN 
         DO I=1,NUM
            WRITE(F06,1512) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,6)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 8 )
         WRITE(F06,1513) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,6), FILL(1: 0), (MIN_ANS(J),J=1,6), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,6), FILL(1: 0)
         NUM_TERMS = 6
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'SHELL' )

      ELSE IF ((TYPE == 'TRIA3   ') .OR. (TYPE == 'QUAD4   ')) THEN 
         IF (TYPE == 'TRIA3   ') THEN
             ELEMENT_TYPE = 74
         ELSE IF (TYPE == 'QUAD4   ') THEN
             ELEMENT_TYPE = 33  ! todo: verify no ELEMENT_TYPE=144
         !ELSE
         !   error
         ENDIF
        ! -MEMBRANE FORCES-   -BENDING MOMENTS- -TRANSVERSE SHEAR FORCES -
        !     FX FY FXY           MX MY MXY            QX QY         DO I=1,NUM
        ! [fx, fy, fxy,  mx,  my,  mxy, qx, qy]
         NUM_WIDE = 9
         NVALUES = NUM * NUM_WIDE
         CALL WRITE_OEF3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, ELEMENT_TYPE, NUM_WIDE, &
                                TITLEI, STITLEI, LABELI, FIELD5_INT_MODE, FIELD6_EIGENVALUE)
         WRITE(OP2) NVALUES
         WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE, (REAL(OGEL(I,J),4),J=1,8), I=1,NUM)

         DO I=1,NUM
            WRITE(F06,1522) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,8)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 8 )
         WRITE(F06,1523) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,8), FILL(1: 0), (MIN_ANS(J),J=1,8), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,8), FILL(1: 0)
         NUM_TERMS = 8
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'SHELL' )

      ELSE IF (TYPE(1:4) == 'BUSH') THEN                   ! Engr force for BUSH was put into OGEL(I,1-6)
         ELEMENT_TYPE = 102 ! CBUSH
         NUM_WIDE = 7       ! eid, tx, ty, tz, rx, ry, rz
         NVALUES = NUM * NUM_WIDE
         CALL WRITE_OEF3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, ELEMENT_TYPE, NUM_WIDE, &
                               TITLEI, STITLEI, LABELI, FIELD5_INT_MODE, FIELD6_EIGENVALUE)
         WRITE(OP2) NVALUES
         WRITE(OP2) (EID_OUT_ARRAY(I,1)*10+DEVICE_CODE,(REAL(OGEL(I,J),4),J=1,6), I=1,NUM)

         DO I=1,NUM
            WRITE(F06,1602) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,6)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 6 )
         WRITE(F06,1603) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,6), FILL(1: 0), (MIN_ANS(J),J=1,6), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,6), FILL(1: 0)

         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'BUSH' )
   
      ENDIF
  
! **********************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' OUTPUT FOR SUBCASE ',I8)

  102 FORMAT(' OUTPUT FOR EIGENVECTOR ',I8)

  103 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (boundary ',A,' for grid',I8,' component',I2,')')

  104 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (modal acceleration for mode ',I8,')')

  201 FORMAT(1X,A)

  301 FORMAT(16X,A,'E L E M E N T   E N G I N E E R I N G   F O R C E S')

  302 FORMAT(16X,A,'C B   E L E M E N T   E N G I N E E R I N G   F O R C E   O T M')

  401 FORMAT(16X,A,'F O R   E L E M E N T   T Y P E   ',A11)

! BAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1101 FORMAT(16X,A,' Element       Bend-Moment End A           Bend-Moment End B              - Shear -              Axial'        &
          ,'         Torque'  &
          ,/,16X,A,'    ID       Plane 1       Plane 2       Plane 1       Plane 2      Plane 1       Plane 2        Force')
 
 1102 FORMAT(16X,A,I8,8(1ES14.6))
 
 1103 FORMAT(1X,A,'         ------------- ------------- ------------- ------------- ------------- ------------- -------------',    &
                        ' -------------',/,                                                                                        &
             16X,A,'MAX* :  ',8(ES14.6),/,                                                                                         &
             16X,A,'MIN* :  ',8(ES14.6),//,                                                                                        &
             16X,A,'ABS* :  ',8(ES14.6),/,                                                                                         &
             16X,A,'*for output set')

! ELAS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1201 FORMAT(16X,A,' Element     Force      Element     Force      Element     Force      Element     Force      Element     Force'&
          ,/,16X,A,'    ID                     ID                     ID                     ID                     ID')
 
 1202 FORMAT(16X,A,5(I8,1ES14.6))
  
 1203 FORMAT(16X,A,'         -------------',/,                                                                                     &
             16X,A,'MAX* :  ',1(ES14.6),/,                                                                                         &
             16X,A,'MIN* :  ',1(ES14.6),//,                                                                                        &
             16X,A,'ABS* :  ',1(ES14.6),/,                                                                                         &
             16X,A,'*for output set')

! ROD >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1301 FORMAT(16X,A,' Element     Axial        Torque      Element     Axial        Torque      Element     Axial        Torque'    &
          ,/,16X,A,'    ID       Force                       ID       Force                       ID       Force')
 
 1302 FORMAT(16X,A,3(I8,1ES14.6,1ES14.6))
 
 1303 FORMAT(16X,A,'         ------------- -------------',/,                                                                       &
             16X,A,'MAX* :  ',2(1ES14.6),/,                                                                                        &
             16X,A,'MIN* :  ',2(1ES14.6),//,                                                                                       &
             16X,A,'ABS* :  ',2(1ES14.6),/,                                                                                        &
             16X,A,'*for output set')

! SHEAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1401 FORMAT(16X,A,' Element        N o r m a l   F o r c e s           Element        N o r m a l   F o r c e s          '        &
          ,/,16X,A,'    ID        Nxx           Nyy           Nxy          ID        Nxx           Nyy           Nxy')
 
 1402 FORMAT(1X,A,2(I8,3(1ES14.6),1X))
 
 1403 FORMAT(16X,A,'         ------------- ------------- -------------',/,                                                         &
             16X,A,'MAX* :  ',3ES14.6,/,                                                                                           &
             16X,A,'MIN* :  ',3ES14.6,//,                                                                                          &
             16X,A,'ABS* :  ',3ES14.6,/,                                                                                           &
             16X,A,'*for output set')

! SHELL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1501 FORMAT(16X,A,' Element         N o r m a l   F o r c e s                       M o m e n t s'                                &
          ,19X,'T r a n s v e r s e',/16X,A,'    ID', 89X,'S h e a r   F o r c e s'                                                &
          ,/,16X,A,'              Nxx           Nyy           Nxy           Mxx           Myy           Mxy            Qx         '&
          ,'  Qy')

!            WRITE(F06,1501) FILL(1: 0), FILL(1: 0), FILL(1: 0)
 1512 FORMAT(16X,A,I8,6(1ES14.6))
 
 1513 FORMAT(16X,A,'          ------------- ------------- ------------- ------------- ------------- -------------',/,              &
             16X,A,'MAX* :  ',6(ES14.6),/,                                                                                         &
             16X,A,'MIN* :  ',6(ES14.6),//,                                                                                        &
             16X,A,'ABS* :  ',6(ES14.6),/,                                                                                         &
             16X,A,'*for output set')

 1522 FORMAT(16X,A,I8,8(1ES14.6))
 
 1523 FORMAT(16X,A,'          ------------- ------------- ------------- ------------- ------------- ------------- -------------',  &
                        ' -------------',/,                                                                                        &
             16X,A,'MAX* :  ',8(ES14.6),/,                                                                                         &
             16X,A,'MIN* :  ',8(ES14.6),//,                                                                                        &
             16X,A,'ABS* :  ',8(ES14.6),/,                                                                                         &
             16X,A,'*for output set')

! BUSH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1601 FORMAT(16X,A,' Element      Force         Force         Force        Moment        Moment        Moment'                     &
          ,/,16X,A,'    ID         XE            YE            ZE            XE            YE            ZE')
 
 1602 FORMAT(16X,A,I8,6(1ES14.6))
  
 1603 FORMAT(16X,A,'          ------------- ------------- ------------- ------------- ------------- ------------- ',/,             &
             16X,A,'MAX* :  ',6(ES14.6),/,                                                                                         &
             16X,A,'MIN* :  ',6(ES14.6),//,                                                                                        &
             16X,A,'ABS* :  ',6(ES14.6),/,                                                                                         &
             16X,A,'*for output set')

! **********************************************************************************************************************************
  
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE WRITE_ANS ( WHICH )

      USE PENTIUM_II_KIND, ONLY       :  BYTE

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: WHICH             ! Which of the below to write during this call

      INTEGER(LONG)                   :: II,JJ             ! DO loop indices or counters

! **********************************************************************************************************************************
      IF (WHICH == 'Headers') THEN

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,*)
            IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
               WRITE(ANS,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN
            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
            WRITE(F06,102) JSUB

            ELSE IF (SOL_NAME(1:5) == 'MODES') THEN
               WRITE(ANS,102) JSUB

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
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
               ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
                  WRITE(ANS,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
               ELSE
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
               ENDIF

            ENDIF

            WRITE(ANS,*)

            IF ((TYPE(1:4) /= 'HEXA') .AND. (TYPE(1:5) /= 'PENTA') .AND. (TYPE(1:5) /= 'TETRA')) THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(ANS,302) FILL(1:16)
            ELSE
               WRITE(ANS,301) FILL(1:16)
            ENDIF
               WRITE(ANS,401) FILL(1:16), ONAME
            ENDIF

            IF      (TYPE(1:4) == 'ELAS') THEN
               WRITE(ANS,1111) FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'BAR     ') THEN
               WRITE(ANS,1211) FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'ROD     ') THEN
               WRITE(ANS,1311) FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'SHEAR   ') THEN
               WRITE(ANS,1411) FILL(1:16), FILL(1:16)

            ELSE IF ((TYPE(1:5)== 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN 
               WRITE(ANS,1511) FILL(1:16), FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'BUSH    ') THEN
               WRITE(ANS,1611) FILL(1:16), FILL(1:16)

            ENDIF

         ENDIF

      ELSE IF (WHICH == 'ELAS' ) THEN
         WRITE(ANS,1112) FILL(1:16), (EID_OUT_ARRAY(II,1),OGEL(II,1),II=1,NUM)
         WRITE(ANS,1113) (MAX_ANS(JJ),JJ=1,1),(MIN_ANS(JJ),JJ=1,1),(ABS_ANS(JJ),JJ=1,1)

      ELSE IF (WHICH == 'BAR'  ) THEN
         DO II=1,NUM
            WRITE(ANS,1212) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,8)
         ENDDO   
         WRITE(ANS,1213) (MAX_ANS(JJ),JJ=1,8),(MIN_ANS(JJ),JJ=1,8),(ABS_ANS(JJ),JJ=1,8)

      ELSE IF (WHICH == 'ROD'  ) THEN
         WRITE(ANS,1312) (FILL(1:16), EID_OUT_ARRAY(II,1), OGEL(II,7), OGEL(II,8),II=1,NUM)
         WRITE(ANS,1313) (MAX_ANS(JJ),JJ=7,8),(MIN_ANS(JJ),JJ=7,8),(ABS_ANS(JJ),JJ=7,8)

      ELSE IF (WHICH == 'SHEAR') THEN
         DO II=1,NUM
            WRITE(ANS,1412) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,3)
         ENDDO   
         WRITE(ANS,1413) (MAX_ANS(JJ),JJ=1,8),(MIN_ANS(JJ),JJ=1,8),(ABS_ANS(JJ),JJ=1,3)

      ELSE IF (WHICH == 'SHELL') THEN
         DO II=1,NUM
            WRITE(ANS,1512) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,NUM_TERMS)
         ENDDO   
         WRITE(ANS,1513) (MAX_ANS(JJ),JJ=1,8),(MIN_ANS(JJ),JJ=1,8),(ABS_ANS(JJ),JJ=1,NUM_TERMS)

      ELSE IF (WHICH == 'BUSH' ) THEN
         DO II=1,NUM
            WRITE(ANS,1612) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,6)
         ENDDO   
         WRITE(ANS,1613) (MAX_ANS(JJ),JJ=1,6),(MIN_ANS(JJ),JJ=1,6),(ABS_ANS(JJ),JJ=1,6)

      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' OUTPUT FOR SUBCASE ',I8)

  102 FORMAT(' OUTPUT FOR EIGENVECTOR ',I8)

  103 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (boundary ',A,' for grid',I8,' component',I2,')')

  104 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (modal acceleration for mode ',I8,')')

  301 FORMAT(39X,A,'E L E M E N T   E N G I N E E R I N G   F O R C E S')

  302 FORMAT(33X,A,'C B   E L E M E N T   E N G I N E E R I N G   F O R C E   O T M')

  401 FORMAT(44X,A,'F O R   E L E M E N T   T Y P E   ',A11)

! ELAS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1111 FORMAT(2X,A,'Element     Force'                                                                                              &
          ,/,2X,A,'   ID')
 
 1112 FORMAT(A,5(I8,1ES14.6))
  
 1113 FORMAT(11X,'              -------------',/,                                                                                  &
             1X,'MAX (for output set):  ',1(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',1(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',1(ES14.6))

! BAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1211 FORMAT(2X,A,'Element       Bend-Moment End A           Bend-Moment End B              - Shear -              Axial'          &
          ,'         Torque'  &
          ,/,2X,A,'   ID       Plane 1       Plane 2       Plane 1       Plane 2      Plane 1       Plane 2        Force')
 
 1212 FORMAT(A,I8,8(1ES14.6))

 1213 FORMAT(11X,'              ------------- ------------- ------------- ------------- ------------- ------------- -------------',&
                             ' -------------',/,                                                                                   &
             1X,'MAX (for output set):  ',8(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',8(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',8(ES14.6))

! ROD >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1311 FORMAT(2X,A,'Element     Axial        Torque'                                                                                &
          ,/,2X,A,'   ID       Force')
 
 1312 FORMAT(A,I8,1ES14.6,1ES14.6)
 
 1313 FORMAT(11X,'              ------------- -------------',/,                                                                    &
             1X,'MAX (for output set):  ',2(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',2(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',2(ES14.6))

! SHEAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1411 FORMAT(1X,A,'                                        Element        N o r m a l   F o r c e s'                               &
          ,/,1X,A,'                                           ID        Nxx           Nyy           Nxy')
 
 1412 FORMAT(A,40X,I8,3(1ES14.6))
 
 1413 FORMAT(1X,A,'                                                ------------- ------------- -------------',/,                   &
             1X,A,'                                        MAX* : ',3(ES14.6),/,                                                   &
             1X,A,'                                        MIN* : ',3(ES14.6),//,                                                  &
             1X,A,'                                        ABS* : ',3(ES14.6),/,                                                   &
             1X,A,'                                        *for output set')

! SHELL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1511 FORMAT(2X,A,'Element        N o r m a l   F o r c e s                       M o m e n t s'                                   &
          ,19X,'T r a n s v e r s e',/,A,95x,'S h e a r   F o r c e s'                                                             &
          ,/,2X,A,'   ID       Nxx           Nyy           Nxy           Mxx           Myy           Mxy            Qx          '  &
          ,'  Qy')

 1512 FORMAT(A,I8,8(1ES14.6))
 
 1513 FORMAT(11X,'              ------------- ------------- ------------- ------------- ------------- ------------- -------------',&
                             ' -------------',/,                                                                                   &
             1X,'MAX (for output set):  ',8(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',8(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',8(ES14.6))

! BUSH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1611 FORMAT(1X,A,' Element      Force         Force         Force         Force         Force         Force' &
          ,/,1X,A,'    ID         TX            TY            TZ            RX            RY            RZ')
 
 1612 FORMAT(A,I8,6(1ES14.6))
  
 1613 FORMAT(1X,'                        ------------- ------------- ------------- ------------- ------------- ------------- ',/,  &
             1X,'MAX (for output set):  ',6(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',6(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',6(ES14.6))

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_ANS

! ##################################################################################################################################

      SUBROUTINE GET_MAX_MIN_ABS ( BEG_COL, END_COL )

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: BEG_COL           ! Col number in OGEL where to beg for averaging to get max, min, abs
      INTEGER(LONG), INTENT(IN)       :: END_COL           ! Col number in OGEL where to end for averaging to get max, min, abs
      INTEGER(LONG)                   :: II,JJ             ! DO loop indices or counters

! **********************************************************************************************************************************
      ! Get MAX, MIN, ABS values
      DO JJ=BEG_COL,END_COL
         MAX_ANS(JJ) = -MACH_LARGE_NUM
      ENDDO 

      DO II=1,NUM
         DO JJ=BEG_COL,END_COL
            IF (OGEL(II,JJ) > MAX_ANS(JJ)) THEN
               MAX_ANS(JJ) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO JJ=BEG_COL,END_COL
         MIN_ANS(JJ) = MAX_ANS(JJ)
      ENDDO

      DO II=1,NUM
         DO JJ=BEG_COL,END_COL
            IF (OGEL(II,JJ) < MIN_ANS(JJ)) THEN
               MIN_ANS(JJ) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO II=BEG_COL,END_COL
         ABS_ANS(II) = MAX( DABS(MAX_ANS(II)), DABS(MIN_ANS(II)) )
      ENDDO

      END SUBROUTINE GET_MAX_MIN_ABS

      END SUBROUTINE WRITE_ELEM_ENGR_FORCE
