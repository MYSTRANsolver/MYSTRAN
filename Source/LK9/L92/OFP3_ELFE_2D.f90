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
 
      SUBROUTINE OFP3_ELFE_2D ( JVEC, FEMAP_SET_ID, ITE, OT4_EROW )

! Processes element engr force output requests for 2D (TRIA3, QUAD4, SHEAR) elements for one subcase. Results go into array OGEL
! for later output in LINK9
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELOUT_ELFE_BIT, FATAL_ERR, IBIT, INT_SC_NUM, MBUG, MOGEL,                   &
                                         WARN_ERR, NELE, NCQUAD4, NCQUAD4K, NCSHEAR, NCTRIA3, NCTRIA3K, SOL_NAME                   
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  OFP3_ELFE_2D_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_NUMS, FEMAP_EL_VECS
      USE PARAMS, ONLY                :  OTMSKIP, POST
      use model_stuf, only            :  pcomp_props
      USE MODEL_STUF, ONLY            :  ANY_ELFE_OUTPUT, EDAT, EPNT, ETYPE, FCONV, EID, ELMTYP, ELOUT, METYPE, NUM_EMG_FATAL_ERRS,&
                                         PLY_NUM, TYPE, STRESS
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, MAXREQ, OGEL
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ELFE, TXT_ELFE
  
      USE OFP3_ELFE_2D_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'OFP3_ELFE_2D'
      CHARACTER( 1*BYTE), PARAMETER   :: IHDR      = 'Y'   ! An input to subr WRITE_GRID_OUTPUTS, called herein
      CHARACTER(20*BYTE)              :: FORCE_ITEM(8)     ! Char description of element engineering forces
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option indicators for subr EMG, called herein
      CHARACTER(31*BYTE)              :: OT4_DESCRIPTOR    ! Descriptor for rows of OT4 file
      CHARACTER(30*BYTE)              :: REQUEST           ! Text for error message
 
      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! Set ID for FEMAP output
      INTEGER(LONG), INTENT(IN)       :: ITE               ! Unit number for text files for OTM row descriptors 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
     !INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! the op2 subtable number, should be -3, -5, ...
      INTEGER(LONG), INTENT(INOUT)    :: OT4_EROW          ! Row number in OT4 file for elem related OTM descriptors
      INTEGER(LONG)                   :: ELOUT_ELFE        ! If > 0, there are ELFORCE(ENGR) requests for some elems                
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IERROR       = 0  ! Local error count
!xx   INTEGER(LONG)                   :: IROW_MAT          ! Row number in OTM's
!xx   INTEGER(LONG)                   :: IROW_TXT          ! Row number in OTM text file
      INTEGER(LONG)                   :: NELREQ(METYPE)    ! Count of the no. of requests for ELFORCE(NODE or ENGR) or STRESS
      INTEGER(LONG)                   :: NUM_ELEM          ! No. elems processed prior to writing results to F06 file
      INTEGER(LONG)                   :: NUM_FROWS         ! No. elems processed for FEMAP
      INTEGER(LONG)                   :: NUM_OGEL          ! No. rows written to array OGEL prior to writing results to F06 file
!                                                            (this can be > NUM_ELEM since more than 1 row is written to OGEL
!                                                            for ELFORCE(NODE) - elem nodal forces)
                                                           ! Indicator for output of elem data to BUG file
      integer(long)                   :: num_pcomp_elems   ! number of elements that are composites (used to prevent output of engr
!                                                            forces for PCOMP elems until I fix that output)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OFP3_ELFE_2D_BEGEND
 
      ! OP2 parameters
      INTEGER(LONG)                   :: ITABLE            ! the op2 subtable number
      CHARACTER(8*BYTE)               :: TABLE_NAME        ! the op2 table name

      INTRINSIC IAND
  
! **********************************************************************************************************************************
!     Initialize
      TABLE_NAME = "OEF ERR "
      ITABLE = 0
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process element engineering force requests for plate and USERIN elements.
! For the MIN3,4 elements, the stiffness matrix has to be generated to get FCONV(3). Therefore, set OPT(4) to 'N' initially, and if
! the element being processed is a MIN3 or MIN4, reset OPT(4) to 'Y' in the calculation loop prior to EMG call.
 
      OPT(1) = 'N'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'N'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'Y'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'N'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff
 
      FORCE_ITEM(1) = 'Nxx: Normal x Force '
      FORCE_ITEM(2) = 'Nyy: Normal y Force '
      FORCE_ITEM(3) = 'Nxy: Shear xy Force '
      FORCE_ITEM(4) = 'Mxx: Moment x Plane '
      FORCE_ITEM(5) = 'Myy: Moment y Plane '
      FORCE_ITEM(6) = 'Mxy: Twist Mom xy   ' 
      FORCE_ITEM(7) = 'Qx : Transv Shear x '
      FORCE_ITEM(8) = 'Qy : Transv Shear y '

! Find out how many output requests were made for each element type.

      DO I=1,METYPE                                        ! Initialize the array containing the number of requests per element
         NELREQ(I) = 0
      ENDDO 
 
      num_pcomp_elems = 0                                  ! Remove lower case code when I fix engr force output for PCOMP's
      DO I=1,METYPE
         DO J=1,NELE
            IF((ETYPE(J)(1:5) == 'TRIA3') .OR. (ETYPE(J)(1:5) == 'QUAD4') .OR. (ETYPE(J)(1:5) == 'SHEAR') .OR.                     &
               (ETYPE(J)(1:6) == 'USERIN')) THEN
               IF (ETYPE(J) == ELMTYP(I)) THEN
                  call is_elem_pcomp_props ( j )
                  if (pcomp_props == 'N') then
                     ELOUT_ELFE = IAND(ELOUT(J,INT_SC_NUM),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        NELREQ(I) = NELREQ(I) + 1
                     ENDIF
                  else
                     num_pcomp_elems = num_pcomp_elems + 1
                  endif
               ENDIF
            ENDIF
         ENDDO 
      ENDDO 

!xx   DO I=1,METYPE                                        ! Engr force requests for ELAS elems not honored. Give message
!xx      IF ((ELMTYP(I)(1:4) == 'ELAS') .AND. (NELREQ(I) > 0)) THEN
!xx         WARN_ERR = WARN_ERR + 1
!xx         WRITE(ERR,9204) NELREQ(I), ELMTYP(I)
!xx         IF (SUPWARN == 'N') THEN
!xx            WRITE(F06,9204) NELREQ(I), ELMTYP(I)
!xx         ENDIF
!xx      ENDIF
!xx   ENDDO  
   
      DO I=1,MAXREQ
         DO J=1,MOGEL
            OGEL(I,J) = ZERO
         ENDDO 
      ENDDO   
 
!xx   IROW_MAT = 0
!xx   IROW_TXT = 0
      OT4_DESCRIPTOR = 'Element engineering force'
reqs3:DO I=1,METYPE
         IF (NELREQ(I) == 0) CYCLE reqs3
         NUM_ELEM = 0
         NUM_OGEL = 0
 
elems_3: DO J = 1,NELE
            call is_elem_pcomp_props ( j )                 ! Remove lower case code when I fix engr force output for PCOMP's
            if (pcomp_props == 'N') then
               EID   = EDAT(EPNT(J))
               TYPE  = ETYPE(J)
               IF((ETYPE(J)(1:5) == 'TRIA3') .OR. (ETYPE(J)(1:5) == 'QUAD4') .OR. (ETYPE(J)(1:5) == 'SHEAR') .OR.                  &
                  (ETYPE(J)(1:6) == 'USERIN')) THEN

                  IF (ETYPE(J) == ELMTYP(I)) THEN
                     ELOUT_ELFE = IAND(ELOUT(J,INT_SC_NUM),IBIT(ELOUT_ELFE_BIT))
                     IF (ELOUT_ELFE > 0) THEN
                        IF((ETYPE(J)(1:5) == 'TRIA3') .OR. (ETYPE(J)(1:5) == 'QUAD4') .OR. (ETYPE(J)(1:5) == 'SHEAR')) THEN
                           OPT(4) = 'Y'
                        ENDIF
                        DO K=0,MBUG-1
                           WRT_BUG(K) = 0
                        ENDDO
                        PLY_NUM = 0                        ! 'N' in call to EMG means do not write to BUG file
                        CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) 
                        IF (NUM_EMG_FATAL_ERRS > 0) THEN
                           IERROR = IERROR + 1
                           CYCLE elems_3
                        ENDIF
                        OPT(4) = 'N'
                        CALL ELMDIS
                        CALL ELEM_STRE_STRN_ARRAYS ( 1 )
                        CALL SHELL_ENGR_FORCE_OGEL ( NUM_OGEL )
 
                        IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                           DO K=1,8
                              OT4_EROW = OT4_EROW + 1
                              OTM_ELFE(OT4_EROW,JVEC) = OGEL(NUM_OGEL,K)
                              IF (JVEC == 1) THEN
                                 WRITE(TXT_ELFE(OT4_EROW), 9192) OT4_EROW, OT4_DESCRIPTOR, TYPE, EID, FORCE_ITEM(K)
                              ENDIF
                           ENDDO
                        ENDIF

                        IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (JVEC == 1) .AND. (OT4_EROW >= 1)) THEN
                           DO K=1,OTMSKIP                     ! Write OTMSKIP blank separator lines
                              OT4_EROW = OT4_EROW + 1
                              WRITE(TXT_ELFE(OT4_EROW), 9199)
                           ENDDO
                        ENDIF

                        NUM_ELEM = NUM_ELEM + 1
                        EID_OUT_ARRAY(NUM_ELEM,1) = EID
                        IF (NUM_ELEM == NELREQ(I)) THEN
                           CALL CHK_OGEL_ZEROS ( NUM_OGEL )

 100                       FORMAT("*DEBUG:      ",A,"; ELEMENT_TYPE=",A,"; TABLE_NAME=",A,"; ITABLE=",I8)
                           WRITE(ERR,100) "F3",ETYPE(J),TABLE_NAME,ITABLE
                           CALL SET_OEF_TABLE_NAME(ETYPE(J), TABLE_NAME, ITABLE)
                           WRITE(ERR,100) "F4",ETYPE(J),TABLE_NAME,ITABLE
                           CALL WRITE_ELEM_ENGR_FORCE ( JVEC, NUM_ELEM, IHDR, ITABLE )
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF 
         ENDDO elems_3
      ENDDO reqs3

 10   FORMAT("*DEBUG:      OEF_END 2D:    TABLE_NAME",A)
      WRITE(ERR,10) TABLE_NAME
      IF ((TABLE_NAME .NE. "OEF ERR ") .AND. (ITABLE < 0)) THEN
        CALL END_OP2_TABLE(ITABLE)
      ENDIF

      IF ((POST /= 0) .AND. (ANY_ELFE_OUTPUT > 0)) THEN

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCTRIA3K, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out TRIA3K engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:6) == 'TRIA3K') THEN
               NUM_FROWS= NUM_FROWS+ 1
               OPT(4) = 'Y'
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               OPT(4) = 'N'
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               FEMAP_EL_VECS(NUM_FROWS,1) = FCONV(1)*STRESS(1)       ! X  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,2) = FCONV(1)*STRESS(2)       ! Y  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,3) = FCONV(1)*STRESS(3)       ! XY Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,4) = FCONV(2)*STRESS(4)       ! X  Moment
               FEMAP_EL_VECS(NUM_FROWS,5) = FCONV(2)*STRESS(5)       ! Y  Moment
               FEMAP_EL_VECS(NUM_FROWS,6) = FCONV(2)*STRESS(6)       ! XY Moment
               FEMAP_EL_VECS(NUM_FROWS,7) = FCONV(3)*STRESS(7)       ! X  Transverse Shear
               FEMAP_EL_VECS(NUM_FROWS,8) = FCONV(3)*STRESS(8)       ! Y  Transverse Shear
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'TRIA3K  ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCTRIA3, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out TRIA3 engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:6) == 'TRIA3 ') THEN
               NUM_FROWS= NUM_FROWS+ 1
               OPT(4) = 'Y'
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               OPT(4) = 'N'
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               FEMAP_EL_VECS(NUM_FROWS,1) = FCONV(1)*STRESS(1)       ! X  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,2) = FCONV(1)*STRESS(2)       ! Y  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,3) = FCONV(1)*STRESS(3)       ! XY Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,4) = FCONV(2)*STRESS(4)       ! X  Moment
               FEMAP_EL_VECS(NUM_FROWS,5) = FCONV(2)*STRESS(5)       ! Y  Moment
               FEMAP_EL_VECS(NUM_FROWS,6) = FCONV(2)*STRESS(6)       ! XY Moment
               FEMAP_EL_VECS(NUM_FROWS,7) = FCONV(3)*STRESS(7)       ! X  Transverse Shear
               FEMAP_EL_VECS(NUM_FROWS,8) = FCONV(3)*STRESS(8)       ! Y  Transverse Shear
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'TRIA3   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCQUAD4K, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out QUAD4K engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:6) == 'QUAD4K') THEN
               NUM_FROWS= NUM_FROWS+ 1
               OPT(4) = 'Y'
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               OPT(4) = 'N'
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               FEMAP_EL_VECS(NUM_FROWS,1) = FCONV(1)*STRESS(1)       ! X  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,2) = FCONV(1)*STRESS(2)       ! Y  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,3) = FCONV(1)*STRESS(3)       ! XY Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,4) = FCONV(2)*STRESS(4)       ! X  Moment
               FEMAP_EL_VECS(NUM_FROWS,5) = FCONV(2)*STRESS(5)       ! Y  Moment
               FEMAP_EL_VECS(NUM_FROWS,6) = FCONV(2)*STRESS(6)       ! XY Moment
               FEMAP_EL_VECS(NUM_FROWS,7) = FCONV(3)*STRESS(7)       ! X  Transverse Shear
               FEMAP_EL_VECS(NUM_FROWS,8) = FCONV(3)*STRESS(8)       ! Y  Transverse Shear
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'QUAD4K  ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCQUAD4, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out QUAD4 engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:6) == 'QUAD4 ') THEN
               NUM_FROWS= NUM_FROWS+ 1
               OPT(4) = 'Y'
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               OPT(4) = 'N'
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               FEMAP_EL_VECS(NUM_FROWS,1) = FCONV(1)*STRESS(1)       ! X  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,2) = FCONV(1)*STRESS(2)       ! Y  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,3) = FCONV(1)*STRESS(3)       ! XY Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,4) = FCONV(2)*STRESS(4)       ! X  Moment
               FEMAP_EL_VECS(NUM_FROWS,5) = FCONV(2)*STRESS(5)       ! Y  Moment
               FEMAP_EL_VECS(NUM_FROWS,6) = FCONV(2)*STRESS(6)       ! XY Moment
               FEMAP_EL_VECS(NUM_FROWS,7) = FCONV(3)*STRESS(7)       ! X  Transverse Shear
               FEMAP_EL_VECS(NUM_FROWS,8) = FCONV(3)*STRESS(8)       ! Y  Transverse Shear
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'QUAD4   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCSHEAR, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out SHEAR engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:6) == 'SHEAR') THEN
               NUM_FROWS= NUM_FROWS+ 1
               OPT(4) = 'Y'
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               OPT(4) = 'N'
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               FEMAP_EL_VECS(NUM_FROWS,1) = FCONV(1)*STRESS(1)       ! X  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,2) = FCONV(1)*STRESS(2)       ! Y  Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,3) = FCONV(1)*STRESS(3)       ! XY Membrane Force
               FEMAP_EL_VECS(NUM_FROWS,4) = FCONV(2)*STRESS(4)       ! X  Moment
               FEMAP_EL_VECS(NUM_FROWS,5) = FCONV(2)*STRESS(5)       ! Y  Moment
               FEMAP_EL_VECS(NUM_FROWS,6) = FCONV(2)*STRESS(6)       ! XY Moment
               FEMAP_EL_VECS(NUM_FROWS,7) = FCONV(3)*STRESS(7)       ! X  Transverse Shear
               FEMAP_EL_VECS(NUM_FROWS,8) = FCONV(3)*STRESS(8)       ! Y  Transverse Shear
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'SHEAR   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

      ENDIF

      IF (IERROR > 0) THEN
         REQUEST = 'ELEMENT ENGINEERING FORCE'
         WRITE(ERR,9201) TYPE, REQUEST, EID
         WRITE(F06,9201) TYPE, REQUEST, EID
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9192 FORMAT(I8,1X,A,A8,I8,4X,A20)

 9199 FORMAT(' ')

 9201 FORMAT(' *ERROR  9201: DUE TO ABOVE LISTED ERRORS, CANNOT CALCULATE ',A,' REQUESTS FOR ',A,' ELEMENT ID = ',I8)

 9204 FORMAT(' *WARNING    : ELEMENT ENGINEERING FORCE OUTPUT REQUESTS FOR ',I8,1X,A,' NOT ALLOWED'                                &
                    ,/,14X,' REQUEST STRESS OUTPUT IN CASE CONTROL FOR THESE ELEMENTS')


! **********************************************************************************************************************************

      END SUBROUTINE OFP3_ELFE_2D
