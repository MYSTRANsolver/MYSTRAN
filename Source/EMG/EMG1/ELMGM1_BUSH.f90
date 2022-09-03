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
 
      SUBROUTINE ELMGM1_BUSH ( INT_ELEM_ID, WRITE_WARN )
 
! Calculates and checks some elem geometry for ROD, BAR, BEAM, triangles and provides a transformation matrix (TE) to transform the
! element stiffness matrix in the element system to the basic coordinate system. Calculates grid point coords in local coord system.
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELGP, MOFFSET, NCORD, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMGM1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  BGRID, BUSH_CID, BUSH_OCID, BUSH_VVEC, CAN_ELEM_TYPE_OFFSET, CORD, EID, ELEM_LEN_12,      &
                                         ELEM_LEN_AB, ELGP, EOFF, GRID, NUM_EMG_FATAL_ERRS, OFFDIS, OFFDIS_L, OFFDIS_O, OFFDIS_B,  &
                                         OFFDIS_G, OFFDIS_GA_GB, RCORD, TE, TE_GA_GB, TE_IDENT, TYPE, XEB, XEL
 
      USE ELMGM1_BUSH_USE_IFs

      IMPLICIT NONE
 
      CHARACTER( 1*BYTE)              :: ID(3)              ! Used in deciding whether TE_IDENT = 'Y' or 'N'
      CHARACTER( 5*BYTE)              :: SORT_ORDER         ! Order in which the VX(i) have been sorted in subr CALC_VEC_SORT_ORDER
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMGM1_BUSH'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN         ! If 'Y" write warning messages, otherwise do not
      CHARACTER(1*BYTE)               :: CORD_FND           ! = 'Y' if coord sys ID on CONM2 defined, 'N' otherwise
      CHARACTER(1*BYTE)               :: DO_IT              ! = 'Y' execute code that follows (see where DO_IT is initialized)

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      INTEGER(LONG)                   :: ACID_G             ! Actual coordinate system ID
      INTEGER(LONG)                   :: I,J,K              ! DO loop indices
      INTEGER(LONG)                   :: I3_IN(3)           ! Integer array used in sorting VX. 

      INTEGER(LONG)                   :: I3_OUT(3)          ! Integer array giving order of VX comps. If VX is in the order with
!                                                             comp 2 smallest then comp 3 then comp 1 then I3_OUT is 2, 3, 1 

      INTEGER(LONG)                   :: ICID               ! Internal coord sys no. corresponding to an actual coord sys no. 
      INTEGER(LONG)                   :: ROWNUM             ! A row number in an array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMGM1_BEGEND
  
      REAL(DOUBLE)                    :: DX1(3)             ! Array used in intermediate calc's
      REAL(DOUBLE)                    :: DX2(3)             ! Array used in intermediate calc's
      REAL(DOUBLE)                    :: EPS1               ! A small number to compare to real zero
      REAL(DOUBLE)                    :: LX(3)              ! Distances
      REAL(DOUBLE)                    :: MAGY               ! Magnitude of vector VY
      REAL(DOUBLE)                    :: MAGZ               ! Magnitude of vector VZ
      REAL(DOUBLE)                    :: PHID, THETAD       ! Outputs from subr GEN_T0L
      REAL(DOUBLE)                    :: TET_GA_GB(3,3)     ! Transpose of TE_GA_GB
      REAL(DOUBLE)                    :: T0G(3,3)           ! Matrix to transform offsets from global to basic  coords 
      REAL(DOUBLE)                    :: TG0(3,3)           ! Matrix to transform offsets from basic  to global coords 
      REAL(DOUBLE)                    :: T0I(3,3)           ! Matrix to transform offsets from BUSH OCID to basic coords 
      REAL(DOUBLE)                    :: TET(3,3)           ! Transpose of TE: UEL = TE*UEB 
      REAL(DOUBLE)                    :: VX(3)              ! A vector in the elem x dir
      REAL(DOUBLE)                    :: VY(3)              ! A vector in the elem y dir
      REAL(DOUBLE)                    :: VZ(3)              ! A vector in the elem z dir
      REAL(DOUBLE)                    :: V13(3)             ! A vector from grid 1 to grid 3 (for BAR, BEAM or USER1 it is V vector)
      REAL(DOUBLE)                    :: XIB(2,3)           ! Coords at ends of BUSH elem: XIB(1,J) should = XIB(2,J) for 0 len elem

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME, TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)
 
! Initialize
  
      DO I=1,MELGP
        DO J=1,3
           XEL(I,J) = ZERO
        ENDDO 
      ENDDO 
 
      DO I=1,3
        DO J=1,3
           TE(I,J) = ZERO
        ENDDO 
      ENDDO 
 
      DO I=1,3
         VX(I) = ZERO
      ENDDO

! ----------------------------------------------------------------------------------------------------------------------------------
! Make sure the number of elem grids is not larger than OFFDIS is dimensioned for

      IF ((CAN_ELEM_TYPE_OFFSET == 'Y') .AND. (ELGP > MOFFSET)) THEN
         WRITE(ERR,1954) SUBR_NAME, MOFFSET, ELGP, TYPE
         WRITE(F06,1954) SUBR_NAME, MOFFSET, ELGP, TYPE
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Calculate the TE coord transformation matrix

      IF (ELEM_LEN_12 < .0001) THEN
         IF (BUSH_CID < 0) THEN                               ! Elem len < .0001 so CID must be > 0
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1801) TRIM(TYPE), EID, '0.0001'
            WRITE(F06,1801) TRIM(TYPE), EID, '0.0001'
         ENDIF
      ENDIF

bcid: IF (BUSH_CID > 0) THEN                               ! Get transformation of BUSH_CID to basic and put it into TE

         CORD_FND = 'N'
         ICID = 0
         DO J=1,NCORD
            IF (BUSH_CID == CORD(J,2)) THEN
               CORD_FND = 'Y'
               ICID = J
               EXIT
            ENDIF
         ENDDO   

         IF (CORD_FND == 'Y') THEN                         ! NOTE: This BUSH TE transforms a vector in BUSH_CID to basic
            TET(1,1) = RCORD(ICID, 4)   ;   TET(1,2) = RCORD(ICID, 5)   ;   TET(1,3) = RCORD(ICID, 6)
            TET(2,1) = RCORD(ICID, 7)   ;   TET(2,2) = RCORD(ICID, 8)   ;   TET(2,3) = RCORD(ICID, 9)
            TET(3,1) = RCORD(ICID,10)   ;   TET(3,2) = RCORD(ICID,11)   ;   TET(3,3) = RCORD(ICID,12)
            DO I=1,3
               DO J=1,3
                  TE(I,J) = TET(J,I)
               ENDDO
            ENDDO
            write(f06,*)
         ELSE                                              ! Error, could not find BUSH_CID coord system in RCORD
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1822) 'COORD SYSTEM ', BUSH_CID, TYPE, EID
            WRITE(F06,1822) 'COORD SYSTEM ', BUSH_CID, TYPE, EID
         ENDIF

      ELSE IF (BUSH_CID == 0) THEN                         ! BUSH_CID is basic so TE is the identity matrix

         DO I=1,3
            DO J=1,3
               TE(I,J) = ZERO
            ENDDO
            TE(I,I) = ONE
         ENDDO

      ELSE IF (BUSH_CID < 0) THEN                          ! This means no BUSH_CID was found so we look for the v vector

         IF (BUSH_VVEC /= 0) THEN                          ! A v-vector was specified for this bUSH element

            VX(1) = XEB(2,1) - XEB(1,1)                    ! When no BUSH_CID the x axis is along line between the 2 grids
            VX(2) = XEB(2,2) - XEB(1,2)
            VX(3) = XEB(2,3) - XEB(1,3)
            DO I=1,3
               TE(1,I) = VX(I)/ELEM_LEN_12                 ! For BUSH use length bet 2 grids since length bet elem ends is zero
            ENDDO
            DO I=1,3  
               V13(I) = XEB(ELGP+1,I) - XEB(1,I)
            ENDDO 
            CALL CROSS ( VX, V13, VZ )
            MAGZ = DSQRT(VZ(1)*VZ(1) + VZ(2)*VZ(2) + VZ(3)*VZ(3))
            IF (MAGZ <=  EPS1) THEN
               WRITE(ERR,1906) TYPE, EID
               WRITE(F06,1906) TYPE, EID
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               FATAL_ERR = FATAL_ERR + 1
               RETURN
            ENDIF
            DO I=1,3
               TE(3,I) = VZ(I)/MAGZ
            ENDDO

            CALL CROSS ( VZ, VX, VY )                      ! Calc unit vector in Ye dir.from VZ (cross) VX: If MAGY = 0 quit
            MAGY = DSQRT(VY(1)*VY(1) + VY(2)*VY(2) + VY(3)*VY(3))
            IF(MAGY <= EPS1) THEN
               WRITE(ERR,1912) EID,TYPE
               WRITE(F06,1912) EID,TYPE
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               FATAL_ERR = FATAL_ERR + 1
               RETURN
            ENDIF
            DO I=1,3
               TE(2,I) = VY(I)/MAGY
            ENDDO 

         ELSE IF (BUSH_VVEC == 0) THEN                     ! No v-vec or CID specified for BUSH elem so elem x axis is GA->GB 
!                                                                  --
            VX(1) = XEB(2,1) - XEB(1,1)
            VX(2) = XEB(2,2) - XEB(1,2)
            VX(3) = XEB(2,3) - XEB(1,3)

            DO I=1,3
               TE(1,I) = VX(I)/ELEM_LEN_12
            ENDDO

            DO I=1,3            
               I3_IN(I)   = I
               I3_OUT(I)  = I3_IN(I)
            ENDDO
            CALL CALC_VEC_SORT_ORDER (VX,SORT_ORDER,I3_OUT)! Use this rather than SORT_INT1_REAL1 - didn't work for vec 10.,0.,0.
            IF (SORT_ORDER == '     ') THEN                ! Subr CALC_VEC_SORT_ORDER did not find a sort order
               FATAL_ERR = FATAL_ERR + 1
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               WRITE(ERR,1944) SUBR_NAME, TYPE, EID
               WRITE(F06,1944) SUBR_NAME, TYPE, EID
               RETURN
            ENDIF
                                                           ! See notes on "Some Basic Vector Operations In IDL" on web site:
                                                           ! http://fermi.jhuapl.edu/s1r/idl/s1rlib/vectors/v_basic.html
            VY(I3_OUT(1)) =  ZERO                          !  (a) Component of VY in direction of min VX is set to zero
            VY(I3_OUT(2)) =  VX(I3_OUT(3))                 !  (b) Other 2 VY(i) are corresponding VX(i) switched with one x(-1)
            VY(I3_OUT(3)) = -VX(I3_OUT(2))
            MAGY  = DSQRT(VY(1)*VY(1) + VY(2)*VY(2) + VY(3)*VY(3))


            IF (DABS(MAGY) < EPS1) THEN
               FATAL_ERR = FATAL_ERR + 1
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               WRITE(ERR,1938) SUBR_NAME, 'Y', TYPE, EID, (VY(I),I=1,3)
               WRITE(F06,1938) SUBR_NAME, 'Y', TYPE, EID, (VY(I),I=1,3)
               RETURN
            ENDIF

            DO I=1,3
               TE(2,I) = VY(I)/MAGY
            ENDDO

            CALL CROSS ( VX, VY, VZ )

            MAGZ  = DSQRT(VZ(1)*VZ(1) + VZ(2)*VZ(2) + VZ(3)*VZ(3))

            IF (DABS(MAGZ) < EPS1) THEN
               FATAL_ERR = FATAL_ERR + 1
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               WRITE(ERR,1938) SUBR_NAME, 'Z', TYPE, EID, (VZ(I),I=1,3)
               WRITE(F06,1938) SUBR_NAME, 'Z', TYPE, EID, (VZ(I),I=1,3)
               RETURN
            ENDIF

            DO I=1,3
               TE(3,I) = VZ(I)/MAGZ
            ENDDO

         ENDIF

      ENDIF bcid

      TE_IDENT = 'N'                                       ! Now set TE_IDENT to be 'Y' if TE is an identity matrix.
      DO I=1,3
         ID(I) = 'N'
      ENDDO
      DO I=1,3
         IF (DABS(TE(I,I) - ONE) < EPS1) THEN
            ID(I) = 'Y'
         ELSE
            ID(I) = 'N'
         ENDIF
      ENDDO 
      IF ((ID(1) == 'Y') .AND. (ID(2) == 'Y') .AND. (ID(3) == 'Y')) THEN
         TE_IDENT = 'Y'
      ENDIF


! ----------------------------------------------------------------------------------------------------------------------------------
! Use TE to get array of elem coords in local system.
 
      XEL(1,1) = ZERO
      XEL(1,2) = ZERO
      XEL(1,3) = ZERO
  
      DO I=2,ELGP
         DO J=1,3
            XEL(I,J) = ZERO
            DO K=1,3
               XEL(I,J) = XEL(I,J) + (XEB(I,K) - XEB(1,K))*TE(J,K)
            ENDDO 
         ENDDO 
      ENDDO 
  
      DO I=1,3
         DO J=1,3
            TET(I,J) = TE(J,I)
         ENDDO
      ENDDO

! ----------------------------------------------------------------------------------------------------------------------------------
! Use TE to get array of elem coords in local system.
 
      XEL(1,1) = ZERO
      XEL(1,2) = ZERO
      XEL(1,3) = ZERO
  
      DO I=2,ELGP
         DO J=1,3
            XEL(I,J) = ZERO
            DO K=1,3
               XEL(I,J) = XEL(I,J) + (XEB(I,K) - XEB(1,K))*TE(J,K)
            ENDDO 
         ENDDO 
      ENDDO 
  
! ----------------------------------------------------------------------------------------------------------------------------------
! Calculate a coord transformation matrix, TE_GA_GB, that will transform a vector whose x axis is along the GA-GB axis to basic:
! This will be used for only the x axis when the BUSH offset is along the line GA-GB in order to specify the offsets whenever
! BUSH_OCID is -1 (default) or blank. The other y and z aves of TE_GA_GB don't matter except thet they be normal to x. So use the
! procedure outlined in "Some Basic Vector Operations In IDL" (below) to find the y and z axes of TE_GA_GB 
! This needs to be done if GA and GB are not coincident but also this coord transformation is used in calculating element forces
! from nodal loads (i.e. in subr OFP3_ELFE_1D)

      IF (ELEM_LEN_12 > .0001D0) THEN

         VX(1) = XEB(2,1) - XEB(1,1)                          ! Unit vector in element X direction 
         VX(2) = XEB(2,2) - XEB(1,2)
         VX(3) = XEB(2,3) - XEB(1,3)

         DO I=1,3
            TE_GA_GB(1,I) = VX(I)/ELEM_LEN_12                 ! Row 1 of TE_GA_GB
         ENDDO                                                ! -----------------

         DO I=1,3
            I3_IN(I)   = I
            I3_OUT(I)  = I3_IN(I)
         ENDDO
         CALL CALC_VEC_SORT_ORDER ( VX, SORT_ORDER, I3_OUT)   ! Use this rather than SORT_INT1_REAL1 - didn't work for vec 10., 0., 0.
         IF (SORT_ORDER == '     ') THEN                      ! Subr CALC_VEC_SORT_ORDER did not find a sort order so fatal error
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1944) SUBR_NAME, TYPE, EID
            WRITE(F06,1944) SUBR_NAME, TYPE, EID
            RETURN
         ENDIF
                                                              ! See notes on "Some Basic Vector Operations In IDL" on web site:
                                                              ! http://fermi.jhuapl.edu/s1r/idl/s1rlib/vectors/v_basic.html
         VY(I3_OUT(1)) =  ZERO                                !  (a) Component of VY in direction of min VX is set to zero
         VY(I3_OUT(2)) =  VX(I3_OUT(3))                       !  (b) Other 2 VY(i) are corresponding VX(i) switched with one x(-1)
         VY(I3_OUT(3)) = -VX(I3_OUT(2))
         MAGY  = DSQRT(VY(1)*VY(1) + VY(2)*VY(2) + VY(3)*VY(3))


         IF (DABS(MAGY) < EPS1) THEN
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1938) SUBR_NAME, 'Y', TYPE, EID, (VY(I),I=1,3)
            WRITE(F06,1938) SUBR_NAME, 'Y', TYPE, EID, (VY(I),I=1,3)
            RETURN
         ENDIF

         DO I=1,3
            TE_GA_GB(2,I) = VY(I)/MAGY                        ! Row 2 of TE_GA_GB
         ENDDO                                                ! -----------------

         CALL CROSS ( VX, VY, VZ )

         MAGZ  = DSQRT(VZ(1)*VZ(1) + VZ(2)*VZ(2) + VZ(3)*VZ(3))

         IF (DABS(MAGZ) < EPS1) THEN
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1938) SUBR_NAME, 'Z', TYPE, EID, (VZ(I),I=1,3)
            WRITE(F06,1938) SUBR_NAME, 'Z', TYPE, EID, (VZ(I),I=1,3)
            RETURN
         ENDIF

         DO I=1,3
            TE_GA_GB(3,I) = VZ(I)/MAGZ                        ! Row 3 of TE_GA_GB
         ENDDO                                                ! ----------------- 

         DO I=1,3                                             ! Transpose of TE_GA_GB
            DO J=1,3
               TET_GA_GB(I,J) = TE_GA_GB(J,I)
            ENDDO
         ENDDO


      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Offsets for BUSH are specified in a unique coord system. It may or may not be basic or global. So we first transform the input
! offset values to basic and then transform them to global. This way, when we process the offsets in subr ELEM_TRANSFORM_LBG, 
! we can treat the final offset values for the BUSH the same as we do for BAR, BEAM or ROD.
! The coord system for BUSH offsets is BUSH_OCID which can be:
!  (1) -1 means offset lies on the line between GA and GB, or
!  (2)  a positive number indicating a coord system number

      DO I=1,ELGP
         DO J=1,3                                          ! Initialize
            OFFDIS_B(I,J) = ZERO
            OFFDIS_G(I,J) = ZERO
         ENDDO
      ENDDO

      IF (EOFF(INT_ELEM_ID) == 'Y') THEN

         IF (BUSH_OCID >= 0) THEN
            IF (BUSH_OCID > 0) THEN                        ! BUSH offsets are defined in coord system BUSH_OCID
               CORD_FND = 'N'
               ICID = 0
               DO J=1,NCORD
                  IF (BUSH_OCID == CORD(J,2)) THEN
                     CORD_FND = 'Y'
                     ICID = J
                     EXIT
                  ENDIF
               ENDDO   
               IF (CORD_FND == 'Y') THEN
                  CALL GEN_T0L ( BGRID(1), ICID, THETAD, PHID, T0I )
                  DO J=1,3
                     OFFDIS_B(1,J) = T0I(J,1)*OFFDIS_O(1,1) + T0I(J,2)*OFFDIS_O(1,2) + T0I(J,3)*OFFDIS_O(1,3) 
                  ENDDO
                  write(f06,*)
                  NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1822) 'COORD SYSTEM ', BUSH_OCID, TYPE, EID
                  WRITE(F06,1822) 'COORD SYSTEM ', BUSH_OCID, TYPE, EID
               ENDIF

            ELSE IF (BUSH_OCID == 0) THEN                  ! Offset is in basic coords
               DO J=1,3
                  OFFDIS_B(1,J) = OFFDIS_O(1,J)
               ENDDO

            ENDIF

         ELSE IF (BUSH_OCID < 0) THEN                      ! Offsets are along the line between GA and GB

            DX1(1) = OFFDIS_O(1,1)                         ! Transform GA offsets (at 1st grid point relative to GA-GB axis) to basic
            DX1(2) = OFFDIS_O(1,2)
            DX1(3) = OFFDIS_O(1,3)
            CALL MATMULT_FFF (TET_GA_GB, DX1, 3, 3, 1, DX2 )
            OFFDIS_B(1,1) = DX2(1)
            OFFDIS_B(1,2) = DX2(2)
            OFFDIS_B(1,3) = DX2(3)

            DX1(1) = OFFDIS_O(2,1)                         ! Transform GA offsets (at 2nd grid point relative to GA-GB axis) to basic
            DX1(2) = OFFDIS_O(2,2)
            DX1(3) = OFFDIS_O(2,3)
            CALL MATMULT_FFF (TET_GA_GB, DX1, 3, 3, 1, DX2 )
            OFFDIS_B(2,1) = DX2(1)
            OFFDIS_B(2,2) = DX2(2)
            OFFDIS_B(2,3) = DX2(3)

         ENDIF

      ENDIF

! Now that we have the offsets at grid 1 in basic coords we can calc offset values for grid 2 and then transform offsets to global. 

      IF (EOFF(INT_ELEM_ID) == 'Y') THEN

         IF (BUSH_OCID /= -1) THEN                         ! We need to calc OFFDIS_B for grid 2. If OCID = -1 then that OFFDIS
!                                                            was already calculated in subr ELMDAT1 since it lay along line GA-GB
            DO I=1,3
               XEB(3,I)      = XEB(1,I) + OFFDIS_B(1,I)    ! Put BUSH basic coords in XEB (has extra row for more than the 2 grids)
               OFFDIS_B(2,I) = XEB(3,I) - XEB(2,I)         ! Offset from GB is location of BUSH minus location of GB
            ENDDO

         ENDIF

         DO I=1,2
            ACID_G = GRID(BGRID(I),3)                      ! Get global coord sys for this grid
            IF (ACID_G /= 0) THEN                          ! Global is not basic so need to transform offset from basic to global
               ICID = 0
               DO J=1,NCORD
                  IF (ACID_G == CORD(J,2)) THEN
                     ICID = J
                     EXIT
                  ENDIF
               ENDDO   
               CALL GEN_T0L ( BGRID(I), ICID, THETAD, PHID, T0G )
               DO J=1,3                                    ! We want transpose of T0G since we are transforming from basic to global
                  DO K=1,3
                     TG0(J,K) = T0G(K,J)
                  ENDDO
               ENDDO
               DO J=1,3
                  OFFDIS_G(I,J) = TG0(J,1)*OFFDIS_B(I,1) + TG0(J,2)*OFFDIS_B(I,2) + TG0(J,3)*OFFDIS_B(I,3) 
               ENDDO   
            ELSE                                           ! Global was basic so no transformation of coords needed
               DO J=1,3
                  OFFDIS_G(I,J) = OFFDIS_B(I,J)
               ENDDO   
            ENDIF
         ENDDO

         DO I=1,ELGP                                       ! Put global offsets in OFFDIS since these are the ones needed when
            DO J=1,3                                       ! offsets are applied in subr ELEM_TRANSFORM_LBG
               OFFDIS(I,J) = OFFDIS_G(I,J)
            ENDDO
         ENDDO

      ELSE                                                 ! There are no offsets so set OFFDIS_B to zero for both grids

         DO I=1,2
            DO J=1,3
               OFFDIS_B(I,J) = ZERO
            ENDDO
         ENDDO

      ENDIF 

! Calculate offsets in local, CID, axes

      DO I=1,3
         DX1(I) = OFFDIS_B(1,I)
      ENDDO
      CALL MATMULT_FFF ( TE, DX1, 3, 3, 1, DX2 )

      DO I=1,3
         OFFDIS_L(1,I) = DX2(I)
         DX1(I) = OFFDIS_B(2,I)
      ENDDO
      CALL MATMULT_FFF ( TE, DX1, 3, 3, 1, DX2 )

      DO I=1,3
         OFFDIS_L(2,I) = DX2(I)
      ENDDO

! Calculate offsets in axes along and normal to BUSH axes (i.e. axes that have x along line between GA-GB)
! This will be needed in subr OFP3_ELFE_1D when BUSH element forces are calculated from node forces

      DO I=1,3
         DX1(I) = OFFDIS_B(1,I)
      ENDDO
      CALL MATMULT_FFF ( TE_GA_GB, DX1, 3, 3, 1, DX2 )

      DO I=1,3
         OFFDIS_GA_GB(1,I) = DX2(I)
         DX1(I) = OFFDIS_B(2,I)
      ENDDO
      CALL MATMULT_FFF ( TE_GA_GB, DX1, 3, 3, 1, DX2 )

      DO I=1,3
         OFFDIS_GA_GB(2,I) = DX2(I)
      ENDDO



      

      
! ----------------------------------------------------------------------------------------------------------------------------------
      LX(1) = ( XEB(2,1) + OFFDIS_B(2,1) ) - ( XEB(1,1) + OFFDIS_B(1,1) )
      LX(2) = ( XEB(2,2) + OFFDIS_B(2,2) ) - ( XEB(1,2) + OFFDIS_B(1,2) )
      LX(3) = ( XEB(2,3) + OFFDIS_B(2,3) ) - ( XEB(1,3) + OFFDIS_B(1,3) )
      ELEM_LEN_AB = DSQRT( LX(1)*LX(1) + LX(2)*LX(2) + LX(3)*LX(3) )
                                                           ! If ELEM_LEN_AB is not equal to zero then write error and return.
      IF (ELEM_LEN_AB > .0001D0) THEN
         WRITE(ERR,1959) SUBR_NAME, TYPE, EID, ELEM_LEN_AB
         WRITE(F06,1959) SUBR_NAME, TYPE, EID, ELEM_LEN_AB
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         RETURN
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
      IF (DEBUG(110) > 0) THEN
         CALL DEBUG_ELMGM1_FOR_BUSH
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1801 FORMAT(' *ERROR  1801 ',A,'element ',I8,' has length (between grids) less than ',A,' so it must have a CID specified')

 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,I8,' IS UNDEFINED')

 1906 FORMAT(' *ERROR  1906: ',A,I8,' HAS INTERNAL GRID 3 (FOR V VECTOR) TOO CLOSE TO LINE BETWEEN INTERNAL GRIDS 1 AND 2')

 1912 FORMAT(' *ERROR  1912: CANNOT CALCULATE VECTOR IN ELEMENT Y DIRECTION FOR ELEMENT ',I8,' TYPE ',A)

 1938 FORMAT(' *ERROR  1938: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT CALCULATE PROPER VECTOR IN ELEMENT LOCAL COORDINATE DIRECTION ',A,' FOR ',A,' ELEMENT ',I8,'.' &
                    ,/,14X,' THE VECTOR COMPONENTS CALCULATED WERE ',3(1ES14.6))

 1944 FORMAT(' *ERROR  1944: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE VX VECTOR FOR ',A,' ELEMENT ',I8,' WAS LEFT UNSORTED. IT MUST BE SORTED TO DETERMINE VY, VZ') 

 1954 FORMAT(' *ERROR  1954: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DIMENSION OF ARRAYS OFFDIS, OFFSET ARE ONLY ',I8,' BUT MUST BE ',I8,' FOR ELEM TYPE ',A)

 1959 FORMAT(' *ERROR  1959: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A,I8,' HAS LENGTH (INCL EFFECTS OF OFFSETS) = ',1ES9.2,'. SHOULD BE ZERO')





! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEBUG_ELMGM1_FOR_BUSH

      IMPLICIT NONE

! **********************************************************************************************************************************

      WRITE(F06,*)
      WRITE(F06,98720)
      WRITE(F06,'(A,I8)') ' BUSH element number ', EID
      WRITE(F06,'(A)')    ' ============================'
      WRITE(F06,*)

      IF (BUSH_CID >= 0) THEN
         WRITE(F06,'(A,I8)') '    The element coordinate system will be BUSH_CID coord system   ',bush_cid
         WRITE(F06,*)
      ELSE
         WRITE(F06,'(A)') '    The element coordinate system will be determined from the 2 grids and the specified VVEC'
         WRITE(F06,*)
      ENDIF

      IF (EOFF(INT_ELEM_ID) == 'Y') THEN

         WRITE(F06,*) '   OFFDIS_O array of offsets based on input on CBUSH and ELEM_LEN_12 (before any coord transformations)'
         WRITE(F06,'(A,3(1ES14.6))') '                                     End A  = ', (OFFDIS_O(1,j),j=1,3)
         WRITE(F06,'(A,3(1ES14.6))') '                                     End B  = ', (OFFDIS_O(2,j),j=1,3)
         WRITE(F06,*)

         WRITE(F06,*) '   OFFDIS_B array of offsets in basic coords:'
         WRITE(F06,'(A,3(1ES14.6))') '                                     End A  = ', (OFFDIS_B(1,j),j=1,3)
         WRITE(F06,'(A,3(1ES14.6))') '                                     End B  = ', (OFFDIS_B(2,j),j=1,3)
         WRITE(F06,*)

         WRITE(F06,*) '   OFFDIS_G array of offsets transformed to global coords:'
         WRITE(F06,'(A,3(1ES14.6))') '                                     END A  = ', (OFFDIS_G(1,J),J=1,3)
         WRITE(F06,'(A,3(1ES14.6))') '                                     End B  = ', (OFFDIS_G(2,j),j=1,3)
         WRITE(F06,*)

         WRITE(F06,*) '   OFFDIS   array of offsets that will be used as global offsets (must equal OFFDIS_G above)'
         WRITE(F06,'(A,3(1ES14.6))') '                                     End A  = ', (OFFDIS(1,j),j=1,3)
         WRITE(F06,'(A,3(1ES14.6))') '                                     End B  = ', (OFFDIS(2,j),j=1,3)
         WRITE(F06,*)

      ENDIF

      WRITE(F06,'(A,3(1ES14.6))') '    ELEM_LEN_AB                             = ',ELEM_LEN_AB
      WRITE(F06,'(A,3(1ES14.6))') '    ELEM_LEN_12                             = ',ELEM_LEN_12

      WRITE(F06,*)

      WRITE(F06,98799)
      WRITE(F06,*)

! **********************************************************************************************************************************
98720 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::::::::START DEBUG(110) OUTPUT FROM SUBROUTINE ELMGM1::::::::::::::::::::::::::',&
             ':::::::::::::::::',/)

98799 FORMAT(' :::::::::::::::::::::::::::::::::::::::::::END DEBUG(110) OUTPUT FROM SUBROUTINE ELMGM1:::::::::::::::::::::::::::',&
             ':::::::::::::::::'                                                                                                ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_ELMGM1_FOR_BUSH

      END SUBROUTINE ELMGM1_BUSH
