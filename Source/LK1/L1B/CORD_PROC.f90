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
 
      SUBROUTINE CORD_PROC
 
! This subroutine calculates coordinate system transformation matrices defined on CORD2R, CORD2C, or CORD2S bulk data
! cards. The final  transformations in this subroutine are between a defined coord systems' principal axes and the
! basic coordinate system.  The principal axes of a coordinate system are the three orthogonal rectangular axes that
! is the base of the coordinate system.  For example, a cylindrical coord system has an implied rectangular system
! from which the radius and angle are measured. The transformations generated in this subroutine are not the final ones
! needed to get a coord transformation for a specific grid point in a cylindrical or spherical coord system since those
! axes depend on the radius and angles in those systems. These transformations are only between the basic system and the
! principal axes of the  coord system.  The final transformations for specific grid points in cylindrical and spherical
! coordinate systems is generated from the transformations in this subroutine and is done in subroutine GEN_T0L.FOR.
 
! The transformation matrices are stored, temporarily, in a 3-D array TN(I,J,K) where K is the internal coord sys
! number (NCORD of them). Thus, TN can be looked at as NCORD 2-D arrays with each 2-D array being a 3x3 coord
! transformation matrix.

!               TN: Transforms a vector to its reference system, RID, from its master system CID

! At the conclusion of this subroutine, the transformation matrices are stored in array RGRID
! along with the basic coordinates of the origin of the coord system. This data is needed in GEN_T0L.FOR
 
! The subroutine is divided into 8 Phases with the description of each phase given in that section below. The final result is arrays
! CORD(I) (modified so that all reference coord systems are basic), RCORD(I) (which has 12 cols: 1-3 are basic coords of origin,
! 4-6 are ist row of TN, 7-9 are 2nd row and 10-12 are 3rd row. TN is the 3x3 coord transformation matrix which transforms a vector
! from CID to basic.
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE80, PI, CONV_DEG_RAD
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MRCORD, NCORD, NCORD1, NCORD2, NGRID, FATAL_ERR
      USE PARAMS, ONLY                :  EPSIL, PRTCORD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CORD_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CORD, GRID, RCORD, RGRID, TN

      USE CORD_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'CORD_PROC'
      CHARACTER( 1*BYTE)              :: ALL_RIDS_0  = 'Y' ! Set to 'Y' when all coord systems' RID is basic
      CHARACTER( 1*BYTE)              :: CIRC_ERR    = 'N' ! Set to 'Y' if a coord sys has a circular reference back to itself
      CHARACTER( 6*BYTE)              :: CORD_NAME(NCORD)  ! Name of coord system (CORD1R, etc)
      CHARACTER( 1*BYTE)              :: FOUND_GA    = 'N' ! Set to 'Y' if grid A for CORD1R coord system is found in array GRID
      CHARACTER( 1*BYTE)              :: FOUND_GB    = 'N' ! Set to 'Y' if grid B for CORD1R coord system is found in array GRID
      CHARACTER( 1*BYTE)              :: FOUND_GC    = 'N' ! Set to 'Y' if grid C for CORD1R coord system is found in array GRID
      CHARACTER( 1*BYTE)              :: FOUND_RID   = 'N' ! Set to 'Y' if the RID for a coord system is a system in array CORD
      CHARACTER( 1*BYTE)              :: TRANS_DONE(NCORD) ! If 'Y' then the transformation to basic for a coord sys has been done
 
      INTEGER(LONG)                   :: CASCADE_PROC_ARRAY1(3,3)
                                                           ! Array which has info about how many coord systems have to be cascaded
!                                                            in order to get a transformation from a coord sys (actual number CID)
!                                                            to basic. There are 3 cols to allow holding info for the 3 ref pts in
!                                                            a CORD1R. Row 1 has the actual coordinate system number
!                                                            Row 2 has the col number in RID_ARRAY to use when cascading
!                                                            coord systems to get the coord transformation from CID to basic. Row 3
!                                                            has the number of coord systems that need to be cascaded to get the
!                                                            transformation from CID to basic.
!                                                             

      INTEGER(LONG)                   :: CASCADE_PROC_ARRAY(3,NCORD)
                                                           ! Same info from CASCADE_PROC_ARRAY1 but values for all coord systems.
!                                                            For  CORD1R, the 3 cols in CASCADE_PROC_ARRAY1 are reduced to have the
!                                                            col which has the shortest path from basic up to the coord system

      INTEGER(LONG)                   :: CID, CIDJ, CIDK   ! Actual coord system numbers
      INTEGER(LONG)                   :: CIDA, CIDB, CIDC  ! Actual coord system numbers
      INTEGER(LONG)                   :: CID_RID0          ! Actual coord sys number for a system that has basic as its' ref system
      INTEGER(LONG)                   :: CORD_TYPE         ! Integer value for coord system type
      INTEGER(LONG)                   :: GA(NCORD)         ! Grid nos put into array CORD for pt A when B.D. was read for CORD1R
      INTEGER(LONG)                   :: GB(NCORD)         ! Grid nos put into array CORD for pt B when B.D. was read for CORD1R
      INTEGER(LONG)                   :: GC(NCORD)         ! Grid nos put into array CORD for pt C when B.D. was read for CORD1R
      INTEGER(LONG)                   :: I,J,K,L,M         ! DO loop indices
      INTEGER(LONG)                   :: I1                ! A computed index
      INTEGER(LONG)                   :: ICID              ! The internal coord sys ID for an actual coord sys number
      INTEGER(LONG)                   :: ICIDA,ICIDB,ICIDC ! The internal coord sys ID for an actual coord sys number
      INTEGER(LONG)                   :: ICID_RID0         ! The internal coord sys ID for actual coord sys number CID_RID0
      INTEGER(LONG)                   :: INT1,INT2         ! Intermediate variables
      INTEGER(LONG)                   :: IERROR    = 0     ! Count of coord sys errors as we process coord systems
      INTEGER(LONG)                   :: NUM_RIDS          ! Number of ref coord systems specified on one CORDij (e.g. 3 for CORD1R)
      INTEGER(LONG)                   :: NUM_LEFT_AT_BEG   ! Num of CORD1 systems left to process at some point in Phase 7
      INTEGER(LONG)                   :: NUM_LEFT_AT_END   ! Num of CORD1 systems left to process at some point in Phase 7
      INTEGER(LONG)                   :: RID               ! The reference coord sys from a CORD array entry

      INTEGER(LONG)                   :: RID_ARRAY(NCORD+1,3*NCORD1+NCORD2)
                                                           ! An array that shows the chain of coord sys refs for all coord systems
!                                                            There has to be more cols than number of coord systems since CORD1R can
!                                                            take up 3 cols (one for each ref pt on the CORD1R). The top of the
!                                                            chain is the coord system being traced and the references follow. For
!                                                            CORD1R each of the 3 cols will have the same coord sys value in row 1.

      INTEGER(LONG)                   :: RID_ARRAY_COL     ! Col number in RID_ARRAY
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CORD_PROC_BEGEND
  
      REAL(DOUBLE)                    :: EMTN(3,3)         ! A coord transf matrix from some coord system to basic
      REAL(DOUBLE)                    :: EPS1              ! A small number
      REAL(DOUBLE)                    :: IVEC(3)           ! A unit vector in the x direction of a coord system
      REAL(DOUBLE)                    :: JVEC(3)           ! A unit vector in the y direction of a coord system
      REAL(DOUBLE)                    :: KVEC(3)           ! A unit vector in the z direction of a coord system
      REAL(DOUBLE)                    :: MAGVI             ! Magnitude of VI
      REAL(DOUBLE)                    :: MAGVJ             ! Magnitude of VJ
      REAL(DOUBLE)                    :: MAGVK             ! Magnitude of VK
      REAL(DOUBLE)                    :: PHI               ! Elevation angle in a sph coord system
      REAL(DOUBLE)                    :: RADIUS            ! Radius in a cyl or sph coord system
      REAL(DOUBLE)                    :: RGA(NCORD,3)      ! Coords of origin (pt A) in a coord system definition
      REAL(DOUBLE)                    :: RGB(NCORD,3)      ! Coords of a pt (B) on the z axis in a coord system definition
      REAL(DOUBLE)                    :: RGC(NCORD,3)      ! Coords of a pt (C) in the x-z plane in a coord system definition
      REAL(DOUBLE)                    :: RO(3,NCORD)       ! Array of basic coords of origin of coord sys rel to origin of basic sys
      REAL(DOUBLE)                    :: ROJ(3)            ! A column from array RO
      REAL(DOUBLE)                    :: RP(3,NCORD)       ! Array of basic coords of origin of coord sys rel to their ref sys
      REAL(DOUBLE)                    :: RPJ(3)            ! A column from array RP
      REAL(DOUBLE)                    :: T0A(3,3)          ! A coord transformation matrix in an intermediate calc
      REAL(DOUBLE)                    :: T0B(3,3)          ! A coord transformation matrix in an intermediate calc
      REAL(DOUBLE)                    :: T0C(3,3)          ! A coord transformation matrix in an intermediate calc
      REAL(DOUBLE)                    :: THETA             ! Azimuth angle in a cyl or sph coord system
      REAL(DOUBLE)                    :: V0A(3)            ! Vector from the basic system origin to pt A on a CORD1 entry
      REAL(DOUBLE)                    :: V0B(3)            ! Vector from the basic system origin to pt B on a CORD1 entry
      REAL(DOUBLE)                    :: V0C(3)            ! Vector from the basic system origin to pt C on a CORD1 entry
      REAL(DOUBLE)                    :: V1(3),V2(3),V3(3) ! Vectors in an intermediate calc
      REAL(DOUBLE)                    :: V_0_CIDA(3)       ! Vector from the basic system origin to origin of coord sys A
      REAL(DOUBLE)                    :: V_0_CIDB(3)       ! Vector from the basic system origin to origin of coord sys B
      REAL(DOUBLE)                    :: V_0_CIDC(3)       ! Vector from the basic system origin to origin of coord sys C
      REAL(DOUBLE)                    :: V_CIDA_CID_PTA(3) ! Vector from the orogin of ref sys CIDA to point A of coord sys CID
      REAL(DOUBLE)                    :: V_CIDB_CID_PTB(3) ! Vector from the origin of ref sys CIDB to point B of coord sys CID
      REAL(DOUBLE)                    :: V_CIDC_CID_PTC(3) ! Vector from the origin of ref sys CIDC to point C of coord sys CID
      REAL(DOUBLE)                    :: VI(3)             ! Vector in the x-z plane of a coord system
      REAL(DOUBLE)                    :: VJ(3)             ! Vector in the y direction in a coord system
      REAL(DOUBLE)                    :: VK(3)             ! Vector in the z direction in a coord system
 
      INTRINSIC                       :: DCOS, DSIN, DSQRT
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize
 
      EPS1    = DABS(EPSIL(1))
 
      DO I=1,NCORD
         TRANS_DONE = 'N'
         IF (CORD(I,1) == 11) CORD_NAME(I) = 'CORD1R'
         IF (CORD(I,1) == 12) CORD_NAME(I) = 'CORD1C'
         IF (CORD(I,1) == 13) CORD_NAME(I) = 'CORD1S'
         IF (CORD(I,1) == 21) CORD_NAME(I) = 'CORD2R'
         IF (CORD(I,1) == 22) CORD_NAME(I) = 'CORD2C'
         IF (CORD(I,1) == 23) CORD_NAME(I) = 'CORD2S'         
      ENDDO

      DO I=1,NCORD
         DO J=1,3                                          ! Initialize
            RGA(I,J) = ZERO
            RGB(I,J) = ZERO
            RGC(I,J) = ZERO
         ENDDO
      ENDDO

! **********************************************************************************************************************************
! Phase 1: For CORD1R: (1) Change grid numbers (that were put into array CORD when B.D. was read) to the reference coordinate system
!                          number for that grid. 
!                      (2) Put coords of the reference grids on the CORD1R into array RCORD. 

      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '11' )
 
      IERROR = 0

      CALL CORDCHK ( IERROR )                              ! CORDCHK makes sure that all cord system ID's are unique
 
doi11:DO I=1,NCORD

         CORD_TYPE = CORD(I,1)
         IF ((CORD_TYPE == 11) .OR. (CORD_TYPE == 12) .OR. (CORD_TYPE == 13)) THEN

            FOUND_GA = 'N'                                 ! Chg GA,GB,GC vals in array CORD with the ref coord sys for those grids
            FOUND_GB = 'N'
            FOUND_GC = 'N'

            GA(I) = CORD(I,3)
            GB(I) = CORD(I,4)
            GC(I) = CORD(I,5)

doj11:      DO J=1,NGRID
               IF ((FOUND_GA == 'Y') .AND. (FOUND_GB == 'Y') .AND. (FOUND_GC == 'Y')) EXIT doj11
               IF      (GRID(J,1) == GA(I)) THEN
                  FOUND_GA = 'Y'
                  CORD(I,3) = GRID(J,2)
                  DO K=1,3
                     RCORD(I,K) = RGRID(J,K)
                     RGA(I,K)   = RGRID(J,K)
                  ENDDO
                  CYCLE doj11
               ELSE IF (GRID(J,1) == GB(I)) THEN
                  FOUND_GB = 'Y'
                  CORD(I,4) = GRID(J,2)
                  DO K=1,3
                     RCORD(I,K+3) = RGRID(J,K)
                     RGB(I,K)     = RGRID(J,K)
                  ENDDO
                  CYCLE doj11
               ELSE IF (GRID(J,1) == GC(I)) THEN
                  FOUND_GC = 'Y'
                  CORD(I,5) = GRID(J,2)
                  DO K=1,3
                     RCORD(I,K+6) = RGRID(J,K)
                     RGC(I,K)     = RGRID(J,K)
                  ENDDO
                  CYCLE doj11
               ENDIF
            ENDDO doj11

            IF (FOUND_GA == 'N') THEN
               IERROR = IERROR + 1
               WRITE(ERR,1315) GA, CORD(I,2) 
               WRITE(F06,1315) GA, CORD(I,2) 
            ENDIF

            IF (FOUND_GB == 'N') THEN
               IERROR = IERROR + 1
               WRITE(ERR,1315) GB, CORD(I,2) 
               WRITE(F06,1315) GB, CORD(I,2) 
            ENDIF

            IF (FOUND_GC == 'N') THEN
               IERROR = IERROR + 1
               WRITE(ERR,1315) GC, CORD(I,2) 
               WRITE(F06,1315) GC, CORD(I,2) 
            ENDIF

            IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '12' )

         ENDIF

      ENDDO doi11

! Check IERROR and quit if > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,1304)
         WRITE(F06,1304)
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
! Phase 2: Change coords on cylindrical and spherical systems to coords in terms of the defining rectangular axes of that system

      DO I = 1,NCORD
 
         CORD_TYPE = CORD(I,1)

         CID  = CORD(I,2)                                  ! CID is the actual coord system no. for internal no. I.
         RID  = CORD(I,3)                                  ! RID is the ref coord sys for coord sys I
 
         IF (RID /= 0) THEN                                ! If RID is not basic, determine the coord type for RID.
            DO J=1,NCORD
               CIDJ = CORD(J,2)
               IF(CIDJ == RID) THEN
                  CORD_TYPE = CORD(J,1)
                  EXIT
               ENDIF
            ENDDO 
                                                           
            IF      ((CORD_TYPE == 12) .OR. (CORD_TYPE == 22)) THEN
               DO J=1,7,3                                  ! If RID is cyl, replace R and THETA in RCORD w/ X, Y coords that are
                  RADIUS = RCORD(I,J)                      ! the defining rectangular axes of the cyl system with the X-Y plane
                  THETA  = RCORD(I,J+1)*CONV_DEG_RAD       ! at THETA = 0.
                  RCORD(I,J)   = RADIUS*DCOS(THETA)
                  RCORD(I,J+1) = RADIUS*DSIN(THETA)
               ENDDO 
            ELSE IF ((CORD_TYPE == 13) .OR. (CORD_TYPE == 23)) THEN
               DO J=1,7,3                                  ! If RID is cyl, replace R, PHI, THETA in RCORD w/ X, Y, Z coords that
                  RADIUS = RCORD(I,J)                      ! are the defining rectangular axes of the cyl system with the X-Y plane
                  THETA  = RCORD(I,J+1)*CONV_DEG_RAD       ! at THETA = 0.
                  PHI    = RCORD(I,J+2)*CONV_DEG_RAD
                  RCORD(I,J)   = RADIUS*DSIN(THETA)*DCOS(PHI)
                  RCORD(I,J+1) = RADIUS*DSIN(THETA)*DSIN(PHI)
                  RCORD(I,J+2) = RADIUS*DCOS(THETA)
               ENDDO 
            ENDIF
         ENDIF

      ENDDO

      IF (PRTCORD == 2) CALL PARAM_PRTCORD_OUTPUT ( '21' )
 
! **********************************************************************************************************************************
! Phase 3: Check coordinate system data for logic. 
!          (1) There must be a final reference to the basic system (0)
!          (2) There can be no circular references (i.e. a coord system can reference any number of other coord sys but cannot
!              reference itself in that chain of refs).
!          (3) Create RID_ARRAY which shows the chain of references of each coord system 

! A complete check is made and processing is not stopped until all errors of the above kind are found. Bulk Data parameter
! PRTCORD is used to  print out information from these checks as well as the coord transformation matrices later.
 
      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '31' )

      IERROR = 0                                           ! Initialize IERROR for Phase 2

      IF (IERROR > 0) THEN
         WRITE(ERR,1304)
         WRITE(F06,1304)
         CALL OUTA_HERE ( 'Y' )
      ENDIF
 
! Begin loop over all coord systems

      RID_ARRAY_COL = 0
doi21:DO I=1,NCORD

         CID = CORD(I,2)                                   ! CID is the coordinate system number from the coord card

         CORD_TYPE = CORD(I,1)
         IF  ((CORD_TYPE == 11) .OR. (CORD_TYPE == 12) .OR. (CORD_TYPE == 13)) THEN
            NUM_RIDS = 3                                   ! CORD1R has 3 ref coord systems
         ELSE
            NUM_RIDS = 1                                   ! CORD2R,C,S has 1 ref coord system
         ENDIF

dom21:   DO M=1,NUM_RIDS

            RID_ARRAY_COL = RID_ARRAY_COL + 1

            RID = CORD(I,2+M)                              ! RID is the reference coord system number for CID

doj21:      DO J=1,NCORD+1                                 ! Initially set all of RID_ARRAY to some negative number since we know,
               RID_ARRAY(J,RID_ARRAY_COL) = -99            ! after this CID has been traced, it cannot have any negative coord ID's
            ENDDO doj21                                    ! but it must have 0 (basic) as the last coord sys in the chain
 
            I1  = 1                                        ! We need I1 to increment this index later
            RID_ARRAY(I1,RID_ARRAY_COL) = CID
            I1  = 2
            RID_ARRAY(I1,RID_ARRAY_COL) = RID

            IF (PRTCORD == 2) CALL PARAM_PRTCORD_OUTPUT ( '32' )

            IF (RID /= 0) THEN                             ! If RID is not the basic system, make sure it is defined
 
               FOUND_RID = 'N'
doj22:         DO J=1,NCORD
                  IF (CORD(J,2) == RID) THEN
                     FOUND_RID = 'Y'
                     EXIT doj22
                  ENDIF
               ENDDO doj22
 
               IF (FOUND_RID == 'N') THEN                  ! Could not find reference coord sys (RID) for coord sys CID   
                  WRITE(ERR,1300) RID,CID
                  WRITE(F06,1300) RID,CID
                  IERROR = IERROR + 1
                  FATAL_ERR = FATAL_ERR + 1
               ENDIF
 
! Build chain of references for CID. Initially, RID was read from field 3 of the CORD card, above.
! Now go thru all CORD cards until we find one that has RID as the defined system in field 2. When we find this CORD
! card, we reset RID to be the value from field 3 of that card and continue to cascade down until we come to a CORD
! card with 0 (basic) in field 3. When we find this system, exit from the loops. Two loops on NCORD are needed to do
! the check. The inner (K) loop steps thru the CORD cards looking for the one that defines the RID system. When it is
! found, we continue in that loop looking for the next system (if necessary). However, that system may have already
! been passed in the K loop, so the outer loop starts the process over if necessary. 
  
               CIRC_ERR = 'N'
doj23:         DO J=1,NCORD
                  IF (J /= I) THEN
dok21:               DO K=1,NCORD
                        IF (K /= I) THEN
                           IF (CORD(K,2) == RID) THEN
                              CIDK = CORD(K,2)
                              RID  = CORD(K,3)
                              I1 = I1 + 1
                              IF (I1 > NCORD+1) THEN
                                 WRITE(ERR,1305) SUBR_NAME
                                 WRITE(F06,1305) SUBR_NAME
                                 FATAL_ERR = FATAL_ERR + 1
                                 CALL OUTA_HERE ( 'Y' )
                              ENDIF
                              RID_ARRAY(I1,RID_ARRAY_COL) = RID

                              IF (PRTCORD == 2) CALL PARAM_PRTCORD_OUTPUT ( '33' )

                              IF (RID == 0) EXIT dok21

dol21:                        DO L=1,I1-1                  ! Check for circular reference
                                 IF(RID_ARRAY(I1,RID_ARRAY_COL) == RID_ARRAY(L,RID_ARRAY_COL)) THEN
                                    WRITE(ERR,1302) RID_ARRAY(I1,RID_ARRAY_COL)
                                    WRITE(F06,1302) RID_ARRAY(I1,RID_ARRAY_COL)
                                    FATAL_ERR = FATAL_ERR + 1
                                    IERROR = IERROR + 1
                                    CIRC_ERR = 'Y'
                                    EXIT dol21
                                 ENDIF
                              ENDDO dol21
         
                           ENDIF
                        ENDIF
                        IF ((RID ==  0 ) .OR. (CIRC_ERR == 'Y')) EXIT dok21
                     ENDDO dok21
                     IF ((RID ==  0 ) .OR. (CIRC_ERR == 'Y')) EXIT doj23
                  ENDIF
               ENDDO doj23
 
               IF (RID_ARRAY(I1,RID_ARRAY_COL) == 0) THEN  ! Check if last sys in RID_ARRAY is basic. If it is, CYCLE back to the I
                  CYCLE dom21                              ! loop to begin with a new coord system.
               ELSE                                        ! Otherwise write error and CYCLE
                  WRITE(ERR,1303) CID
                  WRITE(F06,1303) CID
                  IERROR = IERROR + 1                      ! Coord sys CID does not have a final reference to basic
                  FATAL_ERR = FATAL_ERR + 1
                  CYCLE dom21
               ENDIF

            ENDIF

         ENDDO dom21

      ENDDO doi21
    
      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '34' )

! Check IERROR and quit if > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,1304) 
         WRITE(F06,1304) 
         CALL OUTA_HERE ( 'Y' )
      ENDIF
  
! **********************************************************************************************************************************
! Phase 4: Solve for array CASCADE_PROC_ARRAY. It will be used to do the cascading of coord references to get the transformation
! from CID to basic

      DO J=1,NCORD
         CASCADE_PROC_ARRAY(1,J) = 0
         CASCADE_PROC_ARRAY(2,J) = 0
         CASCADE_PROC_ARRAY(3,J) = NCORD + 1
      ENDDO

      RID_ARRAY_COL = 1
doi31:DO I=1,NCORD

         CORD_TYPE = CORD(I,1)
         IF  ((CORD_TYPE == 11) .OR. (CORD_TYPE == 12) .OR. (CORD_TYPE == 13)) THEN
            NUM_RIDS = 3                                   ! CORD1R,C,S has 3 ref coord systems
         ELSE
            NUM_RIDS = 1                                   ! CORD2R,C,S have 1 ref coord system
         ENDIF

         DO J=1,3
            CASCADE_PROC_ARRAY1(1,J) = 0
            CASCADE_PROC_ARRAY1(2,J) = 0
            CASCADE_PROC_ARRAY1(3,J) = NCORD + 1
         ENDDO

doj31:   DO J=1,NUM_RIDS
doK31:      DO K=NCORD+1,1,-1
               IF (RID_ARRAY(K,RID_ARRAY_COL) == 0) THEN
                  IF (K <= CASCADE_PROC_ARRAY1(3,J)) THEN

                     CASCADE_PROC_ARRAY1(1,J) = RID_ARRAY(1,RID_ARRAY_COL)
                     CASCADE_PROC_ARRAY1(2,J) = RID_ARRAY_COL
                     CASCADE_PROC_ARRAY1(3,J) = K - 1

                     CASCADE_PROC_ARRAY(1,I)  = RID_ARRAY(1,RID_ARRAY_COL)
                     CASCADE_PROC_ARRAY(2,I)  = RID_ARRAY_COL
                     CASCADE_PROC_ARRAY(3,I)  = K - 1

                     CYCLE
                  ENDIF
               ENDIF
            ENDDO dok31
            RID_ARRAY_COL = RID_ARRAY_COL + 1
         ENDDO doj31

         IF (NUM_RIDS == 3) THEN
            DO J=2,3
               IF(CASCADE_PROC_ARRAY1(3,J) < CASCADE_PROC_ARRAY1(3,J-1)) THEN
                  CASCADE_PROC_ARRAY(1,I) = CASCADE_PROC_ARRAY1(1,J)  ! Coord system ID (CID)
                  CASCADE_PROC_ARRAY(2,I) = CASCADE_PROC_ARRAY1(2,J)  ! Row in RID_ARRAY where to find the coord references for CID
                  CASCADE_PROC_ARRAY(3,I) = CASCADE_PROC_ARRAY1(3,J)  ! 
               ENDIF
            ENDDO
         ENDIF

      ENDDO doi31

      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '41' )

! **********************************************************************************************************************************
! Phase 5: Get the unit vectors in each of the CORD2 coord systems in terms of their reference system. Put the results into array
! RCORD where the transformation matrices go. These are NOT the final transformations; they are the transformations from a CID
! to its reference system and ONLY for CORD2 systems (CORD1 systams will be handled later).

      IF (PRTCORD == 2) CALL PARAM_PRTCORD_OUTPUT ( '51' )

      IERROR = 0
doi32:DO I=1,NCORD

         CORD_TYPE = CORD(I,1)                             ! Process only CORD2C,R,S first
         IF ((CORD_TYPE == 21) .OR. (CORD_TYPE == 22) .OR. (CORD_TYPE == 23)) THEN

            RID_ARRAY_COL = CASCADE_PROC_ARRAY(2,I)
            IF (RID_ARRAY_COL <= 0) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1321) SUBR_NAME, 'RID_ARRAY_COL in the Phase 5 calcs', RID_ARRAY_COL
               WRITE(F06,1321) SUBR_NAME, 'RID_ARRAY_COL in the Phase 5 calcs', RID_ARRAY_COL
               CALL OUTA_HERE ( 'Y' )
            ENDIF

            CID = RID_ARRAY(1,RID_ARRAY_COL)               ! Coord sys for which we are getting the transformation to its RID
            RID = RID_ARRAY(2,RID_ARRAY_COL)               ! The  reference system, RID, for CID

            DO J=1,NCORD                                   ! Find the internal coord sys ID for CID
               IF (CORD(J,2) == CID) THEN
                  ICID = J
                  EXIT
               ENDIF
            ENDDO

            DO J=1,3
               VK(J) = RCORD(ICID,3+J) - RCORD(ICID,J)     ! Vector in z dir of CID expressed in it's ref system
               VI(J) = RCORD(ICID,6+J) - RCORD(ICID,J)     ! Vector in x-z plane of CID expressed in it's ref system
            ENDDO
            MAGVK = DSQRT( VK(1)*VK(1) + VK(2)*VK(2) + VK(3)*VK(3) )
            MAGVI = DSQRT( VI(1)*VI(1) + VI(2)*VI(2) + VI(3)*VI(3) )

            IF (MAGVK < EPS1) THEN                         ! If MAGVK = 0 then points A and B on the coord card are too close.
               WRITE(ERR,1306) CORD(ICID,2)
               WRITE(F06,1306) CORD(ICID,2)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF

            IF (MAGVI < EPS1) THEN                         ! If MAGVI = 0 then points A and C on the coord card are too close.
               WRITE(ERR,1307) CORD(ICID,2)
               WRITE(F06,1307) CORD(ICID,2)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF

            DO J=1,3                                       ! Unit vector in z direction is KVEC
               KVEC(J) = VK(J)/MAGVK
               RCORD(ICID,9+J) = KVEC(J)
            ENDDO 

            IF ((MAGVK >= EPS1) .AND. (MAGVI >= EPS1)) THEN! Calc unit vector in y dir if vectors VK, VI are not null
               CALL CROSS ( KVEC, VI, VJ )
               MAGVJ = DSQRT(VJ(1)*VJ(1)+VJ(2)*VJ(2)+VJ(3)*VJ(3))

               IF (MAGVJ < EPS1) THEN                      ! If MAGVJ = 0 then vec from pt A to pt B is || to vec from pts A to C
                  WRITE(ERR,1308) CORD(ICID,2)
                  WRITE(F06,1308) CORD(ICID,2)
                  IERROR = IERROR + 1
                  FATAL_ERR = FATAL_ERR + 1
               ENDIF 
            ENDIF

            IF (IERROR > 0) CYCLE

            DO J = 1,3                                     ! Unit vector in y direction is JVEC
               JVEC(J) = VJ(J)/MAGVJ
               RCORD(ICID,6+J) = JVEC(J)
            ENDDO 

            CALL CROSS ( JVEC, KVEC, IVEC )                ! Unit vector in x direction is IVEC


            DO J = 1,3                                     ! Put unit vecs into array TN: Transforms a vector to RID from CID
               TN(J,1,ICID) = IVEC(J)                      ! -- Col 1 of TN is IVEC
               TN(J,2,ICID) = JVEC(J)                      ! -- Col 2 of TN is JVEC
               TN(J,3,ICID) = KVEC(J)                      ! -- Col 3 of TN is KVEC
            ENDDO
   
            DO J=1,3
               RCORD(ICID,3+J) = TN(1,J,ICID)              ! Row 1 of TN goes into RCORD cols  4- 6
               RCORD(ICID,6+J) = TN(2,J,ICID)              ! Row 1 of TN goes into RCORD cols  7- 9
               RCORD(ICID,9+J) = TN(3,J,ICID)              ! Row 1 of TN goes into RCORD cols 10-12
            ENDDO 

            IF (PRTCORD == 2) CALL PARAM_PRTCORD_OUTPUT ( '52' )

         ENDIF

      ENDDO doi32

! Check IERROR and quit if > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,1304)
         WRITE(F06,1304)
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
! Phase 6:  Calculate transformations to basic (zero ) system for all CORD2 coordinate systems. 

! The TN transformation matrices generated in Phase 5 give the transformation from CID system to RID system for one coord card.
! In Phase 6 we cascade these to get the transformation from basic to CID system for each CORD2 coord system. That is, consider the
! following example:
 
!             CID             RID             TN from Phase 2
!     CORD2R   1               0                   TN01
!     CORD2R   2               1                   TN12
!     CORD2C   3               2                   TN23
!     CORD2S   4               1                   TN14
 
! Where, e.g., TN12 is the transformation matrix from coord sys 2 to coord sys 1 (princ. axes) found in Phase 5.
! The transformation  from 2 to 0 is TN01 x TN12.  The transformations generated here are:
  
!       TN02 = TN01 x TN12
 
! and stores the result back into array TN and resets the RID to 0 for CID 2
 
!       TN03 = TN02 x TN23
!       TN04 = TN01 x TN14
  
! and TN01 was already determined in Phase 5.  
 
! The procedure is to work backwards.  Begin looking for a coord system that has 0 as a reference (or RID = 0).  The coord
! transformation to basic for that system is already in TN.  If that coord system is referenced on another coord
! card, then we multiply those two matrices together to get the transformation to basic for the second system, and
! so on. This transformation matrix is stored over the original TN found in Phase 5.  Once we have gotten the
! transformation to basic for a CORD2 coord system,  we reset RID (i.e. CORD(I,3)) to zero to indicate that its' TN is
! now referenced to basic.
  
! The basic coords of the origin of each coord system are also calculated. The calc begins by taking the point A coords from the
! system that has 0 as reference and adding the amounts from each coord sys A point after transforming it to basic.
 
      IERROR = 0
main: DO                                                   ! Until all RID's are 0
         ALL_RIDS_0 = 'Y'                                  ! Find out if there are coord systems whose RID is not, or has not been
         DO J=1,NCORD                                      ! transformed to, basic
            CORD_TYPE = CORD(J,1)
            IF ((CORD_TYPE == 21) .OR. (CORD_TYPE == 22) .OR. (CORD_TYPE == 23)) THEN
               IF (CORD(J,3) /= 0) THEN
                  ALL_RIDS_0 = 'N'
                  EXIT
               ELSE
                  TRANS_DONE(J) = 'Y'                      ! This coord sys has basic as its RID so set TRANS_DONE
               ENDIF
            ENDIF
         ENDDO

         IF (ALL_RIDS_0 == 'N') THEN                       ! From above search we found at least 1 coord sys whose RID /= 0
                                                           ! The following loops (J, K) search through the coord system list to find
                                                           ! a sys that has RID = 0. When found, it sets this sys CID to be CID_RID0
jloop1:     DO J =1,NCORD                                  ! The inner loop then searches for a sys that uses this CID as its RID.
                                                           ! When found, it exits the outer loop, does the coord transformation, and
               CORD_TYPE = CORD(J,1)                       ! resets the RID on this system to 0
               IF ((CORD_TYPE == 21) .OR. (CORD_TYPE == 22) .OR. (CORD_TYPE == 23)) THEN

                  RID = CORD(J,3)

                  CID_RID0  = 0
                  ICID_RID0 = 0
                  IF (RID == 0) THEN                       ! We have found a sys that has RID = 0. Set CID_RID0 to that systems CID
                     CID       = CORD(J,2)
                     CID_RID0  = CID
                     ICID_RID0 = J

kloop2:              DO K = 1,NCORD                        ! Go through list looking for the sys that has, as its' RID, CID_RID0
                        CORD_TYPE = CORD(K,1)
                        IF ((CORD_TYPE == 21) .OR. (CORD_TYPE == 22) .OR. (CORD_TYPE == 23)) THEN
                           IF (K == J) CYCLE kloop2        ! Skip the sys found in the J loop above since it has RID = 0 already
                           CID = CORD(K,2)
                           RID = CORD(K,3)
                           IF (RID == CID_RID0) THEN       ! We have found one coord sys that has its' RID equal to CID_RID0
                              EXIT jloop1                  ! so exit this loop and get the transformation to basic for system CID
                           ELSE
                           ENDIF
                        ENDIF
                     ENDDO kloop2

                  ELSE

                     CYCLE

                  ENDIF

               ENDIF

            ENDDO jloop1

            IF (ICID_RID0 == 0) THEN
               WRITE(ERR,1316) SUBR_NAME, ICID_RID0
               WRITE(F06,1316) SUBR_NAME, ICID_RID0
               IERROR = IERROR + 1
               CYCLE main
            ENDIF

            RO(1,ICID_RID0) = RCORD(ICID_RID0,1)
            RO(2,ICID_RID0) = RCORD(ICID_RID0,2)
            RO(3,ICID_RID0) = RCORD(ICID_RID0,3)

jloop2:     DO J = 1,NCORD                                 ! Loop on J since there may be more than 1 sys that references CID_RID0

               CORD_TYPE = CORD(J,1)                       ! resets the RID on this system to 0
               IF ((CORD_TYPE == 21) .OR. (CORD_TYPE == 22) .OR. (CORD_TYPE == 23)) THEN

                  IF (J == ICID_RID0) CYCLE jloop2

                  RID = CORD(J,3)

                  IF (RID == CID_RID0) THEN                ! This is a coord sys that has CID_RID0 as its RID, so we can mult
                                                           ! matrices to get its' transformation to basic

                     CALL MATMULT_FFF ( TN(1,1,ICID_RID0), TN(1,1,J), 3, 3, 3, EMTN )

                     RO(1,J) = RCORD(J,1)
                     RO(2,J) = RCORD(J,2)
                     RO(3,J) = RCORD(J,3)

                     DO K=1,3
                        ROJ(K) = RO(K,J)
                     ENDDO
                     CALL MATMULT_FFF ( TN(1,1,ICID_RID0), ROJ, 3, 3, 1, RPJ )
                     DO K=1,3
                        RP(K,J) = RPJ(K)
                     ENDDO

                     DO K = 1,3
                        RCORD(J,K) = RO(K,ICID_RID0) + RP(K,J)! Coords of origin of system J in basic coords
                        DO L = 1,3
                           TN(K,L,J) = EMTN(K,L)
                        ENDDO 
                     ENDDO 

                     CORD(J,3) = 0                         ! Reset RID on this coord sys to 0 since TN matrix now is trans to basic
                     TRANS_DONE(J) = 'Y'                   ! We have completed the transformation to basic for this coord sys

                  ENDIF

               ENDIF

            ENDDO jloop2

         ELSE

            EXIT main

         ENDIF
 
      ENDDO main
  
! Check IERROR and quit if > 0

      IF (IERROR > 0) THEN
         WRITE(ERR,1304)
         WRITE(F06,1304)
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Now put transformation for the CORD2 systems matrices in RCORD. First 3 words in RCORD are now the basic coords of the origin of
! the coord system. Next 9 words give the 3 rows (row 1 followed by row 2 then row 3) of the transformation matrix which will
! transform a vector in CID (of coord principal axes) to a vector in basic coord system.
 
      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '61' )

      DO I=1,NCORD
         CORD_TYPE = CORD(I,1)
         IF ((CORD_TYPE == 21) .OR. (CORD_TYPE == 22) .OR. (CORD_TYPE == 23)) THEN
            DO J=1,3
               DO K=1,3
                  L = 3 + 3*(J-1) + K
                  RCORD(I,L) = TN(J,K,I)
               ENDDO 
            ENDDO
            IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '62' )
         ENDIF
      ENDDO 
  
      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '63' )

! **********************************************************************************************************************************
! Phase 7 Process CORD1 entries to get transformations to basic and put results into array RCORD

      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '71' )

main7:DO I=1,NCORD

         CORD_TYPE = CORD(I,1)                             ! Only process CORD1 entries. CORD2's were handled above
         IF ((CORD_TYPE == 11) .OR. (CORD_TYPE == 12) .OR. (CORD_TYPE == 13)) THEN

            NUM_LEFT_AT_BEG = 0                            ! Find out how many CORD1 systems still need to be processed
            DO K=1,NCORD
               IF (TRANS_DONE(K) == 'N') THEN
                  NUM_LEFT_AT_BEG = NUM_LEFT_AT_BEG + 1
               ENDIF
            ENDDO
            IF (NUM_LEFT_AT_BEG == 0) EXIT main7

            IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '72' )

            IERROR   = 0
big_loop:   DO J=1,NCORD                                   ! Find a CORD1 with all RID's already processed and calc RCORD, TN for it
               IF (TRANS_DONE(J) == 'Y') THEN
                  CYCLE big_loop
               ENDIF

               CID  = CORD(J,2)                            ! Coord and reference ID's on this CORD1
               CIDA = CORD(J,3)
               CIDB = CORD(J,4)
               CIDC = CORD(J,5)

               ICIDA = -99                                 ! Get the internal coord ID's for the ref systems on this CORD1
               ICIDB = -99
               ICIDC = -99
               DO K=1,NCORD

                  IF (CORD(K,2) == CID ) THEN
                     ICID  = K
                  ENDIF

                  IF (CORD(J,3) /= 0) THEN
                     IF (CORD(K,2) == CIDA) ICIDA = K
                  ELSE
                     ICIDA = 0
                  ENDIF

                  IF (CORD(J,4) /= 0) THEN
                     IF (CORD(K,2) == CIDB) ICIDB = K
                  ELSE
                     ICIDB = 0
                  ENDIF

                  IF (CORD(J,5) /= 0) THEN
                     IF (CORD(K,2) == CIDC) ICIDC = K
                  ELSE
                     ICIDC = 0
                  ENDIF

               ENDDO

               IF      (ICIDA == -99) THEN                 ! Set error and cycle if we couldn't find the internal ID's for CIDA,B,C
                  IERROR = IERROR + 1
                  INT1 = CIDA   ;   INT2 = ICIDA
               ELSE IF (ICIDB == -99) THEN 
                  IERROR = IERROR + 1
                  INT1 = CIDB   ;   INT2 = ICIDB
               ELSE IF (ICIDC== -99) THEN 
                  IERROR = IERROR + 1
                  INT1 = CIDC   ;   INT2 = ICIDC
               ENDIF
               IF ((ICIDA == -99) .OR. (ICIDB == -99) .OR. (ICIDC == -99)) THEN
                  IERROR = IERROR + 1
                  WRITE(ERR,1317) CORD_NAME(J), INT1, INT2
                  WRITE(F06,1317) CORD_NAME(J), INT1, INT2
                  CYCLE main7
               ENDIF


               IF (ICIDA /= 0) THEN
                  IF (TRANS_DONE(ICIDA) == 'N') CYCLE big_loop
               ENDIF
               IF (ICIDB /= 0) THEN
                  IF (TRANS_DONE(ICIDB) == 'N') CYCLE big_loop
               ENDIF
               IF (ICIDC /= 0) THEN
                  IF (TRANS_DONE(ICIDC) == 'N') CYCLE big_loop
               ENDIF

               IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '73' )

               DO K=1,3
                  DO L=1,3
                     IF (ICIDA /= 0) THEN
                        TN(K,L,ICIDA) = RCORD(ICIDA,3*(K-1)+L+3) ! This TN matrix will transform a vector to basic (0) from CIDA
                        T0A(K,L) = TN(K,L,ICIDA)
                     ENDIF
                     IF (ICIDB /= 0) THEN
                        TN(K,L,ICIDB) = RCORD(ICIDB,3*(K-1)+L+3) ! This TN matrix will transform a vector to basic (0) from CIDB
                        T0B(K,L) = TN(K,L,ICIDB)
                     ENDIF
                     IF (ICIDC /= 0) THEN
                        TN(K,L,ICIDC) = RCORD(ICIDC,3*(K-1)+L+3) ! This TN matrix will transform a vector to basic (0) from CIDC
                        T0C(K,L) = TN(K,L,ICIDC)
                     ENDIF
                  ENDDO
               ENDDO

               DO K=1,3                                    ! Vectors from basic system origin to origins of reference systems A,B,C

                  IF (ICIDA /= 0) THEN
                     V_0_CIDA(K) = RCORD(ICIDA,K)
                  ELSE
                     V_0_CIDA(K) = ZERO
                  ENDIF

                  IF (ICIDB /= 0) THEN
                     V_0_CIDB(K) = RCORD(ICIDB,K)
                  ELSE
                     V_0_CIDB(K) = ZERO
                  ENDIF

                  IF (ICIDC /= 0) THEN
                     V_0_CIDC(K) = RCORD(ICIDC,K)
                  ELSE
                     V_0_CIDC(K) = ZERO
                  ENDIF

               ENDDO

               DO K=1,3                                    ! Vectors in sys 0 from origin of sys CIDA,B,C to pts A,B,C of sys CID
                  V1(K) = RGA(J,K)
                  V2(K) = RGB(J,K)
                  V3(K) = RGC(J,K)
               ENDDO

               IF (ICIDA /= 0) THEN
                  CALL MATMULT_FFF ( T0A, V1, 3, 3, 1, V_CIDA_CID_PTA )
               ELSE
                  DO K=1,3
                     V_CIDA_CID_PTA(K) = V1(K)
                  ENDDO
               ENDIF

               IF (ICIDB /= 0) THEN
                  CALL MATMULT_FFF ( T0B, V2, 3, 3, 1, V_CIDB_CID_PTB )
               ELSE
                  DO K=1,3
                     V_CIDB_CID_PTB(K) = V2(K)
                  ENDDO
               ENDIF

               IF (ICIDC /= 0) THEN
                  CALL MATMULT_FFF ( T0C, V3, 3, 3, 1, V_CIDC_CID_PTC )
               ELSE
                  DO K=1,3
                     V_CIDC_CID_PTC(K) = V3(K)
                  ENDDO
               ENDIF

! Coords of points A, B, C of CID in their reference systems, CIDA, CIDB, CIDC

               DO K=1,3                                    ! Vectors in sys 0 from sys 0 origin to pts A,B,C on coord sys CID

                  V0A(K) = V_0_CIDA(K) + V_CIDA_CID_PTA(K)
                  V0B(K) = V_0_CIDB(K) + V_CIDB_CID_PTB(K)
                  V0C(K) = V_0_CIDC(K) + V_CIDC_CID_PTC(K)

               ENDDO

               DO K=1,3                                    ! Vectors i sys 0 from A->B and A->C for sys CID
                  VK(K)  = V0B(K) - V0A(K)
                  VI(K)  = V0C(K) - V0A(K)
               ENDDO

               MAGVK = DSQRT( VK(1)*VK(1) + VK(2)*VK(2) + VK(3)*VK(3) )
               MAGVI = DSQRT( VI(1)*VI(1) + VI(2)*VI(2) + VI(3)*VI(3) )

               IF (MAGVK < EPS1) THEN
                  WRITE(ERR,1306) CORD(J,2)
                  WRITE(F06,1306) CORD(J,2)
                  IERROR = IERROR + 1
                  FATAL_ERR = FATAL_ERR + 1
               ENDIF

               IF (MAGVI < EPS1) THEN
                  WRITE(ERR,1307) CORD(J,2)
                  WRITE(F06,1307) CORD(J,2)
                  IERROR = IERROR + 1
                  FATAL_ERR = FATAL_ERR + 1
               ENDIF

               DO K = 1,3                                  ! Unit vector in basic coord z direction for z axis of CID is KVEC
                  KVEC(K) = VK(K)/MAGVK
               ENDDO 

               IF ((MAGVK >= EPS1) .AND. (MAGVI >= EPS1)) THEN! Calc unit vector in y dir if vectors VK, VI are not null
                  CALL CROSS ( KVEC, VI, VJ )
                  MAGVJ = DSQRT(VJ(1)*VJ(1)+VJ(2)*VJ(2)+VJ(3)*VJ(3))

                  IF (MAGVJ < EPS1) THEN
                     WRITE(ERR,1308) CORD(J,2)
                     WRITE(F06,1308) CORD(J,2)
                     IERROR = IERROR + 1
                     FATAL_ERR = FATAL_ERR + 1
                  ENDIF 
               ENDIF

               IF (IERROR > 0) CYCLE main7

               DO K = 1,3                                  ! Unit vector in basic coord y direction for z axis of CID is JVEC
                  JVEC(K) = VJ(K)/MAGVJ
               ENDDO 
                                                           ! Unit vector in basic coord x direction for z axis of CID is IVEC
               CALL CROSS ( JVEC, KVEC, IVEC )

               IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '74' )

               DO K=1,3                                    ! Put unit vecs into arrays TN and RCORD
                  TN(K,1,ICID)    = IVEC(K)
                  TN(K,2,ICID)    = JVEC(K)
                  TN(K,3,ICID)    = KVEC(K)
               ENDDO

               DO K=1,3
                  RCORD(ICID,K) = V0A(K)                   ! Origin of CID is V0A
                  DO L=1,3
                     M = 3 + 3*(K-1) + L
                     RCORD(ICID,M) = TN(K,L,ICID)
                  ENDDO 
               ENDDO

               IF (IERROR == 0) THEN
                  TRANS_DONE(ICID) = 'Y'
                  IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '75' )
              ENDIF

            ENDDO big_loop

            NUM_LEFT_AT_END = 0                            ! Find out how many CORD1 systems still need to be processed
            DO K=1,NCORD
               IF (TRANS_DONE(K) == 'N') THEN
                  NUM_LEFT_AT_END = NUM_LEFT_AT_END + 1
               ENDIF
            ENDDO
            IF (NUM_LEFT_AT_END < NUM_LEFT_AT_BEG) THEN
               CONTINUE
            ELSE
               IERROR = IERROR + 1
               WRITE(ERR,1318)
               DO K=1,NCORD
                  IF (TRANS_DONE(K) == 'N') THEN
                     WRITE(ERR,7001) CORD_NAME(K), CORD(K,2)
                  ENDIF
               ENDDO 
               WRITE(F06,1318) 
               DO K=1,NCORD
                  IF (TRANS_DONE(K) == 'N') THEN
                     WRITE(F06,7001) CORD_NAME(K), CORD(K,2)
                  ENDIF
               ENDDO 
               CALL OUTA_HERE ( 'Y' )
            ENDIF

         ENDIF

      ENDDO main7

! **********************************************************************************************************************************
! Phase 8: Check that all systems have been transformed to basic and rewrite RID's in array CORD to reflect this

      IF (IERROR > 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1304)
         WRITE(F06,1304)
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      IERROR = 0                                           ! Check to make sure all systems have been transformed to basic
      DO I=1,NCORD
         IF (TRANS_DONE(I) /= 'Y') THEN
            IERROR = IERROR + 1
         ENDIF
      ENDDO
      IF (IERROR == 0) THEN                                ! --- No error so reset all RID's that have not already been reset to 0
         DO I=1,NCORD
            DO J=3,5
               IF (CORD(I,J) /= 0) THEN
                  DO K=1,NCORD
                     IF (CORD(K,2) == CORD(I,J)) ICID = K
                  ENDDO
               ENDIF
               IF (TRANS_DONE(ICID) == 'Y') THEN
                  CORD(I,J) = 0
               ENDIF
            ENDDO
         ENDDO
      ELSE                                                 ! --- Something wrong - all systems have not been transformed to basic
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1319) SUBR_NAME
         DO K=1,NCORD
            IF (TRANS_DONE(K) == 'N') THEN
               WRITE(ERR,7001) CORD_NAME(K), CORD(K,2)
            ENDIF
         ENDDO 
         WRITE(F06,1319) SUBR_NAME
         DO K=1,NCORD
            IF (TRANS_DONE(K) == 'N') THEN
               WRITE(F06,7001) CORD_NAME(K), CORD(K,2)
            ENDIF
         ENDDO 
         CALL OUTA_HERE ( 'Y' )
      ENDIF
            
      IF ((PRTCORD == 1) .OR. (PRTCORD == 2)) CALL PARAM_PRTCORD_OUTPUT ( '81' )

! Check IERROR and quit if > 0

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1300 FORMAT(' *ERROR  1300: REFERENCE COORDINATE SYSTEM NUMBER ',I8,' ON  COORDINATE SYSTEM NUMBER ',I8,' IS UNDEFINED')

 1302 FORMAT(' *ERROR  1302: COORDINATE SYSTEM NUMBER ',I8,' HAS A CIRCULAR REFERENCE')

 1303 FORMAT(' *ERROR  1303: COORDINATE SYSTEM NUMBER ',I8,' DOES NOT HAVE A FINAL REFERENCE TO THE BASIC SYSTEM')

 1304 FORMAT(' PROCESSING TERMINATED DUE TO PRIOR COORDINATE SYSTEM ERRORS')

 1305 FORMAT(' *ERROR  1305: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DIMENSION OF ARRAY RID_ARRAY TOO SMALL')

 1306 FORMAT(' *ERROR  1306: POINTS A AND B ON COORDINATE SYSTEM NUMBER ',I8,' ARE TOO CLOSE')

 1307 FORMAT(' *ERROR  1307: POINTS A AND C ON COORDINATE SYSTEM NUMBER ',I8,' ARE TOO CLOSE')

 1308 FORMAT(' *ERROR  1308: VECTOR FROM POINT A TO POINT B, AND VECTOR FROM POINT A TO POINT C ARE PARALLEL ON COORD SYSTEM ',I8)

 1315 FORMAT(' *ERROR  1315: GRID ',I8,' UNDEFINED ON COORDINATE SYSTEM NUMBER ',I8)

 1316 FORMAT(' *ERROR  1316: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' VARIABLE ICID_RIDO = ',I8,'. MUST BE A POSITIVE INTEGER')

 1317 FORMAT(' *ERROR  1317: CANNOT FIND COORD TRANSFORMATION MATRIX FOR ',A,I8,' (INTERNAL COORD SYS ID IS = ',I8,')')

 1318 FORMAT(' *ERROR  1318: CANNOT COMPLETE THE TRANSFORMATION TO BASIC FOR THE COORD SYSTEMS LISTED BELOW:')

 1319 FORMAT(' *ERROR  1319: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TRANSFORMATION TO BASIC HAS NOT BEEN COMPLETED FOR THE FOLLOWING COORD SYSTEMS:')

 1321 FORMAT(' *ERROR  1403: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                     ,/,14X,' VARIABLE ',A,' HAS AN INVALID VALUE = ',I8)

 7001 FORMAT('               ',A,2X,I8)


! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################
 
      SUBROUTINE CORDCHK ( IERROR )
     
! Checks array CORD to make sure that there are not more than 1 coord systems with the same ID. It does this by
! sorting the coord system ID's and then checking the sorted dummy array for uniqueness
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NCORD, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CORD_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CORD
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CORDCHK'

      INTEGER(LONG), INTENT(OUT)      :: IERROR            ! Count of the number of duplicate coord system ID's
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: DUMCORD(NCORD)    ! Dummy array of coord system ID's sorted
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CORD_PROC_BEGEND
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Create DUMCORD to be an array of the coordinate system ID's
 
      DO I=1,NCORD
         DUMCORD(I) = CORD(I,2)
      ENDDO 
 
! Sort cord system ID's into numerically increasing order using the shell sort method
 
      IF (NCORD > 1) THEN
         CALL SORT_INT1 ( SUBR_NAME, 'CORD', NCORD, DUMCORD )
      ENDIF
 
! Check for duplicate numbers
  
      IERROR = 0
      DO I=1,NCORD-1
         IF (DUMCORD(I) == DUMCORD(I+1)) THEN
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1309) DUMCORD(I+1)
            WRITE(F06,1309) DUMCORD(I+1)
         ENDIF
      ENDDO 
    
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1309 FORMAT(' *ERROR  1309: COORDINATE SYSTEM NUMBER ',I8,' IS A DUPLICATE.')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE CORDCHK
 
! ##################################################################################################################################

      SUBROUTINE PARAM_PRTCORD_OUTPUT ( WHICH )

      CHARACTER( 2*BYTE)              :: WHICH             ! Decides what to print out for this call to this subr

                                                           ! Char array with RID_ARRAY values
      CHARACTER( 9*BYTE)              :: CRID_ARRAY(NCORD+1,3*NCORD1+NCORD2)
      CHARACTER( 9*BYTE)              :: CRID_ARRAY_BLANK
      CHARACTER( 6*BYTE)              :: NAME1
      CHARACTER(24*BYTE)              :: NAME2

      INTEGER(LONG)                   :: II,JJ             ! Local DO loop indices
      INTEGER(LONG)                   :: IA,IB,IC          ! Array indices

! **********************************************************************************************************************************
      IF      (WHICH == '11') THEN

         WRITE(F06,1101)
         WRITE(F06,1102)
         WRITE(F06,1103) ' 1'
         WRITE(F06,1104)

      ELSE IF (WHICH == '12') THEN
         WRITE(F06,1201)
            NAME1 = '      '
         DO II=1,NCORD
            IF (CORD(II,1) == 11) NAME1 = 'CORD1R'
            IF (CORD(II,1) == 12) NAME1 = 'CORD1C'
            IF (CORD(II,1) == 13) NAME1 = 'CORD1S'
            IF (CORD(II,1) == 21) NAME1 = 'CORD2R'
            IF (CORD(II,1) == 22) NAME1 = 'CORD2C'
            IF (CORD(II,1) == 23) NAME1 = 'CORD2S'
            IF       ((CORD(II,1) == 11) .OR. (CORD(II,1) == 12) .OR. (CORD(II,1) == 13)) THEN
               WRITE(F06,1202) NAME1, (CORD(II,JJ),JJ=2,5)
             ELSE IF ((CORD(II,1) == 21) .OR. (CORD(II,1) == 22) .OR. (CORD(II,1) == 23)) THEN
               WRITE(F06,1202) NAME1, (CORD(II,JJ),JJ=2,3)
            ENDIF
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)
            
         WRITE(F06,1203)
         DO II=1,NCORD
            NAME1(1:) = ' '   ;   NAME2(1:) = ' '
            IF (CORD(II,1) == 11) THEN   ;   NAME1 = 'CORD1R'   ;   NAME2 = ' from coords on GRID'   ;   ENDIF
            IF (CORD(II,1) == 12) THEN   ;   NAME1 = 'CORD1C'   ;   NAME2 = ' from coords on GRID'   ;   ENDIF
            IF (CORD(II,1) == 13) THEN   ;   NAME1 = 'CORD1S'   ;   NAME2 = ' from coords on GRID'   ;   ENDIF
            IF (CORD(II,1) == 21) THEN   ;   NAME1 = 'CORD2R'   ;   NAME2 = ' from coords on CORD'   ;   ENDIF
            IF (CORD(II,1) == 22) THEN   ;   NAME1 = 'CORD2C'   ;   NAME2 = ' from coords on CORD'   ;   ENDIF
            IF (CORD(II,1) == 23) THEN   ;   NAME1 = 'CORD2S'   ;   NAME2 = ' from coords on CORD'   ;   ENDIF
            WRITE(F06,1204) NAME1, CORD(II,2),'A',(RCORD(II,JJ),JJ=1,3), NAME2, GA(I)
            WRITE(F06,1205)                   'B',(RCORD(II,JJ),JJ=4,6), NAME2, GB(I)
            WRITE(F06,1205)                   'C',(RCORD(II,JJ),JJ=7,9), NAME2, GC(I)
            WRITE(F06,*)
         ENDDO
         WRITE(F06,*)

      ELSE IF (WHICH == '21') THEN

         WRITE(F06,1102)
         WRITE(F06,1103) ' 2'
         WRITE(F06,2100)

         WRITE(F06,2101)
         DO II=1,NCORD
            NAME1 = '      '
            IF (CORD(II,1) == 11) NAME1 = 'CORD1R'
            IF (CORD(II,1) == 12) NAME1 = 'CORD1C'
            IF (CORD(II,1) == 13) NAME1 = 'CORD1S'
            IF (CORD(II,1) == 21) NAME1 = 'CORD2R'
            IF (CORD(II,1) == 22) NAME1 = 'CORD2C'
            IF (CORD(II,1) == 23) NAME1 = 'CORD2S'
            WRITE(F06,2102) NAME1, CORD(II,2),'A',(RCORD(II,JJ),JJ=1,3), CORD(II,3)
            WRITE(F06,2103)                   'B',(RCORD(II,JJ),JJ=4,6), CORD(II,3)
            WRITE(F06,2103)                   'C',(RCORD(II,JJ),JJ=7,9), CORD(II,3)
            WRITE(F06,*)
         ENDDO
         WRITE(F06,*)

      ELSE IF (WHICH == '31') THEN

         WRITE(F06,1102)
         WRITE(F06,1103) ' 3'
         WRITE(F06,3101)
         WRITE(F06,3102)

      ELSE IF (WHICH == '32') THEN

         WRITE(F06,*)
         WRITE(F06,*)
         WRITE(F06,3201) CID
         WRITE(F06,3202) CID,RID

      ELSE IF (WHICH == '33') THEN

         WRITE(F06,3202) CIDK,RID

      ELSE IF (WHICH == '34') THEN

         WRITE(F06,*)
         WRITE(F06,*)
         WRITE(F06,3401)
         DO II=1,NCORD+1
            DO JJ=1,3*NCORD1+NCORD2
               IF      (RID_ARRAY(II,JJ) /= -99) THEN
                  WRITE(CRID_ARRAY(II,JJ),'(I9)') RID_ARRAY(II,JJ)
               ELSE
                  WRITE(CRID_ARRAY(II,JJ),'(A9)') '        '
               ENDIF
            ENDDO
         ENDDO

         DO II = 1,NCORD+1
            CRID_ARRAY_BLANK = 'Y'
            DO JJ=1,3*NCORD1+NCORD2
               IF (CRID_ARRAY(II,JJ)(1:) /= ' ') CRID_ARRAY_BLANK = 'N'
            ENDDO
            IF (CRID_ARRAY_BLANK == 'N') THEN
               IF (II == 1) THEN
                  WRITE(F06,3404)     (CRID_ARRAY(II,JJ),JJ=1,3*NCORD1+NCORD2)
               ELSE
                  WRITE(F06,3405) II-1, (CRID_ARRAY(II,JJ),JJ=1,3*NCORD1+NCORD2)
               ENDIF
            ENDIF
         ENDDO
         WRITE(F06,*)

      ELSE IF (WHICH == '41') THEN

         WRITE(F06,1102)
         WRITE(F06,1103) ' 4'
         WRITE(F06,4101)
         WRITE(F06,4102)
         WRITE(F06,4110) (II,II=1,NCORD)
         WRITE(F06,4111) (CORD_NAME(II),II=1,NCORD)
         WRITE(F06,4112) (' --------',II=1,NCORD)
         WRITE(F06,4104) (CASCADE_PROC_ARRAY(1,II),II=1,NCORD)
         WRITE(F06,4105) (CASCADE_PROC_ARRAY(2,II),II=1,NCORD)
         WRITE(F06,4106) (CASCADE_PROC_ARRAY(3,II),II=1,NCORD)
         WRITE(F06,*)
         WRITE(F06,4107)
         WRITE(F06,4108)
         WRITE(F06,4109)
         WRITE(F06,*)

      ELSE IF (WHICH == '51') THEN
         WRITE(F06,1102)
         WRITE(F06,1103) ' 5'
         WRITE(F06,5101)
         WRITE(F06,5102) 'relative to it''s reference system. (only for CORD2 systems at this point)'

      ELSE IF (WHICH == '52') THEN
         WRITE(F06,5202) CID, 'it''s reference coordiate system', RID, (RCORD(I,JJ),JJ=1,3), CID, RID,                             &
                                   (RCORD(I,JJ),JJ=4,6), RID, CID, (RCORD(I,JJ),JJ=7,9), RID, CID, (RCORD(I,JJ),JJ=10,12), RID, CID

      ELSE IF (WHICH == '61') THEN
         WRITE(F06,1102)
         WRITE(F06,1103) ' 6'
         WRITE(F06,6100)
         WRITE(F06,5102) 'relative to the basic system. (only for CORD2 systems at this point)'

      ELSE IF (WHICH == '62') THEN
         WRITE(F06,5202) CORD(I,2), 'the basic system', CORD(I,3), (RCORD(I,JJ),JJ=1,3),                                           &
                                                        CORD(I,2), CORD(I,3), (RCORD(I,JJ),JJ=4,6),                                &
                                                        CORD(I,3), CORD(I,2), (RCORD(I,JJ),JJ=7,9),                                &
                                                        CORD(I,3), CORD(I,2), (RCORD(I,JJ),JJ=10,12), CORD(I,3), CORD(I,2)

      ELSE IF (WHICH == '63') THEN
         WRITE(F06,*)
         WRITE(F06,*) '  Test to see which coordinate transformation matrices have been cascaded back to basic (0) coord system'
         WRITE(F06,*) '  ----'
         WRITE(F06,6301) (CORD_NAME(II),II=1,NCORD)
         WRITE(F06,6302) (CORD(II,2),II=1,NCORD)
         WRITE(F06,6303) (TRANS_DONE(II),II=1,NCORD)
         WRITE(F06,*)
         WRITE(F06,*)

      ELSE IF (WHICH == '71') THEN
         WRITE(F06,1102)
         WRITE(F06,1103) ' 7'
         WRITE(F06,7101)

      ELSE IF (WHICH == '72') THEN
         WRITE(F06,7201) NUM_LEFT_AT_BEG
         DO II=1,NCORD
            IF (TRANS_DONE(II) == 'N') THEN
               IA = -99
               IB = -99
               IC = -99
               DO JJ=1,NCORD
                  IF (CORD(JJ,2) == CORD(II,3)) IA = JJ
                  IF (CORD(JJ,2) == CORD(II,4)) IB = JJ
                  IF (CORD(JJ,2) == CORD(II,5)) IC = JJ
               ENDDO
               IF ((IA >= 0) .AND. (IB >= 0) .AND. (IC >= 0)) THEN
                  WRITE(F06,7202) CORD_NAME(II), CORD(II,2), CORD(II,3), TRANS_DONE(IA), CORD(II,4), TRANS_DONE(IB),               &
                                                            CORD(II,5), TRANS_DONE(IC)
               ENDIF
            ENDIF
         ENDDO 
         WRITE(F06,*)

      ELSE IF (WHICH == '73') THEN
         WRITE(F06,7301) CORD_NAME(J), CID, GA(J), GB(J), GC(J)
         WRITE(F06,*)
         WRITE(F06,7302) CID, GA(J), CIDA, (RGA(J,K),K=1,3)
         WRITE(F06,7302) CID, GB(J), CIDB, (RGB(J,K),K=1,3)
         WRITE(F06,7302) CID, GC(J), CIDC, (RGC(J,K),K=1,3)
         WRITE(F06,*)

      ELSE IF (WHICH == '74') THEN
         WRITE(F06,*) '  Vectors used in the transformation of CORD1 systems:'
         WRITE(F06,*) '  -------'
         WRITE(F06,7402) 'V_0_CIDA', CIDA, (V_0_CIDA(JJ),JJ=1,3)
         WRITE(F06,7402) 'V_0_CIDB', CIDB, (V_0_CIDB(JJ),JJ=1,3)
         WRITE(F06,7402) 'V_0_CIDC', CIDC, (V_0_CIDC(JJ),JJ=1,3)
         WRITE(F06,*)

         WRITE(F06,7403) 'V_CIDB_CID_PTA', CIDA, 'A', CID, (V_CIDA_CID_PTA(JJ),JJ=1,3)
         WRITE(F06,7403) 'V_CIDB_CID_PTB', CIDB, 'B', CID, (V_CIDB_CID_PTB(JJ),JJ=1,3)
         WRITE(F06,7403) 'V_CIDB_CID_PTC', CIDC, 'C', CID, (V_CIDC_CID_PTC(JJ),JJ=1,3)
         WRITE(F06,*)

         WRITE(F06,7404) '     V0A            :in sys 0 from origin of sys        0 to pt A on coord sys ', CID, (V0A(JJ),JJ=1,3)
         WRITE(F06,7404) '     V0B            :in sys 0 from origin of sys        0 to pt B on coord sys ', CID, (V0B(JJ),JJ=1,3)
         WRITE(F06,7404) '     V0C            :in sys 0 from origin of sys        0 to pt C on coord sys ', CID, (V0C(JJ),JJ=1,3)
         WRITE(F06,*)

         WRITE(F06,7405) '     VK             :in sys 0 from point A to point B on coord sys             ', CID, (VK(JJ) ,JJ=1,3)
         WRITE(F06,7405) '     VI             :in sys 0 from point A to point C on coord sys             ', CID, (VI(JJ) ,JJ=1,3)
         WRITE(F06,*)

         WRITE(F06,7406) '     IVEC           :in sys 0 in x direction for coord sys                     ', CID, (IVEC(JJ) ,JJ=1,3)
         WRITE(F06,7406) '     JVEC           :in sys 0 in y direction for coord sys                     ', CID, (JVEC(JJ) ,JJ=1,3)
         WRITE(F06,7406) '     KVEC           :in sys 0 in z direction for coord sys                     ', CID, (KVEC(JJ) ,JJ=1,3)
         WRITE(F06,*)

      ELSE IF (WHICH == '75') THEN
         WRITE(F06,5202) CORD(J,2), 'the basic system',0, (RCORD(J,JJ),JJ= 1, 3), CORD(J,2), 0,                                    &
                                                          (RCORD(J,JJ),JJ= 4, 6), 0, CORD(J,2),                                    &
                                                          (RCORD(J,JJ),JJ= 7, 9), 0, CORD(J,2),                                    &
                                                          (RCORD(J,JJ),JJ=10,12), 0, CORD(J,2)
         WRITE(F06,7501) CORD_NAME(J),CID

      ELSE IF (WHICH == '81') THEN
         WRITE(F06,1102)
         WRITE(F06,1103) ' 8'
         WRITE(F06,8101)

         WRITE(F06,*) 'Test to see which coordinate transformation matrices have been cascaded back to basic (0) coord system'
         WRITE(F06,*) '----'
         WRITE(F06,6301) (CORD_NAME(II),II=1,NCORD)
         WRITE(F06,6302) (CORD(II,2),II=1,NCORD)
         WRITE(F06,6303) (TRANS_DONE(II),II=1,NCORD)
         WRITE(F06,*)
         WRITE(F06,*)

         WRITE(F06,8104)
         WRITE(F06,8105)
         DO II=1,NCORD
            CORD_TYPE = CORD(II,1)
            IF ((CORD_TYPE == 11) .OR. (CORD_TYPE == 11) .OR. (CORD_TYPE == 11)) THEN
               WRITE(F06,8106) CORD_NAME(II), (CORD(II,JJ),JJ=2,5)
            ELSE
               WRITE(F06,8106) CORD_NAME(II), (CORD(II,JJ),JJ=2,3)
            ENDIF
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

         WRITE(F06,8107)
         WRITE(F06,8108)
         DO II=1,NCORD
            WRITE(F06,8109) ' CID ', CORD(II,2),' |',(RCORD(II,JJ),JJ= 1, 3),' |', (RCORD(II,JJ),JJ= 4, 6),' |'                    &
                                                    ,(RCORD(II,JJ),JJ= 7, 9),' |', (RCORD(II,JJ),JJ=10,12),' |'
         ENDDO
         WRITE(F06,*)

 8107 FORMAT(42X,'A R R A Y   R C O R D   W I T H   O R I G I N S   A N D   3 X 3   T R A N F O R M A T I O N   M A T R I C E S',/,&
             70X,'T H A T   T R A N S F O R M   C I D   T O   B A S I C',/)
 
 8108 FORMAT(14X,'|         Coords of origin of CID        |  Row 1 of coord transformation matrix  |',                            &
                  '  Row 2 of coord transformation matrix  |  Row 3 of coord transformation matrix')

 8109 FORMAT(A,I8,A,3ES13.6,A,3ES13.6,A,3ES13.6,A,3ES13.6,A)
         WRITE(F06,8901)

      ENDIF

! **********************************************************************************************************************************
 1101 FORMAT(' ___________________________________________________________________________________________________________________'&
            ,'________________'                                                                                                ,//,&
             ' ::::::::::::::::::::::::::::::::::::::START PARAM PRTCORD OUTPUT FROM SUBROUTINE CORD_PROC::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1102 FORMAT(1X,'****************************************************************************************************************',&
                '*******************')

 1103 FORMAT(58X,'P H A S E   ',A,/,58X,'--------------',/)

 1104 FORMAT(' Phase 1: For CORD1R: (1) Change grid numbers (that were put into array CORD when B.D. was read) to the reference',  &
                                      ' coordinate system'                                                                      ,/,&
             '                          number for that grid.'                                                                  ,/,&
             '                      (2) Put coords of the reference grids on the CORD1R into array RCORD.',//)

 1201 FORMAT(' Array CORD after replacing the 3 grid numbers for CORD1 entries with the CORD1,2 reference system for that grid' ,/,&
             ' ---------- 1st col is the coord type (CORD1R,C,S or CORD2R,C,S).'                                                ,/,&
             '            2nd col is the coordinate system number (CID).'                                                       ,/,&
             '            3rd col and on give the ref coord system numbers (RID) for the CID. CORD2 types have 1 RID, CORD1',      &
             ' types have 3 RID''s',//,42X,'Coord type      CID      RID''s ->',/)

 1202 FORMAT('                                            ',A,2X,4I9)     

 1203 FORMAT(' Array RCORD (cols 1-3) with coords of the 3 reference points from CORD2 entries and coords of the 3 grids on',      &
             ' CORD1 entries',/,&
             ' -----------',/)

 1204 FORMAT('     For ',A,' system ',I8,' the location of pt '   ,A,' is:',3(1ES14.6),A,I8)

 1205 FORMAT('                                                   ',A,' is:',3(1ES14.6),A,I8)

 2100 FORMAT(' Phase 2: Change coords on cylindrical and spherical systems to coords in terms of the defining rectangular axes',   &
             ' of that system',//)

 2101 FORMAT(' Array RCORD (cols 1-3) with coords of the 3 reference points in terms of the defining rectangular axes of the',     &
             ' reference system',/,' -----------',/)

 2102 FORMAT('     For ',A,' system ',I8,' the location of pt '   ,A,' is:',3(1ES14.6),' in reference system ',I8)

 2103 FORMAT('                                                   ',A,' is:',3(1ES14.6),' in reference system ',I8)

 3101 FORMAT(' Phase 3: Check coordinate system data for logic:'                                                                ,/,&
             '          (1) There must be a final reference to the basic system (0)'                                            ,/,&
             '          (2) There can be no circular references (i.e. a coord system can reference any number of other coord sys', &
                          ' but cannot'                                                                                         ,/,&
             '              reference itself in that chain of refs).'                                                           ,/,&
             '          (3) Create RID_ARRAY which shows the chain of references of each coord system',//)

 3102 FORMAT(' Trace of coordinate chain of references to basic system:'                                                        ,/,&
             ' --------------------------------------------------------')

 3201 FORMAT('   References for coordinate system number ',I8                                                                    ,/&
             '   ------------------------------------------------')

 3202 FORMAT('     Coordinate system ',I8,' references coordinate system ',I8)

 3401 FORMAT('   RID_ARRAY: The cols below show the trace of a coord sys (number CID) through it''s reference systems',            &
             ' (RID''s in the cols below CID)',/,                                                                                  &
             '   ---------  to the basic system (0). ICID is the internal coord system ID for actual coord system CID.',           &
             ' Each CORD1C,R,S system will',/,                                                                                     &
             '              have 3 cols (one for each reference point on the CORD1C,R,S BDF entry).',          &
             ' Only 1 col for each CID will be used in',/,                                                                         &
             '              cascading the RID''s back to CID to get the coord trnasformations from basic to CID.',/)

 3404 FORMAT('     CID     ->',32767A9)

 3405 FORMAT('     RID',I4,' ->',32767A9)

 4101 FORMAT(' Phase 4: Solve for array CASCADE_PROC_ARRAY. It will be used to do the cascading of coord references to get the',   &
                     ' transformation from'                                                                                     ,/,&
             '         CID to basic',//)

 4102 FORMAT('   CASCADE_PROC_ARRAY array: (info from above RID_ARRAY used in the cascading of RID''s from CID to basic(0))',/,    &
             '   -------------------')

 4104 FORMAT('     Row 1:',32767I9)

 4105 FORMAT('     Row 2:',32767I9)

 4106 FORMAT('     Row 3:',32767I9)

 4107 FORMAT('     Row 1 of CASCADE_PROC_ARRAY is the CID coord sys number from RID_ARRAY')

 4108 FORMAT('     Row 2 of CASCADE_PROC_ARRAY is the col number in RID_ARRAY to use when solving for the coord transformation',   &
             ' from basic to CID')

 4109 FORMAT('     Row 3 of CASCADE_PROC_ARRAY is the num of coord system refs above the basic sys (0) in RID_ARRAY',              &
             ' (cascade from basic up to CID)')

 4110 FORMAT('     ICID  ',32767I9)

 4111 FORMAT('     Type  ',32767(3X,A6))

 4112 FORMAT('           ',32767A9)

 5101 FORMAT(' Phase 5: Get the unit vectors in each of the CORD2 coord systems in terms of their reference system. Put the',      &
                      ' results into array'                                                                                     ,/,&
             '          RCORD where the transformation matrices go. These are NOT the final transformations; they are the',        &
                      ' transformations from a'                                                                                 ,/,&
             '          CID to its reference system and ONLY for CORD2 systems (CORD1 systams will be handled later).',//)

 5102 FORMAT(' RCORD array with data for a coord system ',A,/,                                                                     &
             ' -----------',/)

 5202 FORMAT ('   RCORD array for coordinate system CID = ',I9,' with values relative to ',A,                                      &
              ' (RID) = ',I9   ,/,&
              '   -----------'                                                                                                  ,/,&
              '     Cols  1- 3: ',3(1ES13.6),': Origin of coord system ',I8,' as measured in coord system ',I8,/                ,/,&
              '     Cols  4- 6: ',3(1ES13.6),': Row 1 of matrix that transforms a vector to ',I9,' from ',I9                    ,/,&
              '     Cols  7- 9: ',3(1ES13.6),': Row 2 of matrix that transforms a vector to ',I9,' from ',I9                    ,/,&
              '     Cols 10-12: ',3(1ES13.6),': Row 3 of matrix that transforms a vector to ',I9,' from ',I9,/)

 6100 FORMAT(' Phase 6:  Calculate transformations to basic (zero ) system for all CORD2 coordinate systems.',//)

 6101 FORMAT(' Coord transformations between principal directions of CORD2 basic coord systems and all other coord systems:'    ,/,&
             ' -----------------------------------------------------------------------------------------------------------',/)

 6102 FORMAT('   The following matrix will transform a vector to coord system ',I8,' from a vector in coord system ',I8          ,/&
             '   ------------------------------------------------------------------------------------------------------------')

 6103 FORMAT(3(1ES20.8))

 6104 FORMAT(' Basic coords of the origin of CORD2 coord system number ',I8,' are : ',3(1ES16.8))

 6301 FORMAT('   Coordinate system type         :',32767(3X,A6))

 6302 FORMAT('   Coordinate system ID           :',32767I9)

 6303 FORMAT('   Is transformation to basic done?',32767A9)

 7201 FORMAT(' There are',I8,' CORD1 entries remaining to be processed at this time. The table below shows which systems need to', &
             ' be processed,',/,' the reference systems they use, and whether the reference systems have been transformed to basic'&
            ,' (Y or N).',/,' In order to process a CORD1 system all 3 of the ref systems myst have been transformed to basic',/)

 7101 FORMAT(' Phase 7: Process CORD1 entries to get transformations to basic and put results into array RCORD',//)

 7202 FORMAT(3X,A,2X,I8,' has 3 ref systems (Y/N means is it transformed to basic?):',3(I8,' (',A,')'))

 7301 FORMAT(' Working on ',A,2X,I9,' with 3 reference grids: ',3I9,/,' ----------------------------')

 7302 FORMAT('   For coord sys ',I9,' the coords of grid ',I9,' in ref system ',I9,' are:     ',3(1ES14.6))

 7402 FORMAT(5X,A,7X,':in sys 0 from origin of sys        0 to origin of  sys    ', I8,' =',3(1ES14.6))

 7403 FORMAT(5X,A,1X,':in sys 0 from origin of sys ',I8,' to point ',A,' of sys    '   ,I8,' =',3(1ES14.6))

 7404 FORMAT(A,I8,' =', 3(1ES14.6))

 7405 FORMAT(A,I8,' =', 3(1ES14.6))

 7406 FORMAT(A,I8,' =', 3(1ES14.6))

 7501 FORMAT('   Transformation to basic is completed for ',A,2X,I8                                                             ,/,&
             '   =========================================================',//)
 8101 FORMAT(' Phase 8: Check that all systems have been transformed to basic and rewrite RID''s in array CORD to reflect this',//)

 8104 FORMAT(' Arrays CORD and RCORD at end of subr CORD_PROC',/,' ----------------------------------------------',/)

 8105 FORMAT(66X,'A R R A Y   C O R D',/,61X,'(all RID''s should be zero now)',//,60X,'Coord type      CID      RID''s ->',/)

 8106 FORMAT(62X,A,2X,5I9)

 8901 FORMAT(' :::::::::::::::::::::::::::::::::::::::END PARAM PRTCORD OUTPUT FROM SUBROUTINE CORD_PROC:::::::::::::::::::::::::' &
             ,':::::::::::::::::'                                                                                               ,/,&
             ' ___________________________________________________________________________________________________________________'&
            ,'________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE PARAM_PRTCORD_OUTPUT

      END SUBROUTINE CORD_PROC
