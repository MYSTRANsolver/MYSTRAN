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
 
      SUBROUTINE RFORCE_PROC
 
! RFORCE load processor. Forces on grids for an RFORCE are:

!           Fi = Mi*[W x (W x (Ri - Ra)) + A x (Ri - Ra)]

! where x means a vector cross product and:

!           Mi = 6x6 mass matrix at grid i
!           W  = angular velocity of the model about grid i (variable ANG_VEL)
!           Ri = radius from basic coord system origin to grid i (variable RI)
!           Ra = radius from basic coord system origin to the reference point for angular velocity/accel (variable RA)
!           A  = angular acceleration (variable ANG_ACC)
 
! The input B.D. RFORCE load data is transferred to system force data in the SYS_LOAD array for DOF's in the G set.

! File LINK1U was written when RFORCE B.D entries were read, with one record for each such card.
! There are NRFORCE total number of records written to file LINK1U with each record containing:

!               SETID         = Load set ID
!               ACID_L        = Local coord sys ID that RFORCE load is given in
!               RFORCE_GRID   = ID of grid that rotational (components 4, 5, 6) RFORCE velocity/accels are about
!               SCALEF_AV     = Scale factor for angular velocity
!               SCALEF_AA     = Scale factor for angular accel
!               VEC(1-3)      = 3 components of the vector for the velocity and/or accel

! The process in creating array SYS_LOAD from this information is as follows:

!  (1) For each record (1 to NRFORCE) in file LINK1U:
 
!      (a) Read a record 

!      (b) Transform coords from local (on RFORCE card) to basic. Transformation to global is done later (see 2a(iii))

!      (d) Write records, similar to those in LINK1U, but in global coords, to a scratch file (SCRATCH-991).

!  (2) For each subcase (1 to NSUB):

!      (a) Generate LSID, RSID tables of load set ID's/scale factors for load sets for this subcase:

!          (  i) LLOADC is the max number of pairs of scale factors/load set ID's over all LOAD Bulk Data cards
!                including the pair defined by the set ID and overall scale factor on the LOAD Bulk Data card.
!                LSID and RSID are dimensioned 1 to LLOADC and:
!                   LSID(1) is always the load set ID requested in Case Control for this subcase.
!                   RSID(1) is always 1.0

!          ( ii) If the load set requested in Case Control is not a set ID from a LOAD Bulk data card then the
!                load must be on a separate RFORCE card in which case LSID(1) and RSID(1) are all that is needed
!                to define the load set contribution due to the RFORCE card for this subcase.
 
!          (iii) If the load set requested in Case Control is a set ID from a LOAD Bulk data card then the ramainder
!                (K = 2,LLOADC) of entries into LSID and RSID will be the pairs of load set ID's/scale factors from
!                that LOAD Bulk Data card (with RSID also multiplied by the overall scale factor on the LOAD Bulk data
!                card. The load set ID's are in array LOAD_SIDS(i,j) created when LOAD Bulk Data cards were read.
!                The scale factors are in array LOAD_FACS(i,j) also created when LOAD Bulk Data cards were read.
!                Note, there may not be as many as LLOADC pairs of set ID's/scale factors on a given LOAD Bulk Data
!                card since LLOADC is the max, from all LOAD Bulk Data cards, of pairs.
!                Thus, the entries in LSID from the last entry (for a given LOAD card) to LLOADC will be zero (LSID
!                was initialized to zero). This fact is used in a DO loop to EXIT when LSID(K) = 0 

!      (b) For each record in SCRATCH-991 (1 to NRFORCE)

!          (  i) Read a record
  
!          ( ii) Scan LSID and RSID to get the scale factor (SCALE) for the ACCEL_RB components in SETID, if this
!                RFORCE's set ID is in LSID. When found, reset RFORCE vector components to ACCEL_RB = SCALE*ACCEL_RB.
!                At this point ACCEL_RB has the correct magnitudes and is in basic coordinates.                

!          (iii) For a grid point, determine if a transformation of ACCEL_RB to global is needed and transform it if so.

!          ( iv) Calculate accel at a grid (ACCEL_I) based on rigid body motion of ACCEL_RB

!          (  v) Get the 6 x 6 mass matrix for a grid point times ACCEL_I to get RFORCE forces at this grid

!          ( vi) Load the RFORCE forces into the SYS_LOAD (systems load) array
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, FILE_NAM_MAXLEN, L1U, LINK1U, L1U_MSG, SC1, SCR, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LLOADC, NCORD, NRFORCE, NGRID, NLOAD, NSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  RFORCE_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  SUPWARN
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  CORD, GRID, GRID_ID, LOAD_FACS, LOAD_SIDS, RCORD, RGRID, SYS_LOAD, SUBLOD
 
      USE RFORCE_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RFORCE_PROC'
      CHARACTER( 1*BYTE)              :: FOUND             ! Indicator on whether we found something we were looking for
      CHARACTER( 1*BYTE)              :: GRID_MGG_FND      ! Indicator on whether a mass matrix was found in MGG for a given grid
      CHARACTER( 8*BYTE)              :: NAME              ! Name for output error purposes
      CHARACTER(24*BYTE)              :: MESSAG            ! File description.  
      CHARACTER(FILE_NAM_MAXLEN*BYTE) :: SCRFIL            ! File name
 
      INTEGER(LONG)                   :: ACID      = 0     ! A coord system ID (actual)
      INTEGER(LONG), PARAMETER        :: ACID_0    = 0     ! Basic coord system
      INTEGER(LONG)                   :: ACID_L            ! Actual local  coord sys ID on FORCE or MOMENT card
      INTEGER(LONG)                   :: ACID_G            ! Actual global coord sys ID for an actual grid
      INTEGER(LONG)                   :: CID_ERR   = 0     ! Count of coord systems undefined
      INTEGER(LONG)                   :: GID_ERR   = 0     ! Count of grids undefined
      INTEGER(LONG)                   :: GDOF              ! G-set DOF number for a grid
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no. in array TDOF where G-set DOF's are kept 
      INTEGER(LONG)                   :: I,J,K,L,m         ! DO loop indices
      INTEGER(LONG)                   :: ICID              ! Internal coordinate system ID for ACID_L or ACID_G
      INTEGER(LONG)                   :: IERRT             ! Total number of errors found
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: LSID(LLOADC+1)    ! Array of load SID's, for RFORCE cards, needed for one S/C 
      INTEGER(LONG)                   :: NCOLA             ! No. cols in a matrix. For subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NCOLB             ! No. cols in a matrix. For subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NROWA             ! No. rows in a matrix. For subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NSID              ! Count on no. of pairs of entries on a LOAD B.D. card (<= LLOADC) 
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: READ_ERR  = 0     ! Cum. count of errors as we read, and check cards from file LINK1K
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: RFORCE_GRD        ! ID of grid that rotational RFOECE vel/accels are about
      INTEGER(LONG)                   :: RFORCE_GRD_ROW_NUM! Row number in array GRID_ID where an actual grid ID is found
      INTEGER(LONG)                   :: ROW_NUM           ! Row no. in array TDOF corresponding to GDOF 
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG)                   :: SETID             ! Load set ID read from record in file LINK1U
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RFORCE_PROC_BEGEND
      
      REAL(DOUBLE)                    :: ACCEL_I(6)        ! 6 components of accel due to gravity at a grid
      REAL(DOUBLE)                    :: ACCEL_I_T1(3)     ! 3 transl components of accel due to RFORCE at a grid in basic  coords
      REAL(DOUBLE)                    :: ACCEL_I_T2(3)     ! 3 transl components of accel due to RFORCE at a grid in global coords
      REAL(DOUBLE)                    :: ACCEL_I_R1(3)     ! 3 rotat  components of accel due to RFORCE at a grid in basic  coords
      REAL(DOUBLE)                    :: ACCEL_I_R2(3)     ! 3 rotat  components of accel due to RFORCE at a grid in global coords
      REAL(DOUBLE)                    :: ANG_ACC(3)        ! Angular acceleration (SCALEF_AA*VEC(I))
      REAL(DOUBLE)                    :: ANG_VEL(3)        ! Angular velocity     (SCALEF_AV*VEC(I))
      REAL(DOUBLE)                    :: DRI(3)            ! Components of the vector formed by RI - RA
      REAL(DOUBLE)                    :: FORCE_I(6)        ! 6 forces at a grid due to the RFORCE loading
      REAL(DOUBLE)                    :: GRID_MGG(6,6)     ! 6 X 6 mass matrix for one grid point
      REAL(DOUBLE)                    :: PHID, THETAD      ! Outputs from subr GEN_T0L
      REAL(DOUBLE)                    :: RSID(LLOADC+1)    ! Array of RFORCE magnitudes (for LSID set ID's) needed for one S/C
      REAL(DOUBLE)                    :: RA(3)             ! Vector components, in basic coords, of GID
      REAL(DOUBLE)                    :: RI(3)             ! Vector components, in basic coords, of grid i
      REAL(DOUBLE)                    :: SCALE             ! Scale factor for a load (on a LOAD Bulk Data entry)
      REAL(DOUBLE)                    :: SCALEF_AA         ! Magnitude of the RFORCE angular acceleration 
      REAL(DOUBLE)                    :: SCALEF_AV         ! Magnitude of the RFORCE angular velocity
      REAL(DOUBLE)                    :: T12(3,3)          ! Coord transformation matrix
      REAL(DOUBLE)                    :: VEC(3)            ! 3 components of RFORCE vector at RFORCE_GRD in any coords
      REAL(DOUBLE)                    :: VEC_LOCAL(3)      ! 3 components of RFORCE vector at RFORCE_GRD in local coords, ACID_L
      REAL(DOUBLE)                    :: VEC_BASIC(3)      ! 3 components of RFORCE vector at RFORCE_GRD in basic coords, ACID_0

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NAME = 'RFORCE  '

! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! Open a scratch file that will be used to rewrite data from LINK1U after the coords have been transformed to global.
  
      SCRFIL(1:)  = ' '
      SCRFIL(1:9) = 'SCRATCH-991'
      OPEN (SCR(1),STATUS='SCRATCH',POSITION='REWIND',FORM='UNFORMATTED',ACTION='READWRITE',IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN
         CALL OPNERR ( IOCHK, SCRFIL, OUNT, 'Y' )
         CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )
         CALL OUTA_HERE ( 'Y' )                                    ! Error opening scratch file, so quit
      ENDIF
      REWIND (SCR(1))
 
! **********************************************************************************************************************************
! (1) Read record from L1U and transform coords for RFORCE VEC from local sys (on RFORCE bulk data card) to basic and write results
!     to SCRATCH-991.
 
i_do1:DO I=1,NRFORCE
                                                           ! Read a record from L1U
         READ(L1U,IOSTAT=IOCHK) SETID, ACID_L, RFORCE_GRD, SCALEF_AV, SCALEF_AA, (VEC(J),J=1,3)
         IF (IOCHK /= 0) THEN
            REC_NO = I
            CALL READERR ( IOCHK, LINK1U, L1U_MSG, REC_NO, OUNT, 'Y' )
            READ_ERR = READ_ERR + 1                        ! Increment READ_ERR and go back to read another RFORCE card
            CYCLE i_do1                                    
         ENDIF
 
         DO J=1,3
            RA(J) = ZERO
         ENDDO
         IF (RFORCE_GRD > 0) THEN
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, RFORCE_GRD, RFORCE_GRD_ROW_NUM )
            IF (RFORCE_GRD_ROW_NUM == -1) THEN
               WRITE(ERR,1822) 'GRID ', RFORCE_GRD, NAME, SETID
               WRITE(F06,1822) 'GRID ', RFORCE_GRD, NAME, SETID
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
            ELSE
               RA(1) = RGRID(RFORCE_GRD_ROW_NUM,1)
               RA(2) = RGRID(RFORCE_GRD_ROW_NUM,2)
               RA(3) = RGRID(RFORCE_GRD_ROW_NUM,3)
            ENDIF
         ENDIF
                                                           ! The local system that RFORCE is defined in is ACID_L.
         DO J=1,3
            VEC_LOCAL(J) = VEC(J)
         ENDDO 
         IF (ACID_L /= 0) THEN                             ! ACID_L is not basic, so find it and transform coords
            FOUND = 'N'
j_do12:     DO J=1,NCORD
               IF (CORD(J,2) == ACID_L) THEN
                  FOUND = 'Y'     
                  ICID = J                                 ! ICID is the internal coord ID corresponding to ACID_L
                  EXIT j_do12
               ENDIF
            ENDDO j_do12
            IF (FOUND == 'N') THEN
               WRITE(ERR,1822) 'COORD SYSTEM ', ACID_L, NAME, SETID
               WRITE(F06,1822) 'COORD SYSTEM ', ACID_L, NAME, SETID
               CID_ERR = CID_ERR + 1                       ! Increment READ_ERR and go back to read another RFORCE card
               FATAL_ERR = FATAL_ERR + 1
               CYCLE i_do1
            ENDIF

            DO J=1,3                                       ! Get coord transf matrix (don't need GEN_T0L since ICID is rect.)
               DO K=1,3
                  L = 3 + 3*(J-1) + K
                  T12(J,K) = RCORD(ICID,L)
               ENDDO 
            ENDDO 

            NROWA  = 3                                     ! Transform coordinates 
            NCOLA  = 3
            NCOLB  = 1
            CALL MATMULT_FFF ( T12, VEC_LOCAL, NROWA, NCOLA, NCOLB, VEC_BASIC )
         ELSE                                              ! No transformation needed since ACID_L is basic
            DO J=1,3
               VEC_BASIC(J) = VEC_LOCAL(J)
            ENDDO   
         ENDIF
 
         DO J=1,3                                          ! Reset VEC(I) to values in basic coords.
           VEC(J) = VEC_BASIC(J)
         ENDDO 
                                                           ! Write data to scratch file. RFORCE vec now in basic coords
         WRITE(SCR(1)) SETID, ACID_0, RFORCE_GRD, SCALEF_AV, SCALEF_AA, (VEC(J),J=1,3)
 
      ENDDO i_do1
  
! Quit if CID_ERR, GID_ERR or READ_ERR > 0
 
      IF ((CID_ERR > 0) .OR. (GID_ERR > 0) .OR. (READ_ERR > 0)) THEN
         IERRT = CID_ERR + GID_ERR + READ_ERR
         WRITE(ERR,1599) SUBR_NAME,IERRT
         WRITE(F06,1599) SUBR_NAME,IERRT
         CALL OUTA_HERE ( 'Y' )                                    ! Errors from reading RFORCE data, so quit
      ENDIF
 
      REWIND (SCR(1))
    
! **********************************************************************************************************************************
! Now process RFORCE loads into SYS_LOAD
 
      DO I=1,LLOADC                                         ! Initialize LSID, RSID arrays
         LSID(I) = 0
         RSID(I) = ZERO
      ENDDO

      MESSAG = 'SCRATCH FILE IN FORCEP  '
 
      WRITE(SC1, * )
      IERRT = 0
i_do2:DO I = 1,NSUB                                        ! Loop through the S/C's

         IF (SUBLOD(I,1) == 0) THEN                        ! If no load for this S/C, CYCLE
            CYCLE i_do2
         ENDIF
                                                           ! (2-a) Generate LSID/RSID tables for this S/C.
         NSID    = 1                                       ! There is always 1 pair (more if there are LOAD B.D cards).
         LSID(1) = SUBLOD(I,1)                             ! Note: If there are no LOAD B.D. cards, LSID(1) and RSID(1) will be
         RSID(1) = ONE                                     ! for the RFORCE card in file LINK1U that matches SUBLOD(I,1)
         DO J = 1,NLOAD                                    ! Then, the actual mag. will come from RSID(1) & the ACCEL_RB components

            IF (LSID(1) == LOAD_SIDS(J,1)) THEN            ! The load requested in CC for this S/C is the j-th LOAD BD card
k_do_211:      DO K = 2,LLOADC                             ! Get load sets defined on this LOAD BD card and put into LSID (if any)
                  IF (LOAD_SIDS(J,K) /= 0) THEN
                     NSID = K
                     LSID(K) = LOAD_SIDS(J,K)
                     RSID(K) = LOAD_FACS(J,1)*LOAD_FACS(J,K)
                  ELSE
                     CYCLE k_do_211                        ! If a LSID field left blank on LOAD BD card, CYCLE
                  ENDIF
               ENDDO k_do_211
            ENDIF

         ENDDO   

j_do_22: DO J = 1,NRFORCE                                  ! Process RFORCE card info that is now in basic coords
                                                           ! (2-b-  i) Read a RFORCE record (in basic coords) from scratch unit
            READ(SCR(1),IOSTAT=IOCHK) SETID, ACID, RFORCE_GRD, SCALEF_AV, SCALEF_AA, (VEC(K),K=1,3)

            IF (IOCHK /= 0) THEN
               REC_NO = J
               CALL READERR ( IOCHK, SCRFIL, MESSAG, REC_NO, OUNT, 'Y' )
               CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading scratch file, so quit
            ENDIF

            IF (ACID /= 0) THEN
               WRITE(ERR,1515) SUBR_NAME,ACID
               WRITE(F06,1515) SUBR_NAME,ACID
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                      ! Coding error, so quit
            ENDIF

            FOUND = 'N'                                    ! (2-b- ii). Scan through LSID to find set that matches SETID read.
k_do221:    DO K = 1,NSID                                  ! There is a match; we made sure all requested loads were in B.D. deck
               IF (SETID == LSID(K)) THEN                  ! We start with K = 1 to cover the case of no LOAD B.D cards
                  SCALE = RSID(K)
                  FOUND = 'Y'
                  DO L=1,3
                     ANG_ACC(L) = SCALE*SCALEF_AA*VEC(L)   ! Ang accel and vel of model due to RFORCE angular vel, accel entries
                     ANG_VEL(L) = SCALE*SCALEF_AV*VEC(L)
                  ENDDO 
                  EXIT k_do221 
               ENDIF
            ENDDO k_do221  

            IF (FOUND /= 'Y') THEN                         ! This RFORCE set ID isn't called for in this S/C, so CYCLE on RFORCE's
               CYCLE j_do_22
            ENDIF   

            DO K = 1,NGRID
               WRITE(SC1,12345,ADVANCE='NO') K, NGRID, I, CR13

               ACID_G = GRID(K,3)                          ! The global coord sys for this grid is ACID_G

               RI(1)  = RGRID(K,1)
               RI(2)  = RGRID(K,2)
               RI(3)  = RGRID(K,3)
               DO L=1,3
                  DRI(L) = RI(L) - RA(L)
               ENDDO
               

               CALL GET_GRID_ANG_ACCEL

               IF (ACID_G /= 0) THEN                       ! ACID_G is not basic so transform coords to global
l_do_2211:        DO L=1,NCORD
                     IF (CORD(L,2) == ACID_G) THEN
                        ICID = L                           ! ICID is the internal coord sys ID corresponding to ACID_G
                        EXIT l_do_2211
                     ENDIF
                  ENDDO l_do_2211
                                                           ! T12 is coord transf matrix that would transf a global vector to basic
!                                                            and its transpose will transform a basic coord sys vector to global
                  CALL GEN_T0L ( K, ICID, THETAD, PHID, T12 )

                  NROWA  = 3
                  NCOLA  = 3
                  NCOLB  = 1
                  CALL MATMULT_FFF_T ( T12, ACCEL_I_T1, NROWA, NCOLA, NCOLB, ACCEL_I_T2 )
                  CALL MATMULT_FFF_T ( T12, ACCEL_I_R1, NROWA, NCOLA, NCOLB, ACCEL_I_R2 )
                  DO L=1,3
                     ACCEL_I(L)   = ACCEL_I_T2(L)          ! ACCEL_I (grid accel) is now in global coords if it was not before
                     ACCEL_I(L+3) = ACCEL_I_R2(L)
                  ENDDO
               ELSE 
                  DO L=1,3
                     ACCEL_I(L)   = ACCEL_I_T1(L)          ! ACCEL_I (grid accel) is now in global coords if it was not before
                     ACCEL_I(L+3) = ACCEL_I_R1(L)
                  ENDDO
               ENDIF

               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(K), IGRID )
               IF (IGRID == -1) THEN
                  IERRT     = IERRT + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1822) 'GRID ', GRID_ID(K), NAME, SETID
                  WRITE(F06,1822) 'GRID ', GRID_ID(K), NAME, SETID
               ENDIF
               IF (GRID(IGRID,6) == 1) THEN                ! Scalar point so do not generate grav load on it. Give warn, not fatal
                  IERRT = IERRT + 1
                  WARN_ERR = WARN_ERR + 1
                  WRITE(ERR,9901) GRID_ID(K)
                  IF (SUPWARN == 'N') THEN
                     WRITE(ERR,9901) GRID_ID(K)
                  ENDIF
               ENDIF
               IF (IERRT == 0) THEN
                  CALL GET_GRID_6X6_MASS ( GRID_ID(K), IGRID, GRID_MGG_FND, GRID_MGG )
               ENDIF

               DO L=1,6
                  FORCE_I(L) = ZERO
               ENDDO 

               IF (GRID_MGG_FND == 'Y') THEN
                  NROWA  = 6
                  NCOLA  = 6
                  NCOLB  = 1
                  CALL MATMULT_FFF ( GRID_MGG, -ACCEL_I, NROWA, NCOLA, NCOLB, FORCE_I )
               ENDIF

!xx            CALL CALC_TDOF_ROW_NUM ( GRID(K,1), ROW_NUM_START, 'N' )
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID(K,1), IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
l_do_2214:     DO L = 1,6
                  CALL TDOF_COL_NUM ( 'G ', G_SET_COL_NUM )
                  ROW_NUM = ROW_NUM_START + L - 1
                  GDOF = TDOF(ROW_NUM,G_SET_COL_NUM)
                  SYS_LOAD(GDOF,I) = SYS_LOAD(GDOF,I) + FORCE_I(L) 
               ENDDO l_do_2214

            ENDDO   
 
         ENDDO j_do_22
         REWIND (SCR(1))                                       ! Need to read all of the RFORCE records again for the next S/C
 
      ENDDO i_do2
 
      WRITE(SC1,*) CR13 

      CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1515 FORMAT(' *ERROR  1515: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' COORD SYSTEM ID READ FROM FILE SCRATCH-991 MUST BE BASIC (0) BUT IS ',I8)

 1599 FORMAT(/,' PROCESSING TERMINATED IN SUBROUTINE ',A,' DUE TO ABOVE LISTED ',I8,' ERRORS')

 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,I8,' IS UNDEFINED')

 9901 FORMAT(' *WARNING    : NO RFORCE LOAD IS BEING PROCESSED FOR GRID ',I8,' SINCE IT IS A SCALAR POINT (SPOINT)')

12345 FORMAT(5X,'Proc grid ',I8,' of ',I8,', subcase ',I8, A)
















99910 format(' In RFORCE_PROC: Rigid body angular velocity     = ',3(1es14.6))

99911 format(' In RFORCE_PROC: Rigid body angular acceleration = ',3(1es14.6))

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE GET_GRID_ANG_ACCEL

      IMPLICIT NONE

      INTEGER(LONG)                   :: II                ! DO loop index

      REAL(DOUBLE)                    :: DUM1(3)           ! Intermediate vector in cross product
      REAL(DOUBLE)                    :: DUM2(3)           ! Intermediate vector in cross product
      REAL(DOUBLE)                    :: DUM3(3)           ! Intermediate vector in cross product

! **********************************************************************************************************************************

      CALL CROSS ( ANG_VEL, DRI , DUM1 )
      CALL CROSS ( ANG_VEL, DUM1, DUM2 )
      CALL CROSS ( ANG_ACC, DRI , DUM3 )
      DO II = 1,3
         ACCEL_I_T1(II) = DUM2(II) + DUM3(II)
      ENDDO
      DO II = 1,3
         ACCEL_I_R1(II) = DUM3(II)
      ENDDO




! **********************************************************************************************************************************

      END SUBROUTINE GET_GRID_ANG_ACCEL

      END SUBROUTINE RFORCE_PROC
