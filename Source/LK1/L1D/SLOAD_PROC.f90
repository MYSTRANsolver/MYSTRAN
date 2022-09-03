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
 
      SUBROUTINE SLOAD_PROC
 
! Scalar load processor
 
! Transforms the input B.D. scalar load data to system force data in the SYS_LOAD array for dof's in the G set.
! File LINK1W was written when SLOAD B.D entries were read, with one record for each such pair of scalar point/load values.
! There are NSLOAD total no. records written to file LINK1W with each record containing:

!               SETID  = Load set ID on the SLOAD card
!               SPOINT = Scalar point where load acts
!               FMAG   = Scalar point load magnitude

! The process in creating array SYS_LOAD from this information is as follows:

!  (1) For each subcase (1 to NSUB):

!      (a) Generate LSID, RSID tables of load set ID's/scale factors for load sets for this subcase:

!          (  i) LLOADC is the max number of pairs of scale factors/load set ID's over all LOAD Bulk Data cards
!                including the pair defined by the set ID and overall scale factor on the LOAD Bulk Data card.
!                LSID and RSID are dimensioned 1 to LLOADC and:
!                   LSID(1) is always the load set ID requested in Case Control for this subcase.
!                   RSID(1) is always 1.0

!          ( ii) If the load set requested in Case Control is not a set ID from a LOAD Bulk data card then the
!                load must be on a separate FORCE/MOMENT in which case LSID(1) and RSID(1) are all that is needed
!                to define the load set contribution due to the FORCE/MOMENT cards for this subcase.
 
!          (iii) If the load set requested in Case Control is a set ID from a LOAD Bulk data card then the ramainder
!                (K = 2,LLOADC) of entries into LSID and RSID will be the pairs of load set ID's/scale factors from
!                that LOAD Bulk Data card (with RSID also multiplied by the overall scale factor on the LOAD Bulk data
!                card. The load set ID's are in array LOAD_SIDS(i,j) created when LOAD Bulk Data cards were read.
!                The scale factors are in array LOAD_FACS(i,j) also created when LOAD Bulk Data cards were read.
!                Note, there may not be as many as LLOADC pairs of set ID's/scale factors on a given LOAD Bulk Data
!                card since LLOADC is the max, from all LOAD Bulk Data cards, of pairs.
!                Thus, the entries in LSID from the last entry (for a given LOAD card) to LLOADC will be zero (LSID
!                was initialized to zero). This fact is used in a DO loop to EXIT when LSID(K) = 0 

!      (b) For each record in LINK1W (1 to NSLOAD)

!          (  i) Read a record from file: (SETID, SPOINT, FMAG)
 
!          ( ii) Scan LSID and RSID to get the scale factor for the SLOAD components in SETID, if this
!                SLOAD's set ID is in LSID                

!          (iii) Load these force values values into the system load array, SYS_LOAD
 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, WRT_ERR, WRT_LOG, ERR, F04, F06, L1W, LINK1W, L1W_MSG, L1WSTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LLOADC, NGRID, NLOAD, NSLOAD, NSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SLOAD_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  LOAD_SIDS, LOAD_FACS, SYS_LOAD, SUBLOD, GRID, GRID_ID
 
      USE SLOAD_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SLOAD_PROC'
      CHARACTER( 1*BYTE)              :: FOUND             ! Indicator on whether we found something we were looking for
 
      INTEGER(LONG)                   :: GDOF              ! G-set DOF no. for actual grid AGRID
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where AGRID is found
      INTEGER(LONG)                   :: G_SET_COL_NUM     ! Col no. in array TDOF where G-set DOF's are kept 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: SETID             ! Load set ID read from record in file LINK1I
      INTEGER(LONG)                   :: LSID(LLOADC+1)     ! Array of load SID's, from a LOAD Bulk Data card, for one S/C 
      INTEGER(LONG)                   :: NSID              ! Count on no. of pairs of entries on a LOAD B.D. card (<= LLOADC) 
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: ROW_NUM           ! Row no. in array TDOF corresponding to GDOF 
      INTEGER(LONG)                   :: SPOINT            ! Scalra point read from a record of L1W (point where force acts)
      INTEGER(LONG)                   :: XTIME             ! Time stamp read from file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SLOAD_PROC_BEGEND

      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: FMAG              ! Force magnitude read from a L1W record (force on the SPOINT)
      REAL(DOUBLE)                    :: RSID(LLOADC+1)     ! Array of load magnitudes (for LSID set ID's) needed for one S/C
      REAL(DOUBLE)                    :: SCALE             ! Scale factor for a load

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! **********************************************************************************************************************************
! Now process forces into SYS_LOAD for each subcase.
 
      DO I=1,LLOADC                                         ! Initialize LSID, RSID arrays
         LSID(I) = 0
         RSID(I) = ZERO
      ENDDO

i_do2:DO I=1,NSUB                                          ! Loop through the S/C's
 
         IF (SUBLOD(I,1) == 0) THEN                        ! If no load for this S/C, CYCLE
            CYCLE i_do2
         ENDIF
                                                           ! (2-a) Generate LSID/RSID tables for this S/C.
         NSID    = 1                                       ! There is always 1 pair (more if there are LOAD B.D cards).
         LSID(1) = SUBLOD(I,1)                             ! Note: If there are no LOAD B.D. cards, LSID(1) and RSID(1) will be
         RSID(1) = ONE                                     ! for the FORCE or MOMENT card in file LINK1I that matches SUBLOD(I,1)
         DO J = 1,NLOAD                                    ! Then, the actual mag. will come from RSID(1) & FMAG
            IF (SUBLOD(I,1) == LOAD_SIDS(J,1)) THEN
k_do21:        DO K = 2,LLOADC
                  IF (LOAD_SIDS(J,K) == 0) THEN
                     EXIT k_do21  
                  ELSE
                     NSID = K                              ! Note: NSID will not get larger than LLOADC
                     RSID(K) = LOAD_FACS(J,1)*LOAD_FACS(J,K)
                     LSID(K) = LOAD_SIDS(J,K)
                   ENDIF
                ENDDO k_do21   
            ENDIF
         ENDDO         

j_do22:  DO J=1,NSLOAD                                     ! Process SLOAD card info
                                                           ! (2-b-  i) Read a record from scratch - forces are in global coords
            READ(L1W,IOSTAT=IOCHK) SETID, SPOINT, FMAG
            IF (IOCHK /= 0) THEN
               REC_NO = J
               CALL READERR ( IOCHK, LINK1W, 'SLOAD FILE', REC_NO, OUNT, 'Y' )
               CALL FILE_CLOSE ( L1W, LINK1W, L1WSTAT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                              ! Error reading scratch file, so quit
            ENDIF
 
            FOUND = 'N'                                    ! (2-b- ii). Scan through LSID to find set that matches SETID read.
k_do221:    DO K = 1,NSID                                  ! There is a match; we made sure all requested loads were in B.D. deck
               IF (SETID == LSID(K)) THEN                  ! We start with K = 1 to cover the case of no LOAD B.D cards
                  SCALE = RSID(K)
                  FOUND = 'Y'
                  EXIT k_do221 
               ENDIF
            ENDDO k_do221  

            IF (FOUND == 'N') THEN                         ! Cycle back on J loop and read another force/moment card   
               CYCLE j_do22  
            ENDIF
                                                           ! Get GRID_ID_ROW_NUM, we checked it's existence earlier
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, SPOINT, GRID_ID_ROW_NUM )
 
            IF (DABS(FMAG) < EPS1) THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,1513) 'SLOAD', SETID
               IF (SUPWARN == 'N') THEN                     ! Issue warning if all force components zero
                  WRITE(F06,1513) 'SLOAD', SETID
               ENDIF
            ENDIF
 
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, SPOINT, IGRID )
            ROW_NUM = TDOF_ROW_START(IGRID)

            CALL TDOF_COL_NUM ( 'G ', G_SET_COL_NUM )      ! (2-b-iii). Put forces and moments into SYS_LOAD array
            GDOF = TDOF(ROW_NUM,G_SET_COL_NUM)
            SYS_LOAD(GDOF,I) = SYS_LOAD(GDOF,I) + SCALE*FMAG
 
         ENDDO j_do22
 
         REWIND (L1W)                                       ! Need to read all of the FORCE/MOMENT records again for the next S/C
         READ(L1W,IOSTAT=IOCHK) XTIME
         IF (IOCHK /= 0) THEN
            REC_NO = 1
            CALL READERR ( IOCHK, LINK1W, 'SLOAD FILE', REC_NO, OUNT, 'Y' )
            CALL FILERR ( OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )
         ENDIF
 
      ENDDO i_do2
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************






 1513 FORMAT(' *WARNING    : ',A8,1X,I8,' HAS ZERO FORCE MAGNITUDE')

! **********************************************************************************************************************************
 
      END SUBROUTINE SLOAD_PROC
