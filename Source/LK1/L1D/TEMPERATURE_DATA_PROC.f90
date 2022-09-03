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
 
      SUBROUTINE TEMPERATURE_DATA_PROC
 
! Grid and element temperature data processor

! Processes model, grid and elem temperature data. The model (TEMPD), grid (TEMP), or element temperature (TEMPRB, TEMPP1) entries
! were written to file LINK1K when the Bulk Data was read. Here, that data is read and processed into arrays needed for temp load
! calculation.
 
! There are NTCARD records in file LINK1K. There is
!   1 record  for any model TEMPD         B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   1 record  for any grid  TEMP          B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   n records for any elem  TEMPRB/TEMPP1 B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!     where n are the no. of physical cards (parent + continuations)

! The process involves 3 passes through the data in file LINK1K and is as follows:

! Pass 1: Process model temperature cards: TEMPD          (see subr TEMPD_DATA_PROC     for details)
! Pass 2: Process grid  temperature cards: TEMP           (see subr GRID_TEMP_DATA_PROC for details)
! Pass 3: Process elem  temperature cards: TEMPRB, TEMPP1 (see subr ELEM_TEMP_DATA_PROC for details)

! The three passes are done in the order shown so that TEMPD cards will first set all grids to a value that can be overridden
! by a TEMP card. Finally, element temperature cards can override TEMP cards for the grids belonging to that element.
! During these 3 passes through LINK1K, the grid and element temperature data arrays are constructed:

!   GTEMP : Array of temperatures for all grids and subcases
!   ETEMP : Array of avg bulk temperatures for all elems and subcases
!   TPNT  : Pointer array for where, in array TDATA, that elem temperature data begins for an element
!   TDATA : Array of element temperature data from TEMPRB, TEMPP1 cards
!   CGTEMP: Char array that indicates whether or not a grid has a temperature defined, for a subcase
!   CETEMP: Char array that indicates whether or not a elem has a temperature defined, for a subcase

! At the end, array CETEMP is scanned to make sure that every elem has had a temperature defined
! either on a TEMPRB or TEMPP1 card directly, or indirectly on TEMPD/TEMP cards for each grid belonging to the element.
! If all elems have temperature defined, file LINK1K is closed and reopened to write the above temperature arrays
! for further processing in the element subr ELMDAT.  


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR,     F04,     F06,     L1K
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                            LINK1K
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                            L1K_MSG
      USE SCONTR, ONLY                :  DATA_NAM_LEN, NELE, NGRID, NTDAT, NTSUB, NSUB, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TEMPERATURE_DATA_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CETEMP, CETEMP_ERR, CGTEMP, CGTEMP_ERR, ETEMP, GTEMP, TDATA, TPNT, GRID_ID, ESORT1, ETYPE,&
                                         SCNUM, SUBLOD, eid
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE TEMPERATURE_DATA_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TEMPERATURE_DATA_PROC'
      CHARACTER(LEN=DATA_NAM_LEN)     :: DATA_SET_NAME     ! A data set name for output purposes
      CHARACTER(  1*BYTE)             :: NOTE              ! Used to indicate whether or not to print out a message
      CHARACTER(  1*BYTE)             :: TEMP_ELM          ! Descriptor of how elem temp is defined
 
      INTEGER(LONG)                   :: ELID_ERR          ! Output from subr ELEM_TEMP_DATA_PROC (elem ID's undefined).
      INTEGER(LONG)                   :: GID_ERR           ! Output from subr GRID_TEMP_DATA_PROC (grid ID's undefined).
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERRT             ! Cum. count of grids with no temp defined after all data processed
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: READ_ERR  = 0     ! Count of read errors when temperature data file is read
      INTEGER(LONG)                   :: TCASE1(NSUB)      ! TCASE1(I) gives the internal thermal case number for internal S/C I
!                                                            If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                            thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                            TCASE1(1-5) = 0, 0, 1, 0, 2
      INTEGER(LONG)                   :: TCASE2(NSUB)      ! TCASE2(I) gives the internal subcase no. for internal thermal case I
!                                                            If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                            thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                            TCASE2(1-5) = 3, 5, 0, 0, 0 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TEMPERATURE_DATA_PROC_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! Generate TCASE1, TCASE2 arrays.
 
      J = 0
      DO I=1,NSUB
         TCASE1(I) = 0
      ENDDO 
      DO I = 1,NSUB
         IF (SUBLOD(I,2) > 0) THEN
            J = J + 1
            TCASE1(I) = J
         ENDIF
      ENDDO
    
      DO I = 1,NSUB
         TCASE2(I) = 0
      ENDDO 
      J = 0
      DO I = 1,NSUB
         IF (SUBLOD(I,2) /= 0) THEN
            J = J + 1
            TCASE2(J) = I
         ENDIF
      ENDDO   
 
! **********************************************************************************************************************************
! (1) Pass 1: Read and process TEMPD cards from L1K and set G.P. temperatures
 
      CALL TEMPD_DATA_PROC ( TCASE1, OUNT, READ_ERR ) 

! **********************************************************************************************************************************
! (2) Pass 2: Read and process TEMP cards from L1K and set G.P. temperatures
 
      CALL GRID_TEMP_DATA_PROC ( TCASE1, OUNT, READ_ERR, GID_ERR )  

! **********************************************************************************************************************************
! (3) Pass 3: Read and process the element temperature cards, TEMPRB, TEMPP1 and set elem temperatures
 
      CALL ELEM_TEMP_DATA_PROC ( TCASE1, OUNT, READ_ERR, ELID_ERR )

! **********************************************************************************************************************************
! (4) If no errors, check thermal data for completeness
 
      IF ((READ_ERR /= 0) .OR. (GID_ERR /= 0) .OR. (ELID_ERR /= 0)) THEN

         IERRT = READ_ERR + GID_ERR + ELID_ERR
         WRITE(ERR,9996) SUBR_NAME,IERRT
         WRITE(F06,9996) SUBR_NAME,IERRT
         CALL OUTA_HERE ( 'Y' )                                    ! Quit due to errors reading/processing temperature data

      ELSE                                                 ! Check that all elems have a temperature defined

         IERRT = 0
         DO J=1,NTSUB
            DO I=1,NELE
               IF ((ETYPE(I)(1:4) == 'ELAS') .OR. (ETYPE(I)(1:4) == 'BUSH') .OR. (ETYPE(I)(1:6) == 'PLOTEL')) THEN
                  CETEMP(I,J) = 'N'
               ELSE
                  IF (CETEMP(I,J) /= 'E') THEN
                     CALL ELEM_TEMP_CHK ( TCASE2(J), J, I, TEMP_ELM )
                     IF (TEMP_ELM /= CETEMP_ERR) THEN
                        CETEMP(I,J) = TEMP_ELM
                     ELSE
                        IERRT = IERRT + 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO 
         ENDDO

      ENDIF
 
! Make sure that CETEMP has no error terms

! Debug output of grid and element temperature data

      IF ((DEBUG(8) == 1) .OR. (DEBUG(8) == 3)) THEN
         NOTE = 'N'
         WRITE(F06,99971)
         DO J=1,NTSUB
            DO I=1,NGRID
               IF (CGTEMP(I,J) == CGTEMP_ERR) THEN
                  WRITE(F06,99972) SCNUM(TCASE2(J)),GRID_ID(I),CGTEMP(I,J)
                  NOTE = 'Y'
               ELSE
                  WRITE(F06,99973) SCNUM(TCASE2(J)),GRID_ID(I),GTEMP(I,J),CGTEMP(I,J)
               ENDIF
            ENDDO
            WRITE(F06,*)
         ENDDO
         IF (NOTE == 'Y') THEN
            WRITE(F06,99974) CGTEMP_ERR
         ENDIF
         WRITE(F06,*)
      ENDIF

      IF ((DEBUG(8) == 2) .OR. (DEBUG(8) == 3)) THEN
         NOTE = 'N'
         WRITE(F06,99991)
         DO J=1,NTSUB
            DO I=1,NELE
               IF (CETEMP(I,J) == CETEMP_ERR) THEN
                  WRITE(F06,99972) SCNUM(TCASE2(J)),ESORT1(I),CETEMP(I,J)
                  NOTE = 'Y'
               ELSE
                  WRITE(F06,99973) SCNUM(TCASE2(J)),ESORT1(I),ETEMP(I,J),CETEMP(I,J)
               ENDIF
            ENDDO
            WRITE(F06,*)
         ENDDO
         IF (NOTE == 'Y') THEN
            WRITE(F06,99994) CETEMP_ERR
         ENDIF
         WRITE(F06,*)
      ENDIF

! Quit if there were errors setting element temperatures

      IF (IERRT > 0) THEN
         WRITE(ERR,9998) IERRT
         WRITE(F06,9998) IERRT
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
! (5) Write processed temperature data to L1K
 
! First close and delete L1K file
 
      CALL FILE_CLOSE ( L1K, LINK1K, 'DELETE', 'Y' )
 
! Open L1K for write:
 
      CALL FILE_OPEN ( L1K, LINK1K, OUNT, 'REPLACE', L1K_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
 
      DATA_SET_NAME = 'TPNT'
      WRITE(L1K) DATA_SET_NAME
      WRITE(L1K) NELE
      WRITE(L1K) NTSUB
      DO I = 1,NELE
         DO J = 1,NTSUB
            WRITE(L1K) TPNT(I,J)
         ENDDO
      ENDDO   
 
      DATA_SET_NAME = 'TDATA'
      WRITE(L1K) DATA_SET_NAME
      WRITE(L1K) NTDAT
      DO I = 1,NTDAT
         WRITE(L1K) TDATA(I)
      ENDDO   
 
      DATA_SET_NAME = 'GTEMP'
      WRITE(L1K) DATA_SET_NAME
      WRITE(L1K) NGRID
      WRITE(L1K) NTSUB
      DO I = 1,NGRID
         DO J = 1,NTSUB
            WRITE(L1K) GTEMP(I,J)
         ENDDO
      ENDDO   
 
      DATA_SET_NAME = 'CGTEMP'
      WRITE(L1K) DATA_SET_NAME
      WRITE(L1K) NGRID
      WRITE(L1K) NTSUB
      DO I = 1,NGRID
         DO J = 1,NTSUB
            WRITE(L1K) CGTEMP(I,J)
         ENDDO
      ENDDO   
 
      DATA_SET_NAME = 'CETEMP'
      WRITE(L1K) DATA_SET_NAME
      WRITE(L1K) NELE
      WRITE(L1K) NTSUB
      DO I = 1,NGRID
         DO J = 1,NTSUB
            WRITE(L1K) CGTEMP(I,J)
         ENDDO
      ENDDO   
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9998 FORMAT(/,' Processing terminated due to',I8,' element temperatures undefined. see output file for details')

 9996 FORMAT(/,' Processing terminated in subroutine ',A,' due to above ',I8,' ERRORS')

99971 FORMAT(/,' OUTPUT FROM SUBROUTINE TEMPERATURE_DATA_PROC:',//,'   SUBCASE      GRID         TEMPERATURE    CGTEMP')  

99972 FORMAT(1X,2I10,20X    ,9X,A1)

99973 FORMAT(1X,2I10,1ES20.6,9X,A1)

99974 FORMAT(1X,A,' INDICATES NO TEMPERATURE DEFINED FOR THIS SUBCASE FOR THIS GRID')

99991 FORMAT(/,' OUTPUT FROM SUBROUTINE TEMPERATURE_DATA_PROC:',//,'   SUBCASE   ELEMENT         TEMPERATURE    CETEMP')   

99994 FORMAT(1X,A,' INDICATES NO TEMPERATURE DEFINED FOR THIS SUBCASE FOR THIS ELEMENT')

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################
 
      SUBROUTINE TEMPD_DATA_PROC ( TCASE1, OUNT, IERR )

! Overall model temperature data processor

! Processes data on TEMPD B.D. cards. These card images were written to file LINK1K when the Bulk Data was read.
! Here, that data is read and processed into arrays needed for temp load calculation.
 
! There are NTCARD records in file LINK1K. There is
!   1 record  for any model TEMPD         B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   1 record  for any grid  TEMP          B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   n records for any elem  TEMPRB/TEMPP1 B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!     where n are the no. of physical cards (parent + continuations)

! This 1st of 3 passes through file LINK1K processes TEMPD cards as follows:

!  (1) Read a record from file LINK1K. If it is TEMPD and the set ID (SID) matches a subcase request, then
!      ( i) Load the temperature value into array GTEMP for all grids for this subcase
!      (ii) Write 'D' to array CGTEMP for all grids for this subcase (grid temperature defined) to indicate that the
!           temperature is defined via a default (hence 'D') TEMPD card. 


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, JCARD_LEN, NGRID, NSUB, NTCARD, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TEMPERATURE_DATA_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CGTEMP, GTEMP, SUBLOD
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TEMPD_DATA_PROC'
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of 8 characters in CARD
      CHARACTER(LEN=BD_ENTRY_LEN)     :: CARD              ! B.D elem temp card image read from file LINK1K
 
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), INTENT(IN)       :: TCASE1(NSUB)      ! TCASE1(I) gives the internal thermal case number for internal S/C I
!                                                            If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                            thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                            TCASE1(1-5) = 0, 0, 1, 0, 2
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices
      INTEGER(LONG), INTENT(INOUT)    :: IERR              ! Cum. count of errors as we read, and check cards from file LINK1K
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: SID               ! Thermal load set ID read from an elem temperature B.D. card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TEMPERATURE_DATA_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: RTEMP             ! Real value of a temperature on a TEMPD or TEMP B.D. card
   
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,NTCARD
         READ(L1K,IOSTAT=IOCHK) CARD
         IF (IOCHK /= 0) THEN
            REC_NO = I
            CALL READERR ( IOCHK, LINK1K, L1K_MSG, REC_NO, OUNT, 'Y' )
            IERR = IERR + 1
            CYCLE
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF ((CARD(1:6) == 'TEMPD ') .OR. (CARD(1:6) == 'TEMPD*')) THEN
            DO J = 1,4
               IF (JCARD(2*J) == '        ') THEN
                  CYCLE
               ELSE
                  READ(JCARD(2*J)  ,'(I8  )') SID
                  READ(JCARD(2*J+1),'(F16.0)') RTEMP
                  DO K = 1,NSUB
                     IF (SUBLOD(K,2) == SID) THEN
                        DO L = 1,NGRID
                            GTEMP(L,TCASE1(K)) = RTEMP
                           CGTEMP(L,TCASE1(K)) = 'D'
                        ENDDO 
                     ENDIF
                  ENDDO
               ENDIF 
            ENDDO 
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

      END SUBROUTINE TEMPD_DATA_PROC

! ##################################################################################################################################
 
      SUBROUTINE GRID_TEMP_DATA_PROC ( TCASE1, OUNT, READ_ERR, GID_ERR )

! Grid temperature data processor

! Processes data on TEMP B.D. cards. These card images were written to file LINK1K when the Bulk Data was read.
! Here, that data is read and processed into arrays needed for temp load calculation.
 
! There are NTCARD records in file LINK1K. There is
!   1 record  for any model TEMPD         B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   1 record  for any grid  TEMP          B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   n records for any elem  TEMPRB/TEMPP1 B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!     where n are the no. of physical cards (parent + continuations)

! This 2nd of 3 passes through file LINK1K processes TEMP cards as follows (after REWINDing file LINK1K:

!  (1) Read a record from file LINK1K. If it is TEMP and the set ID (SID) matches a subcase request, then
!      ( i) Load the temperature value into array GTEMP for the grids on this TEMP card for this subcase
!      (ii) Write 'G' to array CGTEMP for these grids for this subcase (grid temperature defined) to indicate that the
!           temperature is defined via a grid (hence 'G') TEMP card. If that grid already has a 'G' issue a warning.
 


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, FATAL_ERR, JCARD_LEN, NGRID, NSUB, NTCARD, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  TEMPERATURE_DATA_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CGTEMP, GTEMP, GRID_ID, SUBLOD
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GRID_TEMP_DATA_PROC'
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of 8 characters in CARD
      CHARACTER( 8*BYTE), PARAMETER   :: NAME = 'GRID    ' ! name for output error purposes
      CHARACTER(LEN=BD_ENTRY_LEN)     :: CARD              ! B.D elem temp card image read from file LINK1K
 
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), INTENT(IN)       :: TCASE1(NSUB)      ! TCASE1(I) gives the internal thermal case number for internal S/C I
      INTEGER(LONG), INTENT(INOUT)    :: READ_ERR          ! Count of read errors when temperature data file is read
      INTEGER(LONG), INTENT(OUT)      :: GID_ERR           ! Count of errors due to a grid ID not defined
      INTEGER(LONG)                   :: AGRID             ! Actual grid ID
      INTEGER(LONG)                   :: GD_REDUNDANT_TEMP ! Count of warning messages when elements have redundant temperatures
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where AGRID is found
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: SID               ! Thermal load set ID read from an elem temperature B.D. card
      INTEGER(LONG)                   :: XTIME             ! Time stamp read from an unformatted file
!                                                            If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                            thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                            TCASE1(1-5) = 0, 0, 1, 0, 2
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TEMPERATURE_DATA_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: RTEMP             ! Real value of a temperature on a TEMPD or TEMP B.D. card
   
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      GID_ERR = 0
      GD_REDUNDANT_TEMP = 0                                ! Initialize warning indicator for redundant grid temp definition

      REWIND (L1K)
      READ(L1K,IOSTAT=IOCHK) XTIME
      IF (IOCHK /= 0) THEN
         REC_NO = 1
         CALL READERR ( IOCHK, LINK1K, L1K_MSG, REC_NO, OUNT, 'Y' )
         CALL OUTA_HERE ( 'Y' )                            ! Cannot read STIME from temperature data file, so quit
      ENDIF
 
      DO I=1,NTCARD
         READ(L1K,IOSTAT=IOCHK) CARD
         IF (IOCHK /= 0) THEN
            REC_NO = I+1
            CALL READERR ( IOCHK, LINK1K, L1K_MSG, REC_NO, OUNT, 'Y' )
            READ_ERR = READ_ERR + 1
            CYCLE
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF ((CARD(1:5) == 'TEMP ') .OR. (CARD(1:5) == 'TEMP*')) THEN
            READ(JCARD(2),'(I8)') SID
            DO J = 1,3
               IF (JCARD(J*2+1)(1:) == ' ') CYCLE
               READ(JCARD(J*2+1),'(I8)'  ) AGRID
               READ(JCARD(J*2+2),'(F16.0)') RTEMP
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
               IF (GRID_ID_ROW_NUM == -1) THEN
                  WRITE(ERR,1822) 'GRID ', AGRID, 'TEMP ', SID
                  WRITE(F06,1822) 'GRID ', AGRID, 'TEMP ', SID
                  GID_ERR   = GID_ERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  CYCLE
               ELSE
                  DO K = 1,NSUB
                    IF (SID == SUBLOD(K,2)) THEN
                        GTEMP(GRID_ID_ROW_NUM,TCASE1(K)) = RTEMP
                        IF (CGTEMP(GRID_ID_ROW_NUM,TCASE1(K)) == 'G') THEN
                           GD_REDUNDANT_TEMP = GD_REDUNDANT_TEMP + 1
                           WARN_ERR = WARN_ERR + 1
                           WRITE(ERR,1531) NAME,AGRID
                           IF (SUPWARN == 'N') THEN 
                              WRITE(F06,1531) NAME,AGRID
                           ENDIF 
                        ENDIF
                        CGTEMP(GRID_ID_ROW_NUM,TCASE1(K)) = 'G'
                    ENDIF
                  ENDDO
               ENDIF   
            ENDDO   
         ENDIF
      ENDDO 

      IF (GD_REDUNDANT_TEMP > 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1527)
         IF (SUPWARN == 'U') THEN
            WRITE(F06,1527)
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,I8,' IS UNDEFINED')

 1527 FORMAT(' *WARNING    : CHECK ERR OUTPUT FILE FOR WARNING MESSAGES REGARDING REDUNDANT TEMPERATURE DEFINITION')

 1531 FORMAT(' *WARNING    : ',A8,1X,I8,' HAS TEMPERATURE DEFINED MORE THAN ONCE. LAST VALUE IN INPUT DECK WILL BE USED')

! **********************************************************************************************************************************

      END SUBROUTINE GRID_TEMP_DATA_PROC

! ##################################################################################################################################
 
      SUBROUTINE ELEM_TEMP_DATA_PROC ( TCASE1, OUNT, READ_ERR, ELID_ERR )

! Element temperature data processor

! Processes data on TEMPRB, TEMPP1 B.D. cards. These card images were written to file LINK1K when the Bulk Data was read.
! Here, that data is read and processed into arrays needed for temp load calculation.
 
! There are NTCARD records in file LINK1K. There is
!   1 record  for any model TEMPD         B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   1 record  for any grid  TEMP          B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!   n records for any elem  TEMPRB/TEMPP1 B.D. card whose set ID (SID) matches that of a subcase TEMP = SID request
!     where n are the no. of physical cards (parent + continuations)

! This 3rd of 3 passes through file LINK1K processes TEMPRB, TEMPP1 cards as follows (after REWINDing file LINK1K:

!  (1) In an outer DO loop, cycle through records from file LINK1K until a TEMPRB or TEMPP1 parent card is found. If one
!      is found, load the temperature data into arrays TDATA and get subr ETPUT to write data to array TPNT

!  (2) In an inner DO loop, read continuation cards for the above parent. If any are found, they will contain additional
!      element ID's in one of several formats (checked when B.D. was read). Call ETPUT to load add these elements to
!      array TPNT.

!      The calls to ETPUT involve up to 3 different sets of circumstances:
!      ( i ) The additional element ID's on the cont. card are all separate element ID's, or
!      (iia) The additional element ID's on the cont. card are specified in an EID1 THRU EID2 format in fields 2-4
!      (iib) The additional element ID's on the cont. card are specified in an EID1 THRU EID2 format in fields 5-7
!      In case ( i ) an error is given if any elem ID's are undefined.
!      In cases (iia) and (iib) the elem ID's are specified by a range and some, or all, of these do not have to exist.
!      Thus, no error is given for these cases. If some elems wind up with no temperature defined at all, them this will
!      be detected in the calling routine 


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, FATAL_ERR, JCARD_LEN, LTDAT, MTDAT_TEMPRB, MTDAT_TEMPP1,      &
                                         NTCARD, NTDAT,  &
                                         NSUB, WARN_ERR  
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TEMPERATURE_DATA_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  TWO
      USE PARAMS, ONLY                :  SUPWARN
      USE MODEL_STUF, ONLY            :  TDATA
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELEM_TEMP_DATA_PROC'
      CHARACTER( 1*BYTE)              :: IELEM_ERR         ! Flag to decide whether to CYCLE a loop
      CHARACTER( 1*BYTE)              :: EFLAG             ! Flag to decide whether a situation in subr ETPUT is an error or not
      CHARACTER(LEN=JCARD_LEN)        :: CARD_NAME         ! The 1st field of the parent card of an entry in file L1K
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters in CARD
      CHARACTER(LEN=JCARD_LEN)        :: OLDTAG            ! Field 10 (continuation field) on CARD 
      CHARACTER( 8*BYTE)              :: THRU              ! ='Y' if THRU option used on TEMPRB, TEMPP1 continuation card
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! Variable to test whether "THRU" option was used on B.D. temp card
      CHARACTER(BD_ENTRY_LEN)         :: CARD              ! B.D elem temp card image read from file LINK1K
 
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), INTENT(IN)       :: TCASE1(NSUB)      ! TCASE1(I) gives the internal thermal case number for internal S/C I
      INTEGER(LONG), INTENT(INOUT)    :: READ_ERR          ! Count of read errors when temperature data file is read
      INTEGER(LONG), INTENT(OUT)      :: ELID_ERR          ! Count of errors due to an elem ID not defined
      INTEGER(LONG)                   :: EID               ! Actual element ID
      INTEGER(LONG)                   :: EID1,EID2         ! The 2 actual elem ID's in "EID1 THRU EID2" on elem temp B.D. card 
      INTEGER(LONG)                   :: EL_REDUNDANT_TEMP ! Count of warning messages when elements have redundant temperatures
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: ICRD              ! Count of the records (cards) read from file LINK1K
      INTEGER(LONG)                   :: IELEM             ! Internal element number for the actual element ID, EID
      INTEGER(LONG)                   :: IFIELD, JFIELD    ! DO loop limits when reading temperature data from a B.D. temp card
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: ITPNT             ! Index in array TPNT (a pointer to where temp data for EID starts)
      INTEGER(LONG)                   :: NFIELD            ! No. fields on TEMPRB, TEMPP1 B.D. cards that have temperature data
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: SID               ! Thermal load set ID read from an elem temperature B.D. card
      INTEGER(LONG)                   :: XTIME             ! Time stamp read from an unformatted file
!                                                            If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                            thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                            TCASE1(1-5) = 0, 0, 1, 0, 2
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TEMPERATURE_DATA_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: TB1               ! Bulk temperature from TEMPRB card 
      REAL(DOUBLE)                    :: TB2               ! Bulk temperature from TEMPRB card 
      REAL(DOUBLE)                    :: TE_BULK           ! Element bulk temperature 
   
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ELID_ERR = 0
      EL_REDUNDANT_TEMP = 0                                ! Initialize warning indicator for redundant elem temp definition

      REWIND (L1K)                                          ! L1K has been read from by subr GRID_TEMP_DATA_PROC earlier
      READ(L1K,IOSTAT=IOCHK) XTIME 
      IF (IOCHK /= 0) THEN
         REC_NO = 1
         CALL READERR ( IOCHK, LINK1K, L1K_MSG, REC_NO, OUNT, 'Y' )
         CALL OUTA_HERE ( 'Y' )                            ! Cannot read STIME from temperature data file, so quit
      ENDIF
 
      ICRD = 0                                             ! ICRD will count records read from file LINK1K

ntcrd:DO                                                   ! Top of loop for reading parent CARD 

         ICRD = ICRD + 1
         IF (ICRD <= NTCARD) THEN                          ! Read and process CARD as long as card count <= NTCARD
            READ(L1K,IOSTAT=IOCHK) CARD
            CARD_NAME = CARD(1:8)
            IF (IOCHK /= 0) THEN
               REC_NO = ICRD
               CALL READERR ( IOCHK, LINK1K, L1K_MSG, REC_NO, OUNT, 'Y' )
               READ_ERR = READ_ERR + 1
               CYCLE ntcrd
            ENDIF
            CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
                                                           ! If we have TEMPRB or TEMPP1, set NFIELD
            IF      ((CARD(1:7) == 'TEMPRB ') .OR. (CARD(1:7) == 'TEMPRB*')) THEN
               NFIELD = MTDAT_TEMPRB
               READ(JCARD(4),'(F16.0)') TB1
               READ(JCARD(5),'(F16.0)') TB2
               TE_BULK = (TB1 + TB2)/TWO
            ELSE IF ((CARD(1:7) == 'TEMPP1 ') .OR. (CARD(1:7) == 'TEMPP1*')) THEN
               NFIELD = MTDAT_TEMPP1
               READ(JCARD(4),'(F16.0)') TE_BULK
            ELSE                                           ! otherwise, CYCLE back to read another parent 
               CYCLE ntcrd
            ENDIF
 
            ITPNT = NTDAT + 1                              ! Set index for pointer array, TPNT
 
            READ(JCARD(2),'(I8)') SID                      ! Get thermal load SID, EID
            READ(JCARD(3),'(I8)') EID
 
            IF ((NTDAT + NFIELD) > LTDAT) THEN             ! Check for overflow in TDATA
               WRITE(ERR,1525) SUBR_NAME,LTDAT
               WRITE(F06,1525) SUBR_NAME,LTDAT
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                              ! Coding error (dim of array TDATA too small), so quit
            ENDIF
 
            IFIELD = 4                                     ! Put temperature data into array TDATA
            JFIELD = IFIELD - 1 + NFIELD
            DO J = IFIELD,JFIELD
               NTDAT = NTDAT + 1
               READ(JCARD(J),'(F16.0)') TDATA(NTDAT)
            ENDDO 

            EFLAG = 'Y'                                    ! First process EID on parent card into TPNT array
            CALL ETPUT ( CARD_NAME, EFLAG, EID, SID, TCASE1, ITPNT, TE_BULK, IELEM, EL_REDUNDANT_TEMP )
            IF (IELEM == -1) THEN
               ELID_ERR = ELID_ERR + 1
            ENDIF
 
! Process continuation cards, if any. If we get read error on CARD, go back to top of main loop to look for new parent card.
 
cont_cards: DO                                             ! Top of loop for reading optional cont. CARD's for the parent CARD 
               IF (ICRD == NTCARD) EXIT cont_cards
               OLDTAG = JCARD(10)
               READ(L1K,IOSTAT=IOCHK) CARD
               CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
               IF (IOCHK /= 0) THEN                        ! Error reading temp. data file, so set error & cycle to read another
                  REC_NO = ICRD
                  CALL READERR ( IOCHK, LINK1K, L1K_MSG, REC_NO, OUNT, 'Y' )
                  READ_ERR = READ_ERR + 1
                  CYCLE ntcrd
               ENDIF
               IF (OLDTAG == JCARD(1)) THEN
                  ICRD = ICRD + 1
               ELSE                                        ! Next card was not a continuation, so cycle back to read another parent 
                  BACKSPACE(L1K)
                  CYCLE ntcrd
               ENDIF
 
! First check for the 2 options on specifying data on continuation card. Either all data are EID's or the THRU option is used
! in which case field 3 and/or 6 will have "THRU". Notice that EFLAG = 'N' when ETPUT is called for the case where EID1 THRU EID2
! is on temp card. This is so that all elements in the range EID1 thru EID2 do not have to exist to avoid an error being flagged.
 
               TOKEN = JCARD(3)(1:8)                       ! Only send the 1st 8 chars of this JCARD. It has been left justified
               CALL TOKCHK ( TOKEN, TOKTYP )
               THRU = 'N'
               IF (TOKTYP == 'THRU    ') THEN
                  THRU = 'Y'
               ENDIF 
  
               IF (THRU == 'N') THEN
                  IELEM_ERR = 'N'
                  EFLAG = 'Y'                              ! Any elem ID's read in fields 2-9 must be valid
                  DO J=2,9
                     IF (JCARD(J) == '        ') EXIT
                     READ(JCARD(J),'(I8)') EID             ! Process individual EID's on continuation card into TPNT array
                     CALL ETPUT ( CARD_NAME, EFLAG, EID, SID, TCASE1, ITPNT, TE_BULK, IELEM, EL_REDUNDANT_TEMP )
                     IF (IELEM == -1) THEN                 ! ETPUT didn't find elem ID = EID
                        ELID_ERR = ELID_ERR + 1
                        IELEM_ERR = 'Y'
                     ENDIF
                  ENDDO
                  IF (IELEM_ERR == 'Y') THEN
                     CYCLE cont_cards
                  ENDIF
               ELSE 
                  READ(JCARD(2),'(I8)') EID1
                  READ(JCARD(4),'(I8)') EID2
                  EFLAG = 'N'                              ! Some, or all, elem ID's can be missing from range EID1 -> EID2
                  DO J=EID1,EID2                           ! Process "EID1 THRU EID2" (fields 2-4) on cont card into TPNT array
                     CALL ETPUT ( CARD_NAME, EFLAG, J, SID, TCASE1, ITPNT, TE_BULK, IELEM, EL_REDUNDANT_TEMP )
                  ENDDO   
  
                  TOKEN = JCARD(6)(1:8)                      ! Only send the 1st 8 chars of this JCARD. It has been left justified
                  CALL TOKCHK ( TOKEN, TOKTYP )
                  IF (TOKTYP == 'THRU    ') THEN
                     READ(JCARD(5),'(I8)') EID1
                     READ(JCARD(7),'(I8)') EID2
                     EFLAG = 'N'                           ! Some, or all, elem ID's can be missing from range EID1 -> EID2
                     DO J=EID1,EID2                        ! Process "EID1 THRU EID2" (fields 5-7) on cont card into TPNT array
                        CALL ETPUT ( CARD_NAME, EFLAG, J, SID, TCASE1, ITPNT, TE_BULK, IELEM, EL_REDUNDANT_TEMP )
                     ENDDO   
                  ENDIF
               ENDIF
 
            ENDDO cont_cards
 
         ELSE
 
            EXIT                                           ! EXIT loop when ICRD exceeds NTCARD 
 
         ENDIF
 
      ENDDO ntcrd

      IF (EL_REDUNDANT_TEMP > 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1527)
         IF (SUPWARN == 'Y') THEN
            WRITE(F06,1527)
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1525 FORMAT(' *ERROR  1525: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MUCH ELEMENT TEMPERATURE DATA. LIMIT = ',I12)

 1527 FORMAT(' *WARNING    : CHECK ERR OUTPUT FILE FOR WARNING MESSAGES REGARDING REDUNDANT TEMPERATURE DEFINITION')

! **********************************************************************************************************************************

      END SUBROUTINE ELEM_TEMP_DATA_PROC

! ##################################################################################################################################
 
      SUBROUTINE ELEM_TEMP_CHK( ISCNO, JTCOL, IELEM, TEMP_ELM )
 
! Called by subr TEMPERATURE_PROC for all elements that do not have their temperature defined via a B.D.
! element temperature card (TEMPRB, TEMPP1) to determine if the element's grids have a temperature
! defined via a TEMPD or TEMP entry. If it does, then an avg bulk temperature is calculated from the grid
! tamperature data. This avg bulk temp is loaded into array ETEMP. Output variable TEMP_ELM is a character
! which gives info about how the elem temp was arrived at.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE   
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MELGP, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TEMPERATURE_DATA_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  ETEMP, CGTEMP, CETEMP_ERR, GTEMP, EDAT, EPNT, ETYPE, GRID_ID, SCNUM,                      &
                                         AGRID, ELGP, EID, TYPE
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELEM_TEMP_CHK'

      CHARACTER( 1*BYTE), INTENT(OUT) :: TEMP_ELM          ! Descriptor of how temperature is defined for elem IELEM:
!                                                             = 'D' if all grids have temperature defined on TEMPD 
!                                                             = 'G' if all grids have temperature defined on TEMP 
!                                                             = 'B' if grids have temperature defined on TEMPD and TEMP 
!                                                             = CETEMP_ERR if one or more grids have no temperature defined 
      CHARACTER( 1*BYTE)              :: TEMP_DEF          ! = 'Y' if any grid for elem IELEM has TEMPD definition
      CHARACTER( 1*BYTE)              :: TEMP_GRD          ! = 'Y' if any grid for elem IELEM has TEMP  definition
      CHARACTER( 1*BYTE)              :: TEMP_ERR          ! = 'Y' if any grid for elem IELEM has no temperature defined

      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where AGRID(I) is found
      INTEGER(LONG)                   :: I                 ! DO loop index  
      INTEGER(LONG), INTENT(IN)       :: IELEM             ! Internal element number for a specific actual element ID
      INTEGER(LONG), INTENT(IN)       :: ISCNO             ! Internal subcase number
      INTEGER(LONG), INTENT(IN)       :: JTCOL             ! Col in thermal array CGTEMP, CETEMP for internal subcase no. ISCNO
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TEMPERATURE_DATA_PROC_BEGEND + 2
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      TEMP_ELM = ' '

      TEMP_DEF = 'N'
      TEMP_GRD = 'N'
      TEMP_ERR = 'N'

      EID  = EDAT(EPNT(IELEM))
      TYPE = ETYPE(IELEM)

      CALL GET_ELGP ( IELEM )

      ETEMP(IELEM,JTCOL) = ZERO
      DO I=1,ELGP    
         AGRID(I) = EDAT(EPNT(IELEM)+I+1)
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(I), GRID_ID_ROW_NUM )
         IF      (CGTEMP(GRID_ID_ROW_NUM,JTCOL) == 'D') THEN
            TEMP_DEF = 'Y'
         ELSE IF (CGTEMP(GRID_ID_ROW_NUM,JTCOL) == 'G') THEN
            TEMP_GRD = 'Y'
         ELSE
            TEMP_ERR = 'Y'
            WRITE(ERR,1526) EID, SCNUM(ISCNO), AGRID(I)
            WRITE(F06,1526) EID, SCNUM(ISCNO), AGRID(I)
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
         ETEMP(IELEM,JTCOL) = ETEMP(IELEM,JTCOL) + GTEMP(GRID_ID_ROW_NUM,JTCOL)
      ENDDO
      ETEMP(IELEM,JTCOL) = ETEMP(IELEM,JTCOL)/ELGP

      TEMP_ELM = ' '
      IF (TEMP_ERR == 'Y') THEN
         TEMP_ELM = CETEMP_ERR
      ELSE IF ((TEMP_DEF == 'Y') .AND. (TEMP_GRD == 'N')) THEN
         TEMP_ELM = 'D'
      ELSE IF ((TEMP_DEF == 'N') .AND. (TEMP_GRD == 'Y')) THEN
         TEMP_ELM = 'G'
      ELSE IF ((TEMP_DEF == 'Y') .AND. (TEMP_GRD == 'Y')) THEN
         TEMP_ELM = 'B'
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1526 FORMAT(' *ERROR  1526: CANNOT CALC TEMPERATURE FOR ELEM ',I8,' SUBCASE ',I8,'. GRID ',I8,' HAS NO TEMPERATURE DEFINED')

! **********************************************************************************************************************************
 
      END SUBROUTINE ELEM_TEMP_CHK

! ##################################################################################################################################
 
      SUBROUTINE ETPUT( CARD_NAME, EFLAG, EID, SID, TCASE1, ITPNT, TE_BULK, IELEM, EL_REDUNDANT_TEMP )
 
! Elem temperature routine - generates the TPNT(i,j) array that points into array TDATA to indicate where temperature
! data for an element begins for a thermal subcase. Also loads avg bulk temp for this elem into array ETEMP and sets
! array CETEMP to state that this elem temp was specified on an elem temp card

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NELE, NSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TEMPERATURE_DATA_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  ETEMP, ESORT1, CETEMP, TPNT, TYPE, SUBLOD
      USE PARAMS, ONLY                :  SUPWARN
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ETPUT'
      CHARACTER( 1*BYTE), INTENT(IN)  :: EFLAG             ! Flag used to decide whether it is an error if an elem in the range
      CHARACTER( 8*BYTE), PARAMETER   :: NAME = 'ELEMENT ' ! name for output error purposes
                                                           ! IEL_LO to IEL_HI does not exist
      CHARACTER(LEN=*), INTENT(IN)    :: CARD_NAME         ! The 1st field of the parent card of an entry in file L1K 
 
      INTEGER(LONG), INTENT(IN)       :: ITPNT             ! Index in array TPNT (a pointer to where temp data for EID starts)
      INTEGER(LONG), INTENT(IN)       :: EID               ! Actual element ID
      INTEGER(LONG), INTENT(IN)       :: SID               ! Set ID from the elem temperature card being processed
      INTEGER(LONG), INTENT(OUT)      :: IELEM             ! Internal element number for the actual element ID, EID
      INTEGER(LONG), INTENT(INOUT)    :: EL_REDUNDANT_TEMP ! Count of warning messages when elements have redundant temperatures
      INTEGER(LONG)                   :: I                 ! DO loop index  
      INTEGER(LONG), INTENT(IN)       :: TCASE1(NSUB)      ! TCASE1(I) gives the internal thermal case number for internal S/C I
!                                                            If there are 5 subcases and internal S/C 3 is the 1-st S/C to have
!                                                            thermal load and internal S/C 5 is the 2-nd to have thermal load:
!                                                            TCASE1(1-5) = 0, 0, 1, 0, 2
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TEMPERATURE_DATA_PROC_BEGEND + 2
 
      REAL(DOUBLE) , INTENT(IN)       :: TE_BULK           ! Bulk temperature from element temperature B.D. card

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      IELEM = 0

! First convert EID to interal value
 
      CALL GET_ARRAY_ROW_NUM ( 'ESORT1', SUBR_NAME, NELE, ESORT1, EID, IELEM )
 
! If EID not found then: error if EFLAG = 'Y', or return if EFLAG /= 'Y'
 
      IF (IELEM == -1) THEN
         IF (EFLAG == 'Y') THEN
            WRITE(ERR,1530) EID, CARD_NAME, SID, TYPE 
            WRITE(F06,1530) EID, CARD_NAME, SID, TYPE
            FATAL_ERR = FATAL_ERR + 1
         ENDIF

         RETURN
      ENDIF
 
! No error, so put pointer into TPNT(i,j) and give warning if the
! element has had a temperature defined previously
 
      DO I = 1,NSUB
         IF (SID == SUBLOD(I,2)) THEN
            IF (TPNT(IELEM,TCASE1(I)) /= 0) THEN
               EL_REDUNDANT_TEMP = EL_REDUNDANT_TEMP + 1
!xx            FATAL_ERR = FATAL_ERR + 1
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,1531) NAME,ESORT1(IELEM) 
               IF (SUPWARN == 'N') THEN
                  WRITE(F06,1531) NAME,ESORT1(IELEM)
               ENDIF 
            ENDIF  
            TPNT(IELEM,TCASE1(I))   = ITPNT
            ETEMP(IELEM,TCASE1(I))  = TE_BULK
            CETEMP(IELEM,TCASE1(I)) = 'E'
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
 1530 FORMAT(' *ERROR  1530: ELEMENT ',I8,' ON ',A,I8,' DOES NOT EXIST OR IS OF WRONG TYPE = "',A,'" FOR THE TEMPERATURE CARD')

 1531 FORMAT(' *WARNING    : ',A8,1X,I8,' HAS TEMPERATURE DEFINED MORE THAN ONCE. LAST VALUE IN INPUT DECK WILL BE USED')

! **********************************************************************************************************************************
 
      END SUBROUTINE ETPUT

      END SUBROUTINE TEMPERATURE_DATA_PROC