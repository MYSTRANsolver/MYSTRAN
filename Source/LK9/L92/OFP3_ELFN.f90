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
 
      SUBROUTINE OFP3_ELFN ( JVEC, FEMAP_SET_ID, ITE, OT4_EROW )

! Processes element node force output requests for one subcase, all element types
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_FIJ, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELOUT_ELFN_BIT, ELDT_BUG_U_P_BIT, ELDT_F25_U_P_BIT, FATAL_ERR,NELE, IBIT,   &
                                         INT_SC_NUM, MBUG, MOGEL, SOL_NAME                                           
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  OFP3_ELFN_BEGEND
      USE PARAMS, ONLY                :  ELFORCEN, OTMSKIP
      USE MODEL_STUF, ONLY            :  EDAT, EPNT, ETYPE, AGRID, EID, ELDT, ELGP, ELMTYP, ELOUT, METYPE, NUM_EMG_FATAL_ERRS,     &
                                         PEB, PEG, PEL, PLY_NUM, TYPE, SCNUM
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, EID_OUT_ARRAY, MAXREQ, OGEL
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ELFN, TXT_ELFN
  
      USE OFP3_ELFN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'OFP3_ELFN'
      CHARACTER( 1*BYTE), PARAMETER   :: IHDR      = 'Y'   ! An input to subr WRITE_GRID_OUTPUTS, called herein
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option indicators for subr EMG, called herein
      CHARACTER(31*BYTE)              :: OT4_DESCRIPTOR    ! Descriptor for rows of OT4 file
      CHARACTER(30*BYTE)              :: REQUEST           ! Text for error message
 
      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! Set ID for FEMAP output
      INTEGER(LONG), INTENT(IN)       :: ITE               ! Unit number for text files for OTM row descriptors 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG), INTENT(INOUT)    :: OT4_EROW          ! Row number in OT4 file for elem related OTM descriptors
      INTEGER(LONG)                   :: DUM_BUG(0:MBUG-1) ! Values from WRT_BUG sent to subr ELMOUT in a particular call
      INTEGER(LONG)                   :: ELOUT_ELFN        ! If > 0, there are ELFORCE(NODE) requests for some elems                
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices
      INTEGER(LONG)                   :: IERROR      = 0   ! Local error count
!xx   INTEGER(LONG)                   :: IROW_MAT          ! Row number in OTM's
!xx   INTEGER(LONG)                   :: IROW_TXT          ! Row number in OTM text file
      INTEGER(LONG)                   :: I2                ! A calculated index into an array
      INTEGER(LONG)                   :: NBUG(METYPE)      ! Count of the no. of requests for ELDATA print requests of UEL, PEL
      INTEGER(LONG)                   :: NDISK(METYPE)     ! Count of the no. of requests for ELDATA disk file requests of UEL, PEL
      INTEGER(LONG)                   :: NELREQ(METYPE)    ! Count of the no. of requests for ELFORCE(NODE or ENGR) or STRESS
      INTEGER(LONG)                   :: NUM_COMPS         ! Either 6 or 1 depending on whether grid is a physical grid or a SPOINT
      INTEGER(LONG)                   :: NUM_ELEM          ! No. elems processed prior to writing results to F06 file
      INTEGER(LONG)                   :: NUM_OGEL          ! No. rows written to array OGEL prior to writing results to F06 file
!                                                            (this can be > NUM_ELEM since more than 1 row is written to OGEL
!                                                            for ELFORCE(NODE) - elem nodal forces)
                                                           ! Indicator for output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OFP3_ELFN_BEGEND
 
      INTRINSIC IAND
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process element node force requests for all elements
 
      OPT(1) = 'N'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'Y'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'N'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'Y'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff
 
! Find out how many output requests were made for each element type.
 
      DO I=1,METYPE                                        ! Initialize the array containing no. requests/elem.
         NELREQ(I) = 0
         NBUG(I)   = 0
         NDISK(I)  = 0
      ENDDO 
 
      DO I=1,METYPE
         DO J=1,NELE
            IF (ETYPE(J) == ELMTYP(I)) THEN
               ELOUT_ELFN = IAND(ELOUT(J,INT_SC_NUM),IBIT(ELOUT_ELFN_BIT))
               WRT_BUG(6) = IAND(ELDT(J)            ,IBIT(ELDT_BUG_U_P_BIT))
               WRT_FIJ(5) = IAND(ELDT(J)            ,IBIT(ELDT_F25_U_P_BIT))
               IF (ELOUT_ELFN > 0) THEN
                  NELREQ(I) = NELREQ(I) + 1
               ENDIF
               IF (WRT_BUG(6) > 0) THEN
                  NBUG(I)   = NBUG(I)   + 1
               ENDIF
               IF (WRT_FIJ(5) > 0) THEN
                  NDISK(I)  = NDISK(I)  + 1
               ENDIF
            ENDIF
         ENDDO 
      ENDDO   
 
!xx   IROW_MAT = 0
!xx   IROW_TXT = 0
      OT4_DESCRIPTOR = 'Element nodal force'

      DO I=0,MBUG-1
         DUM_BUG(I) = 0
      ENDDO

reqs1:DO I=1,METYPE
         IF ((NELREQ(I) + NBUG(I) + NDISK(I)) == 0) CYCLE reqs1
         NUM_ELEM = 0
         NUM_OGEL = 0
 
elems_1: DO J = 1,NELE
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J) == ELMTYP(I)) THEN
               ELOUT_ELFN = IAND(ELOUT(J,INT_SC_NUM), IBIT(ELOUT_ELFN_BIT))
               DUM_BUG(6) = IAND(ELDT(J)            , IBIT(ELDT_BUG_U_P_BIT))
               WRT_FIJ(5) = IAND(ELDT(J), IBIT(ELDT_F25_U_P_BIT))

               IF ((ELOUT_ELFN > 0) .OR. (DUM_BUG(6) > 0) .OR. (WRT_FIJ(5) > 0)) THEN
                  PLY_NUM = 0                              ! 'Y' in call to EMG means write to BUG file
                  CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'Y' )
                  IF (NUM_EMG_FATAL_ERRS > 0) THEN
                     IERROR = IERROR + 1
                     CYCLE elems_1
                  ENDIF
                  CALL ELMDIS
                  CALL CALC_ELEM_NODE_FORCES
                  IF      (ELFORCEN == 'BASIC') THEN       ! Transform to basic or global coords depending on ELFORCEN
                     CALL TRANSFORM_NODE_FORCES ( 'B' )
                  ELSE IF (ELFORCEN == 'GLOBAL') THEN
                     CALL TRANSFORM_NODE_FORCES ( 'G' )
                  ENDIF
                  IF (DUM_BUG(6) > 0) THEN                 ! Output grid point displs and loads in local elem or basic coords
                     CALL ELMOUT ( J, DUM_BUG, SCNUM(INT_SC_NUM), OPT )
                  ENDIF

                  IF (ELOUT_ELFN > 0) THEN
                     NUM_ELEM = NUM_ELEM + 1
                     EID_OUT_ARRAY(NUM_ELEM,1) = EID
                     DO K=1,ELGP
                        GID_OUT_ARRAY(NUM_ELEM,K) = AGRID(K)
                     ENDDO   
 
                     I2 = 0
                     DO K=1,ELGP
                        NUM_OGEL = NUM_OGEL + 1
                        IF (NUM_OGEL > MAXREQ) THEN
                           WRITE(ERR,9200) SUBR_NAME, MAXREQ
                           WRITE(F06,9200) SUBR_NAME, MAXREQ
                           FATAL_ERR = FATAL_ERR + 1
                           CALL OUTA_HERE ( 'Y' )          ! Coding error (dim of array OGEL too small), so quit
                        ENDIF   
                        DO L=1,6
                           OGEL(NUM_OGEL,L) = ZERO
                        ENDDO
                        CALL GET_GRID_NUM_COMPS ( AGRID(K), NUM_COMPS, SUBR_NAME )
                        DO L=1,NUM_COMPS
                           I2 = I2 + 1
                           IF      (ELFORCEN == 'LOCAL') THEN
                              OGEL(NUM_OGEL,L) = PEL(I2)
                           ELSE IF (ELFORCEN == 'BASIC') THEN
                              OGEL(NUM_OGEL,L) = PEB(I2)
                           ELSE IF (ELFORCEN == 'GLOBAL') THEN
                              OGEL(NUM_OGEL,L) = PEG(I2)
                           ENDIF
                           IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                              OT4_EROW = OT4_EROW + 1
                              OTM_ELFN(OT4_EROW,JVEC) = OGEL(NUM_OGEL,L)
                              IF (JVEC == 1) THEN
                                 WRITE(TXT_ELFN(OT4_EROW), 9191) OT4_EROW, OT4_DESCRIPTOR, TYPE, EID, AGRID(K), L
                              ENDIF
                           ENDIF
                        ENDDO 
                     ENDDO   
 
                     IF (NUM_ELEM == NELREQ(I)) THEN
                        CALL CHK_OGEL_ZEROS ( NUM_OGEL )
                        CALL WRITE_ELEM_NODE_FORCE ( JVEC, ELGP, NUM_ELEM, IHDR )
                     ENDIF
                  ENDIF

                  IF (WRT_FIJ(5) > 0) THEN
                     CALL WRITE_FIJFIL ( 5, JVEC )
                  ENDIF
 
                  IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (JVEC == 1) .AND. (OT4_EROW >= 1)) THEN
                     DO K=1,OTMSKIP                        ! Write OTMSKIP blank separator lines
                        OT4_EROW = OT4_EROW + 1
                        WRITE(TXT_ELFN(OT4_EROW), 9199)
                     ENDDO
                  ENDIF

               ENDIF
 
            ENDIF
 
         ENDDO elems_1
 
      ENDDO reqs1

      IF (IERROR > 0) THEN
         REQUEST = 'ELEMENT NODE FORCE'
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
 9191 FORMAT(I8,1X,A,A8,I8,I8,I8)

 9199 FORMAT(' ')

 9200 FORMAT(' *ERROR  9200: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAY OGEL WAS ALLOCATED TO HAVE ',I12,' ROWS. ATTEMPT TO WRITE TO OGEL BEYOND THIS')
 
 9201 FORMAT(' *ERROR  9201: DUE TO ABOVE LISTED ERRORS, CANNOT CALCULATE ',A,' REQUESTS FOR ',A,' ELEMENT ID = ',I8)

! **********************************************************************************************************************************

      END SUBROUTINE OFP3_ELFN