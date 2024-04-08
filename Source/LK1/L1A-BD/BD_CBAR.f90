! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
 
      SUBROUTINE BD_CBAR ( CARD, LARGE_FLD_INP )
 
     ! Processes CBAR and CBEAM Bulk Data Cards:
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LBAROFF, LVVEC, MEDAT_CBAR, &
                                         MEDAT_CBEAM, NBAROFF, NBAROR, NBEAMOR, NCBAR, NCBEAM, NEDAT, NELE, NVVEC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CBAR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  BAROFF, BAROR_G0, BEAMOR_G0, BAROR_PID, BEAMOR_PID, BAROR_VVEC_TYPE, BEAMOR_VVEC_TYPE,    &
                                         BAROR_VV, BEAMOR_VV, EDAT, ETYPE, JBAROR, JBEAMOR, VVEC !, OFFTS

      USE BD_CBAR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CBAR'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD            ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP   ! If 'Y', CARD is large field format
      CHARACTER(LEN=JCARD_LEN)        :: BAR_OR_BEAM     ! Field 1 of CBAR/CBEAM card
      CHARACTER(LEN(CARD))            :: CHILD           ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: ELID            ! Field 2 of CBAR/CBEAM card
      CHARACTER( 1*BYTE)              :: FOUND = 'N'     ! 'Y' if the V vec is one that is already stored in array VVEC
      CHARACTER( 8*BYTE)              :: IP6TYP          ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)       ! The 10 fields of characters making up CARD
      CHARACTER(LEN=JCARD_LEN)        :: JCARD_EDAT(10)  ! JCARD values sent to subr ELEPRO
      CHARACTER(LEN=JCARD_LEN)        :: JCARDO          ! An output from subr IP6CHK called herein
      CHARACTER( 9*BYTE)              :: VVEC_TYPE       ! Type of V vector on this CBAR/CBEAM 
      CHARACTER( 3*BYTE)              :: OFFT            ! OFFT flag
      INTEGER(LONG)                   :: OFFTI           ! OFFT flag as an integer cause I don't want/know how to pass a string
      INTEGER(LONG)                   :: G0 = 0          ! Grid specifying V vector for this CBAR/CBEAM, if input
      INTEGER(LONG)                   :: I4INP = 0       ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: ICONT = 0       ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IDUM            ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: IERR = 0        ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: I,J             ! DO loop indices
      INTEGER(LONG)                   :: JERR = 0        ! A local error count
      INTEGER(LONG)                   :: VVEC_NUM = 0    ! V vector number
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CBAR_BEGEND
 
      REAL(DOUBLE)                    :: VV(3)           ! The 3 components of the V vector for this CBAR/CBEAM elem
      REAL(DOUBLE)                    :: EPS1            ! A small number to compare real zero
      REAL(DOUBLE)                    :: R8INP = ZERO    ! A value read from input file that should be a real value
      LOGICAL                         :: IS_BAR          ! True=CBAR, False=CBEAM
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ! CBAR element Bulk Data Card routine

      !   FIELD   ITEM           ARRAY ELEMENT
      !   -----   ------------   -------------
      !    1      Element type   ETYPE(nele)  =B1 for CBAR
      !    2      Element ID     EDAT(nedat+1)
      !    3      Property ID    EDAT(nedat+2)
      !    4      Grid A         EDAT(nedat+3)
      !    5      Grid B         EDAT(nedat+4)
      !    6-8    V-Vector       (see VVEC explanation below)
      !                          V vector key goes in EDAT(nedat+5)
      !    9      OFFT           EDAT(nedat+6)
      ! on optional second card:
      !    2      Pin Flag A     EDAT(nedat+7)
      !    3      Pin Flag B     EDAT(nedat+8)
      !    4-9    Offsets        (see BAROFF explanation below)
      !                          Offset key goes in EDAT(nedat+9)
       


      ! NOTES:
      ! ------
      ! If fields 3, 6-8 are blank, they are loaded with the data from the 
      ! BAROR/BEAMOR entry (these will remain blank if no BAROR/BEAMOR card 
      ! exists).
      !
      ! If V-vector is specfied via a grid point then EDAT(nedat+5) is set 
      ! to that grid number.
      !
      ! If V-vector is specified via an actual vector, the vector is loaded
      ! into array VVEC(NVVEC,J) (J=1,2,3) unless a vector equal to it has
      ! been put in VVEC. EDAT(nedat+5) is set equal to -NVVEC, where NVVEC
      ! is the row number in array VVEC.
      ! 
      ! Offsets are in fields 4-9 of the first continuation card. If there
      ! are any offsets for this element, they are written to array BAROFF
      ! in row NBAROFF and NBAROFF is written in EDAT(nedat+8).  If there are
      ! no offsets for this element, a zero is entered in array EDAT(nedat+8).

      EPS1 = EPSIL(1)

      ! Make JCARD from CARD
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      BAR_OR_BEAM = JCARD(1)
      ELID        = JCARD(2)
      IS_BAR = (BAR_OR_BEAM(1:4) == 'CBAR')
 
      ! Set JCARD_EDAT to JCARD
      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

      ! Initialize variables
      VVEC_TYPE = 'UNDEFINED'

      ! Check property ID field. Set to:
      ! -  BAROR prop ID, (if present)
      ! - elem ID, if not
      IF (JCARD(3)(1:) == ' ') THEN             ! Prop ID field is blank, so use one of the following:
         IF (IS_BAR) THEN
            IF (BAROR_PID /= 0) THEN            ! Use BAROR prop ID for this CBAR prop ID
               JCARD_EDAT(3) = JBAROR(3)
            ELSE                                ! Use CBAR  elem ID for this CBAR prop ID         
               JCARD_EDAT(3) = JCARD(2)
            ENDIF
         ELSE
            IF (BEAMOR_PID /= 0) THEN           ! Use BEAMOR prop ID for this CBEAM prop ID
               JCARD_EDAT(3) = JBEAMOR(3)
            ELSE                                ! Use CBEAM  elem ID for this CBEAM prop ID         
               JCARD_EDAT(3) = JCARD(2)
            ENDIF
         ENDIF
      ENDIF

      ! Call ELEPRO to increment NELE and load some of the connection 
      ! data into array EDAT
      IF (IS_BAR) THEN
         CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_CBAR , 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'N', 'N' )
         NCBAR = NCBAR+1
         ETYPE(NELE) = 'BAR     '
      ELSE
         CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_CBEAM, 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'N', 'N' )
         NCBEAM = NCBEAM+1
         ETYPE(NELE) = 'BEAM    '
      ENDIF

      ! Get the V vector for this CBAR/CBEAM
      ! (either from this CBAR/CBEAM or from BAROR/BEAMOR values, if present)
      !
      ! Null all components. Some may be read from CBAR card
      DO J=1,3
         VV(J) = ZERO
      ENDDO

      IF (IS_BAR) THEN
         IF (NBAROR > 0) THEN
            ! Set VVEC fields to BAROR values if this CBAR's VVEC fields are blank
            IF ((JCARD(6)(1:) == ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN 
               IF      (BAROR_VVEC_TYPE == 'GRID     ') THEN
                  VVEC_TYPE = 'BAROR_GRD'
                  G0        =  BAROR_G0
               ELSE IF (BAROR_VVEC_TYPE == 'VECTOR   ') THEN
                  VVEC_TYPE = 'BAROR_VEC'
                  DO J=1,3
                     VV(J) = BAROR_VV(J)
                  ENDDO
               ENDIF                                       ! We checked on BAROR_VVEC_TYPE = 'ERROR' in subr BD_BAROR and we will
            ENDIF                                          ! check case where fields 6, 7, 8 are blank below         
         ENDIF
      ELSE
         IF (NBEAMOR > 0) THEN
            ! Set VVEC fields to BEAMOR values if this CBEAM's VVEC fields are blank
            IF ((JCARD(6)(1:) == ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN 
               IF      (BEAMOR_VVEC_TYPE == 'GRID     ') THEN
                  VVEC_TYPE = 'BEAMOR_GRD'
                  G0        =  BEAMOR_G0
               ELSE IF (BEAMOR_VVEC_TYPE == 'VECTOR   ') THEN
                  VVEC_TYPE = 'BEAMOR-VEC'
                  DO J=1,3
                     VV(J) = BEAMOR_VV(J)
                  ENDDO
               ENDIF     ! We checked on BEAMOR_VVEC_TYPE = 'ERROR' in subr BD_BEAMOR and we will
            ENDIF        ! check case where fields 6, 7, 8 are blank below         
         ENDIF
      ENDIF


      ! V vector
      IF (VVEC_TYPE == 'UNDEFINED') THEN
         ! We did not find a V vector so look for one on this CBAR/CBEAM
         ! TODO: this seems buggy...seems like it should look regardless 
         !       and overwrite the default

         ! See if there is an actual V vector.
         DO J=1,JCARD_LEN
            IF ((JCARD(6)(J:J) == '.') .OR. (JCARD(7)(J:J) == '.') .OR. (JCARD(8)(J:J) == '.')) THEN
               VVEC_TYPE = 'VECTOR   '
               EXIT
            ENDIF
         ENDDO

         IF (VVEC_TYPE == 'VECTOR   ') THEN
            ! If there is an actual V vector, get components
            LVVEC = LVVEC + 1
            JERR = 0
            DO J=1,3
               CALL R8FLD ( JCARD(J+5), JF(J+5), R8INP )
               IF (IERRFL(J+5) == 'N') THEN
                  VV(J) = R8INP
               ELSE
                  JERR = JERR + 1
               ENDIF
            ENDDO
            IF (JERR /= 0) THEN
               ! Found error in V vector components, so reset VVEC_TYPE
               VVEC_TYPE = 'ERROR    '
            ENDIF
         ELSE
            ! Check to see if there is a grid no (G0). for specifying VVEC
            IF ((JCARD(6)(1:) /= ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN
               VVEC_TYPE = 'GRID     '    
               CALL I4FLD ( JCARD(6), JF(6), I4INP )
               IF (IERRFL(6) == 'N') THEN
                  G0 = I4INP
                  IF (G0 < 0) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1187) JCARD(1), JCARD(2), G0
                     WRITE(F06,1187) JCARD(1), JCARD(2), G0
                  ENDIF
               ELSE
                  ! Found error in field 6, so reset VVEC_TYPE
                  VVEC_TYPE = 'ERROR    '
               ENDIF
            ELSE
               IF ((JCARD(6)(1:) == ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN
                  VVEC_TYPE = 'UNDEFINED'
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1188) JCARD(1), JCARD(2)
                  WRITE(F06,1188) JCARD(1), JCARD(2)
               ELSE
                  VVEC_TYPE = 'ERROR    '
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1186) JCARD(1), JCARD(2)
                  WRITE(F06,1186) JCARD(1), JCARD(2)
               ENDIF
            ENDIF
         ENDIF
      ENDIF

     ! Load V vector data into EDAT and into VVEC, if not already there
      IF ((VVEC_TYPE == 'GRID     ') .OR. (VVEC_TYPE == 'BAROR_GRD')) THEN
         NEDAT = NEDAT + 1
         EDAT(NEDAT) = G0

      ELSE IF ((VVEC_TYPE == 'VECTOR   ') .OR. (VVEC_TYPE == 'BAROR_VEC')) THEN
         FOUND = 'N'
         DO J=1,NVVEC
            IF ((DABS(VV(1) - VVEC(J,1)) < EPS1) .AND. (DABS(VV(2) - VVEC(J,2)) < EPS1) .AND. (DABS(VV(3) - VVEC(J,3)) < EPS1)) THEN
               VVEC_NUM = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO

         IF (FOUND == 'N') THEN   
            NVVEC = NVVEC + 1
            IF (NVVEC > LVVEC) THEN
               ! Coding error, so quit
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1132) SUBR_NAME,LVVEC
               WRITE(F06,1132) SUBR_NAME,LVVEC
               CALL OUTA_HERE ( 'Y' )
            ENDIF
            VVEC_NUM = NVVEC
            DO J=1,3
               VVEC(NVVEC,J) = VV(J)
            ENDDO
         ENDIF

         NEDAT = NEDAT + 1
         EDAT(NEDAT) = -VVEC_NUM
      ENDIF


      ! offt doesn't have a BAROR/BEAMOR default
      !
      ! OFFT    Orientation Vector  End A Offset  End B Offset flag
      ! -----   ------------------  ------------  -----------------
      ! 1/GGG   Global              Global        Global
      ! 2/BGG   Basic               Global        Global
      ! 3/GGO   Global              Global        Offset
      ! 4/BGO   Basic               Global        Offset
      ! 5/GOG   Global              Offset        Global
      ! 6/BOG   Basic               Offset        Global
      ! 7/GOO   Global              Offset        Offset
      ! 8/BOO   Basic               Offset        Offset
      !
      ! TODO: change flags to be clearer?
      !
      IF (JCARD(9)(1:1) == ' ') THEN  ! offt is blank
          OFFT = 'GGG'
      ELSE
          IF (LEN(TRIM(JCARD(9))) /= 3) THEN
              ! errored because OFFT = JCARD(9) is too long
              WRITE(ERR, 1189) TRIM(ELID), TRIM(JCARD(9))
              WRITE(F06, 1189) TRIM(ELID), TRIM(JCARD(9))
              FATAL_ERR = FATAL_ERR + 1
              FLUSH(ERR)
              FLUSH(F06)
              CALL OUTA_HERE ( 'Y' )
          ENDIF

          OFFT = JCARD(9)
          CALL to_upper3(OFFT)
          IF ((OFFT /= 'GGG') .AND. (OFFT /= 'BGG') .AND. (OFFT /= 'GGO') .AND. (OFFT /= 'BGO') .AND. &
              (OFFT /= 'GOG') .AND. (OFFT /= 'BOG') .AND. (OFFT /= 'GOO') .AND. (OFFT /= 'BOO')) THEN
              ! errored because OFFT is not one of the valid options
              WRITE(ERR, 1189) TRIM(ELID), OFFT
              WRITE(F06, 1189) TRIM(ELID), OFFT
              FATAL_ERR = FATAL_ERR + 1
              FLUSH(ERR)
              FLUSH(F06)
              CALL OUTA_HERE ( 'Y' )
          ENDIF
          
          OFFTI = -1
          IF (OFFT == 'GGG') THEN
              OFFTI = 1
          ELSEIF(OFFT == 'BGG') THEN
              OFFTI = 2
          ELSEIF(OFFT == 'GGO') THEN
              OFFTI = 3
          ELSEIF(OFFT == 'BGO') THEN
              OFFTI = 4
          ELSEIF(OFFT == 'GOG') THEN
              OFFTI = 5
          ELSEIF(OFFT == 'BOG') THEN
              OFFTI = 6
          ELSEIF(OFFT == 'GOO') THEN
              OFFTI = 7
          ELSEIF(OFFT == 'BOO') THEN
              OFFTI = 8
          ENDIF
      ENDIF
      NEDAT = NEDAT + 1
      EDAT(NEDAT) = OFFTI
      !OFFTS(NEDAT) = OFFT


      ! Write warnings and errors if any
      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,7,8,9 )   ! Make sure that there are no imbedded blanks in fields 2-8
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,0 )
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields         
 
      ! Optional Second Line (CARD):
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         ! Get pin flag data, if present
         DO J = 2,3
            IF (JCARD(J)(1:) /= ' ') THEN
               CALL IP6CHK ( JCARD(J), JCARDO, IP6TYP, IDUM )
               IF (IP6TYP == 'COMP NOS') THEN
                  CALL I4FLD ( JCARDO, JF(J), I4INP )
                  IF (IERRFL(J) == 'N') THEN
                     NEDAT = NEDAT + 1
                     EDAT(NEDAT) = I4INP
                  ENDIF
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1130) JCARD(J) ,J, JCARD(1) ,ELID
                  WRITE(F06,1130) JCARD(J) ,J, JCARD(1), ELID
               ENDIF
            ELSE
               ! Null EDAT for this pin flag
               NEDAT = NEDAT + 1
               EDAT(NEDAT) = 0
            ENDIF
         ENDDO

         ! Get offsets, if present
         IF ((JCARD(4)(1:) /= ' ') .OR. (JCARD(5)(1:) /= ' ') .AND. (JCARD(6)(1:) /= ' ') .OR. (JCARD(7)(1:) /= ' ') .AND.         &
             (JCARD(8)(1:) /= ' ') .OR. (JCARD(9)(1:) /= ' ')) THEN
            NBAROFF = NBAROFF + 1
            IF (NBAROFF > LBAROFF) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1161) SUBR_NAME, JCARD(1), LBAROFF
               WRITE(F06,1161) SUBR_NAME, JCARD(1), LBAROFF
               CALL OUTA_HERE ( 'Y' )                      ! Coding error, so quit
            ENDIF
            NEDAT = NEDAT + 1
            EDAT(NEDAT) = NBAROFF
            DO J=1,6
               CALL R8FLD ( JCARD(J+3), JF(J+3), R8INP )
               IF (IERRFL(J+3) == 'N') THEN
                  BAROFF(NBAROFF,J) = R8INP
               ENDIF
            ENDDO
         ELSE

            NEDAT = NEDAT + 1                              ! Null EDAT for the offset flag
            EDAT(NEDAT) = 0

         ENDIF

         CALL BD_IMBEDDED_BLANK ( JCARD,0,0,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 4-9
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ELSE                                                 ! Null 2 pin flag, and 1 bar offset, fields in EDAT since no cont card

         DO J=1,3
            NEDAT = NEDAT + 1
            EDAT(NEDAT) = 0
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
 1130 FORMAT(' *ERROR  1130: INVALID PINFLAG = ',A,' IN FIELD ',I2,' ON CONTINUATION ENTRY OF ',A,' ID = ',A                       &
                    ,/,14X,' PINFLAGS CAN CONTAIN ONLY DIGITS 1-6') 

 1132 FORMAT(' *ERROR  1132: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY V VECTORS. LIMIT IS ',I8)
 
 1161 FORMAT(' *ERROR  1161: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY CBAR/CBEAM OFFSETS. LIMIT IS ',I8) 
 
 1186 FORMAT(' *ERROR  1186: ERROR IN SPECIFYING V VECTOR ON ',A,A,'. EITHER FIELD 6 MUST BE A POSITIVE INTEGER GRID POINT'        &
                    ,/,14X,' OR FIELDS 6, 7, 8 MUST CONTAIN REAL VECTOR COMPONENTS (WITH DECIMAL POINTS)')

 1187 FORMAT(' *ERROR  1187: GRID SPECIFYING V VECTOR ON ',A,A,' MUST BE > 0. VALUE IS = ',I8)

 1188 FORMAT(' *ERROR  1188: NO V VECTOR SPECIFIED FOR ',A,' ELEMENT ID = ',A)

 1189 FORMAT('FATAL: CBAR ID=',A, ' HAS AN INVALID OFFT VALUE OF "', A, '"', / &
             '       USE: GGG, BGG, GGO, BGO, GOG, BOG, GOO, BOO')

! **********************************************************************************************************************************

      END SUBROUTINE BD_CBAR
 

      !function to_upper3(in) result (out)
      SUBROUTINE TO_UPPER3(IN)
      ! it seems like there should be a better way to write an upper function...
      ! https://en.wikibooks.org/wiki/Fortran/strings
      implicit none
      CHARACTER(3), intent (inout) :: IN
      integer                    :: I, J
      CHARACTER(26), parameter   :: UPP = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      CHARACTER(26), parameter   :: LOW = 'abcdefghijklmnopqrstuvwxyz'
      
      DO I = 1,3
          J = index(LOW, IN(I:I))      ! Is ith character in LOW
          IF (J>0) IN(I:I) = UPP(J:J)  ! Yes, then subst with UPP
      ENDDO
      END SUBROUTINE TO_UPPER3
      !end function to_upper
      
      