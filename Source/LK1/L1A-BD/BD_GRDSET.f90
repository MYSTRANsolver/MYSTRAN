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
  
      SUBROUTINE BD_GRDSET ( CARD )
 
! Processes GRDSET Bulk Data Cards. Subr GRDSET0 read fields 3, 7 and 8 of the GRDSET card so that those values
! could be used on GRID cards that may have been in the deck before the GRDSET card. This subr reads tham again
! and checks them for error:

!  1) GRDSET3 is field 3 for a GRID card (the input  coord system)
!  2) GRDSET7 is field 7 for a GRID card (the global coord system)
!  2) GRDSET7 is field 8 for a GRID card (the perm SPC's)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, NGRDSET
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_GRDSET_BEGEND
      USE MODEL_STUF, ONLY            :  GRDSET3, GRDSET7, GRDSET8
 
      USE BD_GRDSET_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_GRDSET'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
 
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: PGM_ERR   = 0     ! A  count of the number of coding errors
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_GRDSET_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! GRDSET Bulk Data Card routine. Values for GRDSET3, 7, 8 have already been
! read when B.D. deck was scanned originally. Here, we read card to detect
! and report read errors and to check for coding error (if values don't
! agree with those read in BD_GRDSET0)
 
!   FIELD         ITEM       ARRAY ELEMENT
!   -----   ---------------- ------------
!    3      Input Coord sys     GRDSET3
!    7      Displ Coord sys     GRDSET7
!    8      PSPC                GRDSET8
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read and check data

      IF (JCARD(3)(1:) /= ' ') THEN                        ! Read input coord sys ID in field 3 and store in GRDSET3
         CALL I4FLD ( JCARD(3), JF(3), I4INP )
         IF (IERRFL(3) == 'N') THEN
            IF (I4INP /= GRDSET3) THEN
               PGM_ERR = PGM_ERR + 1                       ! Coding error: This value doesn't agree with that read in LOADB0
               WRITE(ERR,1185) SUBR_NAME,JF(3),JCARD(1),GRDSET3,I4INP  
               WRITE(F06,1185) SUBR_NAME,JF(3),JCARD(1),GRDSET3,I4INP 
            ENDIF
         ENDIF
      ENDIF
 
      IF (JCARD(7)(1:) /= ' ') THEN                        ! Read global coord sys ID in field 7 and store in GRDSET7
         CALL I4FLD ( JCARD(7), JF(7), I4INP )
         IF (IERRFL(7) == 'N') THEN
            IF (I4INP /= GRDSET7) THEN
               PGM_ERR = PGM_ERR + 1                       ! Coding error: This value doesn't agree with that read in LOADB0 
               WRITE(ERR,1185) SUBR_NAME,JF(7),JCARD(1),GRDSET7,I4INP  
               WRITE(F06,1185) SUBR_NAME,JF(7),JCARD(1),GRDSET7,I4INP 
            ENDIF
         ENDIF
      ENDIF
 
      IF (JCARD(8)(1:) /= ' ') THEN                        ! Read perm SPC's in field 8 and store in GRDSET8
         CALL IP6CHK ( JCARD(8), JCARDO, IP6TYP, IDUM )
         IF (IP6TYP == 'COMP NOS') THEN
            CALL I4FLD ( JCARDO, JF(8), I4INP )
            IF (IERRFL(8) == 'N') THEN
               IF (I4INP /= GRDSET8) THEN
                  PGM_ERR = PGM_ERR + 1                    ! Coding error: This value doesn't agree with that read in LOADB0 
                  WRITE(ERR,1185) SUBR_NAME,JF(8),JCARD(1),GRDSET8,I4INP 
                  WRITE(F06,1185) SUBR_NAME,JF(8),JCARD(1),GRDSET8,I4INP 
               ENDIF
            ENDIF
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1123) JF(8),JCARD(1),JF(8),JCARD(8)
            WRITE(F06,1123) JF(8),JCARD(1),JF(8),JCARD(8)
         ENDIF
      ENDIF
 
      CALL BD_IMBEDDED_BLANK   ( JCARD,0,3,0,0,0,7,0,0 )   ! Make sure that there are no imbed blanks in fields 3,7. Field 8 is PSPC
      CALL CARD_FLDS_NOT_BLANK ( JCARD,2,0,4,5,6,0,0,9 )   ! Issue warning if fields 2, 4, 5, 6, 9 are not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      IF (PGM_ERR > 0) THEN                                ! PGM_ERR /= 0 is a coding error, so quit
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1123 FORMAT(' *ERROR  1123: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' CARD. MUST BE A COMBINATION OF DIGITS 1-6'                &
                    ,/,14X,'HOWEVER, FIELD ',I3, ' HAS: "',A,'"')

 1185 FORMAT(' *ERROR  1185: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' VALUE READ FROM FIELD ',I3,' ON ',A,' CARD IN THIS SUBROUTINE'                                        &
                    ,/,14X,' DOES NOT AGREE WITH THAT READ FROM ORIGINAL BULK DATA DECK SCAN.'                                     &
                    ,/,14X,' VALUES ARE: ',I8,' (ORIGINAL BULK DATA SCAN),'                                                        &
                    ,/,14X,'        AND: ',I8,' (HERE)')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_GRDSET
