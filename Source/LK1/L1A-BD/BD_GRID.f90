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
  
      SUBROUTINE BD_GRID ( CARD )
  
! Processes GRID Bulk Data Cards. Reads and checks:

!  1) Grid ID (field 2) and enters it into array GRID
!  2) Input  coord sys (field 3) and enters it into array GRID (or uses GRDSET3, if input on a GRDSET card)
!  3) Global coord sys (field 7) and enters it into array GRID (or uses GRDSET7, if input on a GRDSET card)
!  4) Permanent SPC's  (field 8) and enters it into array GRID (or uses GRDSET8, if input on a GRDSET card)
!  5) Grid coordinates (fields 4, 5 and 6) and enters tham into array RGRID   
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LGRID, NGRID, NGRDSET
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_GRID_BEGEND
      USE MODEL_STUF, ONLY            :  GRID, RGRID, GRDSET3, GRDSET7, GRDSET8
 
      USE BD_GRID_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_GRID'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! The type of TOKEN (looking for 'INTEGER ')             
 
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_GRID_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! GRID Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Grid number    GRID (ngrid,1)
!    3      Input co-ord   GRID (ngrid,2)
!    4      X1             RGRID(ngrid,1)
!    5      X2             RGRID(ngrid,2)
!    6      X3             RGRID(ngrid,3)
!    7      Disp. co-ord   GRID (ngrid,3)
!    8      PSPC           GRID (ngrid,4)
!   10      LB = 1         GRID (ngrid,5) key to put a line break into F06 file after outputs for this grid
! none      GRID/SPOINT    GRID (ngrid,6) = 6 for a physical grid  
       
! Make JCARD from  ARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NGRID = NGRID+1
!xx   IF (NGRID > LGRID) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LGRID
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LGRID
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read and check data

      CALL I4FLD ( JCARD(2), JF(2), I4INP )                ! Read grid ID
      IF (IERRFL(2) == 'N') THEN
         IF (I4INP > 0) THEN
            GRID(NGRID,1) = I4INP
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1168) JCARD(2)
            WRITE(F06,1168) JCARD(2)
         ENDIF
      ENDIF
 
      IF (JCARD(3)(1:) == ' ') THEN                        ! Read input coord sys or set to GRDSET value
         IF (NGRDSET > 0) THEN
            GRID(NGRID,2) = GRDSET3
         ELSE
            GRID(NGRID,2) = 0
         ENDIF
      ELSE
         CALL I4FLD ( JCARD(3), JF(3), GRID(NGRID,2) )
      ENDIF
 
      CALL R8FLD ( JCARD(4), JF(4), RGRID(NGRID,1) )       ! Read 1st component of grid coordinates
      CALL R8FLD ( JCARD(5), JF(5), RGRID(NGRID,2) )       ! Read 2nd component of grid coordinates
      CALL R8FLD ( JCARD(6), JF(6), RGRID(NGRID,3) )       ! Read 3rd component of grid coordinates
 
      IF (JCARD(7)(1:) == ' ') THEN                        ! Read global coord sys or set to GRDSET value
         IF (NGRDSET > 0) THEN
            GRID(NGRID,3) = GRDSET7
         ELSE
            GRID(NGRID,3) = 0
         ENDIF
      ELSE
         CALL I4FLD ( JCARD(7), JF(7), GRID(NGRID,3) )
      ENDIF
 
      IF (JCARD(8)(1:) == ' ') THEN                        ! Read perm SPC's or set to GRDSET value
         IF (NGRDSET > 0) THEN
            GRID(NGRID,4) = GRDSET8
         ELSE
            GRID(NGRID,4) = 0
         ENDIF
      ELSE
         CALL IP6CHK ( JCARD(8), JCARDO, IP6TYP, IDUM )
         IF (IP6TYP == 'COMP NOS') THEN
            CALL I4FLD ( JCARDO, JF(8), GRID(NGRID,4) )
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1124) JF(8), JCARD(1), JCARD(2), JF(8), JCARD(8)
            WRITE(F06,1124) JF(8), JCARD(1), JCARD(2), JF(8), JCARD(8)
         ENDIF
      ENDIF
 
      TOKEN = JCARD(10)(1:8)                               ! Only send the 1st 8 chars of this JCARD. It has been left justified
      CALL TOKCHK ( TOKEN, TOKTYP )                        ! See if field 10 has an integer. If so call it LB
      IF (TOKTYP == 'INTEGER ') THEN
         CALL I4FLD ( JCARD(10), JF(10), I4INP )           ! Read LB (put this many line breaks in F06 after this grid)
         IF (IERRFL(10) == 'N') THEN
            GRID(NGRID,5) = I4INP
         ENDIF
      ENDIF

      GRID(NGRID,6) = 6                                    ! Set GRID(NGRID,6) = 6 to indicate 6 comps for a physical grid

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,0,0 )     ! Make sure that there are no imb blanks in fields 2-7. Field 8 is PSPC
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1168 FORMAT(' *ERROR  1168: ZERO OR NEGATIVE GRID ID NOT ALLOWED ON GRID CARD. VALUE IS = ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_GRID
