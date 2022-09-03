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
  
      SUBROUTINE BD_FORMOM ( CARD, CC_LOAD_FND )
  
! Processes FORCE, MOMENT Bulk Data Cards. Also, the set ID is written to array FORMOM_SIDS which is checked
! in subroutine LOADB to make sure that all set ID's requested in Case Control were found in the Bulk Data.
! A record is written to file LINK1I for each FORCE or MOMENT Bulk Data card with the following data:

!   SETID, GRID_NO, CID, FORMON1, FORMON2, FORMON3, FOR_OR_MOM
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1I
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LFORCE, LSUB, NFORCE, NSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_FORMOM_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE MODEL_STUF, ONLY            :  FORMOM_SIDS, SUBLOD
 
      USE BD_FORMOM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_FORMOM'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD                ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: FOR_OR_MOM          ! = 'FORCE' or 'MOMENT' (JCARD(1) from parent entry)
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2) ! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
 
      INTEGER(LONG)                   :: CID       = 0       ! Coord ID on the FORCE/MOMENT card
      INTEGER(LONG)                   :: GRID_NO   = 0       ! Grid ID  on the FORCE/MOMENT card
      INTEGER(LONG)                   :: I                   ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0       ! A local error count
      INTEGER(LONG)                   :: SETID  = 0          ! Set ID on the FORCE/MOMENT card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_FORMOM_BEGEND
  
      REAL(DOUBLE)                    :: EPS1                ! A small number to compare real zero
      REAL(DOUBLE)                    :: FORMON1   = ZERO    ! Force/moment magnitude for 1st dir in coord sys CID (= SCALEF*V1)
      REAL(DOUBLE)                    :: FORMON2   = ZERO    ! Force/moment magnitude for 2nd dir in coord sys CID (= SCALEF*V2)
      REAL(DOUBLE)                    :: FORMON3   = ZERO    ! Force/moment magnitude for 3rd dir in coord sys CID (= SCALEF*V3)
      REAL(DOUBLE)                    :: SCALEF    = ZERO    ! Scale factor on the FORCE/MOMENT card
      REAL(DOUBLE)                    :: V1        = ZERO    ! Amplitude of force/mom on FORCE/MOMENT card in 1st direction of CID
      REAL(DOUBLE)                    :: V2        = ZERO    ! Amplitude of force/mom on FORCE/MOMENT card in 2nd direction of CID
      REAL(DOUBLE)                    :: V3        = ZERO    ! Amplitude of force/mom on FORCE/MOMENT card in 3rd direction of CID
      REAL(DOUBLE)                    :: VMAG      = ZERO    ! V!**2 + V2**2 + V3**2
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! FORCE / MOMENT Bulk Data Card routine
 
!   FIELD   ITEM           VARIABLE
!   -----   ------------   -------------
!    2      Load set ID    FORMOM_SIDS(I), SETID
!    3      Grid ID        GRID_NO
!    4      Cord. ID       CID
!    5      Scale factor   SCALEF
!    6      Vect. comp. 1  V1
!    7      Vect. comp. 2  V2
!    8      Vect. comp. 3  V3
 
!    SCALEF, V1, V2, and V3 are processed to force component
!    form ( Fx,Fy,Fz or Mx,My,Mz ) and stored in FORMON1-3
 
      EPS1 = EPSIL(1)

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      IF      (JCARD(1)(1:5) == 'FORCE' ) THEN
         FOR_OR_MOM = 'FORCE   '
      ELSE IF (JCARD(1)(1:6) == 'MOMENT') THEN
         FOR_OR_MOM = 'MOMENT  '
      ENDIF
 
! Check for overflow

      NFORCE = NFORCE+1
!xx   IF (NFORCE > LFORCE) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LFORCE
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LFORCE
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Read and check data
 
      CALL I4FLD ( JCARD(2), JF(2), SETID )
      IF (IERRFL(2) == 'N') THEN
         DO I=1,NSUB
            IF (SETID == SUBLOD(I,1)) THEN
               CC_LOAD_FND(I,1) = 'Y'
            ENDIF
         ENDDO
         FORMOM_SIDS(NFORCE) = SETID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), GRID_NO )              ! Read grid that force is at
      CALL I4FLD ( JCARD(4), JF(4), CID )                  ! Read coord system force is described in
      CALL R8FLD ( JCARD(5), JF(5), SCALEF )               ! Read force scale factor
      CALL R8FLD ( JCARD(6), JF(6), V1 )                   ! Read magnitude in 1st direction of coord sys CID
      CALL R8FLD ( JCARD(7), JF(7), V2 )                   ! Read magnitude in 2nd direction of coord sys CID
      CALL R8FLD ( JCARD(8), JF(8), V3 )                   ! Read magnitude in 3rd direction of coord sys CID

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,0 )     ! Make sure that there are no imbedded blanks in fields 2-8
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      DO I=2,8                                             ! Set JERR if any errors reading above data
         IF (IERRFL(I) == 'Y') THEN
            JERR = JERR + 1
         ENDIF
      ENDDO
       
! Write data to file LINK1I if there were no errors

      IF (JERR == 0) THEN
         VMAG = SQRT(V1*V1+V2*V2+V3*V3)
         IF (DABS(VMAG) < EPS1) THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,101) CARD
            WRITE(ERR,1137) JCARD(1), JCARD(2)
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,101) CARD
               ENDIF
               WRITE(F06,1137) JCARD(1), JCARD(2)
            ENDIF
            FORMON1 = ZERO
            FORMON2 = ZERO
            FORMON3 = ZERO
         ELSE 
            FORMON1 = SCALEF*V1
            FORMON2 = SCALEF*V2
            FORMON3 = SCALEF*V3
         ENDIF
         WRITE(L1I) SETID, GRID_NO, CID, FORMON1, FORMON2, FORMON3, FOR_OR_MOM
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1137 FORMAT(' *WARNING    : ',A,' ENTRY WITH SET ID = ',A,' HAS ZERO COMPONENTS')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_FORMOM
