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
  
      SUBROUTINE BD_GRDSET0 ( CARD )
 
! Processes GRDSET Bulk Data Cards from LOADB0 to set GRDSET3, 7, 8 prior
! to reading bulk data in LOADB. If there are errors reading the GRDSET
! card entries, messages are not printed out until LOADB calls BD_GRDSET

!  1) GRDSET3 is field 3 for a GRID card (the input  coord system)
!  2) GRDSET7 is field 7 for a GRID card (the global coord system)
!  2) GRDSET7 is field 8 for a GRID card (the perm SPC's)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, JF, IERRFL, NGRDSET
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_GRDSET0_BEGEND
      USE MODEL_STUF, ONLY            :  GRDSET3, GRDSET7, GRDSET8
 
      USE BD_GRDSET0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_GRDSET0'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
 
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_GRDSET0_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! GRDSET Bulk Data Card routine
 
!   FIELD         ITEM       ARRAY ELEMENT
!   -----   ---------------- ------------
!    3      Input Coord sys     GRDSET3
!    7      Displ Coord sys     GRDSET7
!    8      PSPC                GRDSET8
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Count the number of GRDSET cards. The count will be checked to make sure that there is no more than 1 in subr LOADB

      NGRDSET = NGRDSET + 1

! Only set GRDSET3, 7, 8 if this is the 1st GRDSET card.
! Reset IERRFL's to 'N', after testing, since CDRERR is not being called until BD_GRDSET is called from LOADB

      IF (NGRDSET == 1) THEN

         IF (JCARD(3)(1:) /= ' ') THEN
            CALL I4FLD ( JCARD(3), JF(3), I4INP )
            IF (IERRFL(3) == 'N') THEN
               GRDSET3 = I4INP
            ELSE
               IERRFL(3) = 'N'
            ENDIF
         ENDIF
 
         IF (JCARD(7)(1:) /= ' ') THEN
            CALL I4FLD ( JCARD(7), JF(7), I4INP )
            IF (IERRFL(7) == 'N') THEN
               GRDSET7 = I4INP
            ELSE
               IERRFL(7) = 'N'
            ENDIF
         ENDIF
 
         IF (JCARD(8)(1:) /= ' ') THEN
            CALL IP6CHK ( JCARD(8), JCARDO, IP6TYP, IDUM )
            IF (IP6TYP == 'COMP NOS') THEN
               CALL I4FLD ( JCARDO, JF(8), I4INP )
               IF (IERRFL(8) == 'N') THEN
                  GRDSET8 = I4INP
               ELSE
                  IERRFL(8) = 'N'
               ENDIF
            ENDIF
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
 
      END SUBROUTINE BD_GRDSET0
