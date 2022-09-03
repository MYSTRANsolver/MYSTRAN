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
  
      SUBROUTINE BD_NLPARM ( CARD, CC_NLSID_FND )
  
! Processes NLPARM Bulk Data Cards.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSUB
      USE TIMDAT, ONLY                :  TSEC
      USE NONLINEAR_PARAMS, ONLY      :  NL_MAXITER, NL_NUM_LOAD_STEPS, NL_SID
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_NLPARM_BEGEND
 
      USE BD_NLPARM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_NLPARM'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD              ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_NLSID_FND(LSUB)! 'Y' if B.D NLPARM card w/ same set ID (SID) as C.C. NLPARM = SID
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CARD_NAME         ! Field 1 of CARD
      CHARACTER(LEN(JCARD))           :: CHAR_SID          ! Field 2 of CARD
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I4INP             ! An integer value read from GRAV entry
      INTEGER(LONG)                   :: SETID             ! NLPARM set id
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_NLPARM_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! NLPARM Bulk Data Card routine
 
!   FIELD   ITEM                VARIABLE
!   -----   ------------        -------------
!     2     NL_SID              Set ID
!     3     NL_NUM_LOAD_STEPS   Number of increments into which the load is to be subdivided
!     7     NL_MAXITER          Maximum number of iterations for convergence in any 1 load step      


! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Initialize

      NL_NUM_LOAD_STEPS = 1

! Read and check data
 
      CARD_NAME = JCARD(1)
      CHAR_SID  = JCARD(2)

      CALL I4FLD ( JCARD(2), JF(2), SETID )
      IF (IERRFL(2) == 'N') THEN
         DO I=1,LSUB
            IF (SETID == NL_SID(I)) THEN
               CC_NLSID_FND(I) = 'Y'
            ENDIF
         ENDDO
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), I4INP )
      IF (IERRFL(3) == 'N') THEN
         IF (I4INP >= 0) THEN
            NL_NUM_LOAD_STEPS = I4INP
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1156) JF(3), CARD_NAME, CHAR_SID, I4INP
            WRITE(F06,1156) JF(3), CARD_NAME, CHAR_SID, I4INP
         ENDIF
      ENDIF

      IF (JCARD(7)(1:) /= ' ') THEN
         CALL I4FLD ( JCARD(7), JF(7), I4INP )
         IF (IERRFL(7) == 'N') THEN
            IF (I4INP >= 0) THEN
               NL_MAXITER = I4INP
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1156) JF(7), CARD_NAME, CHAR_SID, I4INP
               WRITE(F06,1156) JF(7), CARD_NAME, CHAR_SID, I4INP
            ENDIF
         ENDIF
      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,0,0,7,0,0 )     ! Make sure that there are no imbedded blanks in fields 2-3
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,4,5,6,0,8,9 )   ! Issue warning if fields 4,5,6,8,9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1156 FORMAT(' *ERROR  1156: ILLEGAL ENTRY IN FIELD ',I2,' OF ',A,A,' CARD. ENTRY MUST BE > 0 BUT WAS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_NLPARM
