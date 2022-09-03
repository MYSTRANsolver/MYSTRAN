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
  
      SUBROUTINE BD_DEBUG ( CARD )
  
! Processes DEBUG Bulk Data Cards
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, IERRFL, JCARD_LEN, JF, WARN_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_DEBUG_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG, NDEBUG
 
      USE BD_DEBUG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_DEBUG'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: INDEX             ! An index into array DEBUG read on B.D. DEBUG card
      INTEGER(LONG), PARAMETER        :: LOWER    = 1      ! Lower allowable value for an integer parameter
      INTEGER(LONG)                   :: UPPER    = NDEBUG ! Upper allowable value for an integer parameter
      INTEGER(LONG)                   :: VALUE             ! Value for DEBUG(INDEX) read on B.D. DEBUG card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_DEBUG_BEGEND
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! DEBUG Bulk Data Card routine
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read DEBUG index and DEBUG(INDEX) value

      CALL I4FLD ( JCARD(2), JF(2), INDEX )
      IF (IERRFL(2) == 'N') THEN
         IF ((INDEX >= LOWER) .AND. (INDEX <= UPPER)) THEN
            CALL I4FLD ( JCARD(3), JF(3), VALUE )
            IF (IERRFL(3) == 'N') THEN
               DEBUG(INDEX) = VALUE
            ENDIF
         ELSE
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,101) CARD
            WRITE(ERR,1120) LOWER,UPPER,INDEX
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,101) CARD
               ENDIF
               WRITE(F06,1120) LOWER,UPPER,INDEX
            ENDIF
         ENDIF
      ENDIF
  
      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,0,0,0,0,0,0 )   ! Make sure that there are no imbedded blanks in fields 2, 3
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,4,5,6,7,8,9 )   ! Issue warning if fields 4-9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1120 FORMAT(' *WARNING    : DEBUG INDEX MUST BE >= ',I4,' AND <= ',I4,' BUT INPUT VALUE IS: ',I8,'. ENTRY IGNORED')
   
! **********************************************************************************************************************************
 
      END SUBROUTINE BD_DEBUG
