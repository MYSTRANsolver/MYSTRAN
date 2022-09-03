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
  
      SUBROUTINE BD_PARAM0 ( CARD )
  
! Processes several PARAM Bulk Data entries that need to be pre-processed before LOADB reads the Bulk data deck
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EPSIL1_SET, IERRFL, JCARD_LEN, JF, MEPSIL, MPBARLU
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL, GRIDSEQ, PBARLDEC, PBARLSHR
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PARAM0_BEGEND
 
      USE BD_PARAM0_USE_IFs

      IMPLICIT NONE
 
      INTEGER(LONG)                   :: I                 ! DO loop index

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PARAM0'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CHARINP           ! A character field from CARD
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PARAM0_BEGEND
      INTEGER(LONG)                   :: I4INP             ! An integer value read

      REAL(DOUBLE)                    :: R8INP             ! A real value read

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      CALL LEFT_ADJ_BDFLD ( JCARD(3) )
 
      IF      (JCARD(2) == 'GRIDSEQ ') THEN

         IF      (JCARD(3)(1:6) == 'BANDIT'  ) THEN
            GRIDSEQ = 'BANDIT  '
         ELSE IF (JCARD(3)(1:4) == 'GRID'    ) THEN
            GRIDSEQ = 'GRID    '
         ELSE IF (JCARD(3)(1:5) == 'INPUT'   ) THEN
            GRIDSEQ = 'INPUT   '
         ENDIF

      ELSE IF (JCARD(2)(1:8) == 'PBARLDEC') THEN

         CALL I4FLD ( JCARD(3), JF(3), I4INP )
         IF (IERRFL(3) == 'N') THEN
            IF ((I4INP >= 0) .AND. (I4INP <= MPBARLU)) THEN
               PBARLDEC = I4INP
            ENDIF
         ENDIF

      ELSE IF (JCARD(2)(1:8) == 'PBARLSHR') THEN
         CALL CHAR_FLD ( JCARD(3), JF(3), CHARINP )
         IF (IERRFL(3) == 'N') THEN
            PBARLSHR = CHARINP
         ENDIF

      ELSE IF (JCARD(2)(1:8) == 'EPSIL   ') THEN
         CALL I4FLD ( JCARD(3), JF(3), I )
         IF (IERRFL(3) == 'N') THEN
            IF ((I > 0) .AND. (I <= MEPSIL)) THEN
               CALL R8FLD ( JCARD(4), JF(4), R8INP )
               EPSIL(I) = R8INP
               IF (I == 1) THEN
                  EPSIL1_SET = 'Y'
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
 
      END SUBROUTINE BD_PARAM0

