! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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
 
      SUBROUTINE GET_MATANGLE_FROM_CID ( ACID )
 
! Calcs THETAM for plate elements that have the material angle specified via a coord sys ID (ACID here) 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD, ZERO, ONE
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  CORD, EID, NUM_EMG_FATAL_ERRS, NUM_EMG_FATAL_ERRS, RCORD, TE, THETAM, TYPE
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_MATANGLE_FROM_CID_BEGEND
 
      USE GET_MATANGLE_FROM_CID_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_MATANGLE_FROM_CID'
      CHARACTER( 1*BYTE)              :: CORD_FND          ! If 'Y', ACID internal coord sys ID was found in array CORD

      INTEGER(LONG), INTENT(IN)       :: ACID              ! Actual coord system ID for the sys that defines the material axes
      INTEGER(LONG)                   :: I                 ! DO loop indices
      INTEGER(LONG)                   :: ICID              ! Internal coord sys ID for ACID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_MATANGLE_FROM_CID_BEGEND
 
      REAL(DOUBLE)                    :: DOT_XM            ! Dot product of VEC_XE and VEC_ME
      REAL(DOUBLE)                    :: EPS1              ! A small number to comapre to zero
      REAL(DOUBLE)                    :: MAG2_XE           ! Magnitude squared of VEC_XE
      REAL(DOUBLE)                    :: MAG2_ME           ! Magnitude squared of VEC_ME
      REAL(DOUBLE)                    :: MAG_XE            ! Magnitude of VEC_XE
      REAL(DOUBLE)                    :: MAG_ME            ! Magnitude of VEC_ME
      REAL(DOUBLE)                    :: VEC_XE(3)         ! Vector in x direction in element coord sys
      REAL(DOUBLE)                    :: VEC_XM(3)         ! Vector in x direction in material angle coord sys
      REAL(DOUBLE)                    :: VEC_ZE(3)         ! Vector in z direction in element coord sys
      REAL(DOUBLE)                    :: VEC_ME(3)         ! Vector proj of VEC_XM onto elem plane

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

      DO I=1,3
         VEC_XM(I) = ZERO
      ENDDO

      CORD_FND = 'N'
      ICID     = -1
      IF (ACID /= 0) THEN
i_do1:   DO I=1,NCORD
            IF (ACID == CORD(I,2)) THEN
               CORD_FND = 'Y'
               ICID = I
               EXIT i_do1
            ENDIF
         ENDDO i_do1
         IF (CORD_FND == 'N') THEN
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1822) 'COORD SYSTEM ', ACID, TYPE, EID
            WRITE(F06,1822) 'COORD SYSTEM ', ACID, TYPE, EID
            RETURN
         ENDIF
         DO I=1,3
            VEC_XM(I) = RCORD(ICID,3*I+1)
         ENDDO
      ELSE
         VEC_XM(1) = ONE
      ENDIF


      DO I=1,3
         VEC_XE(I) = TE(1,I)
         VEC_ZE(I) = TE(3,I)
      ENDDO


      CALL PROJ_VEC_ONTO_PLANE ( VEC_XM, VEC_ZE, VEC_ME )

      MAG2_XE = ZERO
      MAG2_ME = ZERO
      DOT_XM  = ZERO
      DO I=1,3
         MAG2_XE = MAG2_XE + VEC_XE(I)*VEC_XE(I)
         MAG2_ME = MAG2_ME + VEC_ME(I)*VEC_ME(I)
         DOT_XM  = DOT_XM  + VEC_XE(I)*VEC_ME(I)
      ENDDO
      MAG_XE = DSQRT(MAG2_XE)
      MAG_ME = DSQRT(MAG2_ME)
      
      IF ((DABS(MAG_XE) > EPS1) .AND. (DABS(MAG_ME) > EPS1)) THEN
         THETAM = ACOS( DOT_XM/(MAG_XE*MAG_ME) )
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         WRITE(ERR,1829) TYPE, EID, ACID, MAG_XE, MAG_ME
         WRITE(F06,1829) TYPE, EID, ACID, MAG_XE, MAG_ME
         THETAM = ZERO
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

 1829 FORMAT(' *ERROR  1829: CANNOT FIND MATERIAL ANGLE FOR ',A,I8,' USING COORD SYSTEM ',I8,'. EITHER THE VECTOR IN THE',         &
                           ' ELEM X DIRECTION'                                                                                     &
                    ,/,14X,' (VEC_XE) OR THE PROJECTION (VEC_ME) OF THE VECTOR IN THE X DIR OF THE ABOVE COORD SYSTEM IS NULL:'    &
                    ,/,14X,' MAG VEC_XE = ',1ES9.2,' AND MAG VEC_ME = ',1ES9.2)

! **********************************************************************************************************************************

      END SUBROUTINE GET_MATANGLE_FROM_CID
