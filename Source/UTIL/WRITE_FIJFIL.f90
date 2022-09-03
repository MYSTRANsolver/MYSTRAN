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
 
      SUBROUTINE WRITE_FIJFIL ( WHICH, JVEC )
 
! Writes elem matrices to unformatted disk files if disk file output for elem data is requested.
! User must have Case Control entries ELDATA in order to get these files written
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, F06, F21, F22, F23, F24, F25, F21_MSG, F22_MSG, F23_MSG, F24_MSG, F25_MSG
      USE DEBUG_PARAMETERS
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MAX_STRESS_POINTS, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_FIJFIL_BEGEND
      USE MODEL_STUF, ONLY            :  EID, TYPE, ELGP, ELDOF, KE, ME, PEB, PEG, PEL, PPE, PTE,                                  &
                                         SE1, SE2, SE3, STE1, STE2, STE3, UEB, UEG, UEL 
      USE PARAMS, ONLY                :  ELFORCEN

      USE WRITE_FIJFIL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_FIJFIL'
 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Internal subcase or vector number for data to be written
      INTEGER(LONG), INTENT(IN)       :: WHICH             ! Which F2j file to write to
      INTEGER(LONG)                   :: I,J, K            ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_FIJFIL_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF
 
! **********************************************************************************************************************************

      IF (( DEBUG(193) == 1) .OR. (DEBUG(193) == 999)) THEN
         CALL FILE_INQUIRE ( 'In WRITE_FIJFIL' )
      ENDIF

! Write element thermal, pressure, loads to disk file if requested

      IF ((WHICH == 1) .OR. (WHICH == 9999)) THEN
         WRITE(F21) F21_MSG
         WRITE(F21) EID
         WRITE(F21) TYPE
         WRITE(F21) ELDOF
         WRITE(F21) NTSUB
         WRITE(F21) NSUB
         DO I=1,NTSUB
            DO J=1,ELDOF
               WRITE(F21) PTE(J,I)
            ENDDO 
         ENDDO 
         DO I=1,NSUB
            DO J=1,ELDOF
               WRITE(F21) PPE(J,I)
            ENDDO 
         ENDDO 
!xx      WRITE(F21) 'FINISHED'
      ENDIF
   
! Write element mass matrix to disk file if requested
 
      IF ((WHICH == 2) .OR. (WHICH == 9999)) THEN
         WRITE(F22) F22_MSG
         WRITE(F22) EID
         WRITE(F22) TYPE
         WRITE(F22) ELDOF
         DO I=1,ELDOF
            DO J=I,ELDOF
               WRITE(F22) ME(I,J)
            ENDDO
         ENDDO
!xx      WRITE(F22) 'FINISHED'
      ENDIF   

! Write element stiffness matrix to disk file if requested
 
      IF ((WHICH == 3) .OR. (WHICH == 9999)) THEN
         WRITE(F23) F23_MSG
         WRITE(F23) EID
         WRITE(F23) TYPE
         WRITE(F23) ELDOF
         DO I=1,ELDOF
            DO J=I,ELDOF
               WRITE(F23) KE(I,J)
            ENDDO
         ENDDO
!xx      WRITE(F23) 'FINISHED'
      ENDIF   
 
! Write element stress recovery matrices to disk file if requested
 
      IF ((WHICH == 4) .OR. (WHICH == 9999)) THEN

         WRITE(F24) F24_MSG
         WRITE(F24) EID
         WRITE(F24) TYPE
         WRITE(F24) ELDOF
         WRITE(F24) NTSUB
         WRITE(F24) MAX_STRESS_POINTS

         DO K=1,MAX_STRESS_POINTS+1 
             DO I=1,3
                DO J=1,ELDOF
                   WRITE(F24) SE1(I,J,K)
                ENDDO
             ENDDO   
 
             DO I=1,3
                DO J=1,ELDOF
                   WRITE(F24) SE2(I,J,K)
                ENDDO
             ENDDO   
 
             DO I=1,3
                DO J=1,ELDOF
                   WRITE(F24) SE3(I,J,K)
                ENDDO
             ENDDO   
 
             DO J=1,NTSUB
                DO I=1,3
                   WRITE(F24) STE1(I,J,K)
                ENDDO
             ENDDO   
 
             DO J=1,NTSUB
                DO I=1,3
                   WRITE(F24) STE2(I,J,K)
                ENDDO
             ENDDO   
 
             DO J=1,NTSUB
                DO I=1,3
                   WRITE(F24) STE3(I,J,K)
                ENDDO
             ENDDO   

         ENDDO

!xx      WRITE(F24) 'FINISHED'
      ENDIF
 
! Write element loads, displ's to disk file if requested
 
      IF ((WHICH == 5) .OR. (WHICH == 9999)) THEN
         WRITE(F25) F25_MSG
         WRITE(F25) 'Displs and forces are in coord system: ', ELFORCEN
         WRITE(F25) EID
         WRITE(F25) TYPE
         WRITE(F25) ELDOF
         WRITE(F25) JVEC
         IF      (ELFORCEN == 'LOCAL') THEN
            DO I=1,ELDOF
               WRITE(F25) UEL(I),PEL(I)
            ENDDO
         ELSE IF (ELFORCEN == 'BASIC') THEN
            DO I=1,ELDOF
               WRITE(F25) UEB(I),PEB(I)
            ENDDO
         ELSE IF (ELFORCEN == 'GLOBAL') THEN
            DO I=1,ELDOF
               WRITE(F25) UEG(I),PEG(I)
            ENDDO
         ENDIF
!xx      WRITE(F25) 'FINISHED'
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_FIJFIL
