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
  
      SUBROUTINE PINFLG ( NUM_PFLAG_DOFS )
  
! Processes element pin flags to modify stiffness matrix
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  PINFLG_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE MODEL_STUF, ONLY            :  EID, ELDOF, NUM_EMG_FATAL_ERRS, KE, DOFPIN, TYPE
 
      USE PINFLG_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PINFLG'

      INTEGER(LONG), INTENT(IN)       :: NUM_PFLAG_DOFS    ! The number of pin flagged DOF's for this element

                                                           ! Indicator if a DOF to be pinned has zero stiffness
      CHARACTER( 1*BYTE)              :: ZERO_STIFF(NUM_PFLAG_DOFS)

      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Count of errors. Error occurs if a diag KE for a pinflaged DOF is zero
      INTEGER(LONG)                   :: PDOF              ! A DOF component number (1 digit) from array DOFPIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PINFLG_BEGEND
  
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare for real zero
 
      INTRINSIC DABS
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Check to make sure that the diagonal stiffness for the pin flagged DOF's are not zero
  
      IERROR = 0
      DO I=1,NUM_PFLAG_DOFS
         ZERO_STIFF(I) = 'N'
         PDOF = DOFPIN(I)
         IF (DABS(KE(PDOF,PDOF)) <= EPS1) THEN
            IERROR = IERROR + 1
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,1921) PDOF, TYPE, EID
            IF (SUPWARN == 'N') THEN
               WRITE(F06,1921) PDOF, TYPE, EID
            ENDIF
!xx         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
!xx         FATAL_ERR = FATAL_ERR + 1
            ZERO_STIFF(I) = 'Y'
         ENDIF
      ENDDO   
      IF (IERROR > 0) THEN
         RETURN
      ENDIF
  
! Process pin flags in KE
  
i_do: DO I = 1,NUM_PFLAG_DOFS
         IF (ZERO_STIFF(I) == 'N') THEN
            PDOF = DOFPIN(I)
            DO J=1,ELDOF
               IF (J /= PDOF) THEN
                  DO K=1,ELDOF
                     IF (K /= PDOF) THEN
                        IF (DABS(KE(PDOF,PDOF)) > EPS1) THEN
                           KE(J,K) = KE(J,K) - KE(PDOF,K)*KE(J,PDOF)/KE(PDOF,PDOF)
                        ELSE
                           WRITE(ERR,1937) TYPE, EID, PDOF
                           WRITE(F06,1937) TYPE, EID, PDOF
                           NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
                           FATAL_ERR = FATAL_ERR + 1
                           CYCLE i_do
                        ENDIF 
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO   
  
! Set row and column PDOF (pin flagged) to zero
  
            DO J=1,ELDOF
               KE(PDOF,J) = ZERO
               KE(J,PDOF) = ZERO
            ENDDO

         ENDIF

      ENDDO i_do   
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1921 FORMAT(' *WARNING    : DISPL COMPONENT ',I3,' ON ',A,' ELEMENT ',I8,' HAS NO STIFFNESS AND CANNOT BE PIN FLAGGED')

 1937 FORMAT(' *ERROR  1937: DURING THE PROCESS OF APPLYING PIN FLAGS ON ',A,' ELEMENT ',I8,' THE DIAGONAL STIFFNESS FOR DISPL '   &
                    ,/,14X,' COMPONENT ',I2,' HAS BECOME ZERO. CANNOT PIN FLAG THIS COMPONENT')



! **********************************************************************************************************************************
 
      END SUBROUTINE PINFLG
