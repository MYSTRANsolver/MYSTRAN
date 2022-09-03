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
 
      SUBROUTINE JAC3D ( SSI, SSJ, SSK, DPSHG, WRT_BUG_THIS_TIME, JAC, JACI, DETJ )
  
! Computes Jacobian for 3D elements.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  JACOBIAN_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  EID, ELGP, NUM_EMG_FATAL_ERRS, TYPE, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
  
      USE JAC3D_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'JAC3D'
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = JACOBIAN_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: SSI               ! A Gauss point coord.
      REAL(DOUBLE) , INTENT(IN)       :: SSJ               ! A Gauss point coord.
      REAL(DOUBLE) , INTENT(IN)       :: SSK               ! A Gauss point coord.
      REAL(DOUBLE) , INTENT(IN)       :: DPSHG(3,ELGP)     ! 3 x ELGP array of derivatives of element shape functions evaluated at
!                                                            Gauss abscissa coords
      REAL(DOUBLE) , INTENT(OUT)      :: DETJ              ! Determinant of JAC
      REAL(DOUBLE) , INTENT(OUT)      :: JAC(3,3)          ! 3 x 3 Jacobian matrix
      REAL(DOUBLE) , INTENT(OUT)      :: JACI(3,3)         ! 3 x 3 inverse of JAC
      REAL(DOUBLE)                    :: B(3,3)            ! Array used in calculating inverse of JAC
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: XL(ELGP,3)        ! Array of local element coords for the element (note: cannot use XEL
!                                                            directly since it is dimensioned MELGP x 3, not ELGP x 3)
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, WRT_BUG_THIS_TIME, WRT_BUG(7), WRT_BUG(8), WRT_BUG(9)
 9001    FORMAT(1X,A,' BEGN ',F10.3, 3X, A1, 3(I3))
      ENDIF
! **********************************************************************************************************************************
! Initialize outputs

      DETJ = ZERO

      DO I=1,3
         DO J=1,3
            JAC(I,J)  = ZERO
            JACI(I,J) = ZERO
         ENDDO
      ENDDO

      EPS1 = EPSIL(1)
  
      DO I=1,ELGP                                          ! Use XL array since XEL is dimensioned MELGP x 3, not ELGP x 3
         DO J=1,3
            XL(I,J) = XEL(I,J)
         ENDDO
      ENDDO

      CALL MATMULT_FFF ( DPSHG, XL, 3, ELGP, 3, JAC )

      B(1,1) = JAC(2,2)*JAC(3,3) - JAC(2,3)*JAC(3,2)
      B(1,2) = JAC(2,1)*JAC(3,3) - JAC(2,3)*JAC(3,1)
      B(1,3) = JAC(2,1)*JAC(3,2) - JAC(2,2)*JAC(3,1)

      B(2,1) = JAC(1,2)*JAC(3,3) - JAC(1,3)*JAC(3,2)
      B(2,2) = JAC(1,1)*JAC(3,3) - JAC(1,3)*JAC(3,1)
      B(2,3) = JAC(1,1)*JAC(3,2) - JAC(1,2)*JAC(3,1)

      B(3,1) = JAC(1,2)*JAC(2,3) - JAC(1,3)*JAC(2,2)
      B(3,2) = JAC(1,1)*JAC(2,3) - JAC(1,3)*JAC(2,1)
      B(3,3) = JAC(1,1)*JAC(2,2) - JAC(1,2)*JAC(2,1)

      DETJ = JAC(1,1)*(JAC(2,2)*JAC(3,3)  - JAC(2,3)*JAC(3,2))                                                                     &
           - JAC(1,2)*(JAC(2,1)*JAC(3,3)  - JAC(2,3)*JAC(3,1))                                                                     &
           + JAC(1,3)*(JAC(2,1)*JAC(3,2)  - JAC(2,2)*JAC(3,1))
 
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(7) > 0)) THEN
         WRITE(BUG,1101) SSI, SSJ, SSK
         WRITE(BUG,1102)
         DO I=1,3
            WRITE(BUG,1103) (JAC(I,J),J=1,3)
         ENDDO 
         WRITE(BUG,*)
         WRITE(BUG,*)
         WRITE(BUG,1104) DETJ
         WRITE(BUG,*)
         WRITE(BUG,*)
      ENDIF

! If DETJ is not zero, continue. Else, write error and return:
 
      IF (DETJ > EPS1) THEN
  
         JACI(1,1) =  B(1,1)/DETJ
         JACI(1,2) = -B(2,1)/DETJ
         JACI(1,3) =  B(3,1)/DETJ

         JACI(2,1) = -B(1,2)/DETJ
         JACI(2,2) =  B(2,2)/DETJ
         JACI(2,3) = -B(3,2)/DETJ

         JACI(3,1) =  B(1,3)/DETJ
         JACI(3,2) = -B(2,3)/DETJ
         JACI(3,3) =  B(3,3)/DETJ
  
         IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(7) > 0)) THEN
            WRITE(BUG,1105)
            DO I=1,3
               WRITE(BUG,1103) (JACI(I,J),J=1,3)
            ENDDO 
            WRITE(BUG,*)
            WRITE(BUG,*)
            WRITE(BUG,1106)
         ENDIF

      ELSE

         WRITE(ERR,1928) EID,TYPE,DETJ
         WRITE(F06,1928) EID,TYPE,DETJ
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         RETURN

      ENDIF
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1928 FORMAT(' *ERROR  1928: ELEMENT ',I8,', TYPE ',A,', HAS JACOBIAN LESS THAN OR EQUAL TO ZERO ( = ',1ES8.1,'). BAD GEOMETRY')

 1101 format(39x,'Outputs from subroutine JAC3D for Gauss point location:',/,39x,'------------------------------------------------'&
                 '-------',/,41x,'SSI = ', F9.6, '   ','SSJ = ', F9.6, '   ', 'SSK = ', F9.6,/)

 1102 FORMAT(58X,'Jacobian matrix:')

 1103 FORMAT(40X,3(1ES16.6))

 1104 FORMAT(53X,'Determinant of the Jacobian:',/,58X,1ES15.6)

 1105 FORMAT(51X,'Inverse of the Jacobian matrix:')

 1106 FORMAT(' ::::::::::::::::::::::::::::::END DEBUG( 6) OUTPUT FROM SHAPE FUNCTION AND JACOBIAN MATRIX SUBROUTINES:::::::::::', &
             '::::::::::::::::::',/,                                                                                               &
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

11228 format(20(1es14.6))

11229 format(3(1es14.6))



! **********************************************************************************************************************************
  
      END SUBROUTINE JAC3D
