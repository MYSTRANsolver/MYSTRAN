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
 
      SUBROUTINE WRITE_GRD_PCH_OUTPUTS ( JSUB, NUM, WHAT )
 
! Writes "punch" output for grid point related quantities (accels, displacements, eigenvectors, applied loads and SPC, MPC forces)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, PCH
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, INT_SC_NUM, PCH_LINE_NUM, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_GRD_PCH_OUTPUTS_BEGEND
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, OGEL
      USE MODEL_STUF, ONLY            :  GRID, LABEL, SCNUM, SUBLOD, STITLE, TITLE
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL
  
      USE WRITE_GRD_PCH_OUTPUTS_USE_IFs

      IMPLICIT NONE
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_GRD_PCH_OUTPUTS'
      CHARACTER(LEN=*) , INTENT(IN)   :: WHAT              ! Indicator whether to process displ or force output requests
      CHARACTER(LEN=1)                :: G_OR_S            ! 'G' if a grid point or 'S' if a scalar point
      CHARACTER(LEN=19)               :: OUTNAM            ! An output name for a header for the PCH file

      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_GRD_PCH_OUTPUTS_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  Make sure that WHAT is a valid value
      IF ((WHAT == 'ACCE') .OR. (WHAT == 'DISP') .OR. (WHAT == 'OLOAD') .OR. (WHAT == 'SPCF') .OR. (WHAT(1:4) == 'MPCF')) THEN
         CONTINUE
      ELSE
         WRITE(ERR,9100) WHAT
         WRITE(F06,9100) WHAT
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Write output headers.

      PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9003)  TITLE(INT_SC_NUM)(1:61), PCH_LINE_NUM
      PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9004) STITLE(INT_SC_NUM)(1:61), PCH_LINE_NUM
      PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9005)  LABEL(INT_SC_NUM)(1:61), PCH_LINE_NUM

      IF      (WHAT == 'ACCE' ) THEN
         OUTNAM = 'ACCELERATION       '
      ELSE IF (WHAT == 'DISP' ) THEN
         IF      ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN 
            OUTNAM = 'DISPLACEMENT OUTPUT'
         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN
            OUTNAM = 'DISPLACEMENT OUTPUT'
         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
            OUTNAM = 'EIGENVECTOR OUTPUT '
         ELSE IF  (SOL_NAME(1:5) == 'MODES') THEN
            OUTNAM = 'EIGENVECTOR OUTPUT '
         ENDIF
      ELSE IF (WHAT == 'OLOAD') THEN
         OUTNAM = 'OLOAD OUTPUT       '
      ELSE IF (WHAT == 'SPCF' ) THEN
         OUTNAM = 'SPCF OUTPUT        '
      ELSE IF (WHAT == 'MPCF' ) THEN
         OUTNAM = 'MPCF OUTPUT        '
      ENDIF
      PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9006) OUTNAM, PCH_LINE_NUM

      PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9006) 'REAL OUTPUT        ', PCH_LINE_NUM

      IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9011) SCNUM(JSUB), PCH_LINE_NUM

      ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN

         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9011) SCNUM(LOAD_ISTEP), PCH_LINE_NUM

      ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN

         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9011) SCNUM(LOAD_ISTEP), PCH_LINE_NUM
         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9013) EIGEN_VAL(JSUB), JSUB, PCH_LINE_NUM

      ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9011) SCNUM(LOAD_ISTEP), PCH_LINE_NUM
         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9013) EIGEN_VAL(JSUB), JSUB, PCH_LINE_NUM

      ENDIF

! Write accels, displ's, applied forces or SPC forces (also calc TOTALS for forces if that is being output)
! TOTALS(J) is summation of G.P. values of applied forces, SPC forces, or MFC forces, for each of the J=1,6 components.
  
      DO I=1,NUM

         IF      (GRID(I,6) == 1) THEN
            G_OR_S = 'S'
         ELSE IF (GRID(I,6) == 6) THEN
            G_OR_S = 'G'
         ELSE
! ERROR
         ENDIF

         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9902) GID_OUT_ARRAY(I,1), G_OR_S, (OGEL(I,J),J=1,3), PCH_LINE_NUM

         PCH_LINE_NUM = PCH_LINE_NUM + 1  ;  WRITE(PCH,9903) (OGEL(I,J),J=4,6), PCH_LINE_NUM          

      ENDDO
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9003 FORMAT('$TITLE   = ',A,I8)

 9004 FORMAT('$SUBTITLE= ',A,I8)

 9005 FORMAT('$LABEL   = ',A,I8)

 9006 FORMAT('$',A,52X,I8)

 9011 FORMAT('$SUBCASE ID = ',I11,47X,I8)

 9012 FORMAT('$EIGENVECTOR ',I8,51X,I8)

 9013 FORMAT('$EIGENVALUE = ',1ES14.7,2X,'MODE =',I6,30X,I8)

 9100 FORMAT(' *ERROR  9100: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ILLEGAL INPUT FOR VARIABLE "WHAT" = ',A)

 9902 FORMAT(I10,7X,A1,3(1ES18.6),I8)

 9903 FORMAT('-CONT-',12X,3(1ES18.6),I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_GRD_PCH_OUTPUTS
