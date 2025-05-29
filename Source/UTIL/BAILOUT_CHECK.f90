! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
 
      FUNCTION BAILOUT_CHECK ( CALLING_SUBR, MATIN_NAME, MATIN_SET, NROWS, NTERMS, I_MATIN, MATIN, PRT_ERRS, FACTOR_DIAG )

! Performs two checks on the factorization of a matrix stored in Compressed Row Storage (CRS) format:
! 1. Non-positive definite if any of the diagonals of the factor are ~zero or negative.
! 2. Nearly singular if the ratio of any value on the diagonal of the matrix to its corresponding value on the diagonal of the factor
!    is greater than MAXRATIO.
! Returns TRUE if it's either non-positive definite or nearly singular.
! Writes messages to F06 and ERR.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LINKNO
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC       
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, MAXRATIO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  

      IMPLICIT NONE

      LOGICAL                         :: BAILOUT_CHECK

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*) , INTENT(IN)   :: CALLING_SUBR      ! The subr that called this subr (used for output error purposes)
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_NAME        ! Name of matrix to be decomposed
      CHARACTER(LEN=*) , INTENT(IN)   :: MATIN_SET         ! Set designator for the input matrix. If it corresponds to a MYSTRAN
!                                                            displ set (e.g. 'L ' set) then error messages about singulatities
!                                                            can reference the grid/comp that is singular (otherwise the row/col
!                                                            where the singularity occurs is referenced). If it is not a MYSTRAN
!                                                            set designator it should be blank
      CHARACTER(LEN=*) , INTENT(IN)   :: PRT_ERRS          ! If not 'N', print singularity errors
      CHARACTER(54*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run
      CHARACTER( 1*BYTE)              :: NONPOS_DEF        ! Indicates matrix was nonpositive definite

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzeros in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)  ! Indicators of number of nonzero terms in rows of matrix MATIN
      INTEGER(LONG)                   :: COMPV             ! Component number (1-6) of a grid DOF
      INTEGER(LONG)                   :: GRIDV             ! Grid number
      INTEGER(LONG)                   :: I                 ! DO loop index             
      INTEGER(LONG)                   :: IIMAX             ! Row/Col in MATIN where max diagonal term occurs
      
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERMS)     ! Matrix values
      REAL(DOUBLE)                    :: MATIN_DIAG(NROWS) ! Diagonal terms from MATIN matrix
      REAL(DOUBLE) , INTENT(IN)       :: FACTOR_DIAG(NROWS)! The diagonal of the factor
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: FAC_DIAG          ! Diagonal term in the tringular factor of MATIN
      REAL(DOUBLE)                    :: MAXIMAX_RATIO     ! Largest of the ratios of matrix diagonal to factor diagonal
      REAL(DOUBLE)                    :: RATIO             ! Ratio of matrix diagonal to factor diagonal


! **********************************************************************************************************************************

      EPS1 = EPSIL(1)
      
! Calculate and print ratios of diag to factor diag (if they are zero or negative or > MAXRATIO).

      CALL OURTIM
      MODNAM = 'CALC MAX RATIO OF MATRIX DIAGONAL TO FACTOR DIAGONAL'
      WRITE(SC1,3092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

      CALL COUNTER_INIT("     Getting diagonal of matrix, row", NROWS)
      DO I=1,NROWS                                         ! First, get diagonal terms from MATIN
         IF (I_MATIN(I) == I_MATIN(I+1)) THEN
            MATIN_DIAG(I) = ZERO
         ELSE
            MATIN_DIAG(I) = MATIN(I_MATIN(I))
         ENDIF
         CALL COUNTER_PROGRESS(I)
      ENDDO
      WRITE(SC1,*) CR13

      MAXIMAX_RATIO = -MACH_LARGE_NUM                                  ! Calc ratio of MATIN diag to factor diag
      NONPOS_DEF    = 'N'
      CALL COUNTER_INIT("     Calc ratios of matrix diag to factor diag: row", NROWS)
      DO I=1,NROWS

         CALL GET_GRID_AND_COMP ( MATIN_SET, I, GRIDV, COMPV  )

         FAC_DIAG = FACTOR_DIAG(I)

         IF (FAC_DIAG <= EPS1) THEN                        ! Zero or negative factor diagonal. (MATIN is nonpositive definite)

            NONPOS_DEF = 'Y'
            IF (PRT_ERRS /= 'N') THEN
               WRITE(ERR,982) MATIN_NAME, FAC_DIAG
               WRITE(F06,982) MATIN_NAME, FAC_DIAG
               IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
                  WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
                  WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
               ELSE 
                  WRITE(ERR,9812) I, CALLING_SUBR
                  WRITE(F06,9812) I, CALLING_SUBR
               ENDIF
            ENDIF

         ELSE

            RATIO = MATIN_DIAG(I)/FAC_DIAG
!                                                          Ratio is greater than param MAXRATIO
            IF ((DABS(RATIO) > MAXRATIO) .AND. (PRT_ERRS /= 'N')) THEN
               WRITE(ERR,983) MATIN_NAME, RATIO, MAXRATIO
               WRITE(F06,983) MATIN_NAME, RATIO, MAXRATIO
               IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
                  WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
                  WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
               ELSE 
                  WRITE(ERR,9812) I, CALLING_SUBR
                  WRITE(F06,9812) I, CALLING_SUBR
               ENDIF
            ENDIF

            IF (RATIO > MAXIMAX_RATIO) THEN                ! This is the largest of the ratios
               MAXIMAX_RATIO = RATIO
               IIMAX = I
            ENDIF

         ENDIF
         CALL COUNTER_PROGRESS(I)
      ENDDO
      WRITE(SC1,*) CR13  

      IF (NONPOS_DEF == 'N') THEN

         WRITE(ERR,984) MATIN_NAME, MAXIMAX_RATIO
         WRITE(F06,984) MATIN_NAME, MAXIMAX_RATIO
         CALL GET_GRID_AND_COMP ( MATIN_SET, IIMAX, GRIDV, COMPV  )
         IF ((GRIDV > 0) .AND. (COMPV > 0)) THEN
            WRITE(ERR,9811) GRIDV, COMPV, CALLING_SUBR 
            WRITE(F06,9811) GRIDV, COMPV, CALLING_SUBR
         ELSE 
            WRITE(ERR,9812) IIMAX, CALLING_SUBR
            WRITE(F06,9812) IIMAX, CALLING_SUBR
         ENDIF

      ENDIF

      BAILOUT_CHECK = (DABS(MAXIMAX_RATIO) > MAXRATIO) .OR. (NONPOS_DEF == 'Y')


!***********************************************************************************************************************************

  982 FORMAT(' *ERROR   982: MATRIX ',A,' IS NONPOSITIVE DEFINITE. A DIAGONAL TERM IS ZERO OR NEGATIVE = ',1ES14.6)

  983 FORMAT(' *ERROR   983: MATRIX ',A,' HAS AN ABSOLUTE VALUE OF THE RATIO OF MATRIX DIAG TO FACTOR DIAG = ',1ES10.2,            &
                           ' (GREATER THAN BULK DATA PARAM MAXRATIO = ',1ES10.2,')'                                                &
                    ,/,14X,' THIS WILL ONLY BE A FATAL ERROR IF PARAM BAILOUT > 0')

  984 FORMAT(' *INFORMATION: THE MAXIMUM ABSOLUTE VALUE OF THE RATIO OF MATRIX DIAGONAL TO FACTOR DIAG FOR MATRIX ',A,' = ',1ES14.6)

 3092 FORMAT(1X,I2,'/',A54,8X,2X,I2,':',I2,':',I2,'.',I3)

 9811 FORMAT('               THIS IS FOR ROW AND COL IN THE MATRIX FOR GRID POINT ',I8,' COMP ',I3,'. THE CALLING SUBR WAS: ',A,/)

 9812 FORMAT('               THIS IS FOR ROW AND COL ',I8,' IN THE MATRIX. THE CALLING SUBR WAS: ',A,/)

      RETURN

!***********************************************************************************************************************************

      END FUNCTION BAILOUT_CHECK
