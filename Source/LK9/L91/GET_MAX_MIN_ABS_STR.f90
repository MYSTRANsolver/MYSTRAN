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

      SUBROUTINE GET_MAX_MIN_ABS_STR ( NUM_ROWS, NUM_COLS, SECOND_LINE, MAX_ANS, MIN_ANS, ABS_ANS )

! Calculates maximums, minimums, absolute max's for columns of stress or strain output columns in array OGEL

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  
      USE LINK9_STUFF, ONLY           :  OGEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_MAX_MIN_ABS_STR_BEGEND

      USE GET_MAX_MIN_ABS_STR_USE_IFs                      ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_MAX_MIN_ABS_STR'
      CHARACTER(1*BYTE), INTENT(IN)   :: SECOND_LINE       ! If 'Y' then there are 2 lines of OGEL for each strain

      INTEGER(LONG) , INTENT(IN)      :: NUM_ROWS          ! Number of stress or strain rows in OGEL
      INTEGER(LONG) , INTENT(IN)      :: NUM_COLS          ! Number of MAX, MIN, ABS to calc (number of cols in OGEL)
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_MAX_MIN_ABS_STR_BEGEND

      REAL(DOUBLE) , INTENT(OUT)      :: ABS_ANS(NUM_COLS) ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE) , INTENT(OUT)      :: MAX_ANS(NUM_COLS) ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE) , INTENT(OUT)      :: MIN_ANS(NUM_COLS) ! Min for all grids output for each of the 6 disp components

      INTRINSIC                       :: MAX, MIN, DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO J=1,NUM_COLS
         ABS_ANS(J) =  ZERO
         MAX_ANS(J) = -MACH_LARGE_NUM 
         MIN_ANS(J) =  ZERO
      ENDDO 

      K = 0                                                ! Get max stresses or strains
      DO I=1,NUM_ROWS

         K = K + 1
         DO J=1,NUM_COLS
            IF (OGEL(K,J) > MAX_ANS(J)) THEN
               MAX_ANS(J) = OGEL(K,J)
            ENDIF
         ENDDO

         IF (SECOND_LINE == 'Y') THEN
            K = K + 1
            DO J=1,NUM_COLS
               IF (OGEL(K,J) > MAX_ANS(J)) THEN
                  MAX_ANS(J) = OGEL(K,J)
               ENDIF
            ENDDO
         ENDIF

      ENDDO

      DO J=1,NUM_COLS
         MIN_ANS(J) = MAX_ANS(J)
      ENDDO

      K = 0                                                ! Get min stresses or strains
      DO I=1,NUM_ROWS

         K = K + 1
         DO J=1,NUM_COLS
            IF (OGEL(K,J) < MIN_ANS(J)) THEN
               MIN_ANS(J) = OGEL(K,J)
            ENDIF
         ENDDO

         IF (SECOND_LINE == 'Y') THEN
            K = K + 1
            DO J=1,NUM_COLS
               IF (OGEL(K,J) < MIN_ANS(J)) THEN
                  MIN_ANS(J) = OGEL(K,J)
               ENDIF
            ENDDO
         ENDIF

      ENDDO

      DO J=1,NUM_COLS                                      ! Get absolute max stresses or strain
         ABS_ANS(J) = MAX( DABS(MAX_ANS(J)), DABS(MIN_ANS(J)) )
      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE GET_MAX_MIN_ABS_STR

