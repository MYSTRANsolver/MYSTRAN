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

      SUBROUTINE GET_GRID_NUM_COMPS ( GRID_NUM, NUM_COMPS, CALLING_SUBR )

! Gets the number of components for a "grid" number from array GRID by testing GRID(I,6)
! If GRID(I,6) is 6 then this is a physical grid with 6 comps of displ and if GRID(I,6) is 1 then this is an SPOINT.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  GRID
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_GRID_NUM_COMPS_BEGEND

      USE GET_GRID_NUM_COMPS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_GRID_NUM_COMPS'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this one

      INTEGER(LONG), INTENT(IN)       :: GRID_NUM          ! A grid number (calling subr checked that it is an actual grid)
      INTEGER(LONG), INTENT(OUT)      :: NUM_COMPS         ! 6 if GRID_NUM is an physical grid, 1 if an SPOINT
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_GRID_NUM_COMPS_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      NUM_COMPS = 0

! Calc NUM_COMPS

      DO I=1,NGRID
         IF (GRID(I,1) == GRID_NUM) THEN
            NUM_COMPS = GRID(I,6)
            EXIT
         ENDIF
      ENDDO

! Error if NUM_COMPS not 1 or 6

      IF ((NUM_COMPS /= 1) .AND. (NUM_COMPS /= 6)) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,950) SUBR_NAME, CALLING_SUBR, GRID_NUM, NUM_COMPS
         WRITE(F06,950) SUBR_NAME, CALLING_SUBR, GRID_NUM, NUM_COMPS
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  950 FORMAT(' *ERROR   945: PROGRAMMING ERROR IN SUBROUTINE ',A,' CALLED BY SUBROUTINE ',A                                        &
                    ,/,14X,' FOR GRID ',I8,' THE NUMBER OF DISPL COMPONENTS IS ',I8,' BUT MUST BE EITHER 1 (SPOINT) OR 6 (GRID)')

! **********************************************************************************************************************************

      END SUBROUTINE GET_GRID_NUM_COMPS