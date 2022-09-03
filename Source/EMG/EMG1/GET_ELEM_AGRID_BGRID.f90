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

      SUBROUTINE GET_ELEM_AGRID_BGRID ( INT_ELEM_ID, CHECK_AGRID )

! Gets element actual and internal grid numbers given the element's internal ID

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, medat0_cuserin, MELGP, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_ELEM_AGRID_BGRID_BEGEND
      USE MODEL_STUF, ONLY            :  AGRID, BGRID, EDAT, EID, ELGP, EPNT, ETYPE, GRID, GRID_ID, TYPE

      USE GET_ELEM_AGRID_BGRID_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_ELEM_AGRID_BGRID'
      CHARACTER(LEN=*), INTENT(IN)    :: CHECK_AGRID       ! If 'Y' perform check on AGRID's to see if appropriate type

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID for which
      INTEGER(LONG)                   :: IERR        = 0   ! Local error count for BGRID not defined
      INTEGER(LONG)                   :: EPNTK             ! Value from array EPNT at the row for this internal elem ID. It is the
!                                                            row number in array EDAT where data begins for this element. 
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row num in GRID_ID where AGRID(I) exists
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: DELTA             ! Offset in EDAT (from 1st record for an elem) where grid no's begin
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_ELEM_AGRID_BGRID_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPNTK = EPNT(INT_ELEM_ID)
      TYPE  = ETYPE(INT_ELEM_ID)
      EID   = EDAT(EPNTK)

! AGRID/BGRID contain the G.P. no's (actual/internal) for points that the elem connects to (not for the v vector)
 
      DO I=1,MELGP+1
         AGRID(I) = 0
         BGRID(I) = 0
      ENDDO 

      CALL GET_ELGP ( INT_ELEM_ID )

      DO I=1,ELGP
         DELTA = 1
         IF (TYPE == 'BUSH    ') THEN                      ! 1st grid in EDAT for BUSH is at EPNTK+3 since "Num grids" is EPNTK+2
            DELTA = 2
         ENDIF
         IF (TYPE == 'USERIN  ') THEN
            DELTA = MEDAT0_CUSERIN - 1
         ENDIF
         AGRID(I) = EDAT(EPNTK+I+DELTA)
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(I), BGRID(I) )
         IF (BGRID(I) == -1) THEN
            WRITE(ERR,1900) AGRID(I), EID, TYPE
            WRITE(F06,1900) AGRID(I), EID, TYPE
            IERR = IERR + 1
            FATAL_ERR = FATAL_ERR + 1
            CYCLE
         ENDIF
      ENDDO

! Test to determine if AGRID's are appropriate for the elem TYPE (do test only if grid exists)

      IF (CHECK_AGRID == 'Y') THEN
         IF ((TYPE(1:4) /= 'ELAS') .AND. (TYPE /= 'USERIN  ')) THEN
            DO I=1,ELGP
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(I), GRID_ID_ROW_NUM )
               IF (GRID_ID_ROW_NUM > 0) THEN
                  IF (GRID(GRID_ID_ROW_NUM,6) /= 6) THEN
                     IERR = IERR + 1
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1951) TYPE, EID, AGRID(I)
                     WRITE(F06,1951) TYPE, EID, AGRID(I)
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF

      IF (IERR > 0) THEN
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
 1900 FORMAT(' *ERROR  1900: GRID ',I8,' ON ELEMENT ',I8,' TYPE ',A,' NOT  DEFINED') 

 1951 FORMAT(' *ERROR  1951: ',A,I8,' USES GRID ',I8,' WHICH IS A SCALAR POINT. SCALAR POINTS NOT ALLOWED FOR THIS ELEM TYPE')

! **********************************************************************************************************************************

      END SUBROUTINE GET_ELEM_AGRID_BGRID
