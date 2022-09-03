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

      SUBROUTINE GRID_ELEM_CONN_TABLE

! Calculates an array that has as many rows as there are grids and as many cols as the max number of elements connected to any grid.
! This array is used for 2 purposes:

!   1) If user has requested any grid point force balance output requests, or
!   2) the user has a PARAM PRTCONN Bulk data entry

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MAX_ELEM_DEGREE, NELE, NGRID
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  AGRID, ELGP, ETYPE, ESORT1, ESORT2, GRID_ID, GRID_ELEM_CONN_ARRAY 
      USE PARAMS, ONLY                :  PRTCONN 
      USE SUBR_BEGEND_LEVELS, ONLY    :  GRID_ELEM_CONN_TABLE_BEGEND

      USE GRID_ELEM_CONN_TABLE_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GRID_ELEM_CONN_TABLE'

      INTEGER(LONG)                   :: GRD_NUM_ELEM(NGRID)! Array that specifies the number of elements connected to each grid
      INTEGER(LONG)                   :: I,J,K              ! DO loop indices
      INTEGER(LONG)                   :: IGRID              ! Internal grid ID (row in array GRID_ID where an act grid num exists)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GRID_ELEM_CONN_TABLE_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,NGRID
         GRD_NUM_ELEM(I) = 0
      ENDDO

      DO J=1,NELE
         CALL GET_ELEM_AGRID_BGRID ( J, 'N' )
         DO K=1,ELGP
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(K), IGRID )
            GRD_NUM_ELEM(IGRID) = GRD_NUM_ELEM(IGRID) + 1
         ENDDO
      ENDDO

      MAX_ELEM_DEGREE = 0
      DO I=1,NGRID
         IF (GRD_NUM_ELEM(I) > MAX_ELEM_DEGREE) THEN
            MAX_ELEM_DEGREE = GRD_NUM_ELEM(I)
         ENDIF
      ENDDO

      CALL ALLOCATE_MODEL_STUF ( 'GRID_ELEM_CONN_ARRAY', SUBR_NAME )

      DO I=1,NGRID
         GRID_ELEM_CONN_ARRAY(I,1) = GRID_ID(I)
         GRID_ELEM_CONN_ARRAY(I,2) = GRD_NUM_ELEM(I)
      ENDDO

      DO I=1,NGRID
         GRD_NUM_ELEM(I) = 0
      ENDDO

      DO J=1,NELE
         CALL GET_ELEM_AGRID_BGRID ( J, 'N' )
         DO K=1,ELGP
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID(K), IGRID )
            GRD_NUM_ELEM(IGRID) = GRD_NUM_ELEM(IGRID) + 1
            IF (GRD_NUM_ELEM(IGRID) > MAX_ELEM_DEGREE) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, MAX_ELEM_DEGREE, 'GRID_ELEM_CONN_ARRAY')
            GRID_ELEM_CONN_ARRAY(IGRID,GRD_NUM_ELEM(IGRID)+2) = ESORT1(J)
         ENDDO
      ENDDO

      IF (PRTCONN > 0) THEN
         WRITE(F06,*)
         WRITE(F06,9100)
         DO I=1,NGRID
            WRITE(F06,9101) (GRID_ELEM_CONN_ARRAY(I,J),J=1,GRD_NUM_ELEM(I)+2)
         ENDDO
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9100 FORMAT(1X,'                     Table of elements connected to each grid ',//,                                               &
             1X,'         Grid    Num elems     ID''s of elements connected to this grid -->',/)

 9101 FORMAT(1X,2I13,32767I9)

! **********************************************************************************************************************************

      END SUBROUTINE GRID_ELEM_CONN_TABLE


