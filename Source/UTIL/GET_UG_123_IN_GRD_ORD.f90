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
 
      SUBROUTINE GET_UG_123_IN_GRD_ORD ( IERR )
 
! Gets the 3 translation (T1, T2, T3) displ values fro the G-set displ vector (UG_COL) for 1 subcase and puts those values into an
! NGRID x 3 array where T1 is col1, T2 is col 2 and T3 is col3

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  GRID_ID
      USE DOF_TABLES, ONLY            :  TDOFI 
      USE COL_VECS, ONLY              :  UG_COL
      USE MISC_MATRICES, ONLY         :  UG_T123_MAT
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_UG_123_IN_GRD_ORD_BEGEND

      USE GET_UG_123_IN_GRD_ORD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_UG_123_IN_GRD_ORD'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_UG_123_IN_GRD_ORD_BEGEND
      INTEGER(LONG), INTENT(OUT)      :: IERR              ! Local error indicator
      INTEGER(LONG)                   :: GRDS_GLOBAL(NGRID)! 
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IGRID             ! Count of grids (1 to NGRID)
      INTEGER(LONG)                   :: IDOFG             ! Count of G-set DOF's  (1 to NDOFG)
      INTEGER(LONG)                   :: NUM_COMPS         ! 6 if GRID_NUM is an physical grid, 1 if an SPOINT
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IERR = 0

! Put the grid numbers that are in TDOFI (which are in global order) into array GRDS_GLOBAL

      IGRID = 0
      IDOFG = 0
      DO I = 1,NGRID
         CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
         DO J = 1,NUM_COMPS
            IDOFG = IDOFG + 1
            IF (IDOFG > NDOFG) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, IDOFG, 'TDOFI' )
            IF (J == 1) THEN
               IGRID = IGRID + 1
               IF (IGRID > NGRID) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, IGRID, 'GRDS_GLOBAL' )
               GRDS_GLOBAL(IGRID) = TDOFI(IDOFG,1)
            ENDIF
         ENDDO
      ENDDO
! Make sure IGRID = NGRID and IDOFG = NDOFG. Otherwise something is wrong in the code above. That code was checked to make sure
! IGRID did not attempt to exceed NGRID and IDOFG did not attempt to exceed NDOFG. Now we make sure they are equal.

      IF (IGRID /= NGRID) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,966) SUBR_NAME, 'IGRID', IGRID, 'NGRID', NGRID
         WRITE(F06,966) SUBR_NAME, 'IGRID', IGRID, 'NGRID', NGRID
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      IF (IDOFG /= NDOFG) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,966) SUBR_NAME, 'IDOFG', IDOFG, 'NDOFG', NDOFG
         WRITE(F06,966) SUBR_NAME, 'IDOFG', IDOFG, 'NDOFG', NDOFG
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Put the data from UG_COL into an array that has 3 cols (for each of the T1, T2, T3 translation freedoms) and rows equal to NDOFG.

      IDOFG = 0
      DO I = 1,NGRID
         CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
j_do2:   DO J = 1,NUM_COMPS
            IDOFG = IDOFG + 1
            IF (J <= 3) THEN                               ! We only want the 3 translations (or 1 if SPOINT)
               UG_T123_MAT(I,J) = UG_COL(IDOFG)
            ELSE
               CYCLE j_do2
            ENDIF
         ENDDO j_do2
      ENDDO
! Sort GRDS_GLOBAL and UG_T123_MAT so that GRDS_GLOBAL is in numerical grid order

      CALL SORT_INT1_REAL3 ( SUBR_NAME, 'GRDS_GLOBAL and UG_T123_MAT', NGRID, GRDS_GLOBAL, UG_T123_MAT )
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  966 FORMAT(' *ERROR   966: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' VARIABLE ',A,' = ',I8,' BUT SHOULD BE THE SAME AS VARIABLE ',A,' = ',I8)

! **********************************************************************************************************************************

      END SUBROUTINE GET_UG_123_IN_GRD_ORD

