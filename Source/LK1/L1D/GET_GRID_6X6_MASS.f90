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
 
      SUBROUTINE GET_GRID_6X6_MASS ( AGRID, IGRID, FOUND, GRID_MGG )
 
! Gets a 6 x 6 mass matrix for 1 grid point from the MGG mass matrix (which contains block diagonal 6 x 6 grid mass matrices).
! THis subr was not coded for SPOINT's so check if AGRID is an SPOINT and give program error and quit if it is
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NTERM_MGG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_GRID_6X6_MASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SPARSE_MATRICES, ONLY       :  I_MGG, J_MGG, MGG
 
      USE GET_GRID_6X6_MASS_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_GRID_6X6_MASS'
      CHARACTER( 1*BYTE), INTENT(OUT) :: FOUND             ! 'Y' if there is a mass matrix for this grid and 'N' otherwise

      INTEGER(LONG), INTENT(IN)       :: AGRID             ! Actual grid number of grid for which we want the 6 x 6 mass matrix
      INTEGER(LONG), INTENT(IN)       :: IGRID             ! Internal grid number of grid for which we want the 6 x 6 mass matrix
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters   
      INTEGER(LONG)                   :: NUM_COMPS         ! No. displ components (1 for SPOINT, 6 for actual grid)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_GRID_6X6_MASS_BEGEND
      INTEGER(LONG)                   :: ROW
      INTEGER(LONG)                   :: ROW_DOF
      INTEGER(LONG)                   :: COL_DOF

      REAL(DOUBLE), INTENT(OUT)       :: GRID_MGG(6,6)     ! 6 x 6 mass matrix for internal grid IGRID

      INTRINSIC                       :: MODULO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! If AGRID is an SPOINT give error and quit

      CALL GET_GRID_NUM_COMPS ( AGRID, NUM_COMPS, SUBR_NAME )
      IF (NUM_COMPS /= 6) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1502) AGRID, NUM_COMPS
         WRITE(F06,1502) AGRID, NUM_COMPS
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      GRID_MGG(:,:) = ZERO

      DO ROW_DOF=1,6
         ROW = 6*(IGRID - 1) + ROW_DOF
         DO K=I_MGG(ROW),NTERM_MGG                         ! Search from the first non-zero in this row onwards.

            COL_DOF = MODULO( J_MGG(K)-1,6)+1
            IF( COL_DOF == ROW_DOF ) THEN                  ! Found the diagonal of a 6x6 submatrix.
               GRID_MGG(ROW_DOF,COL_DOF) = GRID_MGG(ROW_DOF,COL_DOF) + MGG(K)
            ENDIF
                                       
            IF(K>=I_MGG(ROW+1)) THEN                       ! Stop searching after the end of the row.
               EXIT
            ENDIF

         ENDDO
      
      ENDDO

      FOUND = 'Y'                                          ! Always found because diagonals are always non-zero.

                                                           ! Copy upper triangle to lower triangle.
      DO I=1,6
         DO J=1,I-1
            GRID_MGG(I,J) = GRID_MGG(J,I)
         ENDDO 
      ENDDO 


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

 1502 FORMAT(' *ERROR  1515: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SUBR NOT PROGRAMMED FOR ANYTHING BUT 6 COMP GRIDS BUT WAS CALLED FOR GRID ',I8,' WHICH HAS ',I3,      &
                           ' NUMBER OF COMPONENTS')


! **********************************************************************************************************************************

      END SUBROUTINE GET_GRID_6X6_MASS
