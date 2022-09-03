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
 
      SUBROUTINE GET_GRID_6X6_MASS ( AGRID, IGRID, FOUND, GRID_MGG )
 
! Gets a 6 x 6 mass matrix for 1 grid point from the MGG mass matrix (which contains block diagonal 6 x 6 grid mass matrices).
! THis subr was not coded for SPOINT's so check if AGRID is an SPOINT and give program error and quit if it is
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NGRID, NTERM_MGG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_GRID_6X6_MASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DOF_TABLES, ONLY            :  TDOF
      USE SPARSE_MATRICES, ONLY       :  I2_MGG, J_MGG, MGG
 
      USE GET_GRID_6X6_MASS_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_GRID_6X6_MASS'
      CHARACTER( 1*BYTE), INTENT(OUT) :: FOUND             ! 'Y' if there is a mass matrix for this grid and 'N' otherwise

      INTEGER(LONG), INTENT(IN)       :: AGRID             ! Actual grid number of grid for which we want the 6 x 6 mass matrix
      INTEGER(LONG), INTENT(IN)       :: IGRID             ! Internal grid number of grid for which we want the 6 x 6 mass matrix
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters   
      INTEGER(LONG)                   :: I1,J1             ! Indices
      INTEGER(LONG)                   :: IGRID_DOF_NUM     ! G-set DOF number for IGRID
      INTEGER(LONG)                   :: NUM_COMPS         ! No. displ components (1 for SPOINT, 6 for actual grid)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_GRID_6X6_MASS_BEGEND

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

! Initialize outputs

      FOUND = 'N'

      DO I=1,6
         DO J=1,6
            GRID_MGG(I,J) = ZERO
         ENDDO
      ENDDO

      IGRID_DOF_NUM = 6*(IGRID - 1) + 1
k_do: DO K=1,NTERM_MGG

         IF ((I2_MGG(K) >= IGRID_DOF_NUM) .AND. (I2_MGG(K) <= IGRID_DOF_NUM + 5)) THEN

            I1 = MODULO(I2_MGG(K),6)
            IF (I1 > 0) THEN
               I = I1
            ELSE
               I = 6
            ENDIF

            J1 = MODULO( J_MGG(K),6)
            IF (J1 > 0) THEN
               J = J1
            ELSE
               J = 6
            ENDIF

            GRID_MGG(I,J) = MGG(K)
            FOUND = 'Y'

         ELSE

            IF (FOUND == 'Y') THEN
               EXIT k_do
            ELSE
               CYCLE k_do
            ENDIF

         ENDIF

      ENDDO k_do 

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
 1500 FORMAT(' *ERROR  1500: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT FIND INTERNAL GRID ID, IN ARRAY TDOF, FOR ACTUAL GRID ',I8)

 1501 FORMAT(' *ERROR  1501: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INTERNAL GRID ID, FOUND IN ARRAY TDOF, FOR ACTUAL GRID ',I8,' MUST BE > 0 BUT IS ',I8)

 1502 FORMAT(' *ERROR  1515: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SUBR NOT PROGRAMMED FOR ANYTHING BUT 6 COMP GRIDS BUT WAS CALLED FOR GRID ',I8,' WHICH HAS ',I3,      &
                           ' NUMBER OF COMPONENTS')


! **********************************************************************************************************************************

      END SUBROUTINE GET_GRID_6X6_MASS
