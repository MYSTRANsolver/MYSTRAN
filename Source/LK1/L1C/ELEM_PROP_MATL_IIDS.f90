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
 
      SUBROUTINE ELEM_PROP_MATL_IIDS
 
! Fix up routine for element property and material ID's. When the Bulk Data File was read, element and material property ID's
! were actual ID values. WE need to convert these to consequetive integer ID's so that the element property and material arrays
! can be accessed sequentially. This subr performs that function. 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, IN4FIL_NUM, NUM_IN4_FILES, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DEDAT_Q4_SHELL_KEY, DEDAT_T3_SHELL_KEY, FATAL_ERR, MPCOMP0, MPCOMP_PLIES,   &
                                         NCMASS, NELE, NMATL, NPBAR, NPBEAM,                                                       &
                                         NPBUSH, NPCOMP, NPELAS, NPMASS, NPROD, npshear, NPSHEL, NPSOLID, NPUSER1, NPUSERIN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELEM_PROP_MATL_IIDS_BEGEND
      USE MODEL_STUF, ONLY            :  CMASS, ETYPE, EPNT, EDAT, PELAS, PROD, PBAR, PBEAM, PBUSH, PCOMP, PMASS, PSHEAR,          &
                                         PSHEL, PSOLID, PUSER1, PUSERIN, MATL
 
      USE ELEM_PROP_MATL_IIDS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME        = 'ELEM_PROP_MATL_IIDS'
      CHARACTER( 6*BYTE)              :: CMASS_TYPE        ! 'CMASS1', 'CMASS2' 'CMASS3' or 'CMASS4'
      CHARACTER( 1*BYTE)              :: FOUND             ! Used to indicate if a prop or mat'l ID was found 
      CHARACTER( 1*BYTE)              :: FOUND_PCOMP       ! Used to indicate if a PCOMP prop ID was found 
      CHARACTER( 1*BYTE)              :: FOUND_PSHEL       ! Used to indicate if a PSHELL prop ID was found 
      CHARACTER( 8*BYTE)              :: NAME = 'MATERIAL' ! Used for output error message
      CHARACTER( 8*BYTE)              :: PROPERTY_NAME     ! Name of an element property card
 
      INTEGER(LONG)                   :: EPNTK             ! Value from array EPNT at the row for this internal elem ID. It is the
!                                                            row number in array EDAT where data begins for this element.

      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Cumulative error count
      INTEGER(LONG)                   :: IN4_ID            ! 
      INTEGER(LONG)                   :: MATERIAL_ID       ! Material ID
      INTEGER(LONG)                   :: PCOMP_INDEX       ! Index into PCOMP array
      INTEGER(LONG)                   :: PCOMP_PLIES       ! Number of plies in PCOMP array
      INTEGER(LONG)                   :: PROPERTY_ID       ! Property ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELEM_PROP_MATL_IIDS_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      PROPERTY_NAME = '        '
      IERROR = 0

! Process PID'S on element connection cards to internal values
 
      DO I = 1,NELE
         EPNTK = EPNT(I)

         FOUND = 'N'

         IF      (ETYPE(I) == 'BAR     ') THEN             ! Process property ID's for BAR elements
            PROPERTY_NAME(1:4) = 'PBAR'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPBAR
               IF (PBAR(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF

         ELSE IF (ETYPE(I) == 'BEAM    ') THEN             ! Process property ID's for BEAM elements
            PROPERTY_NAME(1:4) = 'PBEAM'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPBEAM
               IF (PBEAM(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF

         ELSE IF (ETYPE(I)(1:4) == 'BUSH') THEN            ! Process property ID's for BUSH elements
            PROPERTY_NAME(1:5) = 'PBUSH'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPBUSH
               IF (PBUSH(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF

         ELSE IF (ETYPE(I)(1:4) == 'ELAS') THEN            ! Process property ID's for ELAS elements
            PROPERTY_NAME(1:5) = 'PELAS'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPELAS
               IF (PELAS(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
                                                           ! Process property ID's for solid elements 
         ELSE IF ((ETYPE(I) == 'HEXA8   ') .OR. (ETYPE(I) == 'HEXA20  ') .OR.                                                      &
                  (ETYPE(I) == 'PENTA6  ') .OR. (ETYPE(I) == 'PENTA15 ') .OR.                                                      &
                  (ETYPE(I) == 'TETRA4  ') .OR. (ETYPE(I) == 'TETRA10 ')) THEN
            PROPERTY_NAME(1:6) = 'PSOLID'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPSOLID
               IF (PSOLID(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
                                                           ! Process property ID's for shell elements
         ELSE IF ((ETYPE(I)(1:5) == 'QUAD4') .OR. (ETYPE(I)(1:5) == 'TRIA3')) THEN
            PROPERTY_ID = EDAT(EPNTK+1)

            FOUND_PSHEL = 'N'
            DO J = 1,NPSHEL                                ! Search PSHEL to see if we find ID
               IF (PSHEL(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND_PSHEL = 'Y'
                  EXIT
               ENDIF
            ENDDO
            IF (FOUND_PSHEL == 'Y') THEN   
               PROPERTY_NAME(1:6) = 'PSHELL'
               IF      (ETYPE(I)(1:5) == 'QUAD4') THEN     ! Set flag to indicate, in EDAT, that this element refers to a PSHELL
                  EDAT(EPNTK+DEDAT_Q4_SHELL_KEY) = 1       !     flag for QUAD's using PSHELL is 1 
               ELSE IF (ETYPE(I)(1:5) == 'TRIA3') THEN
                  EDAT(EPNTK+DEDAT_T3_SHELL_KEY) = 1       !     flag for TRIA's using PSHELL is 1
               ENDIF
            ENDIF

            FOUND_PCOMP = 'N'
            DO J = 1,NPCOMP                                ! Search PCOMP to see if we find ID
               IF (PCOMP(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND_PCOMP = 'Y'
                  EXIT
               ENDIF
            ENDDO
            IF (FOUND_PCOMP == 'Y') THEN   
               PROPERTY_NAME(1:5) = 'PCOMP'
               IF (ETYPE(I)(1:1) == 'Q') THEN              ! Set flag to indicate, in EDAT, that this element refers to a PSHELL
                  EDAT(EPNTK+DEDAT_Q4_SHELL_KEY) = 2       !     flag for QUAD's using PCOMP is 2
               ELSE
                  EDAT(EPNTK+DEDAT_T3_SHELL_KEY) = 2       !     flag for TRIA's using PCOMP  is 2
               ENDIF
            ENDIF

            IF ((FOUND_PSHEL == 'N') .AND. (FOUND_PCOMP == 'N')) THEN   
               WRITE(ERR,1404) 'PSHELL/PCOMP', PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) 'PSHELL/PCOMP', PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
 
            IF ((FOUND_PSHEL == 'Y') .AND. (FOUND_PCOMP == 'Y')) THEN   
               WRITE(ERR,1406) PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1406) PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
 
         ELSE IF (ETYPE(I) == 'SHEAR   ') THEN             ! Process property ID's for SHEAR elements
            PROPERTY_NAME(1:6) = 'PSHEAR'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPSHEAR
               IF (PSHEAR(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
 
         ELSE IF (ETYPE(I) == 'ROD     ') THEN             ! Process property ID's for ROD elements
            PROPERTY_NAME(1:4) = 'PROD'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPROD
               IF (PROD(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
 
         ELSE IF (ETYPE(I) == 'USER1   ') THEN             ! Process property ID's for USER1 elements
            PROPERTY_NAME(1:6) = 'PUSER1'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPUSER1
               IF (PUSER1(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
 
         ELSE IF (ETYPE(I) == 'USERIN  ') THEN             ! Process property ID's for USERIN elements
            PROPERTY_NAME(1:7) = 'PUSERIN'
            PROPERTY_ID = EDAT(EPNTK+1)
            DO J = 1,NPUSERIN
               IF (PUSERIN(J,1) == PROPERTY_ID) THEN
                  EDAT(EPNTK+1) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, ETYPE(I), EDAT(EPNTK)
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
            ENDIF
 
         ELSE IF (ETYPE(I) == 'PLOTEL  ') THEN
            CONTINUE

         ELSE                                              

            WRITE(ERR,1403) SUBR_NAME,ETYPE(I) 
            WRITE(F06,1403) SUBR_NAME,ETYPE(I)
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                         ! Coding error (elem type not valid), so quit
 
         ENDIF
 
      ENDDO   
 
! **********************************************************************************************************************************
! Process MID'S on property cards to internal pointers
 
      PROPERTY_NAME(1:4) = 'PBAR'                          ! Process material ID's on PBAR
      DO I = 1,NPBAR
         FOUND = 'N'
         MATERIAL_ID = PBAR(I,2)
         DO J = 1,NMATL
            IF (MATL(J,1) == MATERIAL_ID) THEN
               PBAR(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PBAR(I,1)
            WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PBAR(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
      PROPERTY_NAME(1:4) = 'PBEAM'                         ! Process material ID's on PBEAM
      DO I = 1,NPBEAM
         FOUND = 'N'
         MATERIAL_ID = PBEAM(I,2)
         DO J = 1,NMATL
            IF (MATL(J,1) == MATERIAL_ID) THEN
               PBEAM(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PBEAM(I,1)
            WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PBEAM(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
      PROPERTY_NAME(1:4) = 'PSHEAR'                          ! Process material ID's on PSHEAR
      DO I = 1,NPSHEAR
         FOUND = 'N'
         MATERIAL_ID = PSHEAR(I,2)
         DO J = 1,NMATL
            IF (MATL(J,1) == MATERIAL_ID) THEN
               PSHEAR(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PSHEAR(I,1)
            WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PSHEAR(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
      PROPERTY_NAME(1:4) = 'PROD'                          ! Process material ID's on PROD
      DO I = 1,NPROD
         FOUND = 'N'
         MATERIAL_ID = PROD(I,2)
         DO J = 1,NMATL
            IF (MATL(J,1) == MATERIAL_ID) THEN
               PROD(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PROD(I,1)
            WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PROD(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
     PROPERTY_NAME(1:6) = 'PSHELL'                         ! Process material ID's on PSHELL
      DO I = 1,NPSHEL
         DO K = 2,5
            FOUND = 'N'
            MATERIAL_ID = PSHEL(I,K)
            IF (MATERIAL_ID == 0) CYCLE
            DO J = 1,NMATL
               IF (MATL(J,1) == MATERIAL_ID) THEN
                  PSHEL(I,K) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PSHEL(I,1)
               WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PSHEL(I,1)
               FATAL_ERR = FATAL_ERR + 1
               IERROR = IERROR + 1
            ELSE
               CYCLE
            ENDIF
         ENDDO
      ENDDO   

      PROPERTY_NAME(1:5) = 'PCOMP'                          ! Process material ID's on PCOMP
      DO I = 1,NPCOMP
         PCOMP_PLIES = PCOMP(I,5)
         DO K = 1,PCOMP_PLIES
            PCOMP_INDEX = MPCOMP0 + MPCOMP_PLIES*(K-1) + 1
            FOUND = 'N'
            MATERIAL_ID = PCOMP(I,PCOMP_INDEX)
            DO J = 1,NMATL
               IF (MATL(J,1) == MATERIAL_ID) THEN
                  PCOMP(I,PCOMP_INDEX) = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO   
            IF (FOUND == 'N') THEN   
               WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PCOMP(I,1)
               WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PCOMP(I,1)
               FATAL_ERR = FATAL_ERR + 1
               IERROR = IERROR + 1
            ELSE
               CYCLE
            ENDIF
         ENDDO
      ENDDO   

      PROPERTY_NAME(1:6) = 'PSOLID'                        ! Process material ID's on PSOLID
      DO I = 1,NPSOLID
         FOUND = 'N'
         MATERIAL_ID = PSOLID(I,2)
            DO J = 1,NMATL
            IF (MATL(J,1) == MATERIAL_ID) THEN
               PSOLID(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PSOLID(I,1)
            WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PSOLID(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
      PROPERTY_NAME(1:6) = 'PUSER1'                        ! Process material ID's on PUSER1
      DO I = 1,NPUSER1
         FOUND = 'N'
         MATERIAL_ID = PUSER1(I,2)
         DO J = 1,NMATL
            IF (MATL(J,1) == MATERIAL_ID) THEN
               PUSER1(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PUSER1(I,1)
            WRITE(F06,1401) NAME, MATERIAL_ID, PROPERTY_NAME, PUSER1(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
      PROPERTY_NAME(1:7) = 'PUSERIN'                       ! Process material ID's on PUSERIN (actually IN4 ID)
      DO I = 1,NPUSERIN
         FOUND = 'N'
         IN4_ID = PUSERIN(I,2)
         DO J = 1,NUM_IN4_FILES
            IF (IN4FIL_NUM(J) == IN4_ID) THEN
               PUSERIN(I,2) = J
               FOUND = 'Y'
               EXIT
            ENDIF
         ENDDO   
         IF (FOUND == 'N') THEN   
            WRITE(ERR,1401) 'IN4FIL_NUM', IN4_ID, PROPERTY_NAME, PUSERIN(I,1)
            WRITE(F06,1401) 'IN4FIL_NUM', IN4_ID, PROPERTY_NAME, PUSERIN(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF
      ENDDO   
 
! **********************************************************************************************************************************
! Check that all PMASS ID's have been defined

      PROPERTY_NAME(1:5) = 'PMASS'
      DO I=1,NCMASS

         PROPERTY_ID = CMASS(I,3)

         IF      (CMASS(I,2) == 1) THEN
            CMASS_TYPE = 'CMASS1'
         ELSE IF (CMASS(I,2) == 2) THEN
            CMASS_TYPE = 'CMASS2'
         ELSE IF (CMASS(I,2) == 3) THEN
            CMASS_TYPE = 'CMASS3'
         ELSE IF (CMASS(I,2) == 4) THEN
            CMASS_TYPE = 'CMASS4'
         ENDIF

         FOUND = 'N'

j_do:    DO J=1,NPMASS
            IF (PMASS(J,1) == PROPERTY_ID) THEN
               FOUND = 'Y'
               EXIT j_do
            ENDIF
         ENDDO j_do

         IF (FOUND == 'N') THEN
            WRITE(ERR,1404) PROPERTY_NAME, PROPERTY_ID, CMASS_TYPE, CMASS(I,1)
            WRITE(F06,1404) PROPERTY_NAME, PROPERTY_ID, CMASS_TYPE, CMASS(I,1)
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
         ENDIF

      ENDDO

! **********************************************************************************************************************************
! Quit if IERROR > 0

      IF (IERROR > 0) THEN
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
 1401 FORMAT(' *ERROR  1401: ',A,I8,' ON ',A,I8,' UNDEFINED')

 1403 FORMAT(' *ERROR  1403: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ELEMENT TYPE ',A,' NOT VALID')

 1404 FORMAT(' *ERROR  1404: ',A,I8,' ON C',A,I8,' UNDEFINED')

 1406 FORMAT(' *ERROR  1406: BOTH A PSHELL AND A PCOMP WITH ID ',I8,' WERE FOUND FOR C',A,I8,'. THEY CANNOT HAVE SAME ID.')


! **********************************************************************************************************************************
 
      END SUBROUTINE ELEM_PROP_MATL_IIDS
