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
 
      SUBROUTINE SHELL_STRAIN_OUTPUTS ( SIZE_ALLOCATED, NUM1, NUM_FEMAP_ROWS, WRITE_OGEL, WRITE_FEMAP )
 
! Calculates element specific strain output from array STRAIN (calc'd in subr ELEM_STRE_STRN_ARRAYS) for shell elements (TRIA3,
! QUAD4, SHEAR) and puts results into array OGEL for later output to F06 file.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SHELL_STRAIN_OUTPUTS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  ANY_FAILURE_THEORY, FAILURE_THEORY, PCOMP_PROPS, STRAIN, STRESS, TYPE, ZS
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRN_OPT
      USE LINK9_STUFF, ONLY           :  FTNAME, OGEL
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_VECS
      USE PARAMS, ONLY                :  POST
 
      USE SHELL_STRAIN_OUTPUTS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SHELL_STRAIN_OUTPUTS'
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_OGEL         ! If 'Y' then write data to array OGEL
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_FEMAP        ! If 'Y' then write data to array FEMAP_EL_VECS
 
      INTEGER(LONG), INTENT(IN)       :: SIZE_ALLOCATED     ! No. of rows allocated to array that will be written to
      INTEGER(LONG), INTENT(IN)       :: NUM_FEMAP_ROWS     ! Number of rows that will be written to FEMAP arrays
      INTEGER(LONG), INTENT(INOUT)    :: NUM1               ! Cum rows written to OGEL prior to running this subr
      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: NUM_ROWS           ! Number of rows of stress for an element (plates have 2 ZS vals)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHELL_STRAIN_OUTPUTS_BEGEND
 
      REAL(DOUBLE)                    :: ANGLE              ! Angle of prin strains in plate elems (calc'd in subr PRINCIPAL_2D)
      REAL(DOUBLE)                    :: FAILURE_INDEX      ! Failure index (scalar value)
      REAL(DOUBLE)                    :: MEAN               ! Mean strains
      REAL(DOUBLE)                    :: STREi(6)           ! 6 components of stress
      REAL(DOUBLE)                    :: STRNi(6)           ! 6 components of strain
      REAL(DOUBLE)                    :: SMAJ,SMIN          ! Major/minor prin strains in plate elems (calc'd in subr PRINCIPAL_2D)
      REAL(DOUBLE)                    :: STRE_ALLOWABLES(9) ! Allowable strains (incl tension and compr for normal strains)
      REAL(DOUBLE)                    :: STRN_ALLOWABLES(9) ! Allowable strains  (incl tension and compr for normal strains)
      REAL(DOUBLE)                    :: SX,SY,SXY          ! In-plane strains in plate elements
      REAL(DOUBLE)                    :: SXZ,SYZ            ! Transverse shear strains in plate elements
      REAL(DOUBLE)                    :: SXYMAX             ! Max shear strain in plate elems (calc'd in subr PRINCIPAL_2D)
      REAL(DOUBLE)                    :: VONMISES           ! von Mises strain

      INTRINSIC DMAX1,DMIN1
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calculates strain output for shell elements (TRIA3, QUAD4, SHEAR) and puts results into array OGEL for later output to F06 file
 
      IF ((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'SHEAR')) THEN

         IF (PCOMP_PROPS == 'Y') THEN

            SX  = STRAIN(1)
            SY  = STRAIN(2)

            SXY = STRAIN(3)
            SXZ = STRAIN(7)
            SYZ = STRAIN(8)
            CALL PRINCIPAL_2D ( SX, SY, SXY, ANGLE, SMAJ, SMIN, SXYMAX, MEAN, VONMISES )
            IF (WRITE_OGEL == 'Y') THEN
               NUM1 = NUM1 + 1
               IF (NUM1 > SIZE_ALLOCATED) THEN
                  WRITE(ERR,9200) SUBR_NAME,SIZE_ALLOCATED
                  WRITE(F06,9200) SUBR_NAME,SIZE_ALLOCATED
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ENDIF   
               OGEL(NUM1,1) = SX
               OGEL(NUM1,2) = SY
               OGEL(NUM1,3) = SXY
               OGEL(NUM1,4) = SXZ
               OGEL(NUM1,5) = SYZ
               OGEL(NUM1,6) = ANGLE
               OGEL(NUM1,7) = SMAJ
               OGEL(NUM1,8) = SMIN
               IF (STRN_OPT == 'VONMISES') THEN
                  OGEL(NUM1,9) = VONMISES
               ELSE
                  OGEL(NUM1,9) = SXYMAX
               ENDIF
               CALL GET_COMP_SHELL_ALLOWS ( STRE_ALLOWABLES, STRN_ALLOWABLES )

               FAILURE_INDEX = ZERO
               IF (FAILURE_THEORY == 'NONE') THEN

                  FTNAME(NUM1)  = 'None'

               ELSE

                  STREi(1) = STRESS(1)                      ! STREi stress order for failure theory calc has #1 = x  direct stress
                  STREi(2) = STRESS(2)                      ! STREi stress order for failure theory calc has #2 = y  direct stress
                  STREi(3) = ZERO                           ! STREi stress order for failure theory calc has #3 = z  direct stress
                  STREi(4) = STRESS(8)                      ! STREi stress order for failure theory calc has #4 = yz shear  stress
                  STREi(5) = STRESS(7)                      ! STREi stress order for failure theory calc has #5 = zx shear  stress
                  STREi(6) = STRESS(3)                      ! STREi stress order for failure theory calc has #6 = xy shear  stress

                  STRNi(1) = STRAIN(1)                      ! STRNi strain order for failure theory calc has #1 = x  direct strain
                  STRNi(2) = STRAIN(2)                      ! STRNi strain order for failure theory calc has #2 = y  direct strain
                  STRNi(3) = ZERO                           ! STRNi strain order for failure theory calc has #3 = z  direct strain
                  STRNi(4) = STRAIN(8)                      ! STRNi strain order for failure theory calc has #4 = yz shear  strain
                  STRNi(5) = STRAIN(7)                      ! STRNi strain order for failure theory calc has #5 = zx shear  strain
                  STRNi(6) = STRAIN(3)                      ! STRNi strain order for failure theory calc has #6 = xy shear  strain

                  ANY_FAILURE_THEORY = 'Y'

                  IF ((FAILURE_THEORY == 'HILL') .OR. (FAILURE_THEORY == 'HOFF') .OR. (FAILURE_THEORY == 'TSAI')) THEN
                     CALL POLY_FAILURE_INDEX  ( STREi, STRE_ALLOWABLES, FAILURE_INDEX )
                  ELSE IF ((FAILURE_THEORY == 'STRE') .OR. (FAILURE_THEORY == 'STRN')) THEN
                     CALL INDEP_FAILURE_INDEX ( STREi, STRNi, STRE_ALLOWABLES, STRN_ALLOWABLES, FAILURE_INDEX )
                  ELSE
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,9205) '', FAILURE_THEORY, 'HILL, HOFF or TSAI'
                     WRITE(F06,9205) '', FAILURE_THEORY, 'HILL, HOFF or TSAI'
                     CALL OUTA_HERE ( 'Y' )
                  ENDIF

                  OGEL(NUM1,10) = FAILURE_INDEX
                  FTNAME(NUM1)  = FAILURE_THEORY(1:4)

               ENDIF

               IF ((POST /= 0) .AND. (WRITE_FEMAP == 'Y')) THEN
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,1) = SX
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,2) = SY
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,3) = SXY
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,4) = SXZ
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,5) = SYZ
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,6) = ANGLE
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,7) = SMAJ
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,8) = SMIN
                  IF (STRN_OPT == 'VONMISES') THEN
                     FEMAP_EL_VECS(NUM_FEMAP_ROWS,9) = VONMISES
                  ELSE
                     FEMAP_EL_VECS(NUM_FEMAP_ROWS,9) = SXYMAX
                  ENDIF
               ENDIF

            ENDIF

         ELSE                                              ! Not PCOMP

            NUM_ROWS = 2
            IF (TYPE(1:5) == 'SHEAR') THEN
               NUM_ROWS = 1
            ENDIF            

            DO I=1,NUM_ROWS
               SX  = STRAIN(1) + ZS(I)*STRAIN(4)
               SY  = STRAIN(2) + ZS(I)*STRAIN(5)
               SXY = STRAIN(3) + ZS(I)*STRAIN(6)
               SXZ = STRAIN(7)
               SYZ = STRAIN(8)
               CALL PRINCIPAL_2D ( SX, SY, SXY, ANGLE, SMAJ, SMIN, SXYMAX, MEAN, VONMISES )
               IF (WRITE_OGEL == 'Y') THEN
                  NUM1 = NUM1 + 1
                  IF (NUM1 > SIZE_ALLOCATED) THEN
                     WRITE(ERR,9200) SUBR_NAME,SIZE_ALLOCATED
                     WRITE(F06,9200) SUBR_NAME,SIZE_ALLOCATED
                     FATAL_ERR = FATAL_ERR + 1
                     CALL OUTA_HERE ( 'Y' )
                  ENDIF   
                  IF (TYPE(1:5) == 'SHEAR') THEN   
                     OGEL(NUM1, 1) = SX
                     OGEL(NUM1, 2) = SY
                     OGEL(NUM1, 3) = SXY
                     DO J=4,7
                        OGEL(NUM1,J) = ZERO
                     ENDDO
                  ELSE
                     OGEL(NUM1, 1) = ZS(I)
                     OGEL(NUM1, 2) = SX
                     OGEL(NUM1, 3) = SY
                     OGEL(NUM1, 4) = SXY
                     OGEL(NUM1, 5) = ANGLE
                     OGEL(NUM1, 6) = SMAJ
                     OGEL(NUM1, 7) = SMIN
                  ENDIF
                  IF (STRN_OPT == 'VONMISES') THEN
                     OGEL(NUM1, 8) = VONMISES
                  ELSE
                     OGEL(NUM1, 8) = SXYMAX
                  ENDIF
                  OGEL(NUM1, 9) = SXZ
                  OGEL(NUM1,10) = SYZ
               ENDIF
               IF ((POST /= 0) .AND. (WRITE_FEMAP == 'Y')) THEN
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 1) = SX
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 2) = SY
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 3) = SXY
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 4) = SMAJ
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 5) = SMIN
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 6) = ANGLE
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 7) = MEAN
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 8) = SXYMAX
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 9) = VONMISES
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+10) = SXZ
!                 FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+11) = SYZ

                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 1) = SX
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 2) = SY
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 3) = SXY
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 4) = SMAJ
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 5) = SMIN
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 6) = ANGLE
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 7) = MEAN
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 8) = SXYMAX
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+ 9) = VONMISES
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+10) = SXZ
                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,11*(I-1)+11) = SYZ

!xx               IF (STRN_OPT == 'VONMISES') THEN
!xx                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,9*(I-1)+8) = VONMISES
!xx               ELSE
!xx                  FEMAP_EL_VECS(NUM_FEMAP_ROWS,9*(I-1)+8) = SXYMAX
!xx               ENDIF
               ENDIF
            ENDDO   
 
         ENDIF

      ELSE IF (TYPE(1:6) == 'USERIN') THEN                 ! Stresses for USERIN elements

         IF (WRITE_OGEL == 'Y') THEN
            NUM1 = NUM1 + 1
            IF (NUM1 > SIZE_ALLOCATED) THEN
               WRITE(ERR,9200) SUBR_NAME,SIZE_ALLOCATED
               WRITE(F06,9200) SUBR_NAME,SIZE_ALLOCATED
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF   
            DO J=1,9
               OGEL(NUM1,J) = STRAIN(J)
            ENDDO
         ENDIF
         IF ((POST /= 0) .AND. (WRITE_FEMAP == 'Y')) THEN
            DO J=1,9
               FEMAP_EL_VECS(NUM_FEMAP_ROWS,J) = STRAIN(J)
            ENDDO
         ENDIF
 
      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9203) SUBR_NAME, TYPE
         WRITE(F06,9203) SUBR_NAME, TYPE
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
 9200 FORMAT(' *ERROR  9200: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAY OGEL WAS ALLOCATED TO HAVE ',I12,' ROWS. ATTEMPT TO WRITE TO OGEL BEYOND THIS')
 
 9203 FORMAT(' *ERROR  9203: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INCORRECT ELEMENT TYPE = "',A,'"')
 
 9205 FORMAT(' *ERROR  9205: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INVALID ',A,' FAILURE THEORY = ',A,'. VALID ONES ARE: ',A)



! **********************************************************************************************************************************

      END SUBROUTINE SHELL_STRAIN_OUTPUTS
