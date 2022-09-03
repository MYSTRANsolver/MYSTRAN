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
 
      SUBROUTINE SOLID_STRESS_OUTPUTS ( SIZE_ALLOCATED, NUM1, NUM_FEMAP_ROWS, WRITE_OGEL, WRITE_FEMAP )
 
! Calculates element specific stress outputs from array STRESS (generated in subr ELEM_STRE_STRN_ARRAYS) for solid elements
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SOLID_STRESS_OUTPUTS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  STRESS, TYPE
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRE_OPT
      USE LINK9_STUFF, ONLY           :  OGEL
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_VECS
      USE PARAMS, ONLY                :  POST
 
      USE SOLID_STRESS_OUTPUTS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SOLID_STRESS_OUTPUTS'
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_OGEL         ! If 'Y' then write data to array OGEL
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_FEMAP        ! If 'Y' then write data to array FEMAP_EL_VECS
 
      INTEGER(LONG), INTENT(IN)       :: SIZE_ALLOCATED     ! No. of rows allocated to array that will be written to
      INTEGER(LONG), INTENT(IN)       :: NUM_FEMAP_ROWS     ! Number of rows that will be written to FEMAP arrays
      INTEGER(LONG), INTENT(INOUT)    :: NUM1               ! Cum rows written to OGEL prior to running this subr
      INTEGER(LONG)                   :: J                  ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SOLID_STRESS_OUTPUTS_BEGEND
 
      REAL(DOUBLE)                    :: MEAN               ! Mean stresses
      REAL(DOUBLE)                    :: PRINCIPAL_STRESS(3)! Principal stresses
      REAL(DOUBLE)                    :: SIG_OCT            ! Octrahedral normal stress
      REAL(DOUBLE)                    :: TAU_OCT            ! Octrahedral shear  stress
      REAL(DOUBLE)                    :: VONMISES           ! von Mises stress

      INTRINSIC DMAX1,DMIN1
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calc engineering stresses from array STRESS and put into array OGEL
 
      IF ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN
         CALL PRINCIPAL_3D ( STRESS, PRINCIPAL_STRESS, MEAN, VONMISES, SIG_OCT, TAU_OCT )
         IF (WRITE_OGEL == 'Y') THEN
            NUM1 = NUM1 + 1
            IF (NUM1 > SIZE_ALLOCATED) THEN
               WRITE(ERR,9200) SUBR_NAME,SIZE_ALLOCATED
               WRITE(F06,9200) SUBR_NAME,SIZE_ALLOCATED
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                      ! Coding error (dim of array OGEL too small), so quit
            ENDIF   
            DO J=1,6
               OGEL(NUM1,J) = STRESS(J)
            ENDDO
            IF (STRE_OPT == 'VONMISES') THEN
               OGEL(NUM1,7) = VONMISES
               OGEL(NUM1,8) = ZERO
            ELSE
               OGEL(NUM1,7) = SIG_OCT
               OGEL(NUM1,8) = TAU_OCT
            ENDIF
            OGEL(NUM1,9) = PRINCIPAL_STRESS(1)
            OGEL(NUM1,10) = PRINCIPAL_STRESS(2)
            OGEL(NUM1,11) = PRINCIPAL_STRESS(3)
            OGEL(NUM1,12) = MEAN
         ENDIF
         IF ((POST /= 0) .AND. (WRITE_FEMAP == 'Y')) THEN
            DO J=1,6
               FEMAP_EL_VECS(NUM_FEMAP_ROWS,J) = STRESS(J)
            ENDDO
            FEMAP_EL_VECS(NUM_FEMAP_ROWS, 7) = PRINCIPAL_STRESS(1)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS, 8) = PRINCIPAL_STRESS(2)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS, 9) = PRINCIPAL_STRESS(3)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,10) = MEAN
            IF (STRE_OPT == 'VONMISES') THEN
               FEMAP_EL_VECS(NUM_FEMAP_ROWS,11) = VONMISES
               FEMAP_EL_VECS(NUM_FEMAP_ROWS,12) = ZERO
            ELSE
               FEMAP_EL_VECS(NUM_FEMAP_ROWS,11) = SIG_OCT
               FEMAP_EL_VECS(NUM_FEMAP_ROWS,12) = TAU_OCT
            ENDIF
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
 
! **********************************************************************************************************************************
 9203 FORMAT(' *ERROR  9203: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INCORRECT ELEMENT TYPE = "',A,'"')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE SOLID_STRESS_OUTPUTS
