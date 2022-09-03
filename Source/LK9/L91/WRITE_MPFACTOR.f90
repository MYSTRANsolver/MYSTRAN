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
 
      SUBROUTINE WRITE_MPFACTOR                ! ( IHDR )
 
! Writes output for modal participation factors
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFG, NDOFR, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_MPFACTOR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, PI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, MPFACTOR_NR, MPFACTOR_N6
      USE MODEL_STUF, ONLY            :  LABEL, STITLE, TITLE
      USE PARAMS, ONLY                :  GRDPNT, MEFMCORD, MEFMGRID, MEFMLOC, MPFOUT
      USE DOF_TABLES, ONLY            :  TDOFI
  
      USE WRITE_MPFACTOR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_MPFACTOR'
!xx   CHARACTER(LEN=*) , INTENT(IN)   :: IHDR              ! Indicator of whether to write an output header
      CHARACTER(1*BYTE)               :: IHDR   = 'Y'      ! Indicator of whether to write an output header

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG)                   :: R_SET_GRIDS(NDOFR)! Array of grids for the R-set
      INTEGER(LONG)                   :: R_SET_COMPS(NDOFR)! Array of displ components for the R-set
      INTEGER(LONG)                   :: R_SET_COL         ! Col in TDOFI array where R-set exists
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_MPFACTOR_BEGEND

      REAL(DOUBLE)                    :: CYCLES            ! Circular frequency of a mode

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CALL TDOF_COL_NUM ( 'R ', R_SET_COL )
      K = 0
      DO I=1,NDOFG
         IF (TDOFI(I,R_SET_COL) /= 0) THEN
            K = K + 1
            R_SET_GRIDS(K) = TDOFI(I,1)
            R_SET_COMPS(K) = TDOFI(I,2)
         ENDIF
      ENDDO

      WRITE(F06,*)

! Write output headers.

      IF (IHDR == 'Y') THEN

         WRITE(F06,9000)

         WRITE(F06,9003) TITLE(1)                          ! There is always a TITLE(1), etc (even if they are blank)
         WRITE(F06,9003) STITLE(1)
         WRITE(F06,9003) LABEL(1)

         WRITE(F06,*)
 
      ENDIF
                                                           ! Write modal participation factors for CB analyses
      IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (MPFOUT == 'R')) THEN

         WRITE(F06,9004) MEFMCORD
         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,9014) MEFMCORD
         ENDIF

         IF (DEBUG(174) == 0) THEN
            WRITE(F06,9101) (I,I=1,NDOFR)
            WRITE(F06,9102) (R_SET_GRIDS(I), R_SET_COMPS(I),I=1,NDOFR)
            WRITE(F06,9103)
         ELSE
            WRITE(F06,9201) (I,I=1,NDOFR)
            WRITE(F06,9202) (R_SET_GRIDS(I), R_SET_COMPS(I),I=1,NDOFR)
            WRITE(F06,9203)
         ENDIF

         DO I=1,NVEC

            CYCLES = DSQRT(DABS(EIGEN_VAL(I)))/(TWO*PI)

            IF (DEBUG(174) == 0) THEN
               WRITE(F06,9301) I, CYCLES, (MPFACTOR_NR(I,J),J=1,NDOFR)
            ELSE
               WRITE(F06,9302) I, CYCLES, (MPFACTOR_NR(I,J),J=1,NDOFR)
            ENDIF

            IF (DEBUG(200) > 0) THEN                       ! Only <= 10 will fit in the DIF Excel spreadsheet
               WRITE(ANS,9311) I, CYCLES, (MPFACTOR_NR(I,J),J=1,10)
            ENDIF

         ENDDO

      ELSE

         WRITE(F06,9005) MEFMCORD
         IF      (MEFMLOC == 'GRDPNT') THEN
            IF (MEFMGRID == 0) THEN
               WRITE(F06,9006)
            ELSE
               WRITE(F06,9007) GRDPNT
            ENDIF
         ELSE IF (MEFMLOC == 'CG    ') THEN
            WRITE(F06,9008)
         ELSE IF (MEFMLOC == 'GRID  ') THEN
            WRITE(F06,9009) MEFMGRID
         ENDIF

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,9015) MEFMCORD
         ENDIF

         IF (DEBUG(174) == 0) THEN
            WRITE(F06,9501)
         ELSE
            WRITE(F06,9502)
         ENDIF

         DO I=1,NVEC

            CYCLES = DSQRT(DABS(EIGEN_VAL(I)))/(TWO*PI)

            IF (DEBUG(174) == 0) THEN
               WRITE(F06,9503) I, CYCLES, (MPFACTOR_N6(I,J),J=1,6)
            ELSE
               WRITE(F06,9504) I, CYCLES, (MPFACTOR_N6(I,J),J=1,6)
            ENDIF

            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,9513) I, CYCLES, (MPFACTOR_N6(I,J),J=1,6)
            ENDIF

         ENDDO         

      ENDIF

      WRITE(F06,*)
      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,*)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9000 FORMAT('--------------------------------------------------------------------------------------------------------------------'&
            ,'----------------')

 9003 FORMAT(1X,A)

 9004 FORMAT(13X,'                           M O D A L   P A R T I C I P A T I O N   F A C T O R S',/,                             &
             13X,'              (dimensionless, in coordinate sys ',I8,' with cols marked by R-set grid/comp)',/)

 9014 FORMAT(20X,'                           M O D A L   P A R T I C I P A T I O N   F A C T O R S',/,                             &
             20X,'              (dimensionless, in coordinate sys ',I8,' with cols marked by R-set grid/comp)',/)

 9005 FORMAT(13X,'                           M O D A L   P A R T I C I P A T I O N   F A C T O R S',/,                             &
             13X,'                                (dimensionless, in coordinate sys ',I8,')')

 9015 FORMAT(20X,'                           M O D A L   P A R T I C I P A T I O N   F A C T O R S',/,                             &
             20X,'                                (dimensionless, in coordinate sys ',I8,')')

 9006 FORMAT(14X,'                          Reference point is the basic coordinate system origin',/)

 9007 FORMAT(14X,'                            Reference point is the PARAM GRDPNT grid: ',I8,/)

 9008 FORMAT(14X,'                              Reference point is the model center of gravity',/)

 9009 FORMAT(14X,'                                    Reference point is grid ',I8,/)

 9101 FORMAT(32X,32767(I8,6X))

 9102 FORMAT(13X,'MODE     CYCLES  ',32767(2X,I8,'-',I1,2X))

 9103 FORMAT(13X,' NUM')

 9201 FORMAT(34X,32767(I8,6X))

 9202 FORMAT(13X,'MODE       CYCLES  ',32767(2X,I8,'-',I1,2X))

 9203 FORMAT(13X,' NUM')

 9301 FORMAT(9X,I8,32767(1ES14.6))

 9311 FORMAT(16X,I8,32767(1ES14.6))

 9302 FORMAT(9X,I8,32767(1ES14.2))

 9501 FORMAT(13X,'MODE     CYCLES          T1            T2            T3            R1            R2            R3',/,            &
             13X,' NUM')

 9502 FORMAT(13X,'MODE       CYCLES          T1            T2            T3            R1            R2            R3',/,          &
             13X,' NUM')

 9503 FORMAT(9X,I8,7(1ES14.6))

 9513 FORMAT(16X,I8,7(1ES14.6))

 9504 FORMAT(9X,I8,7(1ES14.2))

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_MPFACTOR
