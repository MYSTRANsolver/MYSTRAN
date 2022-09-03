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

      SUBROUTINE GET_OU4_MAT_STATS ( MAT, NROWS, NCOLS, FORM, SYM )

! Gets the number of rows and columns and the SYM definition for a specific OUTPUT4 matrix given the matrix name.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFG, NDOFL, NDOFR, NUM_CB_DOFS, NSUB, NVEC
      USE IOUNT1, ONLY                :  ERR, F06
      USE MODEL_STUF, ONLY            :  MCG
      USE PARAMS, ONLY                :  MPFOUT
      USE OUTPUT4_MATRICES

      USE GET_OU4_MAT_STATS_USE_IFs                        ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_OU4_MAT_STATS'
      CHARACTER(LEN=*) , INTENT(IN)   :: MAT               ! Name of matrix to get row, col size for
      CHARACTER(1*BYTE), INTENT(OUT)  :: SYM               ! Y if matrix stored symmetrically

      INTEGER(LONG)   , INTENT(OUT)   :: FORM              ! Matrix format
      INTEGER(LONG)   , INTENT(OUT)   :: NCOLS             ! Number of cols in MAT_NAME
      INTEGER(LONG)   , INTENT(OUT)   :: NROWS             ! Number of rows in MAT_NAME

! **********************************************************************************************************************************
! Initialize

      NROWS =  0
      NCOLS =  0
      FORM  =  0
      SYM   = '?'

! Set output values for NROWS, NCOLS, FORM and SYM

      IF      (MAT =='CG_LTM          ')  THEN; NROWS = 6         ; NCOLS = NUM_CB_DOFS; FORM=2 ; SYM='N' ! 1
      ELSE IF (MAT =='DLR             ')  THEN; NROWS = NDOFL     ; NCOLS = NDOFR      ; FORM=2 ; SYM='N' ! 2
      ELSE IF (MAT =='EIGEN_VAL       ')  THEN; NROWS = NVEC      ; NCOLS = 1          ; FORM=2 ; SYM='N' ! 3
      ELSE IF (MAT =='EIGEN_VEC       ')  THEN; NROWS = NDOFG     ; NCOLS = NVEC       ; FORM=2 ; SYM='N' ! 4
      ELSE IF (MAT =='GEN_MASS        ')  THEN; NROWS = NVEC      ; NCOLS = 1          ; FORM=2 ; SYM='N' ! 5
      ELSE IF (MAT =='IF_LTM          ')  THEN; NROWS = NDOFR     ; NCOLS = NUM_CB_DOFS; FORM=2 ; SYM='N' ! 6
      ELSE IF (MAT =='KAA             ')  THEN; NROWS = NDOFA     ; NCOLS = NDOFA      ; FORM=1 ; SYM='Y' ! 7
      ELSE IF (MAT =='KGG             ')  THEN; NROWS = NDOFG     ; NCOLS = NDOFG      ; FORM=1 ; SYM='Y' ! 8
      ELSE IF (MAT =='KLL             ')  THEN; NROWS = NDOFL     ; NCOLS = NDOFL      ; FORM=1 ; SYM='Y' ! 9
      ELSE IF (MAT =='KRL             ')  THEN; NROWS = NDOFR     ; NCOLS = NDOFL      ; FORM=2 ; SYM='N' !10
      ELSE IF (MAT =='KRR             ')  THEN; NROWS = NDOFR     ; NCOLS = NDOFR      ; FORM=1 ; SYM='Y' !11
      ELSE IF (MAT =='KRRcb           ')  THEN; NROWS = NDOFR     ; NCOLS = NDOFR      ; FORM=1 ; SYM='Y' !12
      ELSE IF (MAT =='KXX             ')  THEN; NROWS = NDOFR+NVEC; NCOLS = NDOFR+NVEC ; FORM=1 ; SYM='Y' !13
      ELSE IF (MAT =='LTM             ')  THEN; NROWS = 6+NDOFR   ; NCOLS = NUM_CB_DOFS; FORM=2 ; SYM='N' !14
      ELSE IF (MAT =='MCG             ')  THEN; NROWS = 6         ; NCOLS = 6          ; FORM=2 ; SYM='N' !15
      ELSE IF (MAT =='MEFFMASS        ')  THEN; NROWS = NVEC      ; NCOLS = 6          ; FORM=2 ; SYM='N' !16
      ELSE IF (MAT =='MPFACTOR_N6     ')  THEN; NROWS = NVEC      ; NCOLS = 6          ; FORM=2 ; SYM='N' !17a
      ELSE IF (MAT =='MPFACTOR_NR     ')  THEN; NROWS = NVEC      ; NCOLS = NDOFR      ; FORM=2 ; SYM='N' !17b
      ELSE IF (MAT =='MAA             ')  THEN; NROWS = NDOFA     ; NCOLS = NDOFA      ; FORM=1 ; SYM='Y' !18
      ELSE IF (MAT =='MGG             ')  THEN; NROWS = NDOFG     ; NCOLS = NDOFG      ; FORM=1 ; SYM='Y' !19
      ELSE IF (MAT =='MLL             ')  THEN; NROWS = NDOFL     ; NCOLS = NDOFL      ; FORM=1 ; SYM='Y' !20
      ELSE IF (MAT =='MRL             ')  THEN; NROWS = NDOFR     ; NCOLS = NDOFL      ; FORM=2 ; SYM='N' !21
      ELSE IF (MAT =='MRN             ')  THEN; NROWS = NDOFR     ; NCOLS = NVEC       ; FORM=2 ; SYM='N' !22
      ELSE IF (MAT =='MRR             ')  THEN; NROWS = NDOFR     ; NCOLS = NDOFR      ; FORM=1 ; SYM='Y' !23
      ELSE IF (MAT =='MRRcb           ')  THEN; NROWS = NDOFR     ; NCOLS = NDOFR      ; FORM=1 ; SYM='Y' !24
      ELSE IF (MAT =='MXX             ')  THEN; NROWS = NDOFR+NVEC; NCOLS = NDOFR+NVEC ; FORM=1 ; SYM='Y' !25
      ELSE IF (MAT =='PA              ')  THEN; NROWS = NDOFA     ; NCOLS = NSUB       ; FORM=1 ; SYM='Y' !26
      ELSE IF (MAT =='PG              ')  THEN; NROWS = NDOFG     ; NCOLS = NSUB       ; FORM=1 ; SYM='Y' !27
      ELSE IF (MAT =='PL              ')  THEN; NROWS = NDOFL     ; NCOLS = NSUB       ; FORM=1 ; SYM='Y' !28
      ELSE IF (MAT =='PHIXG           ')  THEN; NROWS = NDOFG     ; NCOLS = NDOFR+NVEC ; FORM=2 ; SYM='N' !29
      ELSE IF (MAT =='PHIZG           ')  THEN; NROWS = NDOFG     ; NCOLS = NUM_CB_DOFS; FORM=2 ; SYM='N' !30
      ELSE IF (MAT =='RBM0            ')  THEN; NROWS = 6         ; NCOLS = 6          ; FORM=2 ; SYM='N' !31
      ELSE IF (MAT =='TR6_0           ')  THEN; NROWS = NDOFR     ; NCOLS = 6          ; FORM=2 ; SYM='N' !32
      ELSE IF (MAT =='TR6_CG          ')  THEN; NROWS = NDOFR     ; NCOLS = 6          ; FORM=2 ; SYM='N' !33
      ENDIF

      IF ((NROWS == 0) .OR. (NCOLS == 0) .OR. (FORM == 0) .OR. (SYM == '?')) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,946) SUBR_NAME, MAT
         WRITE(F06,946) SUBR_NAME, MAT
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
  946 FORMAT(' *ERROR   946: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' EITHER (1) INVALID VALUE = "',A,'" FOR INPUT ARGUMENT "MAT" OR,'                                      &
                    ,/,14X,'        (2) INCORRECT OUTPUT VALUE FOR "NROWS", "NCOLS", "FORM" OR "SYM"')

! **********************************************************************************************************************************

      END SUBROUTINE GET_OU4_MAT_STATS


