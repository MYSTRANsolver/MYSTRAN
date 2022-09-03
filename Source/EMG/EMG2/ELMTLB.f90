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
 
      SUBROUTINE ELMTLB ( OPT )
 
! Transforms element matrices from local to basic coordinates. Matrices transformed are: ME, KE, KED, PTE, PPE, using elem coord
! transformation matrix TE 
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, f06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELDOF, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMTLB_BEGEND
      USE MODEL_STUF, ONLY            :  ELDOF, ELGP, KE, KED, ME, PTE, PPE, TE
  
      USE ELMTLB_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMTLB'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)

      INTEGER(LONG)                   :: BEG_COL           ! Beginning col of matrix to get partition from
      INTEGER(LONG)                   :: BEG_ROW           ! Beginning row of matrix to get partition from
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NCOL              ! No. cols to get/put for subrs MATGET/MATPUT, called herein
      INTEGER(LONG), PARAMETER        :: NCOLA     = 3     ! No. cols in a matrix for subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NCOLB             ! No. cols in a matrix for subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG), PARAMETER        :: NROW      = 3     ! No. rows to get/put for subrs MATGET/MATPUT, called herein
      INTEGER(LONG), PARAMETER        :: NROWA     = 3     ! No. rows in a matrix for subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMTLB_BEGEND
  
      REAL(DOUBLE)                    :: DUM11(3,3)        ! An intermediate result when calculating transformed KE
      REAL(DOUBLE)                    :: DUM12(3,3)        ! An intermediate result when calculating transformed KE
      REAL(DOUBLE)                    :: PDUM1(3,NSUB)     ! An intermediate result when calculating transformed PTE, PPE
      REAL(DOUBLE)                    :: PDUM2(3,NSUB)     ! An intermediate result when calculating transformed PTE, PPE
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF (OPT(1) == 'Y') THEN                              ! Transform ME to TE' x ME x TE
         NCOL  = 3
         NCOLB = 3
         DO I=1,2*ELGP
            BEG_ROW = 3*I - 2
            DO J=I,2*ELGP
               BEG_COL = 3*J - 2
               CALL MATGET ( ME, MELDOF, MELDOF, BEG_ROW, BEG_COL, NROW, NCOL, DUM11 )
               CALL MATMULT_FFF   ( DUM11, TE, NROWA, NCOLA, NCOLB, DUM12 )
               CALL MATMULT_FFF_T ( TE, DUM12, NROWA, NCOLA, NCOLB, DUM11 )
               CALL MATPUT ( DUM11, MELDOF, MELDOF, BEG_ROW, BEG_COL, NROW, NCOL, ME )
            ENDDO 
         ENDDO

         DO I=1,ELDOF                                      ! Set lower portion of ME using symmetry.
            DO J=1,I-1
               ME(I,J) = ME(J,I)
            ENDDO 
         ENDDO   

      ENDIF

      IF ((OPT(2) == 'Y') .AND. (NTSUB > 0)) THEN          ! Transform PTE to TE' x PTE
         NCOL  = NTSUB
         NCOLB = NTSUB
         DO I=1,2*ELGP
            BEG_ROW = 3*I - 2
            BEG_COL = 1
            CALL MATGET ( PTE, MELDOF, NTSUB, BEG_ROW, BEG_COL, NROW, NCOL, PDUM1 )
            CALL MATMULT_FFF_T ( TE, PDUM1, NROWA, NCOLA, NCOLB, PDUM2 )
            CALL MATPUT ( PDUM2, MELDOF, NTSUB, BEG_ROW, BEG_COL, NROW, NTSUB, PTE )
         ENDDO
      ENDIF

      IF (OPT(4) == 'Y') THEN                              ! Transform KE to TE' x KE x TE
         NCOL  = 3
         NCOLB = 3
         DO I=1,2*ELGP
            BEG_ROW = 3*I - 2
            DO J=I,2*ELGP
               BEG_COL = 3*J - 2
               CALL MATGET ( KE, MELDOF, MELDOF, BEG_ROW, BEG_COL, NROW, NCOL, DUM11 )
               CALL MATMULT_FFF   ( DUM11, TE, NROWA, NCOLA, NCOLB, DUM12 )
               CALL MATMULT_FFF_T ( TE, DUM12, NROWA, NCOLA, NCOLB, DUM11 )
               CALL MATPUT ( DUM11, MELDOF, MELDOF, BEG_ROW, BEG_COL, NROW, NCOL, KE )
            ENDDO 
         ENDDO

 
 
         DO I=1,ELDOF                                      ! Set lower portion of KE using symmetry.
            DO J=1,I-1
               KE(I,J) = KE(J,I)
            ENDDO 
         ENDDO

      ENDIF   

      IF (OPT(5) == 'Y') THEN                              ! Transform PPE to TE' x PPE
         NCOL  = NSUB
         NCOLB = NSUB
         DO I=1,2*ELGP
            BEG_ROW = 3*I - 2
            BEG_COL = 1
            CALL MATGET ( PPE, MELDOF, NSUB, BEG_ROW, BEG_COL, NROW, NCOL, PDUM1 )
            CALL MATMULT_FFF_T ( TE, PDUM1, NROWA, NCOLA, NCOLB, PDUM2 )
            CALL MATPUT ( PDUM2, MELDOF, NSUB, BEG_ROW, BEG_COL, NROW, NSUB, PPE )
         ENDDO 
      ENDIF

      IF (OPT(6) == 'Y') THEN                              ! Transform KED to TE' x KED x TE
         NCOL  = 3
         NCOLB = 3
         DO I=1,2*ELGP
            BEG_ROW = 3*I - 2
            DO J=I,2*ELGP
               BEG_COL = 3*J - 2
               CALL MATGET ( KED, MELDOF, MELDOF, BEG_ROW, BEG_COL, NROW, NCOL, DUM11 )
               CALL MATMULT_FFF   ( DUM11, TE, NROWA, NCOLA, NCOLB, DUM12 )
               CALL MATMULT_FFF_T ( TE, DUM12, NROWA, NCOLA, NCOLB, DUM11 )
               CALL MATPUT ( DUM11, MELDOF, MELDOF, BEG_ROW, BEG_COL, NROW, NCOL, KED )
            ENDDO 
         ENDDO 
 
         DO I=1,ELDOF                                      ! Set lower portion of KED using symmetry.
            DO J=1,I-1
               KED(I,J) = KED(J,I)
            ENDDO 
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
 
      END SUBROUTINE ELMTLB
