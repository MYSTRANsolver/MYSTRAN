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
 
      SUBROUTINE SPARSE_RMG
 
! Reads RMG constraint terms from file LINK1J. Zero terms are stripped and rows are sorted in numerical order. The final sparse RMG
! constraint matrix is written to file LINK1J in format: i, j, RMG(i,j)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1J, LINK1J, L1J_MSG
      USE SCONTR, ONLY                :  NDOFM, NTERM_RMG, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_RMG_BEGEND
      USE PARAMS, ONLY                :  EPSIL
      USE SPARSE_MATRICES, ONLY       :  I_RMG, J_RMG, RMG
 
      USE SPARSE_RMG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_RMG'
 
      INTEGER(LONG)                   :: I,K               ! DO loop indices or counters

      INTEGER(LONG)                   :: I2_RMG(NTERM_RMG) ! Row numbers of all terms in matrix RMG. Coming into this subr there is
!                                                            a conservative est of NTERM_RMG. The actual value will be determined
!                                                            by a count, herein, of the actual number of terms written to L1J

      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: IRMG              ! Row number for RMG
      INTEGER(LONG)                   :: IRMG_OLD          ! Row number for RMG
      INTEGER(LONG)                   :: JRMG              ! Col number for RMG
      INTEGER(LONG)                   :: KTERM_RMG         ! Count of number of terms in RMG
      INTEGER(LONG)                   :: NTERM_ROW_I       ! Number of nonzero terms in row I of RMG
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_RMG_BEGEND
 
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: RRMG              ! Real value for RMG
 
      INTRINSIC DABS
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! Read RMG constraint matrix. The matrix is not in DOF order

      IF (NDOFM > 0) THEN

         CALL FILE_OPEN ( L1J, LINK1J, OUNT, 'OLD', L1J_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )
 
         NTERM_RMG = 0                                     ! First, calc NTERM_RMG
         REC_NO = 0
nterm:   DO                        
            IRMG = 0
            JRMG = 0
            RRMG = ZERO
            READ(L1J,IOSTAT=IOCHK) IRMG, JRMG, RRMG
            REC_NO = REC_NO + 1
            IF      (IOCHK == 0) THEN
               IF (DABS(RRMG) > EPS1) THEN
                  NTERM_RMG = NTERM_RMG + 1
                  CYCLE nterm
               ENDIF
            ELSE IF (IOCHK > 0) THEN
               CALL READERR ( IOCHK, LINK1J, L1J_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading RMG file, so quit
            ELSE
               EXIT nterm
            ENDIF
         ENDDO nterm
         CALL FILE_CLOSE ( L1J, LINK1J, 'KEEP', 'N' )
                                                           ! Allocate memory for RMG sparse arrays
         CALL ALLOCATE_SPARSE_MAT ( 'RMG', NDOFM, NTERM_RMG, SUBR_NAME )

         CALL FILE_OPEN ( L1J, LINK1J, OUNT, 'OLD', L1J_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

         KTERM_RMG = 0
         REC_NO    = 0
read_l1j:DO                                                ! Now calc sparse arrays for RMG
            IRMG      = 0
            JRMG      = 0
            RRMG      = ZERO
            READ(L1J,IOSTAT=IOCHK) IRMG, JRMG, RRMG
            REC_NO = REC_NO + 1
            IF      (IOCHK == 0) THEN
               IF (DABS(RRMG) > EPS1) THEN
                  KTERM_RMG         = KTERM_RMG + 1
                  IF (KTERM_RMG > NTERM_RMG) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_RMG, 'I2_RMG, J_RMG, RMG' )
                  I2_RMG(KTERM_RMG) = IRMG
                   J_RMG(KTERM_RMG) = JRMG
                     RMG(KTERM_RMG) = RRMG
                  CYCLE read_l1j
               ENDIF
            ELSE IF (IOCHK > 0) THEN
               CALL READERR ( IOCHK, LINK1J, L1J_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                              ! Error reading RMG file, so quit
            ELSE
               EXIT read_l1j
            ENDIF
         ENDDO read_l1j

         I_RMG(1) = 1
         DO I=2,NTERM_RMG
            NTERM_ROW_I = 1
            IF (I2_RMG(I) /= I2_RMG(I)) THEN
               I_RMG(I) = I_RMG(I) + NTERM_ROW_I
            ENDIF
         ENDDO

         CALL FILE_CLOSE ( L1J, LINK1J, 'DELETE', 'Y' )

! Sort RMG so that the rows (M-set DOF's) are in numerically increasing order

         CALL SORT_INT2_REAL1 ( SUBR_NAME, 'I2_RMG, J_RMG, RMG', NTERM_RMG, I2_RMG, J_RMG, RMG )

! Rewrite RMG matrix in the format i, j, RMG(i,j) to L1J 
 
         CALL FILE_OPEN ( L1J, LINK1J, OUNT, 'REPLACE', L1J_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
         WRITE(L1J) NTERM_RMG
         DO I=1,NTERM_RMG
            WRITE(L1J) I2_RMG(I),J_RMG(I),RMG(I) 
         ENDDO      
         CALL FILE_CLOSE ( L1J, LINK1J, 'KEEP', 'Y' )

      ENDIF

      IRMG_OLD    = 0
      I_RMG(1) = 1
      DO K = 1,NTERM_RMG
         IRMG = I2_RMG(K)
         DO I=IRMG_OLD+1,IRMG
            I_RMG(I+1) = I_RMG(I)
         ENDDO
         IRMG_OLD = IRMG
         I_RMG(IRMG+1) = I_RMG(IRMG+1) + 1
      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1616 FORMAT(' *ERROR  1616: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF TERMS IN THE RMG MATRIX   = ',I12,' BUT SHOULD BE NTERM_RMG = ',I12,' IN FILE:'         &
                    ,/,15X,A)   



! **********************************************************************************************************************************
 
      END SUBROUTINE SPARSE_RMG
