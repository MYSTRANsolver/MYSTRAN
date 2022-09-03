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

      SUBROUTINE WRITE_PARTNd_MAT_HDRS ( MAT_NAME, ROW_SET, COL_SET, NROWS, NCOLS )

! Writes the grid/comp pairs corresponding to the cols and rows of a partitioned matrix. Used for OUTPUT4 matrices

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MTDOF, NDOFA, NDOFF, NDOFG, NDOFL, NDOFM, NDOFN, NDOFO, NDOFR,   &
                                         NDOFS, NDOFSA, NDOFSB, NDOFSE, NDOFSG, NDOFSZ, NUM_USET_U1, NUM_USET_U2, TSET_CHR_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOFI
      USE OUTPUT4_MATRICES, ONLY      :  OU4_MAT_COL_GRD_COMP, OU4_MAT_ROW_GRD_COMP
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_PARTNd_MAT_HDRS_BEGEND

      USE WRITE_PARTNd_MAT_HDRS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_PARTNd_MAT_HDRS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME            ! Name of the partitioned matrix whose row/col headings will be output
      CHARACTER(LEN=*), INTENT(IN)    :: COL_SET             ! Set name that the cols of MAT_NAME belong to (e.g. 'G ', 'l ', etc)
      CHARACTER(LEN=*), INTENT(IN)    :: ROW_SET             ! Set name that the rows of MAT_NAME belong to (e.g. 'G ', 'l ', etc)

      INTEGER(LONG), INTENT(IN)       :: NCOLS               ! Number of cols in the partitioned matrix MAT_NAME
      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in the partitioned matrix MAT_NAME
      INTEGER(LONG)                   :: J,K                 ! DO loop indices or counters
      INTEGER(LONG)                   :: OUTPUT_1(10)        ! Part of a line of output
      INTEGER(LONG)                   :: OUTPUT_2(10)        ! Part of a line of output
      INTEGER(LONG)                   :: NUM_LEFT            ! Used when printing a line of 10 values in the set
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_PARTNd_MAT_HDRS_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Write the matrix col headers out to F06

      IF (COL_SET /= '--') THEN

         WRITE(F06,101) 'C O L S', MAT_NAME
         WRITE(F06,201) 'COLS', COL_SET

         NUM_LEFT = NCOLS
         DO J=1,NCOLS,10
            IF (NUM_LEFT >= 10) THEN
               DO K=1,10
                  OUTPUT_1(K) = OU4_MAT_COL_GRD_COMP(J+K-1,1)
                  OUTPUT_2(K) = OU4_MAT_COL_GRD_COMP(J+K-1,2)
               ENDDO
               WRITE(F06,401) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,10), J+9
            ELSE
               DO K=1,NUM_LEFT
                  OUTPUT_1(K) = OU4_MAT_COL_GRD_COMP(J+K-1,1)
                  OUTPUT_2(K) = OU4_MAT_COL_GRD_COMP(J+K-1,2)
               ENDDO
               IF (NUM_LEFT == 1) WRITE(F06,301) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 2) WRITE(F06,302) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 3) WRITE(F06,303) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 4) WRITE(F06,304) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 5) WRITE(F06,305) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 6) WRITE(F06,306) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 7) WRITE(F06,307) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 8) WRITE(F06,308) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 9) WRITE(F06,309) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
            ENDIF
            NUM_LEFT = NUM_LEFT - 10
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ENDIF

! Write the matrix row headers out to F06

      IF (ROW_SET /= '--') THEN

         WRITE(F06,101) 'R O W S', MAT_NAME
         WRITE(F06,201) 'ROWS', ROW_SET

         NUM_LEFT = NROWS
         DO J=1,NROWS,10
            IF (NUM_LEFT >= 10) THEN
               DO K=1,10
                  OUTPUT_1(K) = OU4_MAT_ROW_GRD_COMP(J+K-1,1)
                  OUTPUT_2(K) = OU4_MAT_ROW_GRD_COMP(J+K-1,2)
               ENDDO
               WRITE(F06,401) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,10), J+9
            ELSE
               DO K=1,NUM_LEFT
                  OUTPUT_1(K) = OU4_MAT_ROW_GRD_COMP(J+K-1,1)
                  OUTPUT_2(K) = OU4_MAT_ROW_GRD_COMP(J+K-1,2)
               ENDDO
               IF (NUM_LEFT == 1) WRITE(F06,301) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 2) WRITE(F06,302) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 3) WRITE(F06,303) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 4) WRITE(F06,304) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 5) WRITE(F06,305) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 6) WRITE(F06,306) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 7) WRITE(F06,307) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 8) WRITE(F06,308) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
               IF (NUM_LEFT == 9) WRITE(F06,309) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,NUM_LEFT)
            ENDIF
            NUM_LEFT = NUM_LEFT - 10
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)

      ENDIF

! **********************************************************************************************************************************
     IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(5X,'G R I D - C O M P   N U M B E R S   F O R   T H E   ',A,'   O F   P A R T I T I O N E D   M A T R I X  "',A,'"')

  201 FORMAT(47X,A,' ARE FROM THE "',A2,'"',' DISPLACEMENT SET',/,                                                                 &
               16X,'-1-        -2-        -3-        -4-        -5-        -6-        -7-        -8-        -9-       -10-',/)

  301 FORMAT(1X,I6,'=', 1(I9,'-',I1),9(11X))

  302 FORMAT(1X,I6,'=', 2(I9,'-',I1),8(11X))

  303 FORMAT(1X,I6,'=', 3(I9,'-',I1),7(11X))

  304 FORMAT(1X,I6,'=', 4(I9,'-',I1),6(11X))

  305 FORMAT(1X,I6,'=', 5(I9,'-',I1),5(11X))

  306 FORMAT(1X,I6,'=', 6(I9,'-',I1),4(11X))

  307 FORMAT(1X,I6,'=', 7(I9,'-',I1),3(11X))

  308 FORMAT(1X,I6,'=', 8(I9,'-',I1),2(11X))

  309 FORMAT(1X,I6,'=', 9(I9,'-',I1),1(11X))

  401 FORMAT(1X,I6,'=',10(I9,'-',I1),'      =',I6)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_PARTNd_MAT_HDRS