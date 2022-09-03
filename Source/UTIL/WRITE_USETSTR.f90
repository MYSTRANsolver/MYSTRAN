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

      SUBROUTINE WRITE_USETSTR

! Write USET definition tables for any of the MYSTRAN displ sets (G-set, M-set, etc). The user must have had a Bulk Data
! PARAM USETSTR entry with a dilpl set (like G) in field 3. Any number of these PARAM entries for valid displ sets will cause that
! displ set table to be written

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MTDOF, NDOFA, NDOFF, NDOFG, NDOFL, NDOFM, NDOFN, NDOFO, NDOFR,   &
                                         NDOFS, NDOFSA, NDOFSB, NDOFSE, NDOFSG, NDOFSZ, NUM_USET_U1, NUM_USET_U2, TSET_CHR_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_USETSTR_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI, USETSTR_TABLE

      USE WRITE_USETSTR_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_USETSTR'
      CHARACTER(LEN(TSET_CHR_LEN))    :: CHAR_SET            ! Name of a DOF set: G, N, M, etc
      CHARACTER(LEN(TSET_CHR_LEN))    :: NULL_SET_NAME(MTDOF)! Array of names of sets that are null (for output message purposes) 
      CHARACTER( 1*BYTE)              :: USETSTR_OUTPUT      ! If 'Y' then output of USET tables is requested

      INTEGER(LONG)                   :: COL_NUM             ! Column number in TDOF where a DOF set exists
      INTEGER(LONG)                   :: GRID_NUM(NDOFG)     ! Array of grid numbers for members of a DOF set requested in USETSTR
      INTEGER(LONG)                   :: COMP_NUM(NDOFG)     ! Array of comp numbers for members of a DOF set requested in USETSTR
      INTEGER(LONG)                   :: I,J,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_LEFT            ! Used when printing a line of 10 values in the set
      INTEGER(LONG)                   :: NUM_IN_SET          ! A set length (e.g. NDOFM for the M-set
      INTEGER(LONG)                   :: NUM_NULL            ! Number of sets that have been requested for output that are null
      INTEGER(LONG)                   :: OUTPUT_1(10)        ! Part of a line of output
      INTEGER(LONG)                   :: OUTPUT_2(10)        ! Part of a line of output
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_USETSTR_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      NUM_NULL = 0

! Scan table USETSTR to see if there are output requests

      USETSTR_OUTPUT = 'N'
      DO I=1,16
         IF (USETSTR_TABLE(I,2) /= '0 ') THEN
            USETSTR_OUTPUT = 'Y'
         ENDIF
      ENDDO

! If there were requests, process them and output them to F06

      IF (USETSTR_OUTPUT == 'Y') THEN

         WRITE(F06,200)
         DO I=1,MTDOF-4

            IF (USETSTR_TABLE(I,2) /= '0 ') THEN           ! If /= 0 then a request for that DOF set was made via B.D. PARAM USETSTR

               CHAR_SET = USETSTR_TABLE(I,1)

               IF      (CHAR_SET == 'G ') THEN   ;   NUM_IN_SET = NDOFG
               ELSE IF (CHAR_SET == 'M ') THEN   ;   NUM_IN_SET = NDOFM
               ELSE IF (CHAR_SET == 'N ') THEN   ;   NUM_IN_SET = NDOFN
               ELSE IF (CHAR_SET == 'SA') THEN   ;   NUM_IN_SET = NDOFSA
               ELSE IF (CHAR_SET == 'SB') THEN   ;   NUM_IN_SET = NDOFSB
               ELSE IF (CHAR_SET == 'SG') THEN   ;   NUM_IN_SET = NDOFSG
               ELSE IF (CHAR_SET == 'SZ') THEN   ;   NUM_IN_SET = NDOFSZ
               ELSE IF (CHAR_SET == 'SE') THEN   ;   NUM_IN_SET = NDOFSE
               ELSE IF (CHAR_SET == 'S ') THEN   ;   NUM_IN_SET = NDOFS
               ELSE IF (CHAR_SET == 'F ') THEN   ;   NUM_IN_SET = NDOFF
               ELSE IF (CHAR_SET == 'O ') THEN   ;   NUM_IN_SET = NDOFO
               ELSE IF (CHAR_SET == 'A ') THEN   ;   NUM_IN_SET = NDOFA
               ELSE IF (CHAR_SET == 'R ') THEN   ;   NUM_IN_SET = NDOFR
               ELSE IF (CHAR_SET == 'L ') THEN   ;   NUM_IN_SET = NDOFL
               ELSE IF (CHAR_SET == 'U1') THEN   ;   NUM_IN_SET = NUM_USET_U1
               ELSE IF (CHAR_SET == 'U2') THEN   ;   NUM_IN_SET = NUM_USET_U2
               ELSE
                  WRITE(ERR,1327) SUBR_NAME,CHAR_SET
                  WRITE(F06,1327) SUBR_NAME,CHAR_SET
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )                   ! Coding error, so quit
               ENDIF

               IF (NUM_IN_SET > 0) THEN

                  WRITE(F06,201) CHAR_SET
                  DO J=1,NDOFG
                     GRID_NUM(J) = 0
                     COMP_NUM(J) = 0
                  ENDDO
                  CALL TDOF_COL_NUM ( CHAR_SET, COL_NUM )
                  K = 0
                  DO J=1,NDOFG
                     IF (TDOFI(J,COL_NUM) /= 0) THEN
                        K = K + 1
                        GRID_NUM(K) = TDOFI(J,1)
                        COMP_NUM(K) = TDOFI(J,2)
                     ENDIF
                  ENDDO

                  NUM_LEFT = NUM_IN_SET
                  DO J=1,NUM_IN_SET,10
                     IF (NUM_LEFT >= 10) THEN
                        DO K=1,10
                           OUTPUT_1(K) = GRID_NUM(J+K-1)
                           OUTPUT_2(K) = COMP_NUM(J+K-1)
                        ENDDO
                        WRITE(F06,401) J, (OUTPUT_1(K), OUTPUT_2(K), K=1,10), J+9
                     ELSE
                        DO K=1,NUM_LEFT
                           OUTPUT_1(K) = GRID_NUM(J+K-1)
                           OUTPUT_2(K) = COMP_NUM(J+K-1)
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

               ELSE

                  NUM_NULL = NUM_NULL + 1
                  NULL_SET_NAME(NUM_NULL) = CHAR_SET

               ENDIF

            ENDIF

         ENDDO

         IF (NUM_NULL > 0) THEN
            DO I=1,NUM_NULL
               WRITE(ERR,901) NULL_SET_NAME(I)
               WRITE(F06,901) NULL_SET_NAME(I)
            ENDDO
            WRITE(F06,*)
         ENDIF

      ENDIF


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  200 FORMAT(16X,'U S E T   D E F I N I T I O N   T A B L E   ( I N T E R N A L   S E Q U E N C E ,   R O W   S O R T )')

  201 FORMAT(55X,A2,7X,'DISPLACEMENT SET',/,                                                                                       &
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

  901 FORMAT(' *INFORMATION: BULK DATA PARAM USETSTR HAD REQUEST FOR USET DEFINITION TABLE FOR THE "',A,'" SET.',                  &
                           ' HOWEVER, THAT SET IS NULL')

 1327 FORMAT(' *ERROR  1327: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT VARIABLE CHAR_SET = "',A2,'" IS NOT ONE OF THE CORRECT DESIGNATIONS FOR A DISPL SET')

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_USETSTR