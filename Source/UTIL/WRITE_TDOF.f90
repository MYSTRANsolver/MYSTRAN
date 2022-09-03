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

      SUBROUTINE WRITE_TDOF ( TDOF_MSG )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MTDOF, NDOFG, NDOFM, NDOFN, NDOFSA, NDOFSB, NDOFSG, NDOFSZ, NDOFSE, NDOFS,  &
                                         NDOFF, NDOFO, NDOFA, NDOFR, NDOFL, NGRID, NUM_USET_U1, NUM_USET_U2
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI
      USE PARAMS, ONLY                :  PRTDOF
      USE MODEL_STUF, ONLY            :  GRID_ID, INV_GRID_SEQ
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_TDOF_BEGEND

      USE WRITE_TDOF_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_TDOF'
      CHARACTER(LEN=*), INTENT(IN)    :: TDOF_MSG          ! Message to be printed out regarding at what point in the run the TDOF,I
!                                                            tables are printed out
      CHARACTER( 10*BYTE)             :: NAME1             ! A data set name for output purposes
      CHARACTER(  7*BYTE)             :: NAME2             ! A data set name for output purposes

      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IROW              ! Row number in array TDOF or TDOFI
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_TDOF_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Table TDOF is printed in the F06 file if B.D. PARAM PRTDOF = 1 or 3

      IF ((PRTDOF == 1) .OR. (PRTDOF == 3)) THEN
         NAME1 = 'GRID POINT'
         NAME2 = '(TDOF)'
         IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
            WRITE(F06,102) NAME1, NAME2, TDOF_MSG
         ELSE
            WRITE(F06,101) NAME1, NAME2, TDOF_MSG
         ENDIF
         IROW = 0
         DO I = 1,NGRID
            CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
            DO J = 1,NUM_COMPS
               IROW = IROW + 1
               IF (J == 1) THEN
                  IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
                     WRITE(F06,202) (TDOF(IROW,K),K = 1,MTDOF)
                  ELSE
                     WRITE(F06,201) (TDOF(IROW,K),K = 1,MTDOF-2)
                  ENDIF
               ELSE
                  IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
                     WRITE(F06,302) TDOF(IROW,2),TDOF(IROW,4),(TDOF(IROW,K),K = 5,MTDOF)
                  ELSE
                     WRITE(F06,301) TDOF(IROW,2),TDOF(IROW,4),(TDOF(IROW,K),K = 5,MTDOF-2)
                  ENDIF
               ENDIF
            ENDDO
            WRITE(F06,*) 
         ENDDO
         IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
            WRITE(F06,402) NDOFG, NDOFM, NDOFN, NDOFSA, NDOFSB, NDOFSG, NDOFSZ, NDOFSE, NDOFS, NDOFF, NDOFO, NDOFA, NDOFR, NDOFL,  &
                           NUM_USET_U1, NUM_USET_U2
         ELSE
            WRITE(F06,401) NDOFG, NDOFM, NDOFN, NDOFSA, NDOFSB, NDOFSG, NDOFSZ, NDOFSE, NDOFS, NDOFF, NDOFO, NDOFA, NDOFR, NDOFL
         ENDIF
         WRITE(F06,'(//)')
      ENDIF

! Table TDOFI is printed in the F06 file if B.D. PARAM PRTDOF = 2 or 3
 
      IF ((PRTDOF == 2) .OR. (PRTDOF == 3)) THEN
         NAME1 = 'DOF NUMBER'
         NAME2 = '(TDOFI)'
         IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
            WRITE(F06,102) NAME1, NAME2, TDOF_MSG
         ELSE
            WRITE(F06,101) NAME1, NAME2, TDOF_MSG
         ENDIF
         IROW = 0
         DO I = 1,NGRID
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J = 1,NUM_COMPS
               IROW = IROW + 1
               IF (J == 1) THEN
                  IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
                     WRITE(F06,202) (TDOFI(IROW,K),K = 1,MTDOF)
                  ELSE
                     WRITE(F06,201) (TDOFI(IROW,K),K = 1,MTDOF-2)
                  ENDIF
               ELSE
                  IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
                     WRITE(F06,302) TDOFI(IROW,2),TDOFI(IROW,4),(TDOFI(IROW,K),K = 5,MTDOF)
                  ELSE
                     WRITE(F06,301) TDOFI(IROW,2),TDOFI(IROW,4),(TDOFI(IROW,K),K = 5,MTDOF-2)
                  ENDIF
               ENDIF
            ENDDO
            WRITE(F06,*) 
         ENDDO
         IF ((NUM_USET_U1 > 0) .OR. (NUM_USET_U2 > 0)) THEN
            WRITE(F06,402) NDOFG, NDOFM, NDOFN, NDOFSA, NDOFSB, NDOFSG, NDOFSZ, NDOFSE, NDOFS, NDOFF, NDOFO, NDOFA, NDOFR, NDOFL,  &
                           NUM_USET_U1, NUM_USET_U2
         ELSE
            WRITE(F06,401) NDOFG, NDOFM, NDOFN, NDOFSA, NDOFSB, NDOFSG, NDOFSZ, NDOFSE, NDOFS, NDOFF, NDOFO, NDOFA, NDOFR, NDOFL
         ENDIF
         WRITE(F06,'(//)')
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(6X,'                                         DEGREE OF FREEDOM TABLE SORTED ON ',A,1X,A,/,7X,A                        &
          ,/                                                                                                                       &
          ,/,1X,                                                                                                                   &
'EXTERNAL  INTERNAL                                     DOF NUMBER FOR DISPLACEMENT SET:'                                          &
          ,/,1X,                                                                                                                   &
'GRD-COMP  GRD-COMP --------------------------------------------------------------------------------------------------------',     &
                   '--------'                                                                                                      &
          ,/,1X,                                                                                                                   &
'NUMBER    NUMBER          G       M       N      SA      SB      SG      SZ      SE       S       F       O       A       ',      &
                          'R       L',/)
  102 FORMAT(6X,'                                         DEGREE OF FREEDOM TABLE SORTED ON ',A,1X,A,/,7X,A                        &
          ,/                                                                                                                       &
          ,/,1X,                                                                                                                   &
'EXTERNAL  INTERNAL                                     DOF NUMBER FOR DISPLACEMENT SET:'                                          &
          ,/,1X,                                                                                                                   &
'GRD-COMP  GRD-COMP --------------------------------------------------------------------------------------------------------',     &
                   '------------------------'                                                                                      &
          ,/,1X,                                                                                                                   &
'NUMBER    NUMBER          G       M       N      SA      SB      SG      SZ      SE       S       F       O       A       ',      &
                          'R       L      U1      U2',/)

  201 FORMAT(2(I8,'-',I1),14(I8))

  202 FORMAT(2(I8,'-',I1),16(I8))

  301 FORMAT(2(8X,'-',I1),14(I8))

  302 FORMAT(2(8X,'-',I1),16(I8))

  401 FORMAT(8X,'             ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------',    &
                ' ------- -------'                                                                                              ,/,&
             'TOTAL NUMBER OF DOF:',14(I8)                                                                                 ,//,    &
             27X,        'G       M       N      SA      SB      SG      SZ      SE       S       F       O       A       ',       &
                         'R       L',/)

  402 FORMAT(8X,'             ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------',    &
                ' ------- ------- ------- -------'                                                                              ,/,&
             'TOTAL NUMBER OF DOF:',16(I8)                                                                                 ,//,    &
             27X,        'G       M       N      SA      SB      SG      SZ      SE       S       F       O       A       ',       &
                         'R       L      U1      U2',/)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_TDOF
