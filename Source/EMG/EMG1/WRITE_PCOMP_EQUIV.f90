! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

      SUBROUTINE WRITE_PCOMP_EQUIV ( PCOMP_TM, PCOMP_IB, PCOMP_TS )

! Write equiv PSHELL and MAT2's for a PCOMP used, if requested, based on user Bulk Data PARAM PCOMPEQ

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  TWELVE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEMATC, MID1_PCOMP_EQ, MID2_PCOMP_EQ, MID3_PCOMP_EQ,                        &
                                         MID4_PCOMP_EQ, MID1_PCOMP_EQ, MID2_PCOMP_EQ, MID3_PCOMP_EQ, MID4_PCOMP_EQ
      USE PARAMS, ONLY                :  EPSIL, PCOMPEQ, SUPINFO
      USE MODEL_STUF, ONLY            :  INTL_PID, PCOMP, RHO, SHELL_ALP, SHELL_A, SHELL_B, SHELL_D, SHELL_T, SHELL_T_MOD,         &
                                         TREF, ZS

      USE TIMDAT, ONLY                :  TSEC

      USE WRITE_PCOMP_EQUIV_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_PCOMP_EQUIV'
      CHARACTER( 8*BYTE)              :: C8FLD_GIJ(3,3,4)   ! Char representation of MAT2 Gij entries
      CHARACTER( 8*BYTE)              :: C8FLD_ALP(6,MEMATC)! Char representation of MAT2 Ai (CTE) entries
      CHARACTER( 8*BYTE)              :: C8FLD_RHO(4)       ! Char representation of MAT2 RHO entry
      CHARACTER( 8*BYTE)              :: C8FLD_TREF(4)      ! Char representation of MAT2 TREF entry
      CHARACTER( 8*BYTE)              :: C8FLD_TM           ! Char representation of MAT2 TM entry
      CHARACTER( 8*BYTE)              :: C8FLD_ZS(2)        ! Char representation of MAT2 ZS entry
      CHARACTER(18*BYTE)              :: NAME1(4)           ! Name of a SHELL matrix (A, D, T, or B)
      CHARACTER( 7*BYTE)              :: NAME2(4)           ! Name of a SHELL matrix (A, D, T, or B)
      CHARACTER( 1*BYTE)              :: FINITE_MAT_PROPS(4)! Indicator of whether any SHELL matrix had zero diag terms

      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: ICONT              ! Continuation mnemonic for PSHELL equivalent B.D. entry
      INTEGER(LONG)                   :: IERR(4)            ! Error indicator if SHELL_ALP was calculated

      REAL(DOUBLE), INTENT(IN)        :: PCOMP_TM           ! Membrane thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(IN)        :: PCOMP_IB           ! Bending MOI of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(IN)        :: PCOMP_TS           ! Transverse shear thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE)                    :: EPS1               ! Small number
      REAL(DOUBLE)                    :: PCOMP_IBP          ! 12*IB/TM^3
      REAL(DOUBLE)                    :: PCOMP_TSTM         ! TM/TS
      REAL(DOUBLE)                    :: PCOMP_BEN_MAT2(3,3)! MAT2 material matrix for bending for equivalent PSHELL
      REAL(DOUBLE)                    :: PCOMP_MBC_MAT2(3,3)! MAT2 material matrix for mem/ben coupling for equivalent PSHELL
      REAL(DOUBLE)                    :: PCOMP_MEM_MAT2(3,3)! MAT2 material matrix for membrane for equivalent PSHELL
      REAL(DOUBLE)                    :: PCOMP_TSH_MAT2(2,2)! MAT2 material matrix for transverse shear for equivalent PSHELL

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Determine if any SHELL_A, D, T, B  matrices are null

      FINITE_MAT_PROPS(1) = 'Y'
      IF ((DABS(SHELL_A(1,1)) < EPS1) .OR. (DABS(SHELL_A(1,1)) < EPS1) .OR. (DABS(SHELL_A(1,1)) < EPS1) .OR.                       &
          (DABS(SHELL_A(1,1)) < EPS1)) THEN
         FINITE_MAT_PROPS(1) = 'N'
      ENDIF

      FINITE_MAT_PROPS(2) = 'Y'
      IF ((DABS(SHELL_D(1,1)) < EPS1) .OR. (DABS(SHELL_D(1,1)) < EPS1) .OR. (DABS(SHELL_D(1,1)) < EPS1) .OR.                       &
          (DABS(SHELL_D(1,1)) < EPS1)) THEN
         FINITE_MAT_PROPS(2) = 'N'
      ENDIF

      FINITE_MAT_PROPS(3) = 'Y'
      IF ((DABS(SHELL_T(1,1)) < EPS1) .OR. (DABS(SHELL_T(1,1)) < EPS1) .OR. (DABS(SHELL_T(1,1)) < EPS1)) THEN
         FINITE_MAT_PROPS(3) = 'N'
      ENDIF

      FINITE_MAT_PROPS(4) = 'Y'
      IF ((DABS(SHELL_B(1,1)) < EPS1) .OR. (DABS(SHELL_B(1,1)) < EPS1) .OR. (DABS(SHELL_B(1,1)) < EPS1) .OR.                       &
          (DABS(SHELL_B(1,1)) < EPS1)) THEN
         FINITE_MAT_PROPS(4) = 'N'
      ENDIF

! Write message if any CTE's were not able to be calculated, but only if that SHELL matrix had props to be output

      NAME1(1) = 'MEMBRANE'  ;   NAME1(2) = 'BENDING'   ;   NAME1(3) = 'TRANSVERSE SHEAR'   ;   NAME1(4) = 'MEMB/BEND COUPLING'
      NAME2(1) = 'SHELL_A'   ;   NAME2(2) = 'SHELL_D'   ;   NAME2(3) = 'SHELL_T'            ;   NAME2(4) = 'SHELL_B'

      CALL SOLVE_SHELL_ALP ( IERR )                         ! Solve for equiv CTE's for this PCOMP

      WRITE(F06,9900)
      DO I=1,4
         IF      (IERR(I) == 1) THEN
            IF (FINITE_MAT_PROPS(I) == 'Y') THEN
               WRITE(ERR,9801) NAME1(I), PCOMP(INTL_PID,1), NAME2(I)
               IF (SUPINFO == 'N') THEN
                  WRITE(F06,9801) NAME1(I), PCOMP(INTL_PID,1), NAME2(I)
               ENDIF
            ENDIF
         ELSE IF (IERR(I) == 2) THEN
            IF (FINITE_MAT_PROPS(I) == 'Y') THEN
               WRITE(ERR,9802) NAME1(I), PCOMP(INTL_PID,1), NAME2(I)
               IF (SUPINFO == 'N') THEN
                  WRITE(F06,9802) NAME1(I), PCOMP(INTL_PID,1), NAME2(I)
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      PCOMP_IBP  = TWELVE*PCOMP_IB/(PCOMP_TM*PCOMP_TM*PCOMP_TM)
      PCOMP_TSTM = PCOMP_TS/PCOMP_TM
      WRITE(F06,9901) PCOMP(INTL_PID,1), PCOMP_TM, PCOMP_IBP, PCOMP_TSTM 
      IF (SUPINFO == 'N') THEN
         WRITE(F06,9901) PCOMP(INTL_PID,1), PCOMP_TM, PCOMP_IBP, PCOMP_TSTM
      ENDIF 

      DO I=1,3
         DO J =1,3
            PCOMP_MEM_MAT2(I,J) = SHELL_A(I,J)/PCOMP_TM
            PCOMP_MBC_MAT2(I,J) = SHELL_B(I,J)
            PCOMP_BEN_MAT2(I,J) = SHELL_D(I,J)/PCOMP_IB
         ENDDO
      ENDDO

      DO I=1,2
         DO J =1,2
            PCOMP_TSH_MAT2(I,J) = SHELL_T(I,J)/PCOMP_TS
         ENDDO
      ENDDO

      MID1_PCOMP_EQ = MID1_PCOMP_EQ + PCOMP(INTL_PID,1)
      MID2_PCOMP_EQ = MID2_PCOMP_EQ + PCOMP(INTL_PID,1)
      MID3_PCOMP_EQ = MID3_PCOMP_EQ + PCOMP(INTL_PID,1)
      MID4_PCOMP_EQ = MID4_PCOMP_EQ + PCOMP(INTL_PID,1)

      ICONT = MID1_PCOMP_EQ

      IF (PCOMPEQ > 1) THEN

         IF ((IERR(1) == 0) .OR. (IERR(2) == 0) .OR. (IERR(3) == 0) .OR. (IERR(4) == 0)) THEN
            WRITE(F06,9902)
         ENDIF

         IF (FINITE_MAT_PROPS(1) == 'Y') THEN
            IF (IERR(1) == 0) THEN
               WRITE(F06,9903) MID1_PCOMP_EQ, PCOMP_MEM_MAT2(1,1), PCOMP_MEM_MAT2(1,2), PCOMP_MEM_MAT2(1,3),                       &
                                              PCOMP_MEM_MAT2(2,2), PCOMP_MEM_MAT2(2,3), PCOMP_MEM_MAT2(3,3),                       &
                                              SHELL_ALP(1,1), SHELL_ALP(2,1), SHELL_ALP(3,1)
            ELSE
               WRITE(F06,9903) MID1_PCOMP_EQ, PCOMP_MEM_MAT2(1,1), PCOMP_MEM_MAT2(1,2), PCOMP_MEM_MAT2(1,3),                       &
                                              PCOMP_MEM_MAT2(2,2), PCOMP_MEM_MAT2(2,3), PCOMP_MEM_MAT2(3,3)
            ENDIF
         ENDIF

         IF (FINITE_MAT_PROPS(2) == 'Y') THEN
            IF (IERR(2) == 0) THEN
               WRITE(F06,9904) MID2_PCOMP_EQ, PCOMP_BEN_MAT2(1,1), PCOMP_BEN_MAT2(1,2), PCOMP_BEN_MAT2(1,3),                       &
                                              PCOMP_BEN_MAT2(2,2), PCOMP_BEN_MAT2(2,3), PCOMP_BEN_MAT2(3,3),                       &
                                              SHELL_ALP(1,2), SHELL_ALP(2,2), SHELL_ALP(3,2)
            ELSE
               WRITE(F06,9904) MID2_PCOMP_EQ, PCOMP_BEN_MAT2(1,1), PCOMP_BEN_MAT2(1,2), PCOMP_BEN_MAT2(1,3),                       &
                                              PCOMP_BEN_MAT2(2,2), PCOMP_BEN_MAT2(2,3), PCOMP_BEN_MAT2(3,3)
            ENDIF
         ENDIF

         IF (FINITE_MAT_PROPS(3) == 'Y') THEN
            IF (IERR(3) == 0) THEN
               WRITE(F06,9905) MID3_PCOMP_EQ, PCOMP_TSH_MAT2(1,1), PCOMP_TSH_MAT2(1,2), PCOMP_TSH_MAT2(2,2),                       &
                                              SHELL_ALP(5,3), SHELL_ALP(6,3)
            ELSE
               WRITE(F06,9905) MID3_PCOMP_EQ, PCOMP_TSH_MAT2(1,1), PCOMP_TSH_MAT2(1,2), PCOMP_TSH_MAT2(2,2)
            ENDIF
         ENDIF

         IF (FINITE_MAT_PROPS(4) == 'Y') THEN
            IF (IERR(4) == 0) THEN
               WRITE(F06,9906) MID4_PCOMP_EQ, PCOMP_MBC_MAT2(1,1), PCOMP_MBC_MAT2(1,2), PCOMP_MBC_MAT2(1,3),                       &
                                              PCOMP_MBC_MAT2(2,2), PCOMP_MBC_MAT2(2,3), PCOMP_MBC_MAT2(3,3),                       &
                                              SHELL_ALP(1,4), SHELL_ALP(2,4), SHELL_ALP(3,4)
            ELSE
               WRITE(F06,9906) MID4_PCOMP_EQ, PCOMP_MBC_MAT2(1,1), PCOMP_MBC_MAT2(1,2), PCOMP_MBC_MAT2(1,3),                       &
                                              PCOMP_MBC_MAT2(2,2), PCOMP_MBC_MAT2(2,3), PCOMP_MBC_MAT2(3,3)
            ENDIF
         ENDIF

         WRITE(F06,*)

      ENDIF                     

      CALL GET_CHAR8_OUTPUTS ( C8FLD_GIJ, C8FLD_ALP, C8FLD_RHO, C8FLD_TREF, C8FLD_TM, C8FLD_ZS )

! Write PSHELL

      IF (IERR(4) == 0) THEN
         WRITE(F06,9912) PCOMP(INTL_PID,1), MID1_PCOMP_EQ, C8FLD_TM, MID2_PCOMP_EQ, MID3_PCOMP_EQ,ICONT,                           &
                         ICONT, C8FLD_ZS(2), C8FLD_ZS(2), MID4_PCOMP_EQ

      ELSE
         WRITE(F06,9913) PCOMP(INTL_PID,1), MID1_PCOMP_EQ, C8FLD_TM, MID2_PCOMP_EQ, MID3_PCOMP_EQ,ICONT,                           &
                            ICONT, C8FLD_ZS(1), C8FLD_ZS(2)
      ENDIF

! Write MAT2 for membrane

      IF (FINITE_MAT_PROPS(1) == 'Y') THEN
         IF (IERR(1) == 0) THEN

            WRITE(F06,9922) MID1_PCOMP_EQ, C8FLD_GIJ(1,1,1), C8FLD_GIJ(1,2,1), C8FLD_GIJ(1,3,1),                                   &
                                           C8FLD_GIJ(2,2,1), C8FLD_GIJ(2,3,1), C8FLD_GIJ(3,3,1), C8FLD_RHO(1),ICONT+1,             &
                                           ICONT+1, C8FLD_ALP(1,1), C8FLD_ALP(2,1), C8FLD_ALP(3,1), C8FLD_TREF(1)
         ELSE
            WRITE(F06,9923) MID1_PCOMP_EQ, C8FLD_GIJ(1,1,1), C8FLD_GIJ(1,2,1), C8FLD_GIJ(1,3,1),                                   &
                                           C8FLD_GIJ(2,2,1), C8FLD_GIJ(2,3,1), C8FLD_GIJ(3,3,1), C8FLD_RHO(1),ICONT+1,             &
                                           ICONT+1, C8FLD_TREF(1)
         ENDIF
      ENDIF

! Write MAT2 for bending

      IF (FINITE_MAT_PROPS(2) == 'Y') THEN
         IF (IERR(2) == 0) THEN
            WRITE(F06,9922) MID2_PCOMP_EQ, C8FLD_GIJ(1,1,2), C8FLD_GIJ(1,2,2), C8FLD_GIJ(1,3,2),                                   &
                                           C8FLD_GIJ(2,2,2), C8FLD_GIJ(2,3,2), C8FLD_GIJ(3,3,2), C8FLD_RHO(2),ICONT+2,             &
                                           ICONT+2, C8FLD_ALP(1,2), C8FLD_ALP(2,2), C8FLD_ALP(3,2), C8FLD_TREF(1)
         ELSE

            WRITE(F06,9923) MID2_PCOMP_EQ, C8FLD_GIJ(1,1,2), C8FLD_GIJ(1,2,2), C8FLD_GIJ(1,3,2),                                   &
                                           C8FLD_GIJ(2,2,2), C8FLD_GIJ(2,3,2), C8FLD_GIJ(3,3,2), C8FLD_RHO(2),ICONT+2,             &
                                           ICONT+2, C8FLD_TREF(1)
         ENDIF
      ENDIF

! Write MAT2 for transverse shear

      IF (FINITE_MAT_PROPS(3) == 'Y') THEN
         IF (IERR(3) == 0) THEN
            WRITE(F06,9932) MID3_PCOMP_EQ, C8FLD_GIJ(1,1,3), C8FLD_GIJ(1,2,3), '0.0000+0'      ,                                   &
                                           C8FLD_GIJ(2,2,3), '0.0000+0'      , '0.0000+0', C8FLD_RHO(3), ICONT+3,                  &
                                           ICONT+3, C8FLD_ALP(5,3), C8FLD_ALP(6,3), C8FLD_TREF(1)
         ELSE

            WRITE(F06,9933) MID3_PCOMP_EQ, C8FLD_GIJ(1,1,3), C8FLD_GIJ(1,2,3), '0.0000+0'      ,                                   &
                                           C8FLD_GIJ(2,2,3), '0.0000+0'      , '0.0000+0', C8FLD_RHO(3), ICONT+3,                  &
                                           ICONT+3, C8FLD_TREF(1)
         ENDIF
      ENDIF

! Write MAT2 for membrane/bending coupling

      IF (FINITE_MAT_PROPS(4) == 'Y') THEN
         IF (IERR(4) == 0) THEN
            WRITE(F06,9922) MID4_PCOMP_EQ, C8FLD_GIJ(1,1,4), C8FLD_GIJ(1,2,4), C8FLD_GIJ(1,3,4),                                   &
                                           C8FLD_GIJ(2,2,4), C8FLD_GIJ(2,3,4), C8FLD_GIJ(3,3,4), C8FLD_RHO(4),ICONT+4,             &
                                           ICONT+4, C8FLD_ALP(1,4), C8FLD_ALP(2,4), C8FLD_ALP(3,4), C8FLD_TREF(1)
         ELSE

            WRITE(F06,9923) MID4_PCOMP_EQ, C8FLD_GIJ(1,1,4), C8FLD_GIJ(1,2,4), C8FLD_GIJ(1,3,4),                                   &
                                           C8FLD_GIJ(2,2,4), C8FLD_GIJ(2,3,4), C8FLD_GIJ(3,3,4), C8FLD_RHO(4),ICONT+4,             &
                                           ICONT+4, C8FLD_TREF(1)
         ENDIF
      ENDIF

! Write message if we have changed SHELL_T to reflect approx zero transverse shear flex

      IF (SHELL_T_MOD == 'Y') THEN
         WRITE(ERR,9995) MID3_PCOMP_EQ
         IF (SUPINFO == 'N') THEN
            WRITE(F06,9995) MID3_PCOMP_EQ
         ENDIF
      ENDIF

      WRITE(F06,9900)

      PCOMP(INTL_PID,6) = 1                                 ! Lets future calls to this subr know that PSHELL, MAT2 were written

! **********************************************************************************************************************************
 9801 FORMAT(' *INFORMATION: Cannot calculate equiv CTE''s for ',A,' for PCOMP ',I8,' since the det of matrix ',A,' is zero',/)

 9802 FORMAT(' *INFORMATION: Cannot calculate equiv CTE''s for ',A,' for PCOMP ',I8,' since matrix ',A,' cannot be inverted',/)

 9900 FORMAT('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++', &
             '+++++++++++++++++++++++++++++++++++++++++++++++++')

 9901 FORMAT(' *INFORMATION: Equivalent PSHELL amd MAT2 entries for PCOMP ',I8,' with:'                                            &
                    ,/,14X,' membrane thickness TM = ',1ES13.6,',   12*IB/(TM^3) = ',1ES13.6,',   TS/TM = ',1ES13.6,':',/) 

 9902 FORMAT('        Type         MATL ID       G11            G12            G13            G22            G23            G33',  &
             '             A1             A2             A3',/                                                                     &
             ' ------------------ --------  -------------  -------------  -------------  -------------  -------------',            &
             '  -------------  -------------  -------------  -------------')

 9903 FORMAT('      membrane      ',I8,9(1ES15.6))

 9904 FORMAT('      bending       ',I8,9(1ES15.6))

 9905 FORMAT('  transverse shear  ',I8,2(1ES15.6),15X,1ES15.6,30X,2(1ES15.6))

 9906 FORMAT('  mem/bend coupling ',I8,9(1ES15.6))

 9912 FORMAT('PSHELL   ,',I9,',',I9,',',A9,',',I9,',      1.0,',I9,',','      1.0,         , +',I7,/,' +',I7,2(',',A9),I9,/)

 9913 FORMAT('PSHELL   ,',I9,',',I9,',',A9,',',I9,',      1.0,',I9,',','      1.0,         , +',I7,/,' +',I7,2(',',A9),/)

 9922 FORMAT('MAT2     ,',I9,7(',',A9),', +',I7,/,' +',I7,3(',',A9),',',A9,/)
 
 9923 FORMAT('MAT2     ,',I9,7(',',A9),', +',I7,/,' +',I7,3(',',8X),',',A9,/)
 
 9932 FORMAT('MAT2     ,',I9,7(',',A9),', +',I7,/,' +',I7,2(',',A9),',',9X,',',A9,/)

 9933 FORMAT('MAT2     ,',I9,7(',',A9),', +',I7,/,' +',I7,2(',',8X),',',9X,',',A9,/)

 9995 FORMAT(' *INFORMATION: The transverse shear modulii on the above MAT2 ',I8,' entry are the MYSTRAN calculated values to',    &
                           ' replace the zero input values by the user that',/,14X,' were meant to simulate zero transverse shear',&
                           ' flexibility')

! ##################################################################################################################################
 
      CONTAINS

! ##################################################################################################################################

      SUBROUTINE GET_CHAR8_OUTPUTS ( C8FLD_GIJ, C8FLD_ALP, C8FLD_RHO, C8FLD_TREF, C8FLD_TM, C8FLD_ZS )

      IMPLICIT NONE

      CHARACTER(8*BYTE), INTENT(OUT)  :: C8FLD_GIJ(3,3,4)    ! Char representation of MAT2 Gij entries
      CHARACTER(8*BYTE), INTENT(OUT)  :: C8FLD_ALP(6,MEMATC) ! Char representation of MAT2 CTE entries
      CHARACTER(8*BYTE), INTENT(OUT)  :: C8FLD_RHO(4)        ! Char representation of MAT2 RHO entry
      CHARACTER(8*BYTE), INTENT(OUT)  :: C8FLD_TREF(4)       ! Char representation of MAT2 TREF entry
      CHARACTER(8*BYTE), INTENT(OUT)  :: C8FLD_TM            ! Char representation of MAT2 TM entry
      CHARACTER(8*BYTE), INTENT(OUT)  :: C8FLD_ZS(2)         ! Char representation of MAT2 ZS entry

      INTEGER(LONG)                   :: II,JJ,KK            ! DO loop indices

! **********************************************************************************************************************************
      DO II=1,3
         DO JJ=1,3
            DO KK=1,4
               C8FLD_GIJ(II,JJ,KK)(1:8) = ' '
            ENDDO
         ENDDO
      ENDDO

      DO II=1,6
         DO JJ=1,MEMATC
            C8FLD_ALP(II,JJ)(1:8) = ' '
         ENDDO
      ENDDO

! Put GIJ values in character format

      CALL REAL_DATA_TO_C8FLD ( PCOMP_MEM_MAT2(1,1) ,C8FLD_GIJ(1,1,1) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MEM_MAT2(1,2) ,C8FLD_GIJ(1,2,1) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MEM_MAT2(1,3) ,C8FLD_GIJ(1,3,1) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MEM_MAT2(2,2) ,C8FLD_GIJ(2,2,1) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MEM_MAT2(2,3) ,C8FLD_GIJ(2,3,1) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MEM_MAT2(3,3) ,C8FLD_GIJ(3,3,1) )
                                                             
      CALL REAL_DATA_TO_C8FLD ( PCOMP_BEN_MAT2(1,1) ,C8FLD_GIJ(1,1,2) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_BEN_MAT2(1,2) ,C8FLD_GIJ(1,2,2) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_BEN_MAT2(1,3) ,C8FLD_GIJ(1,3,2) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_BEN_MAT2(2,2) ,C8FLD_GIJ(2,2,2) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_BEN_MAT2(2,3) ,C8FLD_GIJ(2,3,2) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_BEN_MAT2(3,3) ,C8FLD_GIJ(3,3,2) )
                                                             
      CALL REAL_DATA_TO_C8FLD ( PCOMP_TSH_MAT2(1,1) ,C8FLD_GIJ(1,1,3) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_TSH_MAT2(1,2) ,C8FLD_GIJ(1,2,3) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_TSH_MAT2(2,2) ,C8FLD_GIJ(2,2,3) )
                                                             
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MBC_MAT2(1,1) ,C8FLD_GIJ(1,1,4) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MBC_MAT2(1,2) ,C8FLD_GIJ(1,2,4) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MBC_MAT2(1,3) ,C8FLD_GIJ(1,3,4) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MBC_MAT2(2,2) ,C8FLD_GIJ(2,2,4) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MBC_MAT2(2,3) ,C8FLD_GIJ(2,3,4) )
      CALL REAL_DATA_TO_C8FLD ( PCOMP_MBC_MAT2(3,3) ,C8FLD_GIJ(3,3,4) )

! Put CTE's in character format

      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(1,1), C8FLD_ALP(1,1) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(2,1), C8FLD_ALP(2,1) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(3,1), C8FLD_ALP(3,1) )

      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(1,2), C8FLD_ALP(1,2) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(2,2), C8FLD_ALP(2,2) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(3,2), C8FLD_ALP(3,2) )

      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(5,3), C8FLD_ALP(5,3) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(6,3), C8FLD_ALP(6,3) )

      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(1,4), C8FLD_ALP(1,4) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(2,4), C8FLD_ALP(2,4) )
      CALL REAL_DATA_TO_C8FLD ( SHELL_ALP(3,4), C8FLD_ALP(3,4) )

! Put RHO's in character format

      CALL REAL_DATA_TO_C8FLD ( RHO(1), C8FLD_RHO(1) )
      CALL REAL_DATA_TO_C8FLD ( RHO(2), C8FLD_RHO(2) )
      CALL REAL_DATA_TO_C8FLD ( RHO(3), C8FLD_RHO(3) )
      CALL REAL_DATA_TO_C8FLD ( RHO(4), C8FLD_RHO(4) )

! Put TREF's in character format

      CALL REAL_DATA_TO_C8FLD ( TREF(1), C8FLD_TREF(1) )
      CALL REAL_DATA_TO_C8FLD ( TREF(2), C8FLD_TREF(2) )
      CALL REAL_DATA_TO_C8FLD ( TREF(3), C8FLD_TREF(3) )
      CALL REAL_DATA_TO_C8FLD ( TREF(4), C8FLD_TREF(4) )

! Put TM in character format

      CALL REAL_DATA_TO_C8FLD ( PCOMP_TM, C8FLD_TM )

! Put ZS's in character format

      CALL REAL_DATA_TO_C8FLD ( ZS(1), C8FLD_ZS(1) )
      CALL REAL_DATA_TO_C8FLD ( ZS(2), C8FLD_ZS(2) )

! **********************************************************************************************************************************

      END SUBROUTINE GET_CHAR8_OUTPUTS

      END SUBROUTINE WRITE_PCOMP_EQUIV
