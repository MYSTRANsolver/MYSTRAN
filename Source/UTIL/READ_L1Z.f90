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
 
      SUBROUTINE READ_L1Z
 
! Reads in data from an unformatted file regarding the Exec and Case Control data, from a CHKPNT run, when a restart is made.
! Checks are made to ensure that nothing has changed that would violate the restart rules
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, L1Z, LINK1Z, L1Z_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NSUB, SOL_NAME
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE MODEL_STUF, ONLY            :  CC_EIGR_SID, MPCSET, SPCSET, SUBLOD
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_L1Z_BEGEND
 
      USE READ_L1Z_USE_IFs


      IMPLICIT NONE
 
      CHARACTER(LEN(BLNK_SUB_NAM))    :: SUBR_NAME = 'READ_L1Z'
      CHARACTER(LEN(SOL_NAME))        :: SOL_NAME_OLD      ! SOL from original run that is being restarted

      INTEGER(LONG)                   :: CC_EIGR_SID_OLD   ! EIGR set ID from original run that is being restarted
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERROR      = 0   ! Local error count
      INTEGER(LONG)                   :: NSUB_OLD          ! Number of subcases from original run that is being restarted
      INTEGER(LONG)                   :: MPCSET_OLD        ! 
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: SPCSET_OLD        ! SPC set ID from original run that is being restarted
      INTEGER(LONG)                   :: SUBLOD1_OLD       ! Load set ID (for 1 subcase) from original run that is being restarted
      INTEGER(LONG)                   :: SUBLOD2_OLD       ! Temp set ID (for 1 subcase) from original run that is being restarted
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_L1Z_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

      CALL FILE_OPEN ( L1Z, LINK1Z, OUNT, 'OLD', L1Z_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

      IERROR = 0

      SOL_NAME_OLD(1:LEN(SOL_NAME)) = ' '
      READ(L1Z) SOL_NAME_OLD                               ! Must have the same SOL
      IF (SOL_NAME /= SOL_NAME_OLD) THEN
         IERROR = IERROR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE (ERR,1819) SOL_NAME, SOL_NAME_OLD
         WRITE (F06,1819) SOL_NAME, SOL_NAME_OLD
      ENDIF

      READ(L1Z) NSUB_OLD                                   ! Must have the same number of subcases in restart run as in original run
      IF (NSUB /= NSUB_OLD) THEN
         IERROR = IERROR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE (ERR,1821) NSUB, NSUB_OLD
         WRITE (F06,1821) NSUB, NSUB_OLD
      ENDIF

      READ(L1Z) MPCSET_OLD                                 ! Must have the same MPC set in restart run as in original run
      IF (MPCSET /= MPCSET_OLD) THEN
         IERROR = IERROR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE (ERR,1828) MPCSET, MPCSET_OLD 
         WRITE (F06,1828) MPCSET, MPCSET_OLD 
      ENDIF

      READ(L1Z) SPCSET_OLD                                 ! Must have the same SPC set in restart run as in original run
      IF (SPCSET /= SPCSET_OLD) THEN
         IERROR = IERROR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE (ERR,1823) SPCSET, SPCSET_OLD
         WRITE (F06,1823) SPCSET, SPCSET_OLD
      ENDIF

      IF (NSUB == NSUB_OLD) THEN                           ! Must have the same load and temp set ID's in restart run as in orig.
         DO I=1,NSUB
            READ(L1Z) SUBLOD1_OLD, SUBLOD2_OLD
            IF ((SUBLOD(I,1) /= SUBLOD1_OLD) .OR. (SUBLOD(I,2) /= SUBLOD2_OLD)) THEN
               IERROR = IERROR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE (ERR,1824) I
               WRITE (F06,1824) I
            ENDIF
         ENDDO 
      ENDIF

      READ(L1Z) CC_EIGR_SID_OLD                            ! Must have the same eigen set in restart run as in original run
      IF (CC_EIGR_SID /= CC_EIGR_SID_OLD) THEN
         IERROR = IERROR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE (ERR,1825) CC_EIGR_SID, CC_EIGR_SID_OLD
         WRITE (F06,1825) CC_EIGR_SID, CC_EIGR_SID_OLD
      ENDIF

      IF (IERROR /= 0) THEN
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
 1819 FORMAT(' *ERROR  1819: RESTART RUN MUST HAVE THE SAME SOLUTION NAME AS THE ORIGINAL RUN.'                                    &
                    ,/,14X,' HOWEVER, THIS RUN IS FOR "',A,'" WHILE THE ORIGINAL RUN WAS FOR "',A,'"')

 1821 FORMAT(' *ERROR  1821: RESTART RUN MUST HAVE THE SAME NUMBER OF SUBCASES AS THE ORIGINAL RUN.'                               &
                    ,/,14X,' HOWEVER, THIS RUN HAS ',I8,' SUBCASES WHILE THE ORIGINAL RUN HAD ',I8,' SUBCASES')

 1823 FORMAT(' *ERROR  1823: RESTART RUN MUST HAVE THE SAME SPC SET AS THE ORIGINAL RUN.'                                          &
                    ,/,14X,' HOWEVER, THIS RUN HAS SPC SET = ',I8,' (OR NO SPC SET SPECIFIED) WHILE THE ORIG RUN HAD SPC SET = ',I8)

 1824 FORMAT(' *ERROR  1824: RESTART RUN MUST HAVE THE SAME LOADS AND TEMPERATURES AS THE ORIGINAL RUN.'                           &
                    ,/,14X,' HOWEVER, THIS RUN HAS DIFFERENT LOADS/TEMPS FOR INTERNAL SUBCASE ',I8)

 1825 FORMAT(' *ERROR  1825: RESTART RUN MUST HAVE THE SAME EIGENVALUE EXTRACTION SET AS THE ORIGINAL RUN.'                        &
                    ,/,14X,' HOWEVER, THIS RUN HAS EIGR SET = ',I8,' REQUESTED WHILE THE ORIGINAL RUN HAD EIGR SET = ',I8)

 1828 FORMAT(' *ERROR  1828: RESTART RUN MUST HAVE THE SAME MPC SET AS THE ORIGINAL RUN.'                                          &
                    ,/,14X,' HOWEVER, THIS RUN HAS MPC SET = ',I8,' WHILE THE ORIG RUN HAD MPC SET  = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE READ_L1Z
