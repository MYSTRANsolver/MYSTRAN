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
 
      SUBROUTINE DOF_PROC ( TDOF_MSG )
 
! DOF Processor 
 
! Part 1: Generate TSET table (see subr TSET_PROC for explanation)
! ------
!    TSET is a table that the DOF set (e.g. "G ", "N ", etc) for each of the 6 components for every grid)
 
! Part 2: Generate USET table (see subr TSET_PROC for explanation)
! ------
!    USET is a table that the user defined set set ("U1" or "U2") for each of the 6 components for every grid)
 
! Part 3: Generate TDOF table from TSET and USET
! ------
!    TDOF is a table that has the DOF number for every DOF and every DOF set
 
! Part 4: Check for errors
! ------
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFSE, NUM_USETSTR, SOL_NAME
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
 
      USE DOF_PROC_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'DOF_PROC'
      CHARACTER(LEN=*), INTENT(IN)    :: TDOF_MSG          ! Message to be printed out regarding at what point in the run the TDOF,I
!                                                            tables are printed out
      CHARACTER(43*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Part 1:  Generate TSET table
! ------
      CALL OURTIM
      MODNAM = ' DOF Set Table                              '
      WRITE(SC1,1093) MODNAM, HOUR, MINUTE, SEC, SFRAC
      CALL TSET_PROC

! Part 2:  Generate USET table if there are any USETSTR entries entries in the Bulk Data.
! ------
      IF (NUM_USETSTR > 0) THEN
         CALL USET_PROC
      ENDIF
 
! Part 3: Generate TDOF table from TSET
! ------
      CALL OURTIM
      MODNAM = ' DOF Number Table                           '
      WRITE(SC1,1093) MODNAM, HOUR, MINUTE, SEC, SFRAC
      CALL TDOF_PROC ( TDOF_MSG )

! Part 4: Make sure that NDOFSE /= 0 only in statics
! ------
      IF (NDOFSE > 0) THEN
         IF ((SOL_NAME(1:7) /= 'STATICS') .AND. (SOL_NAME(1:8) /= 'BUCKLING') .AND. (SOL_NAME(1:8) /= 'NLSTATIC')) THEN
            WRITE(ERR,1323) SOL_NAME
            WRITE(F06,1323) SOL_NAME
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
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
 1093 FORMAT(5X,A,18X,2X,I2,':',I2,':',I2,'.',I3)

 1323 FORMAT(' *ERROR  1323: ENFORCED DISPLACEMENTS ONLY ALLOWED IN STATICS SOLUTION. HOWEVER, SOL = ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE DOF_PROC
