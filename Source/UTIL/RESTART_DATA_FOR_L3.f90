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
 
      SUBROUTINE RESTART_DATA_FOR_L3
 
! Reads matrices needed when a restart is made in LINK3 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE IOUNT1, ONLY                :  L2G, LINK2G, L2G_MSG, L2GSTAT
      USE IOUNT1, ONLY                :  L2H, LINK2H, L2H_MSG, L2HSTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFL, NTERM_KLL, NTERM_PL
      USE TIMDAT, ONLY                :  TSEC 
      USE SPARSE_MATRICES, ONLY       :  I_KLL , J_KLL , KLL ,I_PL , J_PL , PL
      USE SUBR_BEGEND_LEVELS, ONLY    :  RESTART_DATA_FOR_L3_BEGEND

      USE RESTART_DATA_FOR_L3_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: FILE_EXIST        ! Result from INQUIRE is true if a file exists

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RESTART_DATA_FOR_L3'
      CHARACTER(  1*BYTE)             :: CLOSE_IT   = 'Y'  ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close a file or not 
      CHARACTER(  1*BYTE)             :: NTERM_RD   = 'Y'  ! 'Y' or 'N' Input to subr READ_MATRIX_1 
      CHARACTER(  1*BYTE)             :: OPND       = 'N'  ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to open  a file or not 

      INTEGER(LONG)                   :: IERR              ! Local error count

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RESTART_DATA_FOR_L3_BEGEND
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IERR = 0

! Read KLL

      INQUIRE ( FILE=LINK2G, EXIST=FILE_EXIST )
      IF ((FILE_EXIST) .AND. (NTERM_KLL > 0)) THEN
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KLL ', CR13
         WRITE(SC1,*) CR13
         CALL ALLOCATE_SPARSE_MAT ( 'KLL', NDOFL, NTERM_KLL, SUBR_NAME )
         CALL READ_MATRIX_1 ( LINK2G, L2G, OPND, CLOSE_IT, L2GSTAT, L2G_MSG,'KLL', NTERM_KLL, NTERM_RD, NDOFL, I_KLL, J_KLL, KLL)
      ELSE
         IERR = IERR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,101) L2G_MSG, LINK2G
         WRITE(F06,101) L2G_MSG, LINK2G
      ENDIF

! Read PL

      INQUIRE ( FILE=LINK2H, EXIST=FILE_EXIST )
      IF ((FILE_EXIST) .AND. (NTERM_PL > 0)) THEN
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   PL  ', CR13
         WRITE(SC1,*) CR13
         CALL ALLOCATE_SPARSE_MAT ( 'PL', NDOFL, NTERM_PL, SUBR_NAME )
         CALL READ_MATRIX_1 ( LINK2H, L2H, OPND, CLOSE_IT, L2HSTAT, L2H_MSG,'PL', NTERM_PL, NTERM_RD, NDOFL, I_PL, J_PL, PL)
      ELSE
         IERR = IERR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,101) L2G_MSG, LINK2G
         WRITE(F06,101) L2G_MSG, LINK2G
      ENDIF

      IF (IERR > 0) THEN
         WRITE(SC1, * ) ' PROCESSING STOPPED. SEE F06 FILE FOR DETAILS'
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
  101 FORMAT(' *ERROR      : THE FOLLOWING FILE FOR THE ',A                                                                        &
                    ,/,14X,' EITHER DOES NOT EXIST OR IS NULL: '                                                                   &
                    ,/,15X,A)

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************

      END SUBROUTINE RESTART_DATA_FOR_L3
