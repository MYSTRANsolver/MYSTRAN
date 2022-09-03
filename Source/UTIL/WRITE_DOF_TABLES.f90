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

      SUBROUTINE WRITE_DOF_TABLES

! Writess DOF table data to file LINK1C

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, L1C, LINK1C, L1C_MSG
      USE SCONTR, ONLY                :  DATA_NAM_LEN, MTDOF, NDOFG, NGRID, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_DOF_TABLES_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI, TDOF, TSET

      USE WRITE_DOF_TABLES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_DOF_TABLES'
      CHARACTER(LEN=DATA_NAM_LEN)     :: DATA_SET_NAME      ! A data set name for output purposes

      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_DOF_TABLES_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Write TSET, TDOF, TDOFI tables to file L1C

      DATA_SET_NAME = 'TSET'
      WRITE(L1C) DATA_SET_NAME
      WRITE(L1C) NGRID
      DO I = 1,NGRID
         DO J = 1,6
           WRITE(L1C) TSET(I,J)
         ENDDO
      ENDDO 
      DATA_SET_NAME = 'TDOFI'
      WRITE(L1C) DATA_SET_NAME
      WRITE(L1C) NDOFG
      WRITE(L1C) MTDOF
      DO I = 1,NDOFG
         DO J = 1,MTDOF
            WRITE(L1C) TDOFI(I,J)
         ENDDO
      ENDDO 
      DATA_SET_NAME = 'TDOF'
      WRITE(L1C) DATA_SET_NAME
      WRITE(L1C) NDOFG
      WRITE(L1C) MTDOF
      DO I = 1,NDOFG
         DO J = 1,MTDOF
            WRITE(L1C) TDOF(I,J)
         ENDDO
      ENDDO 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_DOF_TABLES
