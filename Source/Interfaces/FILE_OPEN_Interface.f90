! ###############################################################################################################################
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

   MODULE FILE_OPEN_Interface

   INTERFACE

      SUBROUTINE FILE_OPEN (UNIT, FILNAM, OUNT, STATUS, MESSAG, RW_STIME, FORMAT, ACTION, POSITION, WRITE_L1A, WRITE_VER, WRITE_F04)


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ANS, F04, F06, IN1, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, PROG_NAME
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE DEBUG_PARAMETERS
      USE MYSTRAN_Version, ONLY       :  MYSTRAN_VER_NUM, MYSTRAN_VER_MONTH, MYSTRAN_VER_DAY, MYSTRAN_VER_YEAR, MYSTRAN_AUTHOR,  &
                                         MYSTRAN_COMMENT
      USE SUBR_BEGEND_LEVELS, ONLY    :  FILE_OPEN_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: ACTION            ! File description
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: FORMAT            ! File format
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! File description
      CHARACTER(LEN=*), INTENT(IN)    :: POSITION          ! File description 
      CHARACTER(LEN=*), INTENT(IN)    :: STATUS            ! File status indicator (NEW, OLD, REPLACE)
      CHARACTER(LEN=*), INTENT(IN)    :: RW_STIME          ! Indicator of whether to read or write STIME
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_L1A         ! 'Y'/'N' Arg passed to subr OUTA_HERE
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_VER         ! 'Y'/'N' Arg to tell whether to write MYSTRAN version info
 
      INTEGER(LONG), INTENT(IN)       :: UNIT              ! Unit number file is attached to
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)           ! File units to write messages to. Input to subr FILE_OPEN  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FILE_OPEN_BEGEND

      END SUBROUTINE FILE_OPEN

   END INTERFACE

   END MODULE FILE_OPEN_Interface

