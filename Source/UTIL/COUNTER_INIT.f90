! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

! This subroutine inits a counter so you can use COUNTER_PROGRESS.
! It's an alternative to way MYSTRAN used to do counters, which badly affect
! performance due to the blocking nature of terminal output.

! Arguments:
!   PREFIX: a string printed before the progress (e.g. "PROCESSING K-SET")
!    TOTAL: the maximum number this counter will reach.
SUBROUTINE COUNTER_INIT(PREFIX, TOTAL)

   USE PENTIUM_II_KIND, ONLY : LONG
   USE SCONTR, ONLY : COUNTER_VALUE, COUNTER_PERC, COUNTER_TOTAL, &
                      COUNTER_STARTED, COUNTER_PREFIX, FATAL_ERR, COUNTER_FMT
   USE IOUNT1, ONLY : ERR, F06
   USE PARAMS, ONLY : NOCOUNTS 
   
   USE COUNTER_INIT_USE_IFs

   IMPLICIT NONE

   CHARACTER(LEN=*), INTENT(IN) :: PREFIX
   INTEGER(LONG),    INTENT(IN) :: TOTAL

   ! Do nothing if NOCOUNTS is set
   IF (NOCOUNTS == 'Y') THEN
      RETURN
   END IF

   ! Check if the last counter was done
   IF (COUNTER_VALUE /= COUNTER_TOTAL) THEN
      FATAL_ERR = FATAL_ERR + 1
      WRITE(ERR,1414)
      WRITE(F06,1414)
      CALL OUTA_HERE ( 'Y' )
   END IF

   ! Reset everything
   COUNTER_VALUE = 0
   COUNTER_PERC = 0
   COUNTER_TOTAL = TOTAL
   
   ALLOCATE(CHARACTER(LEN(PREFIX)) :: COUNTER_PREFIX)

   ! Set the timestamp and the prefix
   CALL UNIX_TIME(COUNTER_STARTED)
   COUNTER_PREFIX = PREFIX
   WRITE(COUNTER_FMT, "(A, I0, A)") "(I", CEILING(LOG10(REAL(TOTAL))), ")"

   ! Call the progress subroutine and force printing
   CALL COUNTER_PROGRESS(0)

   1414 FORMAT(' *ERROR  1414: A COUNTER WAS STARTED BEFORE THE CURRENT ONE WAS DONE' &
                        ,/,14X,' THIS IS A DEVELOPER ERROR. PLEASE REPORT IT.')
END SUBROUTINE COUNTER_INIT
