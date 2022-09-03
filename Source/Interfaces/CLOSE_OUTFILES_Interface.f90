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

   MODULE CLOSE_OUTFILES_Interface

   INTERFACE

      SUBROUTINE CLOSE_OUTFILES ( BUG_CLOSE_STAT, ERR_CLOSE_STAT, F04_CLOSE_STAT, OP2_CLOSE_STAT, PCH_CLOSE_STAT ) 

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE
      USE IOUNT1, ONLY                :  BUG   , ERR   , F04   , F06   , OP2   , PCH    ,SC1, WRT_LOG,                             &
                                         BUGFIL, ERRFIL, F04FIL, F06FIL, OP2FIL, PCHFIL

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: BUG_CLOSE_STAT    ! Input value for close status for BUG
      CHARACTER(LEN=*), INTENT(IN)    :: ERR_CLOSE_STAT    ! Input value for close status for ERR
      CHARACTER(LEN=*), INTENT(IN)    :: F04_CLOSE_STAT    ! Input value for close status for F04
      CHARACTER(LEN=*), INTENT(IN)    :: OP2_CLOSE_STAT    ! Input value for close status for OP2
      CHARACTER(LEN=*), INTENT(IN)    :: PCH_CLOSE_STAT    ! Input value for close status for PCH

      END SUBROUTINE CLOSE_OUTFILES

   END INTERFACE

   END MODULE CLOSE_OUTFILES_Interface

