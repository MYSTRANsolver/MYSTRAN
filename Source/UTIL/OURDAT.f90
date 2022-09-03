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
 
      SUBROUTINE OURDAT
 
! Returns date info using Fortran DATE_AND_TIME intrinsic procedure
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY
 
      USE OURDAT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER( 8*BYTE)              :: DATE              ! Date returned from intrinsic function DATE_AND_TIME
      CHARACTER(10*BYTE)              :: TIME              ! Time returned from intrinsic function DATE_AND_TIME
      CHARACTER( 5*BYTE)              :: ZONE              ! Zone returned from intrinsic function DATE_AND_TIME
 
      INTEGER(LONG)                   :: VALUES(8)         ! Contains year, month, day from intrinsic function DATE_AND_TIME
 
      INTRINSIC                       :: DATE_AND_TIME
 
! **********************************************************************************************************************************
      CALL DATE_AND_TIME(DATE,TIME,ZONE,VALUES)

      YEAR  = VALUES(1)
      MONTH = VALUES(2)
      DAY   = VALUES(3)
 
      RETURN
 
      END SUBROUTINE OURDAT
