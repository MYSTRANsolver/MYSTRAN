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

   MODULE DATA_SET_NAME_ERROR_Interface

   INTERFACE

      SUBROUTINE DATA_SET_NAME_ERROR ( DATA_NAME_ShouldBe, FILNAM, DATA_NAME_Is )


      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: DATA_NAME_Is      ! Data set name actual
      CHARACTER(LEN=*), INTENT(IN)    :: DATA_NAME_ShouldBe! Data set name that should be
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! name of file data set was read from

      END SUBROUTINE DATA_SET_NAME_ERROR

   END INTERFACE

   END MODULE DATA_SET_NAME_ERROR_Interface

