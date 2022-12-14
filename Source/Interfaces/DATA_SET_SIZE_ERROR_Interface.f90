! ###############################################################################################################################
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

   MODULE DATA_SET_SIZE_ERROR_Interface

   INTERFACE

      SUBROUTINE DATA_SET_SIZE_ERROR ( FILNAM, DATA_SET_NAME, DATA_NAME, INT1, INT2 )


      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06, LINK1A
      USE SCONTR, ONLY                :  FATAL_ERR

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: DATA_SET_NAME     ! Name of data set read from FILNAM
      CHARACTER(LEN=*), INTENT(IN)    :: DATA_NAME         ! Name of data variable that should have been read
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! Nmae of file data was read from

      INTEGER(LONG)   , INTENT(IN)    :: INT1              ! Size of data variable that should have been read
      INTEGER(LONG)   , INTENT(IN)    :: INT2              ! Size of data variable read

      END SUBROUTINE DATA_SET_SIZE_ERROR

   END INTERFACE

   END MODULE DATA_SET_SIZE_ERROR_Interface

