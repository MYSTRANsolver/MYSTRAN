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

   MODULE BANDIT_FILES_Interface

   INTERFACE

      SUBROUTINE BANDIT_FILES ( IOU6, IOU7, IOU8, IOU9, IOU11, IOU12, IOU13, IOU14, IOU15, IOU16, IOU17 )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, F06, SC1

      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: IOU6              ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU7              ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU8              ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU9              ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU11             ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU12             ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU13             ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU14             ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU15             ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU16             ! Bandit file unit number
      INTEGER(LONG), INTENT(IN)       :: IOU17             ! Bandit file unit number

      END SUBROUTINE BANDIT_FILES

   END INTERFACE

   END MODULE BANDIT_FILES_Interface

