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

      SUBROUTINE WRITE_GRID_COORDS

! Writes grid coordinates in basic coords to the F06 file if user has Bulk Data PARAM PRTBASIC defined

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  F06
      USE SCONTR, ONLY                :  NGRID
      USE MODEL_STUF, ONLY            :  GRID, RGRID

      USE WRITE_GRID_COORDS_USE_IFs                        ! Added 2019/07/14

      IMPLICIT NONE

      INTEGER(LONG)                   :: I, J              ! DO loop indices

! **********************************************************************************************************************************
      WRITE(F06,1002)
      WRITE(F06,1003)
      DO I = 1,NGRID
         WRITE(F06,1004) GRID(I,1),(RGRID(I,J),J = 1,3)
      ENDDO
      WRITE(F06,*)   
      WRITE(F06,*)

! **********************************************************************************************************************************
 1002 FORMAT(//,48X,'GRID POINT COORDINATES IN BASIC COORDINATE SYSTEM')

 1003 FORMAT(/,36X,'GRID ID           X                 Y                 Z')

 1004 FORMAT(35X,I8,1X,1ES17.6,1X,1ES17.6,1X,1ES17.6)
 
! **********************************************************************************************************************************

      END SUBROUTINE WRITE_GRID_COORDS

