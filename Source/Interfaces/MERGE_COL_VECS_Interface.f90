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

   MODULE MERGE_COL_VECS_Interface

   INTERFACE

      SUBROUTINE MERGE_COL_VECS ( IN1_COL, IN1_NDOF, UIN1, IN2_COL, IN2_NDOF, UIN2  &

                        ,OUT_COL, OUT_NDOF, UOUT )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_COL_VECS_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI
      
      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN )      :: IN1_COL           ! Column number in TDOF, TDOFI for the displ set for input vector UIN1
      INTEGER(LONG), INTENT(IN )      :: IN2_COL           ! Column number in TDOF, TDOFI for the displ set for input vector UIN2
      INTEGER(LONG), INTENT(IN )      :: OUT_COL           ! Column number in TDOF, TDOFI for the displ set for input vector UOUT
      INTEGER(LONG), INTENT(IN )      :: IN1_NDOF          ! Size of array UIN1
      INTEGER(LONG), INTENT(IN )      :: IN2_NDOF          ! Size of array UIN2
      INTEGER(LONG), INTENT(IN )      :: OUT_NDOF          ! Size of array UOUT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_COL_VECS_BEGEND

      REAL(DOUBLE) , INTENT(IN )      :: UIN1(IN1_NDOF)    ! Input  vector for IN1_COL displ set
      REAL(DOUBLE) , INTENT(IN )      :: UIN2(IN2_NDOF)    ! Input  vector for IN2_COL displ set
      REAL(DOUBLE) , INTENT(OUT)      :: UOUT(OUT_NDOF)    ! Output vector for OUT_COL displ set
 
      END SUBROUTINE MERGE_COL_VECS

   END INTERFACE

   END MODULE MERGE_COL_VECS_Interface

