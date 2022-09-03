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

      SUBROUTINE DATA_SET_NAME_ERROR ( DATA_NAME_ShouldBe, FILNAM, DATA_NAME_Is )

! Writes message indicating that the name of a data set about to be read from an unformatted file is not what was expected
! and then aborts

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE DATA_SET_NAME_ERROR_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: DATA_NAME_Is      ! Data set name actual
      CHARACTER(LEN=*), INTENT(IN)    :: DATA_NAME_ShouldBe! Data set name that should be
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! name of file data set was read from

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: NAME_Is_LEN       ! Length of DATA_NAME_Is       without trailing blanks
      INTEGER(LONG)                   :: NAME_ShouldBe_LEN ! Length of DATA_NAME_ShouldBe without trailing blanks

! **********************************************************************************************************************************
      DO I=LEN(DATA_NAME_Is),1,-1
         IF (DATA_NAME_Is(I:I) == ' ') THEN
            CYCLE
         ELSE
            NAME_Is_LEN = I
            EXIT
         ENDIF
      ENDDO

      DO I=LEN(DATA_NAME_ShouldBe),1,-1
         IF (DATA_NAME_ShouldBe(I:I) == ' ') THEN
            CYCLE
         ELSE
            NAME_ShouldBe_LEN = I
            EXIT
         ENDIF
      ENDDO

      WRITE(ERR,9001) DATA_NAME_ShouldBe(1:NAME_ShouldBe_LEN)
      CALL WRITE_FILNAM ( FILNAM, ERR, 15 )
      WRITE(ERR,9002) DATA_NAME_Is(1:NAME_Is_LEN)

      WRITE(F06,9001) DATA_NAME_ShouldBe(1:NAME_Is_LEN)
      CALL WRITE_FILNAM ( FILNAM, F06, 15 )
      WRITE(F06,9002) DATA_NAME_Is(1:NAME_Is_LEN)

      FATAL_ERR = FATAL_ERR + 1
      CALL OUTA_HERE ( 'Y' )
      WRITE(F06,9002) DATA_NAME_Is

 
! **********************************************************************************************************************************
 9001 FORMAT(' *ERROR   900: SHOULD BE READING DATA SET: "',A,'" FROM FILE:')

 9002 FORMAT('               BUT DATA IS NAMED "',A,'"')

! **********************************************************************************************************************************

      END SUBROUTINE DATA_SET_NAME_ERROR