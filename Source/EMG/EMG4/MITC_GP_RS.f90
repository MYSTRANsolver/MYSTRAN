! #################################################################################################################################
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
      FUNCTION MITC_GP_RS ()

! Returns the isoparametric coordinates R,S of all grid points of the element.
! Index 1 is 1=R, 2=S
! Index 2 is element grid point number
 
      USE PENTIUM_II_KIND, ONLY       :  DOUBLE
      USE MODEL_STUF, ONLY            :  TYPE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR
      USE MODEL_STUF, ONLY            :  ELGP
      
      USE OUTA_HERE_Interface

      IMPLICIT NONE 

      REAL(DOUBLE)                    :: MITC_GP_RS(2,ELGP)

! **********************************************************************************************************************************
      

      IF (TYPE(1:5) == 'QUAD8') THEN

         MITC_GP_RS(1,1) = -ONE
         MITC_GP_RS(1,2) =  ONE
         MITC_GP_RS(1,3) =  ONE
         MITC_GP_RS(1,4) = -ONE
         MITC_GP_RS(1,5) =  ZERO
         MITC_GP_RS(1,6) =  ONE
         MITC_GP_RS(1,7) =  ZERO
         MITC_GP_RS(1,8) = -ONE
  
         MITC_GP_RS(2,1) = -ONE
         MITC_GP_RS(2,2) = -ONE
         MITC_GP_RS(2,3) =  ONE
         MITC_GP_RS(2,4) =  ONE
         MITC_GP_RS(2,5) = -ONE
         MITC_GP_RS(2,6) =  ZERO
         MITC_GP_RS(2,7) =  ONE
         MITC_GP_RS(2,8) =  ZERO

      ELSE

        WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
        WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE', TYPE
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF

      RETURN


! **********************************************************************************************************************************
  
      END FUNCTION MITC_GP_RS
