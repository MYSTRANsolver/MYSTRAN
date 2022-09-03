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

      SUBROUTINE PRINT_ORDER

! Writes abcissa's and weights from subroutines ORDER_GAUSS and ORDER_TRIA used in isoparametric element matrix generation subr's

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MAX_ORDER_GAUSS, MAX_ORDER_TRIA, NUM_TRIA_ORDERS, TRIA_ORDER_NUMS

      USE PRINT_ORDER_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PRINT_ORDER'
      CHARACTER(18*BYTE)              :: SSC, HHC              ! Char var used in writing to internal file for formatting output
      CHARACTER(18*BYTE)              :: SSC_I, SSC_J          ! Char var used in writing to internal file for formatting output

      INTEGER(LONG)                   :: J, KORDER             ! DO loop indices
      INTEGER(LONG)                   :: TORDER                ! Trianular integration order number

      REAL(DOUBLE)                    :: SSS(MAX_ORDER_GAUSS)  ! Gauss abscissa's
      REAL(DOUBLE)                    :: SS_I(MAX_ORDER_TRIA)  ! Tria X abscissa's
      REAL(DOUBLE)                    :: SS_J(MAX_ORDER_TRIA)  ! Tria Y abscissa's
      REAL(DOUBLE)                    :: HHH(MAX_ORDER_GAUSS)  ! Weights
  
! **********************************************************************************************************************************
! Write header

      WRITE(F06,1101)

! Write Gaussian orders

      WRITE(F06,1102)
      WRITE(F06,*)

      DO KORDER=1,MAX_ORDER_GAUSS
         CALL ORDER_GAUSS ( KORDER, SSS, HHH )
         WRITE(F06,1103) KORDER
         DO J=1,KORDER
            WRITE(SSC,1104) SSS(J)
            WRITE(HHC,1104) HHH(J)
            WRITE(F06,1105) J, SSC(1:8), SSC(9:13), SSC(14:18), HHC(1:8), HHC(9:13), HHC(14:18)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,*)
      ENDDO

! Write Triangular orders

      WRITE(F06,1112)
      WRITE(F06,*)

      DO KORDER=1,NUM_TRIA_ORDERS
         TORDER = TRIA_ORDER_NUMS(KORDER)
         IF (TORDER <= MAX_ORDER_TRIA) THEN
            CALL ORDER_TRIA ( TORDER, SS_I, SS_J, HHH )
            WRITE(F06,1113) TORDER
            DO J=1,TORDER
               WRITE(SSC_I,1114) SS_I(J)
               WRITE(SSC_J,1114) SS_J(J)
               WRITE(HHC,1114) HHH(J)
               WRITE(F06,1115) J, SSC_I(1:8), SSC_I(9:13), SSC_I(14:18), SSC_J(1:8), SSC_J(9:13), SSC_J(14:18),                    &
                               HHC(1:8), HHC(9:13), HHC(14:18)
            ENDDO
            WRITE(F06,*)
            WRITE(F06,*)
         ELSE
            WRITE(F06,2001) TORDER, MAX_ORDER_TRIA
            EXIT
         ENDIF
      ENDDO

! Write trailer

      WRITE(F06,1106)

! **********************************************************************************************************************************
 1101 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::::::::START DEBUG( 5) OUTPUT FROM SUBROUTINE ORDER::::::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1102 FORMAT(32X,'Abcissa and Weight Coefficients of the Gaussian Quadrature Formula')

 1103 FORMAT(65X,'n =',I3,/,65x,'------',//,41X,'J            SSS(J)                   HHH(J)')

 1104 FORMAT(F18.15)

 1105 FORMAT(39X, I3, 3X, A8,' ',A5,' ',A5,5X,A8,' ',A5,' ',A5)

 1112 FORMAT(32X,'Abcissa and Weight Coefficients of the Triangulur Quadrature Formula')

 1113 FORMAT(65X,'n =',I3,/,65X,'------',//,29X,'J          SS_I(J)                  SS_J(J)                   HHH(J)')

 1114 FORMAT(F18.15)

 1115 FORMAT(27X, I3, 3X, A8,' ',A5,' ',A5,5X,A8,' ',A5,' ',A5,5X,A8,' ',A5,' ',A5)

 1106 FORMAT(' :::::::::::::::::::::::::::::::::::::::::::END DEBUG( 5) OUTPUT FROM SUBROUTINE ORDER:::::::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

 2001 FORMAT(' *INFORMATION: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TRIANGULAR INTEGRATION ORDER ',I3,' IS GREATER THAM MAX_ORDER_TRIA = ',I3                             &
                    ,/,14X,' CANNOT COMPLETE PRINTOUT OF TRIANGULAR INTEGRATION ORDERS. THIS IS NOT A FATAL ERROR')

! **********************************************************************************************************************************

      END SUBROUTINE PRINT_ORDER
