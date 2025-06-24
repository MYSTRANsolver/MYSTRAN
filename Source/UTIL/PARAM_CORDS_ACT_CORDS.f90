! ##################################################################################################################################
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

      SUBROUTINE PARAM_CORDS_ACT_CORDS ( NROW, IORD, XEP, XEA )

! Converts element isoparametric coordinates to actual local element coordinates using bilinear shape functions.
! This is used in subr POLYNOM_FIT_STRE_STRN for extrapolating stress/strain values at the points at which the stress/strain
! matrices were calculated to element corner nodes

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  TYPE, XEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARAM_CORDS_ACT_CORDS_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PARAM_CORDS_ACT_CORDS'

      INTEGER(LONG), INTENT(IN)       :: IORD              ! Gaussian integration order to be used in obtaining the PSH shape fcns
      INTEGER(LONG), INTENT(IN)       :: NROW              ! Number of rows in XEP, XEA
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PARAM_CORDS_ACT_CORDS_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: XEP(NROW,3)       ! Parametric coords of NCOL points
      REAL(DOUBLE), INTENT(OUT)       :: XEA(NROW,3)       ! Actual local element coords corresponding to XEP

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF     ((TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'QUAD8')) THEN
         CALL GET_QUAD_COORDS
      ELSE
         Write(err,*) ' *ERROR      : Code not written in subr PARAM_CORDS_ACT_CORDS for', type
         Write(f06,*) ' *ERROR      : Code not written in subr PARAM_CORDS_ACT_CORDS for', type
       fatal_err = fatal_err + 1
         call outa_here ( 'y' )
      ENDIF
      
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################


      SUBROUTINE GET_QUAD_COORDS

! Parametric coords of points are in array XEP. They are obtained from the corner node coords in array XEL from:

!                                   XEA = PSH_MAT*XEL

! The terms in PSH_MAT are the shape functions from the PSH rows from subr SHP2DQ for each of the 4 XEP points.

      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  WRT_BUG
      USE MODEL_STUF, ONLY            :  XEL, ELGP

      IMPLICIT NONE

      INTEGER(LONG)                   :: J                   ! DO loop index

      REAL(DOUBLE)                    :: DPSHG(2,ELGP)       ! Derivatives of PSH wrt elem isopar coords (not used here).

                                                             ! 4x4 matrix used to calc Gauss pt coords from node coords
      REAL(DOUBLE)                    :: PSH_MAT(IORD*IORD,ELGP)

! **********************************************************************************************************************************

! The PSH_MAT rows are from subr SHP2DQ for each of the 4 XEP parametric coord points for the element. 
! We want the XEA orderd in the same fashion as the element node coords in XEL (namely 1-2-3-4 clockwise around the element).

      CALL SHP2DQ ( 1, 1, ELGP, SUBR_NAME, ' ', IORD, XEP(1,1), XEP(1,2), 'Y', PSH_MAT(1,:), DPSHG )
      CALL SHP2DQ ( 2, 1, ELGP, SUBR_NAME, ' ', IORD, XEP(2,1), XEP(2,2), 'Y', PSH_MAT(2,:), DPSHG )
      CALL SHP2DQ ( 2, 2, ELGP, SUBR_NAME, ' ', IORD, XEP(3,1), XEP(3,2), 'Y', PSH_MAT(3,:), DPSHG )
      CALL SHP2DQ ( 1, 2, ELGP, SUBR_NAME, ' ', IORD, XEP(4,1), XEP(4,2), 'Y', PSH_MAT(4,:), DPSHG )


! Multiply shape functions by grid point coordinates to get Gauss point coordinates
! Only the first ELGP rows of XEL are used because it may have additional unused rows.
      CALL MATMULT_FFF ( PSH_MAT, XEL(1:ELGP,:), IORD*IORD, ELGP, 3, XEA )


! Debug output

      IF (WRT_BUG(5) == 1) THEN
         WRITE(BUG,*)
         WRITE(BUG,*) '  Parametric and element local coords. (Pt no., parametric coords (XEP), actual coords (XEA)'
         WRITE(BUG,*) '        Pt No.         Parametric Coords, XEP          XEA, coords in local elem coord system'
         WRITE(BUG,*) '                          XI            ET                X             Y               Z'
         WRITE(BUG,1001) '1',(XEP(1,J),J=1,3) , (XEA(1,J),J=1,3) 
         WRITE(BUG,1001) '2',(XEP(2,J),J=1,3) , (XEA(2,J),J=1,3) 
         WRITE(BUG,1001) '3',(XEP(3,J),J=1,3) , (XEA(3,J),J=1,3) 
         WRITE(BUG,1001) '4',(XEP(4,J),J=1,3) , (XEA(4,J),J=1,3)
         WRITE(BUG,*)
      ENDIF 

! **********************************************************************************************************************************
 1001 FORMAT(8X,A,10X,2(1ES15.6),2X,3(1ES15.6))

! **********************************************************************************************************************************

      END SUBROUTINE GET_QUAD_COORDS


      END SUBROUTINE PARAM_CORDS_ACT_CORDS
