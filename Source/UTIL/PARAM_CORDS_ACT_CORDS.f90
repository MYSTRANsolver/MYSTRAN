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
      IF      (TYPE(1:5) == 'QUAD4') THEN
         CALL GET_QUAD4_COORDS
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

      SUBROUTINE GET_QUAD4_COORDS

! Parametric coords of points are in array XEP. They are obtained from the corner node coords in array XEL from:

!                                   XEA = PSH_MAT*XEL

! Only the 1st 4 rows of XEL and XEA are processed (using DUM1 = 1st 4 rows of XEL and DUM2 = 1st 4 rows of XEA).
! The terms in PSH_MAT are the shape functions from the PSH rows from subr SHP2DQ for each of the 4 XEP points.
! Each of these rows is called matrix PSHi (i=1,2,3,4) below.

      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_BUG
      USE MODEL_STUF, ONLY            :  XEL

      IMPLICIT NONE

      INTEGER(LONG)                   :: I,J                 ! DO loop indices

      REAL(DOUBLE)                    :: PSH1(4)             ! Shape functions for Gauss point 1,1
      REAL(DOUBLE)                    :: PSH2(4)             ! Shape functions for Gauss point 2,1
      REAL(DOUBLE)                    :: PSH3(4)             ! Shape functions for Gauss point 2,2
      REAL(DOUBLE)                    :: PSH4(4)             ! Shape functions for Gauss point 1,2
      REAL(DOUBLE)                    :: DUM1(4,3)           ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM2(4,3)           ! Intermediate matrix
      REAL(DOUBLE)                    :: DPSHG(2,4)          ! Derivatives of PSH wrt elem isopar coords (not used here).

                                                             ! 4x4 matrix used to calc Gauss pt coords from node coords
      REAL(DOUBLE)                    :: PSH_MAT(IORD*IORD,4)

! **********************************************************************************************************************************
! Initialize

      DO I=1,IORD*IORD
         DO J=1,4
            PSH_MAT(I,J) = ZERO
         ENDDO
      ENDDO

! The PSH rows are from subr SHP2DQ for each of the 4 XEP parametric coord points for the element. 
! We want the XEA orderd in the same fashion as the element node coords in XEL (namely 1-2-3-4 clockwise around the element).

      CALL SHP2DQ ( 1, 1, 4, SUBR_NAME, ' ', IORD, XEP(1,1), XEP(1,2), 'Y', PSH1, DPSHG )
      CALL SHP2DQ ( 2, 1, 4, SUBR_NAME, ' ', IORD, XEP(2,1), XEP(2,2), 'Y', PSH2, DPSHG )
      CALL SHP2DQ ( 2, 2, 4, SUBR_NAME, ' ', IORD, XEP(3,1), XEP(3,2), 'Y', PSH3, DPSHG )
      CALL SHP2DQ ( 1, 2, 4, SUBR_NAME, ' ', IORD, XEP(4,1), XEP(4,2), 'Y', PSH4, DPSHG )

! Put the PSHi into matrix PSH_MAT which will multiply XEL to get XGL

      PSH_MAT(1,1) = PSH1(1)  ;  PSH_MAT(1,2) = PSH1(2)  ;  PSH_MAT(1,3) = PSH1(3)  ;  PSH_MAT(1,4) = PSH1(4)
      PSH_MAT(2,1) = PSH2(1)  ;  PSH_MAT(2,2) = PSH2(2)  ;  PSH_MAT(2,3) = PSH2(3)  ;  PSH_MAT(2,4) = PSH2(4)
      PSH_MAT(3,1) = PSH3(1)  ;  PSH_MAT(3,2) = PSH3(2)  ;  PSH_MAT(3,3) = PSH3(3)  ;  PSH_MAT(3,4) = PSH3(4)
      PSH_MAT(4,1) = PSH4(1)  ;  PSH_MAT(4,2) = PSH4(2)  ;  PSH_MAT(4,3) = PSH4(3)  ;  PSH_MAT(4,4) = PSH4(4)

! Get 1st 4 rows of XEL in DUM1

      DO I=1,4
         DO J=1,3
            DUM1(I,J) = XEL(I,J)
         ENDDO
      ENDDO

! Mult 1st 3 rows of XEL (node coords) by PSH_MAT to get 1st 3 rows of XGL (Gauss pt coords in local elem axes)

      CALL MATMULT_FFF ( PSH_MAT, DUM1, 4, 4, 3, DUM2 )

! Set XEA ist 4 rows equal to DUM2 from above

      DO I=1,4
         DO J=1,3
            XEA(I,J) = DUM2(I,J)
         ENDDO
      ENDDO

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

      END SUBROUTINE GET_QUAD4_COORDS

      END SUBROUTINE PARAM_CORDS_ACT_CORDS
