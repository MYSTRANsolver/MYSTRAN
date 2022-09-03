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

      SUBROUTINE WRITE_EDAT

! This subr write array EDAT based on user input Bulk Data DEBUG (see module DEBUG_PARAMS). EDAT is an array that contains all of
! the information read fron element connection entries in the Bulk Data

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM  , LGUSERIN      , LSUSERIN      , NELE          , NCUSERIN      , WARN_ERR, &
                                         MEDAT_CBAR    , MEDAT_CBEAM   , MEDAT_CBUSH   , MEDAT_CELAS1  , MEDAT_CELAS2  ,           &
                                         MEDAT_CELAS3  , MEDAT_CELAS4  , MEDAT_CHEXA8  , MEDAT_CHEXA20 , MEDAT_CPENTA6 ,           &
                                         MEDAT_CPENTA15, MEDAT_PLOTEL  , MEDAT_CQUAD   , MEDAT_CROD    ,                           &
                                         MEDAT_CSHEAR  , MEDAT_CTETRA4 , MEDAT_CTETRA10, MEDAT_CTRIA   ,                           &
                                         MEDAT_CUSER1  , MEDAT0_CUSERIN, METYPE
              
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  EDAT, EPNT, ETYPE
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_EDAT_BEGEND

      USE WRITE_EDAT_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_EDAT'
      CHARACTER(10*BYTE)              :: ICHAR             ! 

                                                           ! Descriptors for items in EDAT for each element in the Bulk Data deck
      CHARACTER(16*BYTE)              :: EDAT_DESCR(0:MAX(2*LGUSERIN+LSUSERIN+6,25),METYPE)
      CHARACTER(56*BYTE)              :: EDAT_EXPLAIN_CID  = ' (-99 is a placeholder to indicate that field was blank)'
      CHARACTER(61*BYTE)              :: EDAT_EXPLAIN_VVEC = ' (neg number -i indicates v-vector is in row i in array VVEC)'

      INTEGER(LONG)                   :: EPNTK             ! Value in array EPNT where data begins for an element
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: I1,L              ! Counters
      INTEGER(LONG)                   :: MEDAT             ! Number of terms in EDAT for a specific element type
      INTEGER(LONG)                   :: NG                ! Number of grids defined on a CUSERIN entry
      INTEGER(LONG)                   :: NS                ! Number of scalar points defined on a CUSERIN entry
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_EDAT_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=0,MAX(2*LGUSERIN+LSUSERIN+6,25)
         DO j=1,METYPE
            EDAT_DESCR(I,J)(1:) = ' '
         ENDDO
      ENDDO

! Set values for EDAT_DESCR array

      EDAT_DESCR( 0, 1) = 'BAR             '
      EDAT_DESCR( 1, 1) = 'Elem ID         '
      EDAT_DESCR( 2, 1) = 'Prop ID         '
      EDAT_DESCR( 3, 1) = 'Grid A          '
      EDAT_DESCR( 4, 1) = 'Grid B          '
      EDAT_DESCR( 5, 1) = 'V-vector key    '
      EDAT_DESCR( 6, 1) = 'Pin Flag A      '
      EDAT_DESCR( 7, 1) = 'Pin Flag B      '
      EDAT_DESCR( 8, 1) = 'Offset key      '

      EDAT_DESCR( 0, 2) = 'BEAM            '
      EDAT_DESCR( 1, 2) = 'Elem ID         '
      EDAT_DESCR( 2, 2) = 'Prop ID         '
      EDAT_DESCR( 3, 2) = 'Grid A          '
      EDAT_DESCR( 4, 2) = 'Grid B          '
      EDAT_DESCR( 5, 2) = 'V-vector key    '
      EDAT_DESCR( 6, 2) = 'Pin Flag A      '
      EDAT_DESCR( 7, 2) = 'Pin Flag B      '
      EDAT_DESCR( 8, 2) = 'Offset key      '

      EDAT_DESCR( 0, 3) = 'BUSH            '
      EDAT_DESCR( 1, 3) = 'Elem ID         '
      EDAT_DESCR( 2, 3) = 'Prop ID         '
      EDAT_DESCR( 3, 3) = 'Number of grids '               ! If CBUSH G2 is blank BUSH has only 1 grid. Otherwise 2 grids
      EDAT_DESCR( 4, 3) = 'Grid A          '
      EDAT_DESCR( 5, 3) = 'Grid B          '
      EDAT_DESCR( 6, 3) = 'V-vector key    '
      EDAT_DESCR( 7, 3) = 'CID             '
      EDAT_DESCR( 8, 3) = 'OCID            '
      EDAT_DESCR( 9, 3) = 'Offset key      '

      EDAT_DESCR( 0, 4) = 'ELAS1           '
      EDAT_DESCR( 1, 4) = 'Elem ID         '
      EDAT_DESCR( 2, 4) = 'Prop ID         '
      EDAT_DESCR( 3, 4) = 'Grid A          '
      EDAT_DESCR( 4, 4) = 'Comps at Grid A '
      EDAT_DESCR( 5, 4) = 'Grid B          '
      EDAT_DESCR( 6, 4) = 'Comps at Grid B '

      EDAT_DESCR( 0, 5) = 'ELAS2           '
      EDAT_DESCR( 1, 5) = 'Elem ID         '
      EDAT_DESCR( 2, 5) = 'Prop ID         '
      EDAT_DESCR( 3, 5) = 'Grid A          '
      EDAT_DESCR( 4, 5) = 'Comps at Grid A '
      EDAT_DESCR( 5, 5) = 'Grid B          '
      EDAT_DESCR( 6, 5) = 'Comps at Grid B '

      EDAT_DESCR( 0, 6) = 'ELAS3           '
      EDAT_DESCR( 1, 6) = 'Elem ID         '
      EDAT_DESCR( 2, 6) = 'Prop ID         '
      EDAT_DESCR( 3, 6) = 'Scalar point A  '
      EDAT_DESCR( 4, 6) = 'Scalar point B  '

      EDAT_DESCR( 0, 7) = 'ELAS4           '
      EDAT_DESCR( 1, 7) = 'Elem ID         '
      EDAT_DESCR( 2, 7) = 'Prop ID         '
      EDAT_DESCR( 3, 7) = 'Scalar point A  '
      EDAT_DESCR( 4, 7) = 'Scalar point B  '

      EDAT_DESCR( 0, 8) = 'HEXA8           '
      EDAT_DESCR( 1, 8) = 'Elem ID         '
      EDAT_DESCR( 2, 8) = 'Prop ID         '
      EDAT_DESCR( 3, 8) = 'Grid 1          '
      EDAT_DESCR( 4, 8) = 'Grid 2          '
      EDAT_DESCR( 5, 8) = 'Grid 3          '
      EDAT_DESCR( 6, 8) = 'Grid 4          '
      EDAT_DESCR( 7, 8) = 'Grid 5          '
      EDAT_DESCR( 8, 8) = 'Grid 6          '
      EDAT_DESCR( 9, 8) = 'Grid 7          '
      EDAT_DESCR(10,  8) = 'Grid 8          '

      EDAT_DESCR( 0, 9) = 'HEXA20          '
      EDAT_DESCR( 1, 9) = 'Elem ID         '
      EDAT_DESCR( 2, 9) = 'Prop ID         '
      EDAT_DESCR( 3, 9) = 'Grid  1         '
      EDAT_DESCR( 4, 9) = 'Grid  2         '
      EDAT_DESCR( 5, 9) = 'Grid  3         '
      EDAT_DESCR( 6, 9) = 'Grid  4         '
      EDAT_DESCR( 7, 9) = 'Grid  5         '
      EDAT_DESCR( 8, 9) = 'Grid  6         '
      EDAT_DESCR( 9, 9) = 'Grid  7         '
      EDAT_DESCR(10, 9) = 'Grid  8         '
      EDAT_DESCR(11, 9) = 'Grid  9         '
      EDAT_DESCR(12, 9) = 'Grid 10         '
      EDAT_DESCR(13, 9) = 'Grid 11         '
      EDAT_DESCR(14, 9) = 'Grid 12         '
      EDAT_DESCR(15, 9) = 'Grid 13         '
      EDAT_DESCR(16, 9) = 'Grid 14         '
      EDAT_DESCR(17, 9) = 'Grid 15         '
      EDAT_DESCR(18, 9) = 'Grid 16         '
      EDAT_DESCR(19, 9) = 'Grid 17         '
      EDAT_DESCR(20, 9) = 'Grid 18         '
      EDAT_DESCR(21, 9) = 'Grid 19         '
      EDAT_DESCR(22, 9) = 'Grid 20         '

      EDAT_DESCR( 0,10) = 'PENTA6          '
      EDAT_DESCR( 1,10) = 'Elem ID         '
      EDAT_DESCR( 2,10) = 'Prop ID         '
      EDAT_DESCR( 3,10) = 'Grid 1          '
      EDAT_DESCR( 5,10) = 'Grid 2          '
      EDAT_DESCR( 5,10) = 'Grid 3          '
      EDAT_DESCR( 6,10) = 'Grid 4          '
      EDAT_DESCR( 7,10) = 'Grid 5          '
      EDAT_DESCR( 8,10) = 'Grid 6          '

      EDAT_DESCR( 0,11) = 'PENTA15         '
      EDAT_DESCR( 1,11) = 'Elem ID         '
      EDAT_DESCR( 2,11) = 'Prop ID         '
      EDAT_DESCR( 3,11) = 'Grid  1         '
      EDAT_DESCR( 4,11) = 'Grid  2         '
      EDAT_DESCR( 5,11) = 'Grid  3         '
      EDAT_DESCR( 6,11) = 'Grid  4         '
      EDAT_DESCR( 7,11) = 'Grid  5         '
      EDAT_DESCR( 8,11) = 'Grid  6         '
      EDAT_DESCR( 9,11) = 'Grid  7         '
      EDAT_DESCR(10,11) = 'Grid  8         '
      EDAT_DESCR(11,11) = 'Grid  9         '
      EDAT_DESCR(12,11) = 'Grid 10         '
      EDAT_DESCR(13,11) = 'Grid 11         '
      EDAT_DESCR(14,11) = 'Grid 12         '
      EDAT_DESCR(15,11) = 'Grid 13         '
      EDAT_DESCR(16,11) = 'Grid 14         '
      EDAT_DESCR(17,11) = 'Grid 15         '

      EDAT_DESCR( 0,12) = 'PLOTEL          '
      EDAT_DESCR( 1,12) = 'Elem ID         '
      EDAT_DESCR( 2,12) = 'Elem ID         '
      EDAT_DESCR( 3,12) = 'Grid  1         '
      EDAT_DESCR( 4,12) = 'Grid  2         '

      EDAT_DESCR( 0,13) = 'QUAD4           '
      EDAT_DESCR( 1,13) = 'Elem ID         '
      EDAT_DESCR( 2,13) = 'Prop ID         '
      EDAT_DESCR( 3,13) = 'Grid A          '
      EDAT_DESCR( 4,13) = 'Grid B          '
      EDAT_DESCR( 5,13) = 'Grid C          '
      EDAT_DESCR( 6,13) = 'Grid D          '
      EDAT_DESCR( 7,13) = 'Mtrl angle key  '
      EDAT_DESCR( 8,13) = 'Basic CID or not'
      EDAT_DESCR( 9,13) = 'Plate offset key'
      EDAT_DESCR(10,13) = 'PSHELL or PCOMP '
      EDAT_DESCR(11,13) = 'Plate thick key '

      EDAT_DESCR( 0,14) = 'QUAD4K          '
      EDAT_DESCR( 1,14) = 'Elem ID         '
      EDAT_DESCR( 2,14) = 'Prop ID         '
      EDAT_DESCR( 3,14) = 'Grid A          '
      EDAT_DESCR( 4,14) = 'Grid B          '
      EDAT_DESCR( 5,14) = 'Grid C          '
      EDAT_DESCR( 6,14) = 'Grid D          '
      EDAT_DESCR( 7,14) = 'Mtrl angle key  '
      EDAT_DESCR( 8,14) = 'Basic CID or not'
      EDAT_DESCR( 9,14) = 'Plate offset key'
      EDAT_DESCR(10,14) = 'PSHELL or PCOMP '
      EDAT_DESCR(11,14) = 'Plate thick key '

      EDAT_DESCR( 0,15) = 'ROD             '
      EDAT_DESCR( 1,15) = 'Elem ID         '
      EDAT_DESCR( 2,15) = 'Prop ID         '
      EDAT_DESCR( 3,15) = 'Grid A          '
      EDAT_DESCR( 4,15) = 'Grid B          '

      EDAT_DESCR( 0,16) = 'SHEAR           '
      EDAT_DESCR( 1,16) = 'Elem ID         '
      EDAT_DESCR( 2,16) = 'Prop ID         '
      EDAT_DESCR( 3,16) = 'Grid A          '
      EDAT_DESCR( 4,16) = 'Grid B          '
      EDAT_DESCR( 5,16) = 'Grid C          '
      EDAT_DESCR( 6,16) = 'Grid D          '

      EDAT_DESCR( 0,17) = 'TETRA4          '
      EDAT_DESCR( 1,17) = 'Elem ID         '
      EDAT_DESCR( 2,17) = 'Prop ID         '
      EDAT_DESCR( 3,17) = 'Grid 1          '
      EDAT_DESCR( 4,17) = 'Grid 2          '
      EDAT_DESCR( 5,17) = 'Grid 3          '
      EDAT_DESCR( 6,17) = 'Grid 4          '

      EDAT_DESCR( 0,18) = 'TETRA10         '
      EDAT_DESCR( 1,18) = 'Elem ID         '
      EDAT_DESCR( 2,18) = 'Prop ID         '
      EDAT_DESCR( 3,18) = 'Grid  1         '
      EDAT_DESCR( 4,18) = 'Grid  2         '
      EDAT_DESCR( 5,18) = 'Grid  3         '
      EDAT_DESCR( 6,18) = 'Grid  4         '
      EDAT_DESCR( 7,18) = 'Grid  5         '
      EDAT_DESCR( 8,18) = 'Grid  6         '
      EDAT_DESCR( 9,18) = 'Grid  7         '
      EDAT_DESCR(10,18) = 'Grid  8         '
      EDAT_DESCR(11,18) = 'Grid  9         '
      EDAT_DESCR(12,18) = 'Grid 10         '

      EDAT_DESCR( 0,19) = 'TRIA3           '
      EDAT_DESCR( 1,19) = 'Elem ID         '
      EDAT_DESCR( 2,19) = 'Prop ID         '
      EDAT_DESCR( 3,19) = 'Grid A          '
      EDAT_DESCR( 4,19) = 'Grid B          '
      EDAT_DESCR( 5,19) = 'Grid C          '
      EDAT_DESCR( 6,19) = 'Mtrl angle key  '
      EDAT_DESCR( 7,19) = 'Basic CID or not'
      EDAT_DESCR( 8,19) = 'Plate offset key'
      EDAT_DESCR( 9,19) = 'PSHELL or PCOMP '
      EDAT_DESCR(10,19) = 'Plate thick key '

      EDAT_DESCR( 0,20) = 'TRIA3K          '
      EDAT_DESCR( 1,20) = 'Elem ID         '
      EDAT_DESCR( 2,20) = 'Prop ID         '
      EDAT_DESCR( 3,20) = 'Grid A          '
      EDAT_DESCR( 4,20) = 'Grid B          '
      EDAT_DESCR( 5,20) = 'Grid C          '
      EDAT_DESCR( 6,20) = 'Mtrl angle key  '
      EDAT_DESCR( 7,20) = 'Basic CID or not'
      EDAT_DESCR( 8,20) = 'Plate offset key'
      EDAT_DESCR( 9,20) = 'PSHELL or PCOMP '
      EDAT_DESCR(10,20) = 'Plate thick key '

      EDAT_DESCR( 0,21) = 'USER1           '
      EDAT_DESCR( 1,21) = 'Elem ID         '
      EDAT_DESCR( 2,21) = 'Prop ID         '
      EDAT_DESCR( 3,21) = 'Grid A          '
      EDAT_DESCR( 4,21) = 'Grid B          '
      EDAT_DESCR( 5,21) = 'Grid C          '
      EDAT_DESCR( 6,21) = 'Grid D          '
      EDAT_DESCR( 7,21) = 'Grid V          '
      EDAT_DESCR( 8,21) = 'Pin Flag A      '
      EDAT_DESCR( 9,21) = 'Pin Flag B      '
      EDAT_DESCR(10,21) = 'Pin Flag C      '
      EDAT_DESCR(11,21) = 'Pin Flag D      '

      EDAT_DESCR( 0,22) = 'USERIN          '
      EDAT_DESCR( 1,22) = 'Elem ID         '
      EDAT_DESCR( 2,22) = 'Prop ID         '
      EDAT_DESCR( 3,22) = 'NG              '
      EDAT_DESCR( 4,22) = 'NS              '
      EDAT_DESCR( 5,22) = 'CID0            '

! ----------------------------------------------------------------------------------------------------------------------------------
! Write EDAT table

      WRITE(F06,2000)
      WRITE(F06,2001)
      WRITE(F06,2000)
      WRITE(F06,*)
      WRITE(F06,2002)

      I1 = 1
do_1: DO K=1,NELE

         IF      (ETYPE(K) == 'BAR     ') THEN   ;   MEDAT = MEDAT_CBAR
         ELSE IF (ETYPE(K) == 'BEAM    ') THEN   ;   MEDAT = MEDAT_CBEAM
         ELSE IF (ETYPE(K) == 'BUSH    ') THEN   ;   MEDAT = MEDAT_CBUSH
         ELSE IF (ETYPE(K) == 'ELAS1   ') THEN   ;   MEDAT = MEDAT_CELAS1
         ELSE IF (ETYPE(K) == 'ELAS2   ') THEN   ;   MEDAT = MEDAT_CELAS2
         ELSE IF (ETYPE(K) == 'ELAS3   ') THEN   ;   MEDAT = MEDAT_CELAS3
         ELSE IF (ETYPE(K) == 'ELAS4   ') THEN   ;   MEDAT = MEDAT_CELAS4
         ELSE IF (ETYPE(K) == 'HEXA8   ') THEN   ;   MEDAT = MEDAT_CHEXA8
         ELSE IF (ETYPE(K) == 'HEXA20  ') THEN   ;   MEDAT = MEDAT_CHEXA20
         ELSE IF (ETYPE(K) == 'PENTA6  ') THEN   ;   MEDAT = MEDAT_CPENTA6
         ELSE IF (ETYPE(K) == 'PENTA15 ') THEN   ;   MEDAT = MEDAT_CPENTA15
         ELSE IF (ETYPE(K) == 'PLOTEL  ') THEN   ;   MEDAT = MEDAT_PLOTEL
         ELSE IF (ETYPE(K) == 'QUAD4   ') THEN   ;   MEDAT = MEDAT_CQUAD
         ELSE IF (ETYPE(K) == 'QUAD4K  ') THEN   ;   MEDAT = MEDAT_CQUAD
         ELSE IF (ETYPE(K) == 'ROD     ') THEN   ;   MEDAT = MEDAT_CROD
         ELSE IF (ETYPE(K) == 'SHEAR   ') THEN   ;   MEDAT = MEDAT_CSHEAR
         ELSE IF (ETYPE(K) == 'TETRA4  ') THEN   ;   MEDAT = MEDAT_CTETRA4
         ELSE IF (ETYPE(K) == 'TETRA10 ') THEN   ;   MEDAT = MEDAT_CTETRA10
         ELSE IF (ETYPE(K) == 'TRIA3   ') THEN   ;   MEDAT = MEDAT_CTRIA
         ELSE IF (ETYPE(K) == 'TRIA3K  ') THEN   ;   MEDAT = MEDAT_CTRIA
         ELSE IF (ETYPE(K) == 'USER1   ') THEN   ;   MEDAT = MEDAT_CUSER1
         ELSE IF (ETYPE(K) == 'USERIN  ') THEN
            EPNTK = EPNT(K)
            NG    = EDAT(EPNTK+2)
            NS    = EDAT(EPNTK+3)
            MEDAT = MEDAT0_CUSERIN + 2*NG + NS + 1
            DO J=1,NG
               WRITE(ICHAR,'(I9)') J
               EDAT_DESCR(J+5,22) =  'Grid   ' // ICHAR
            ENDDO
            DO J=1,NS
               WRITE(ICHAR,'(I9)') J
               EDAT_DESCR(J+5+NG,22) = 'SPOINT ' // ICHAR
            ENDDO
            DO J=1,NG
               WRITE(ICHAR,'(I9)') J
               EDAT_DESCR(J+5+NG+NS,22) = 'Comps  ' // ICHAR
            ENDDO
            EDAT_DESCR(2*NG+NS+6,22) = '# boundary DOF  '
         ELSE
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,1001) ETYPE(K), SUBR_NAME
            IF (SUPWARN == 'N') THEN
               WRITE(F06,1001) ETYPE(K), SUBR_NAME
            ENDIF
            EXIT do_1
         ENDIF

         DO J=1,METYPE
            IF (ETYPE(K) == EDAT_DESCR(0,J)(1:8)) THEN
               L = 1
               DO I=I1,I1+MEDAT-1
                  IF (I == I1) THEN
                     WRITE (F06,2003) K, EPNT(K), ETYPE(K), I, 'EPNTK', EDAT(I), EDAT_DESCR(L,J)
                  ELSE
                     IF ( L-1 < 10) THEN
                        IF (EDAT(I) == -99) THEN
                           WRITE (F06,2004) I, '"  +', L-1, EDAT(I), TRIM(EDAT_DESCR(L,J)) // EDAT_EXPLAIN_CID
                        ELSE IF ((EDAT(I) < 0) .AND. (TRIM(EDAT_DESCR(L,J)) == 'V-vector key')) THEN
                           WRITE (F06,2004) I, '"  +', L-1, EDAT(I), TRIM(EDAT_DESCR(L,J)) // EDAT_EXPLAIN_VVEC
                        ELSE
                           WRITE (F06,2004) I, '"  +', L-1, EDAT(I), EDAT_DESCR(L,J)
                        ENDIF
                     ELSE
                        WRITE (F06,2010) I, '"  +', L-1, EDAT(I), EDAT_DESCR(L,J)
                     ENDIF
                  ENDIF
                  L = L + 1
               ENDDO
               I1 = I1 + MEDAT
               WRITE(F06,*)
            ENDIF
         ENDDO

      ENDDO do_1

      IF (NCUSERIN > 0) THEN
         WRITE(F06,*)
         WRITE(F06,2100)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1001 FORMAT(' *WARNING    : ELEMENT TYPE ',A,' IS NOT INCLUDED IN SUBR ',A                                                        &
                   ,/,14X, ' CANNOT PRINT EDAT TABLE AS REQUESTED BY DEBUG(108)')

 2000 FORMAT( 6X,'-------------------------------------------------------------------------------------------------')

 2001 FORMAT( 5X,'|  A R R A Y S   E P N T   A N D   E D A T   O F   E L E M E N T   C O N N E C T I O N   D A T A  |')

 2002 FORMAT(24X,'EDAT is an array of element connection data for all elements',/,                                                 &
             14X,'EPNT is an array that gives the starting position in EDAT for internal element K',//,                            &
             17X,'K         EPNT(K)   Elem type        I     Loction in EDAT   EDAT(I)   Description',/,                           &
              9X,'Internal elem ID  (EPNTK)                         relative to EPNTK',/)

 2003 FORMAT(10X,I8,I13,7X,A8,I9,9X,A,I13,7X,A)

 2004 FORMAT(47X,I8,11X,A,I1,I11,7X,A)

 2010 FORMAT(47X,I8,11X,A,I2,I10,7X,A)

 2100 FORMAT(8X,'NOTE: In the above table for USERIN elements :',/,                                                                &
             8X,'      (1) "Comps  i" are the boundary displacement components for "Grid i"',/,                                    &
             8X,'      (2) "SPOINT i" are the scalar points for the modal DOF for the USERIN element',/)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_EDAT

