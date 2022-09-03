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
 
      SUBROUTINE TDOF_PROC ( TDOF_MSG )
 
! TDOF table generation. TDOF is a table that has the DOF number for every DOF. The table has NDOFG rows and MTDOF columns where:

!   NDOFG = the total number of degrees of freedom in the model (6 times the number of grids plus the number of SPOINT's)
!   MTDOF = the number of different displ sets (currently 14 plus 2 USET user's sets) plus 4 cols for the internal grid/component
!           and external grid/component

! The TDOF table is sorted in grid numerical order. It's sister table, TDOFI, is TDOF sorted in internal G-set order.
! Cols 5 thru MTDOF of these tables give the degree of freedom (DOF) number for each of the 16 different displ sets/user's sets.
! An example of a TDOF table written out to an F06 file is shown below: 
 
!                                              DEGREE OF FREEDOM TABLE SORTED ON GRID NUMBER (TDOF)

! EXTERNAL  INTERNAL                                     DOF NUMBER FOR DISPLACEMENT SET:
! GRD-COMP  GRD-COMP ---------------------------------------------------------------------------------------------------------------
! NUMBER    NUMBER         G      M      N     SA     SB     SG     SZ     SE      S      F      O      A     R      L     U1     U2

!    1022-1       1-1      1      0      1      0      0      0      0      0      0      1      0      1     0      1      0      0
!        -2        -2      2      0      2      0      0      0      0      0      0      2      0      2     0      2      0      1
!        -3        -3      3      0      3      0      0      0      0      0      0      3      0      3     0      3      0      0
!        -4        -4      4      0      4      0      0      0      0      0      0      4      0      4     0      4      0      2
!        -5        -5      5      0      5      0      0      0      0      0      0      5      0      5     0      5      0      0
!        -6        -6      6      0      6      0      0      0      0      0      0      6      0      6     0      6      0      3

!    1043-1       2-1      7      0      7      0      1      0      1      0      1      0      0      0     0      0      1      0
!        -2        -2      8      0      8      0      2      0      2      0      2      0      0      0     0      0      0      0
!        -3        -3      9      0      9      0      0      0      0      0      0      7      0      7     0      7      2      0
!        -4        -4     10      0     10      0      0      0      0      0      0      8      0      8     0      8      0      0
!        -5        -5     11      0     11      0      0      0      0      0      0      9      0      9     0      9      3      0
!        -6        -6     12      0     12      0      0      0      0      0      0     10      0     10     0     10      0      0


!                     ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ----- ------ ------ ------
! TOTAL NUMBER OF DOF:    12      0     12      0      2      6      2      0      2     12      0     10     0     10      3      3


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LDOFG, MTDOF, NDOFA, NDOFF, NDOFG, NDOFL, NDOFM, NDOFN, NDOFO,   &
                                         NDOFR, NDOFS, NDOFSA, NDOFSB, NDOFSE, NDOFSG, NDOFSZ, NGRID, NUM_USET_U1, NUM_USET_U2,    &
                                         SOL_NAME, WARN_ERR
      USE PARAMS, ONLY                :  EIGESTL, PRTDOF
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TSET, TDOF, TDOFI, TDOF_ROW_START, USET
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  EIG_N2, GRID, GRID_ID, GRID_SEQ, INV_GRID_SEQ
 
      USE TDOF_PROC_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TDOF_PROC'
      CHARACTER(LEN=*), INTENT(IN)    :: TDOF_MSG          ! Message to be printed out regarding at what point in the run the TDOF,I
!                                                            tables are printed out
      CHARACTER(  5*BYTE)             :: SET_NAME          ! A data set name for output purposes
 
      INTEGER(LONG)                   ::  A_SET_COL        ! Col no. in array TDOF where the  A-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  F_SET_COL        ! Col no. in array TDOF where the  F-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  G_SET_COL        ! Col no. in array TDOF where the  G-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  L_SET_COL        ! Col no. in array TDOF where the  L-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  M_SET_COL        ! Col no. in array TDOF where the  M-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  N_SET_COL        ! Col no. in array TDOF where the  N-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  O_SET_COL        ! Col no. in array TDOF where the  O-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  R_SET_COL        ! Col no. in array TDOF where the  R-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   ::  S_SET_COL        ! Col no. in array TDOF where the  S-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: SA_SET_COL        ! Col no. in array TDOF where the SA-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: SB_SET_COL        ! Col no. in array TDOF where the SB-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: SE_SET_COL        ! Col no. in array TDOF where the SE-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: SG_SET_COL        ! Col no. in array TDOF where the SG-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: SZ_SET_COL        ! Col no. in array TDOF where the SZ-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: U1_SET_COL        ! Col no. in array TDOF where the U1-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: U2_SET_COL        ! Col no. in array TDOF where the U2-set is (from subr TDOF_COL_NUM)
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I_USET_U1         ! Counter for USET U1
      INTEGER(LONG)                   :: I_USET_U2         ! Counter for USET U2
      INTEGER(LONG)                   :: IGRID             ! Internal grid number
      INTEGER(LONG)                   :: IROW              ! Row number in array TDOF or TDOFI
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      WRITE(SC1, * ) '     TDOF PROC'
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         

! Call routine to calc what row in TDOF each grid's data begins

      CALL CALC_TDOF_ROW_START ( 'Y' )

! First, set NDOFG = LDOFG. It will be counted later.

      NDOFG = LDOFG

! Get column numbers for array TDOF different displ sets

      CALL TDOF_COL_NUM ( 'G ',  G_SET_COL )
      CALL TDOF_COL_NUM ( 'N ',  N_SET_COL )
      CALL TDOF_COL_NUM ( 'M ',  M_SET_COL )
      CALL TDOF_COL_NUM ( 'F ',  F_SET_COL )
      CALL TDOF_COL_NUM ( 'S ',  S_SET_COL )
      CALL TDOF_COL_NUM ( 'SA', SA_SET_COL )
      CALL TDOF_COL_NUM ( 'SB', SB_SET_COL )
      CALL TDOF_COL_NUM ( 'SG', SG_SET_COL )
      CALL TDOF_COL_NUM ( 'SZ', SZ_SET_COL )
      CALL TDOF_COL_NUM ( 'SE', SE_SET_COL )
      CALL TDOF_COL_NUM ( 'O ',  O_SET_COL )
      CALL TDOF_COL_NUM ( 'A ',  A_SET_COL )
      CALL TDOF_COL_NUM ( 'R ',  R_SET_COL )
      CALL TDOF_COL_NUM ( 'L ',  L_SET_COL )
      CALL TDOF_COL_NUM ( 'U1', U1_SET_COL )
      CALL TDOF_COL_NUM ( 'U2', U2_SET_COL )

! Set 1st 4 cols of TDOF (actual grid ID - component number, internal grid ID - component number)

      IROW = 0
      DO I = 1,NGRID
         WRITE(SC1,22345,ADVANCE='NO') '       Process col 1-4 of TDOF', I, NGRID, CR13
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
         DO J = 1,NUM_COMPS
            IROW = IROW + 1
            TDOF(IROW,1) = GRID_ID(I)
            TDOF(IROW,2) = J
            TDOF(IROW,3) = GRID_SEQ(I)
            TDOF(IROW,4) = J
         ENDDO
      ENDDO

! Calc TDOF for G-set (col 5 in TDOF). We can do this at this point since all components go in G-set.
 
      NDOFG = 0
      DO I=1,NGRID
         WRITE(SC1,22345,ADVANCE='NO') '       Process G -set         ', I, NGRID, CR13
         IGRID = INV_GRID_SEQ(I)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            IROW = TDOF_ROW_START(IGRID) + J - 1
            NDOFG = NDOFG + 1
            IF (NDOFG > LDOFG) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, LDOFG, 'TDOF' )
            TDOF(IROW,G_SET_COL) = NDOFG
         ENDDO
      ENDDO

! Make sure NDOFG exactly equals LDOFG (otherwise coding error)

      IF (NDOFG /= LDOFG) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1311) SUBR_NAME, NDOFG, LDOFG
         WRITE(F06,1311) SUBR_NAME, NDOFG, LDOFG
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Put M-set DOF numbers into TDOF (at M_SET_COL)
 
      IF (NDOFM > 0) THEN
         NDOFM = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process M -set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'M ') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFM = NDOFM + 1
                  TDOF(IROW,M_SET_COL) = NDOFM
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Put SA-set DOF numbers into TDOF (at SA_SET_COL)

      IF (NDOFSA > 0) THEN
         NDOFSA = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process SA-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'SA') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFSA = NDOFSA + 1
                  TDOF(IROW,SA_SET_COL) = NDOFSA
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Put SB-set DOF numbers into TDOF (at SB_SET_COL)
 
      IF (NDOFSB > 0) THEN
         NDOFSB = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process SB-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'SB') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFSB = NDOFSB + 1
                  TDOF(IROW,SB_SET_COL) = NDOFSB
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Put SG-set DOF numbers into TDOF (at SG_SET_COL)
 
      IF (NDOFSG > 0) THEN
         NDOFSG = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process SG-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'SG') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFSG = NDOFSG + 1
                  TDOF(IROW,SG_SET_COL) = NDOFSG
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Put SE-set DOF numbers into TDOF (at SE_SET_COL)
 
      IF (NDOFSE > 0) THEN
         NDOFSE = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process SE-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'SE') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFSE = NDOFSE + 1
                  TDOF(IROW,SE_SET_COL) = NDOFSE
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Put O-set DOF numbers into TDOF (at O_SET_COL)
 
      IF (NDOFO > 0) THEN
         NDOFO = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process O -set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'O ') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFO = NDOFO + 1
                  TDOF(IROW,O_SET_COL) = NDOFO
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Put R-set DOF numbers into TDOF (at R_SET_COL)
 
      IF (NDOFR > 0) THEN
         NDOFR = 0
         DO I=1,NGRID
            IGRID = INV_GRID_SEQ(I)
            WRITE(SC1,22345,ADVANCE='NO') '       Process R -set         ', I, NGRID, CR13
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (TSET(IGRID,J) == 'R ') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  NDOFR = NDOFR + 1
                  TDOF(IROW,R_SET_COL) = NDOFR
               ENDIF
            ENDDO
         ENDDO
      ENDIF
 
! Calc TDOF for N-set based on G-set minus M-set = S-set + O-set + R-set + L-set
 
      NDOFN = 0
      DO I=1,NGRID
         WRITE(SC1,22345,ADVANCE='NO') '       Process N -set         ', I, NGRID, CR13
         IGRID = INV_GRID_SEQ(I)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            IROW  = TDOF_ROW_START(IGRID) + J - 1
            IF ((TDOF(IROW,G_SET_COL) > 0) .AND. (TDOF(IROW,M_SET_COL) == 0)) THEN
               NDOFN = NDOFN + 1
               TDOF(IROW,N_SET_COL) = NDOFN
            ELSE
               TDOF(IROW,N_SET_COL) = 0
            ENDIF
         ENDDO
      ENDDO

! Calc DOF'S in SZ-set (all zero SPC's) based on SA + SB + SG
 
      IF ((NDOFSA > 0) .OR. (NDOFSB > 0) .OR. (NDOFSG > 0)) THEN
         NDOFSZ = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process SZ-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IROW  = TDOF_ROW_START(IGRID) + J - 1
               IF ((TDOF(IROW,SA_SET_COL) > 0) .OR. (TDOF(IROW,SB_SET_COL) > 0) .OR. (TDOF(IROW,SG_SET_COL) > 0)) THEN
                  NDOFSZ = NDOFSZ + 1
                  TDOF(IROW,SZ_SET_COL) = NDOFSZ
               ELSE
                  TDOF(IROW,SZ_SET_COL) = 0
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Calc DOF'S in S-set based on SZ + SE
 
      IF ((NDOFSZ > 0) .OR. (NDOFSE > 0)) THEN
         NDOFS = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process S -set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IROW  = TDOF_ROW_START(IGRID) + J - 1
               IF ((TDOF(IROW,SZ_SET_COL) > 0) .OR. (TDOF(IROW,SE_SET_COL) > 0)) THEN
                  NDOFS = NDOFS + 1
                  TDOF(IROW,S_SET_COL) = NDOFS
               ELSE
                  TDOF(IROW,S_SET_COL) = 0
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Calc TDOF for F-set based on N-set minus S-set
 
      NDOFF = 0
      DO I=1,NGRID
         WRITE(SC1,22345,ADVANCE='NO') '       Process F -set         ', I, NGRID, CR13
         IGRID = INV_GRID_SEQ(I)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            IROW  = TDOF_ROW_START(IGRID) + J - 1
            IF ((TDOF(IROW,N_SET_COL) > 0) .AND. (TDOF(IROW,S_SET_COL) == 0)) THEN
               NDOFF = NDOFF + 1
               TDOF(IROW,F_SET_COL) = NDOFF
            ELSE
               TDOF(IROW,F_SET_COL) = 0
            ENDIF
         ENDDO
      ENDDO

! Calc TDOF for A-set based on F-set minus O-set
 
      NDOFA = 0
      DO I=1,NGRID
         WRITE(SC1,22345,ADVANCE='NO') '       Process A -set         ', I, NGRID, CR13
         IGRID = INV_GRID_SEQ(I)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            IROW  = TDOF_ROW_START(IGRID) + J - 1
            IF ((TDOF(IROW,F_SET_COL) > 0) .AND. (TDOF(IROW,O_SET_COL) == 0)) THEN
               NDOFA = NDOFA + 1
               TDOF(IROW,A_SET_COL) = NDOFA
            ELSE
               TDOF(IROW,A_SET_COL) = 0
            ENDIF
         ENDDO
      ENDDO

! Calc TDOF for L-set based on A-set minus R-set
 
      NDOFL = 0
      DO I=1,NGRID
         WRITE(SC1,22345,ADVANCE='NO') '       Process L -set         ', I, NGRID, CR13
         IGRID = INV_GRID_SEQ(I)
         CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
         DO J=1,NUM_COMPS
            IROW  = TDOF_ROW_START(IGRID) + J - 1
            IF ((TDOF(IROW,A_SET_COL) > 0) .AND. (TDOF(IROW,R_SET_COL) == 0)) THEN
               NDOFL = NDOFL + 1
               TDOF(IROW,L_SET_COL) = NDOFL
            ELSE
               TDOF(IROW,L_SET_COL) = 0
            ENDIF
         ENDDO
      ENDDO

! Calc TDOF for USET U1-set and put DOF numbers into TDOF at U1_SET_COL
 
      IF (NUM_USET_U1 > 0) THEN
         I_USET_U1 = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process U1-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (USET(IGRID,J) == 'U1') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  I_USET_U1 = I_USET_U1 + 1
                  TDOF(IROW,U1_SET_COL) = I_USET_U1
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Calc TDOF for USET U2-set and put DOF numbers into TDOF at U2_SET_COL
 
      IF (NUM_USET_U2 > 0) THEN
         I_USET_U2 = 0
         DO I=1,NGRID
            WRITE(SC1,22345,ADVANCE='NO') '       Process U2-set         ', I, NGRID, CR13
            IGRID = INV_GRID_SEQ(I)
            CALL GET_GRID_NUM_COMPS ( GRID_ID(INV_GRID_SEQ(I)), NUM_COMPS, SUBR_NAME )
            DO J=1,NUM_COMPS
               IF (USET(IGRID,J) == 'U2') THEN
                  IROW = TDOF_ROW_START(IGRID) + J - 1
                  I_USET_U2 = I_USET_U2 + 1
                  TDOF(IROW,U2_SET_COL) = I_USET_U2
               ENDIF
            ENDDO
         ENDDO
      ENDIF

! Sort TDOF so that G-set DOF's are in numerical order

      WRITE(SC1,12345,ADVANCE='NO') '       Setting up to get TDOFI', CR13
      DO I=1,NDOFG
         DO J=1,MTDOF
            TDOFI(I,J) = TDOF(I,J)
         ENDDO 
      ENDDO 

      WRITE(SC1,12345,ADVANCE='NO') '       Sort TDOF to get TDOFI ', CR13
      CALL SORT_TDOF ( SUBR_NAME, 'TDOF', NDOFG, TDOFI, G_SET_COL )

! Table TDOF is printed in the F06 file if B.D. PARAM PRTDOF = 1 or 3
 
      IF (PRTDOF > 0) THEN
         CALL WRITE_TDOF ( TDOF_MSG )
      ENDIF

! Make sure that an R-set has been defined if this is CB soln

      IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
         IF (NDOFR < 6) THEN
            SET_NAME = 'R-SET'
            WRITE(ERR,1312) SET_NAME, NDOFR
            WRITE(F06,1312) SET_NAME, NDOFR
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDIF         

! Make sure that if the R set exists it has at least 6 DOF's defined

      IF (NDOFR > 0) THEN
         IF (NDOFR < 6) THEN
            SET_NAME = 'R-SET'
            WRITE(ERR,1304) SET_NAME, NDOFR
            WRITE(F06,1304) SET_NAME, NDOFR
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDIF

! If this is an eigen analysis (or CB) make sure that some spec for the number of modes to be found exists

      IF ((SOL_NAME(1:5) == 'MODES') .OR. (SOL_NAME(1:12) == 'GEN CB MODEL')) THEN
         IF ((NDOFL > EIGESTL) .AND. (EIG_N2 == 0)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1313) NDOFL, EIGESTL
            WRITE(F06,1313) NDOFL, EIGESTL
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDIF

!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
      WRITE(SC1,*) CR13

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1304 FORMAT(' *ERROR  1304: AN ',A,' HAS BEEN DEFINED VIA SUPORT BULK DATA ENTRIES AND THEREFORE MUST HAVE AT LEAST 6 DOF IN IT.' &
                        ,/,14X,' HOWEVER, THERE WERE ONLY ',I2,' DOF DEFINED IN THIS SET')

 1308 FORMAT('  Array TDOF_ROW_START gives the row in array TDOF where the DOF data begins for the GRID listed below',//,          &
             '                  I     GRID   TDOF_ROW_START',/)

 1311 FORMAT(' *ERROR  1311: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NDOFG = ',I12,' CALCULATED IN THIS SUBR MUST AGREE WITH LDOFG = ',I12,'  DETERMINED EARLIER')

 1312 FORMAT(' *ERROR  1312: FOR SOL = ''GEN CB MODEL'' THERE MUST BE AN ',A,'-SET WITH AT LEAST NDOFR = 6 DOF''s.'                &
                    ,/,14X,' HOWEVER ONLY ',I1,' DOF''s WERE DEFINED ON BULK DATA SUPORT ENTRIES')

 1313 format(' *ERROR  1313: FOR SOL = "MODES" OR "GEN CB MODEL" THE EIGRL ENTRY MUST HAVE THE NUMBER OF DESIRED MODES > 0 OR THE',&
                           ' PROBLEM DOF SIZE'                                                                                     &
                    ,/,14X,' (NDOFL = ',I8,') MUST BE LESS THAT PARAM EIGESTL = ',I8,' (OR USE LARGER VALUE FOR PARAM EIGESTL)')

12345 FORMAT(A, A)

22345 FORMAT(A, ', I = ', I8, ' of ', I8, A)

! **********************************************************************************************************************************

      END SUBROUTINE TDOF_PROC
