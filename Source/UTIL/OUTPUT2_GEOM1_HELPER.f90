!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM1_GRID(ITABLE, NGRID_ACTUAL, GRID_INDEX)
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  ERR, OP2
      USE SCONTR, ONLY                :  NGRID
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID, RGRID
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)                  :: ITABLE
      INTEGER(LONG),INTENT(IN)                     :: NGRID_ACTUAL
      INTEGER(LONG), INTENT(IN), DIMENSION(NGRID)  :: GRID_INDEX      ! oversized, but mehhh
      INTEGER(LONG)                                :: I, J
      INTEGER(LONG)                                :: NVALUES
      INTEGER(LONG),PARAMETER                      :: NUM_WIDE = 8
      INTEGER(LONG),PARAMETER                      :: SEID = 0

      WRITE(ERR,1) "WRITE_OP2_GEOM1_GRID"
      WRITE(ERR,2) "NGRID_ACTUAL=",NGRID_ACTUAL
      IF(NGRID_ACTUAL > 0) THEN
 1    FORMAT("****DEBUG:   ", A)
 2    FORMAT("****DEBUG:   ", A, i4)
 3    FORMAT("****DEBUG:   GRID_ID=",i4, "; ID=",i4, "; cp=", i4, "; x=", f8.3, "; y=", f8.3, "; z=", f8.3, &
             "; cd=", i4, "; PS=", i4,"; NDOF=",i4)

        ! if there are GRIDs
        NVALUES = NUM_WIDE * NGRID_ACTUAL
        
        DO J=1,NGRID_ACTUAL
          ! nid, cp, x, y, z, cd, ps, seid
          I = GRID_INDEX(J)
          WRITE(ERR,3) GRID_ID(I), GRID(I,1), GRID(I,2), REAL(RGRID(I,1),4), REAL(RGRID(I,2),4), REAL(RGRID(I,3),4), &
                       GRID(I,3), GRID(I,4), GRID(I,6)
        ENDDO
        ! oh dear...
        !
        ! the core of what we want is...
        !WRITE(OP2) (GRID_ID(I), GRID(I,2),                 &
        !            RGRID(I,1), RGRID(I,2), RGRID(I,3),    &
        !            GRID(I,3), GRID(I,4), SEID, J=1,NGRID_ACTUAL)
        !
        ! We then sub in I = GRID_INDEX(J), which is required because
        ! mystran stores the GRIDs and SPOINTs in a single array.
        !
        ! Then we wrap any doubles in REAL(*,4) to cast it as a float.
        !
        ! Finally we tack on the magic code (4501,45,1).
        ! Thanks to that code, we have to add 3 the nvalues...
        !
        WRITE(OP2) NVALUES + 3
        !           nid, cp, x, y, z, cd, ps, seid
        WRITE(OP2) 4501, 45, 1,                                                                                     &
                    (GRID_ID(GRID_INDEX(J)),         GRID(GRID_INDEX(J),2),                                         &
                    REAL(RGRID(GRID_INDEX(J),1),4), REAL(RGRID(GRID_INDEX(J),2),4), REAL(RGRID(GRID_INDEX(J),3),4), &
                    GRID(GRID_INDEX(J),3), GRID(GRID_INDEX(J),4), SEID, J=1,NGRID_ACTUAL)
        
        CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM1_GRID

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM1_CORD(ITABLE)
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE SCONTR, ONLY                :  NCORD
      USE MODEL_STUF, ONLY            : CORD
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)      :: ITABLE
      INTEGER(LONG)                    :: I ! loop index
      INTEGER(LONG)                    :: NCORD1R = 0, NCORD1C = 0, NCORD1S = 0
      INTEGER(LONG)                    :: NCORD2R = 0, NCORD2C = 0, NCORD2S = 0
      INTEGER(LONG), DIMENSION(NCORD)  :: CORD1R_INDEX, CORD1C_INDEX, CORD1S_INDEX, CORD2R_INDEX, CORD2C_INDEX, CORD2S_INDEX      ! oversized, but mehhh

      ! Each row of CORD, for a CORD1R,C,S has:
      !    Col. 1: Coord type
      !            11 for CORD1R
      !            12 for CORD1C
      !            13 for CORD1S
      !    Col. 2: CID = Coord system ID
      !    Col. 3: RID = Reference coord system ID for Grid A
      !    Col. 4: RID = Reference coord system ID for Grid B
      !    Col. 5: RID = Reference coord system ID for Grid C
      !
      ! Each row of CORD, for a CORD2R,C,S has the same as for CORD1 above but only needs 1 reference system:
      !    Col. 1: Coord type
      !            21 for CORD2R
      !            22 for CORD2C
      !            23 for CORD2S
      !    Col. 2: CID = Coord system ID
      !    Col. 3: RID = Reference coord system ID for this CORD2R,C,S
      !
      ! Each row of RCORD has:
      !   After Bulk Data has been read:
      !     Col.  1: X coord of Pt A in RID coord system 
      !     Col.  2: Y coord of Pt A in RID coord system  
      !     Col.  3: Z coord of Pt A in RID coord system  
      !     Col.  4: X coord of Pt B in RID coord system  
      !     Col.  5: Y coord of Pt B in RID coord system  
      !     Col.  6: Z coord of Pt B in RID coord system  
      !     Col.  7: X coord of Pt C in RID coord system  
      !     Col.  8: Y coord of Pt C in RID coord system  
      !     Col.  9: Z coord of Pt C in RID coord system  
      !     Col. 10: 0.
      !     Col. 11: 0.
      !     Col. 12: 0.
      !   
      !   After subr CORD_PROC has run:
      !     Col.  1: Location of CID origin in basic coord sys X dir
      !     Col.  2: Location of CID origin in basic coord sys Y dir 
      !     Col.  3: Location of CID origin in basic coord sys Z dir 
      !     Col.  4: TN(1,1,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
      !     Col.  5: TN(1,2,M) for M = row no. in RCORD (internal coord. sys. ID = M)  |- 1st row of TN in cols  4- 6 of RCORD
      !     Col.  6: TN(1,3,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
      !     Col.  7: TN(2,1,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
      !     Col.  8: TN(2,2,M) for M = row no. in RCORD (internal coord. sys. ID = M)  |- 2nd row of TN in cols  7- 9 of RCORD
      !     Col.  9: TN(2,3,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
      !     Col. 10: TN(3,1,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
      !     Col. 11: TN(3,2,M) for M = row no. in RCORD (internal coord. sys. ID = M)  |- 3rd row of TN in cols 10-12 of RCORD
      !     Col. 12: TN(3,3,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]


      ! Each row of CORD, for a CORD1R,C,S has:
      !    Col. 1: Coord type
      !            11 for CORD1R
      !            12 for CORD1C
      !            13 for CORD1S
      !    Col. 2: CID = Coord system ID
      !    Col. 3: RID = Reference coord system ID for Grid A
      !    Col. 4: RID = Reference coord system ID for Grid B
      !    Col. 5: RID = Reference coord system ID for Grid C
      !
      ! Each row of CORD, for a CORD2R,C,S has the same as for CORD1 above but only needs 1 reference system:
      !    Col. 1: Coord type
      !            21 for CORD2R
      !            22 for CORD2C
      !            23 for CORD2S
      !    Col. 2: CID = Coord system ID
      !    Col. 3: RID = Reference coord system ID for this CORD2R,C,S
      !        ! identify the GRIDs vs. the SPOINTs
      
      DO I=1,NCORD
        IF (CORD(I,1) == 11) THEN
          NCORD1R = NCORD1R + 1
          CORD1R_INDEX(NCORD1R) = I
        ELSE IF (CORD(I,1) == 12) THEN
          NCORD1C = NCORD1C + 1
          CORD1C_INDEX(NCORD1C) = I
        ELSE IF (CORD(I,1) == 13) THEN
          NCORD1S = NCORD1S + 1
          CORD1S_INDEX(NCORD1S) = I

        ELSE IF (CORD(I,1) == 21) THEN
          NCORD2R = NCORD2R + 1
          CORD2R_INDEX(NCORD2R) = I
        ELSE IF (CORD(I,1) == 22) THEN
          NCORD2C = NCORD2C + 1
          CORD2C_INDEX(NCORD2C) = I 
        ELSE IF (CORD(I,1) == 23) THEN
          NCORD2S = NCORD2S + 1
          CORD2S_INDEX(NCORD2S) = I
        ENDIF
      ENDDO


      ! CORD1C : (1701, 17, 6)
      ! CORD1R : (1801, 18, 5)
      ! CORD1S : (1901, 19, 7)
      !IF(NCORD1R > 0) THEN
      !    CALL WRITE_OP2_CORD1(ITABLE, NCORD1R, CORD1R_INDEX, 1, 1801, 18, 5)
      !ENDIF
      !IF(NCORD1C > 0) THEN
      !    CALL WRITE_OP2_CORD1(ITABLE, NCORD1C, CORD1C_INDEX, 2, 1701, 17, 6)
      !ENDIF
      !IF(NCORD1S > 0) THEN
      !    CALL WRITE_OP2_CORD1(ITABLE, NCORD1S, CORD1S_INDEX, 3, 1901, 19, 7)
      !ENDIF

      ! CORD2C : (2001, 20, 9)
      ! CORD2R : (2101, 21, 8)
      ! CORD2S : (2201, 22, 10)
      IF(NCORD2R > 0) THEN
          CALL WRITE_OP2_CORD2(ITABLE, NCORD2R, CORD2R_INDEX, 1, 2101, 21, 8)
      ENDIF
      IF(NCORD2C > 0) THEN
          CALL WRITE_OP2_CORD2(ITABLE, NCORD2C, CORD2C_INDEX, 2, 2001, 20, 9)
      ENDIF
      IF(NCORD2S > 0) THEN
          CALL WRITE_OP2_CORD2(ITABLE, NCORD2S, CORD2S_INDEX, 3, 2201, 22, 10)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM1_CORD

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_CORD1(ITABLE, NCORD1, CORD1_INDEX, RCS_INT, CODEA, CODEB, CODEC)
      !! TODO: this is probably very wrong...
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  OP2
      USE SCONTR, ONLY                :  NCORD
      USE MODEL_STUF, ONLY            :  CORD, RCORD
      IMPLICIT NONE
      INTEGER(LONG), INTENT(INOUT)                 :: ITABLE                  ! the subtable counter
      INTEGER(LONG), INTENT(IN)                    :: NCORD1                  ! the number of CORD1x cards
      INTEGER(LONG), DIMENSION(NCORD), INTENT(IN)  :: CORD1_INDEX             ! oversized, but mehhh
      INTEGER(LONG), INTENT(IN)                    :: RCS_INT                 ! R, C, or S
      INTEGER(LONG), INTENT(IN)                    :: CODEA, CODEB, CODEC     ! the CORD2x codes
      INTEGER(LONG)                                :: I,J                     ! counter

      INTEGER(LONG)                                :: NVALUES                 ! helper flag
      INTEGER(LONG), PARAMETER                     :: NUM_WIDE = 6            ! helper flag
      INTEGER(LONG), PARAMETER                     :: COORD_INT = 1           ! CORD1x

      ! if there are NCORD1Rs
      NVALUES = NUM_WIDE * NCORD1
!          Col. 1: Coord type
!                  11 for CORD1R
!                  12 for CORD1C
!                  13 for CORD1S
!          Col. 2: CID = Coord system ID
!          Col. 3: RID = Reference coord system ID for Grid A
!          Col. 4: RID = Reference coord system ID for Grid B
!          Col. 5: RID = Reference coord system ID for Grid C

      ! [cid, coord_rcs_int, coord_int, g1, g2, g3]
      ! TODO: is this g1 or g1.rid ???
      WRITE(OP2) CODEA, CODEB, CODEC,                                                      &
                  (CORD(CORD1_INDEX(J),2), RCS_INT, COORD_INT,                             &
                  CORD(CORD1_INDEX(J),3), CORD(CORD1_INDEX(J),3), CORD(CORD1_INDEX(J),3),  &
                  J=1,NCORD1)
      CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      END SUBROUTINE WRITE_OP2_CORD1


!===================================================================================================================================
      SUBROUTINE WRITE_OP2_CORD2(ITABLE, NCORD2, CORD2_INDEX, RCS_INT, CODEA, CODEB, CODEC)
      ! needs a docstring
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  OP2
      USE SCONTR, ONLY                :  NCORD
      USE MODEL_STUF, ONLY            :  CORD, RCORD
      IMPLICIT NONE
      INTEGER(LONG), INTENT(INOUT)                 :: ITABLE                 ! the subtable counter
      INTEGER(LONG), INTENT(IN)                    :: NCORD2                 ! the number of CORD2x cards
      INTEGER(LONG), DIMENSION(NCORD), INTENT(IN)  :: CORD2_INDEX            ! oversized, but mehhh
      INTEGER(LONG), INTENT(IN)                    :: RCS_INT                ! R, C, or S
      INTEGER(LONG), INTENT(IN)                    :: CODEA, CODEB, CODEC    ! the CORD2x codes
      INTEGER(LONG)                                :: I                      ! counter
      INTEGER(LONG)                                :: NVALUES                ! helper flag
      INTEGER(LONG), PARAMETER                     :: NUM_WIDE = 13          ! helper flag
      INTEGER(LONG), PARAMETER                     :: COORD_INT = 2          ! CORD2x

      ! if there are CORD2Rs
      NVALUES = NUM_WIDE * NCORD2
      WRITE(OP2) NVALUES + 3
      ! [cid, coord_rcs_int, coord_int, rid, e1, e2, e3]
      !
      ! Col. 2: CID = Coord system ID
      ! Col. 3: RID = Reference coord system ID for this CORD2R,C,S
      ! Col.  1: X coord of Pt A in RID coord system 
      ! Col.  2: Y coord of Pt A in RID coord system  
      ! Col.  3: Z coord of Pt A in RID coord system  
      ! Col.  4: X coord of Pt B in RID coord system  
      ! Col.  5: Y coord of Pt B in RID coord system  
      ! Col.  6: Z coord of Pt B in RID coord system  
      ! Col.  7: X coord of Pt C in RID coord system  
      ! Col.  8: Y coord of Pt C in RID coord system  
      ! Col.  9: Z coord of Pt C in RID coord system
      
      !! TODO: we want this to be written before CORD_MAP or whatever is called...
      WRITE(OP2) CODEA, CODEB, CODEC,                                                                                &
                  (CORD(CORD2_INDEX(I),2), RCS_INT, COORD_INT, CORD(CORD2_INDEX(I),3),                               &
                  REAL(RCORD(CORD2_INDEX(I),1),4), REAL(RCORD(CORD2_INDEX(I),2),4), REAL(RCORD(CORD2_INDEX(I),3),4), &
                  REAL(RCORD(CORD2_INDEX(I),4),4), REAL(RCORD(CORD2_INDEX(I),5),4), REAL(RCORD(CORD2_INDEX(I),6),4), &
                  REAL(RCORD(CORD2_INDEX(I),7),4), REAL(RCORD(CORD2_INDEX(I),8),4), REAL(RCORD(CORD2_INDEX(I),9),4), &
                  I=1,NCORD2)
      CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      END SUBROUTINE WRITE_OP2_CORD2
