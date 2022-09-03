!==================================================================================================
      SUBROUTINE WRITE_TABLE_HEADER(TABLE_NAME)
      USE PENTIUM_II_KIND, ONLY  :  BYTE, LONG
      USE IOUNT1, ONLY           :  ERR, OP2
      CHARACTER(LEN=8*BYTE), INTENT(IN) :: TABLE_NAME ! The table name
      !INTEGER(LONG) :: ITABLE ! the subtable id
      INTEGER(LONG), DIMENSION(3) :: DATE_
      DATE_ = (/ 3, 24, 2013 /)
      WRITE(ERR,9110) TABLE_NAME

!      table0 = [
!        4, 2, 4,
!        8, table_name.encode('ascii'), 8,
!        #4, 0, 4,
!      ]
      WRITE(OP2) 2
      WRITE(OP2) TABLE_NAME
      !WRITE(OP2) 0
      
      !data_a = [4, -1, 4,]
      !data_c = [4, 7, 4,]
      WRITE(OP2) -1
      WRITE(OP2) 7
!      table1 = [
!        28,
!        102, 0, 0, 0, 512, 0, 0,
!        28,
!      ]
!      WRITE(OP2) 102, 0, 0, 8, 0,   0, 0
      WRITE(OP2) 102, 0, 0, 0, 512, 0, 0
!      data = [
!        4, -2, 4,
!        4, 1, 4,
!        4, 0, 4,
!      ]
      WRITE(OP2) -2
      WRITE(OP2) 1
      WRITE(OP2) 0
!      DYEAR = DATE_(3) - 2000

!      table2 = [
!        4, 7, 4,
!        28,  # 4i -> 13i
!        # todays date 3/6/2014, 0, 1  ( year=year-2000)
!        month, day, dyear, 0, 1,
!        28,
!      ]
      WRITE(OP2) 7
      WRITE(OP2) 0, 1, DATE_(1), DATE_(2), DATE_(3) - 2000, 0, 1
      
      !ITABLE = -3
      !CALL WRITE_ITABLE(ITABLE)
9110 FORMAT(" *DEBUG:       WRITE WRITE_TABLE_HEADER; TABLE_NAME=", A)
      END SUBROUTINE WRITE_TABLE_HEADER

! ##################################################################################################################################
      SUBROUTINE WRITE_ITABLE(ITABLE)
      USE PENTIUM_II_KIND, ONLY  :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY           :  ERR, OP2
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN) :: ITABLE   ! The subtable id
!      INTEGER(LONG), INTENT(IN) :: NTOTAL   ! the width of the block

      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0
!     WRITE(ERR,*) " *INFORMATION: "
!      WRITE(ERR,9114) " *DEBUG:       WRITE ITABLE; ITABLE=", ITABLE
      WRITE(ERR,9114) ITABLE

9114 FORMAT(" *DEBUG:       WRITE ITABLE; ITABLE=", I8)
      END SUBROUTINE WRITE_ITABLE

