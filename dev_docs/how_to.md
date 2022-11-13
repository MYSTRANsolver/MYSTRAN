How to
======
1.  Add a PARAM    - TODO
2.  Add an element - TODO
3.  Modify the Case Control
4.  Open/Close an ASCII File

Modifying the Case Control Deck
===============================
In this example, we'll discuss adding:

   - DISP(CSV) = ALL    ! for CSV output
   - DISP(PLOT) = ALL   ! for OP2 output

This is broken into 42 steps:
 1. Add Case Control parameters to the case control request storage variable (DISP_OUT).
 2. Allow the new parameters (ALLOW_CC_CMD_DESCR).
 3. Go use the DISP_OUT flag.


Let's Modify Source!
--------------------

### 1.  Open "Source/Link1/L1A-CC/CC-DISP.f90"

You should see:

```fortran
! Check to see if BOTH, ENGR or NODE were in the ELFO request
FOUND_PRINT = 'N'
FOUND_PUNCH = 'N'
DO I=1,NCCCD
   IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PRINT') FOUND_PRINT = 'Y'
   IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PUNCH') FOUND_PUNCH = 'Y'
ENDDO

DISP_OUT(1:) = ' '
IF ((FOUND_PRINT == 'Y') .AND. (FOUND_PUNCH == 'N')) DISP_OUT(1:5) = 'PRINT'
IF ((FOUND_PRINT == 'N') .AND. (FOUND_PUNCH == 'Y')) DISP_OUT(1:5) = 'PUNCH'  ;  PCHSTAT = 'KEEP    '
IF ((FOUND_PRINT == 'Y') .AND. (FOUND_PUNCH == 'Y')) DISP_OUT(1:4) = 'BOTH'   ;  PCHSTAT = 'KEEP    '
IF ( DISP_OUT(1:) == ' ') DISP_OUT(1:5) = 'PRINT'    ! Neither PRINT or PUNCH found so default to PRINT

```

Modify that to become:
```fortran
! Check to see if PLOT, PRINT, PUNCH, CSV were in the DISP request
FOUND_PRINT = 'N'
FOUND_PLOT  = 'N'
FOUND_PUNCH = 'N'
FOUND_CSV   = 'N'
DO I=1,NCCCD
   IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PRINT') FOUND_PRINT = 'Y'
   IF (CC_CMD_DESCRIBERS(I)(1:4) == 'PLOT')  FOUND_PLOT  = 'Y'
   IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PUNCH') FOUND_PUNCH = 'Y'
   IF (CC_CMD_DESCRIBERS(I)(1:3) == 'CSV')   FOUND_CSV   = 'Y'
ENDDO
! concatenate the strings
DISP_OUT = TRIM(FOUND_PRINT) // TRIM(FOUND_PLOT) // TRIM(FOUND_PUNCH) // TRIM(FOUND_CSV)

! set a default of "PRINT"
IF (DISP_OUT(1:4) == 'NNNN') THEN
  DISP_OUT = 'YNNN'
ENDIF

```
Unfortunately we had to break DISP_OUT (it used to be "PRINT" or "PUNCH"
and is now "NYYN" for an equivalent result (PRINT, PLOT, PUNCH, CSV order).

However, this is more consistent with Nastran's bit flag system, which uses:
 - PRINT=1
 - PLOT=2
 - PUNCH=4

and adds them together to get the different combinations.

### 2.  Opening to "Source/LK1/LK1A-CC/CHK_CC_CMD_DESCRIBERS.f90"

You should see:

```fortran
!  =================ACCE=================   =================DISP=================   =================ELFO=================
   ALLOW_CC_CMD_DESCR( 1, 1) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 2) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 3) = 'SORT1   '
   ALLOW_CC_CMD_DESCR( 2, 1) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 2) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 3) = 'SORT2   '
   ALLOW_CC_CMD_DESCR( 3, 1) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 2) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 3) = 'PRINT   '
   ALLOW_CC_CMD_DESCR( 4, 1) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 2) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 3) = 'PUNCH   '
   ALLOW_CC_CMD_DESCR( 5, 1) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 2) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 3) = 'PLOT    '
   ALLOW_CC_CMD_DESCR( 6, 1) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 2) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 3) = 'REAL    '
   ALLOW_CC_CMD_DESCR( 7, 1) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 2) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 3) = 'IMAG    '
   ALLOW_CC_CMD_DESCR( 8, 1) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 2) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 3) = 'MAG     '
   ALLOW_CC_CMD_DESCR( 9, 1) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 2) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 3) = 'PHASE   '
   ...
```
Interestingly, PLOT was already there (there is a note about not all flags being implemented), but we'll add CSV.

```fortran
!  =================ACCE=================   =================DISP=================   =================ELFO=================
   ALLOW_CC_CMD_DESCR( 1, 1) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 2) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 3) = 'SORT1   '
   ALLOW_CC_CMD_DESCR( 2, 1) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 2) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 3) = 'SORT2   '
   ALLOW_CC_CMD_DESCR( 3, 1) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 2) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 3) = 'PRINT   '
   ALLOW_CC_CMD_DESCR( 4, 1) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 4, 2) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 4, 3) = 'PLOT    '
   ALLOW_CC_CMD_DESCR( 5, 1) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 5, 2) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 5, 3) = 'PUNCH   '
   ALLOW_CC_CMD_DESCR( 6, 1) = 'CSV     ' ; ALLOW_CC_CMD_DESCR( 6, 2) = 'CSV     ' ; ALLOW_CC_CMD_DESCR( 6, 3) = 'CSV     '
   ALLOW_CC_CMD_DESCR( 7, 1) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 7, 2) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 7, 3) = 'REAL    '
   ALLOW_CC_CMD_DESCR( 8, 1) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 8, 2) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 8, 3) = 'IMAG    '
   ALLOW_CC_CMD_DESCR( 9, 1) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 9, 2) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 9, 3) = 'MAG     '
   ALLOW_CC_CMD_DESCR(10, 1) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR(10, 2) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR(10, 3) = 'PHASE   '
   ...
```
Now shift all the numbers because there are 30+ flags for the 9 results.
I recommend a good text editor like Textpad/Notepad++ to copy/paste columns.

We also shifted the order to be PRINT, PLOT, PUNCH, CSV to be more intuitive
with the previous flag order.

### 3.  Use DISP_FLAG in "Source/LK9/L92/OFP1.f90"

You should see:

```fortran
IF (ISOP2) THEN  ! should be flagged by PLOT
   CALL WRITE_GRD_OP2_OUTPUTS ( JVEC, NUM, WHAT, ITABLE, NEW_RESULT )
ENDIF

IF ((DISP_OUT(1:5) == 'PUNCH') .OR. (DISP_OUT(1:4) == 'BOTH')) THEN
   CALL WRITE_GRD_PCH_OUTPUTS ( JVEC, NUM, WHAT )
ENDIF

IF ((DISP_OUT(1:5) == 'PRINT') .OR. (DISP_OUT(1:4) == 'BOTH')) THEN
   CALL CHK_OGEL_ZEROS ( NUM )
   CALL WRITE_GRD_PRT_OUTPUTS ( JVEC, NUM, WHAT, IHDR, DISP_ALL_SAME_CID, WRITE_OGEL )
ENDIF

```

Change the flag, so:
```fortran
! (1) PRINT, (2) PLOT, (3) PUNCH, (4) CSV
IF (DISP_OUT(2:2) == 'Y')  CALL WRITE_GRD_OP2_OUTPUTS(JVEC, NUM, WHAT, ITABLE, NEW_RESULT)  ! op2/plot
IF (DISP_OUT(3:3) == 'Y')  CALL WRITE_GRD_PCH_OUTPUTS(JVEC, NUM, WHAT)  ! pch/punch
IF (DISP_OUT(4:4) == 'Y')  CALL WRITE_GRD_CSV_OUTPUTS(JVEC, NUM, WHAT)  ! csv
IF (DISP_OUT(1:1) == 'Y')  THEN  ! f06/print
   CALL CHK_OGEL_ZEROS(NUM)  ! changes -0.0 to 0.0
   CALL WRITE_GRD_PRT_OUTPUTS(JVEC, NUM, WHAT, IHDR, DISP_ALL_SAME_CID, WRITE_OGEL)
ENDIF
```

There is that weird if loop at the end, so we'll leave "CALL CHK_OGEL_ZEROS(NUM)" as a special thing...

### 4.  Write the CSV

TODO...

Open/Close an ASCII file
========================

We missed a step, which hopefully you don't have to do very often.  It's a lot...
edit:
 - "Source/Modules/IOUNIT1.f90"
   - increase MAX_FIL
   - add CSVFIL, CSVSTAT, CSV

 - "Source/UTIL/READ_L1A.f90"
   - lots of "USE IOUNT1, ONLY" imports
   - bump all the read values
     "READ(L1A,151,IOSTAT=IOCHKI(  8)) F06,F06STAT,F06_MSG,F06FIL"...so the 8
   - fix the "( 73 > MAX_FIL) THEN"...bump it to 73

 - "Source/MAIN/MYSTRAN_FILES.f90"
   - lots of "USE IOUNT1, ONLY" imports
   - add a block to open/close the file

 - "Source/UTIL/CLOSE_OUTFILES.f90"

 - "Source/UTIL/FILE_INQUIRE.f90"

 - "Source/Interfaces/FILE_INQUIRE_Interface.f90"

 - "Source/Interfaces/MYSTRAN_FILES_Interface.f90"

 - "Source/LK9/LINK9/LINK9.f90"
