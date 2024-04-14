# PARAM

Provide values, other than default values, for parameters that control options during execution.

## Format:
|   1   |   2    | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10 |
| ----- | ------ | -- | -- | -- | -- | -- | -- | -- | -- |
| PARAM | NAME   | V1 | V2 | V3 | V4 |    |    |    |    | 
| PARAM | PRTDOF | 2  |    |    |    |    |    |    |    | 

## Data Description:

| Field | Contents       | Type | Default |
| ----- | -------------- | ---- | ------- |
| NAME  | Parameter name | Char | None    |
| Vi    | Values for the parts of the parameter | Char, Integer or real | Various |

## Remarks:
1. See table below for a list of the various parameters and what action is taken based on their
values. Unless otherwise stated, only value V1 is used. The parameter name always goes in
field 2 and V1 always goes in field 3. When there is more than one Vi, the table explicitly states
in what fields the Vi go.

| Parameter Name                                       | Data Type | Function of Parameter NOTE: Default values of parameters are: N for Char, 0 for Int and 0.0 for real | 
| ---------------                                      | --------- | ---------------------------------------------------------------------------------------------------- |
| ARP_TOL                                              | Real | Default = 1x10-6; Tolerance to use in Lanczos eigenvalue extraction method for convergence |
| ART_KED (for diff stiffness – not fully implemented) | Char | Field 3: ART_KED, default = N. If Y add artificial stiff to diag of KED stiff matrix |
|                                                      |      | Field 4: ART_TRAN_MASS: value for translation degrees of freedom, default 1x10-6 |
|                                                      |      | Field 5: ART_ROT_MASS: value for translation degrees of freedom, default 1x10-6 |

| ART_MASS                                             | Char | Field 3: ART_MASS, default = N. If Y add artificial mass to diag of MGG mass matrix |
|                                                      |      | Field 4: ART_TRAN_MASS: value for translation degrees of freedom, default 1x10-6 |
|                                                      |      | Field 5: ART_ROT_MASS: value for rotation degrees of freedom, default 1x10-6 |
| AUTOSPC                                              | Char | Field 3: AUTOSPC value, default = Y (AUTOSPC), N turns AUTOSPC off. |
|                                                      | Real | Field 4: AUTOSPC_RAT, default = 1x10-8(see Section 3.4.1.1) |
|                                                      | Int  | Field 5: AUTOSPC_NSET, default = 1 (see Section 3.4.1.1) |
|                                                      | Char | Field 6: AUTOSPC_INFO, default = N. If Y then print messages about the AUTOSPC’s |
|                                                      | Char | Field 7: AUTOSPC_SPCF, default = N. If Y print AUTOSPC forces of constraint |

BAILOUT Int Default = 1
If > 0 quit if a singularity in decomposing a matrix is detected.
If <= 0 do not quit
CBMIN3 Real Default = 2.0
CBMIN3 is the constant CB used in tuning the shear correction factor in Ref 3 for
the TRIA3 plate element. The default 2.0 is the value suggested by the author.
CBMIN4 Real Default = 3.6
CBMIN4 is the constant CB used in tuning the shear correction factor in Ref 4 for
the QUAD4 plate element (QUAD4TYP = ‘MIN4 ‘). See Ref 4
CBMIN4T Real Default = 3.6
CBMIN4T is the constant CB used in tuning the shear correction factor in Ref 4 for
the QUAD4 plate element (QUAD4TYP = ‘MIN4T’).
CHKGRDS Char Default = Y. If N do not check that all grids for all elements exist
CRS_CCS Char Default = CRS (compressed row storage of matrices). Also can be CCS
CUSERIN Char
Int
Int
If this parameter is present, Bulk Data entries for Craig-Bampton (CB) reduced
models will be written to the F06 file as a CUSERIN element (including grids, coord
sys, etc)
Field 3: element ID, default = 9999999

Int
Int
Char
Int
Field 4: property ID default = 9999999
Field 5: start index for SPOINT’s to represent modes of the CB model, default =
1001
Field 6: IN4 file # on the PUSERIN entry for this CUSERIN elem, default = 9999999
Field 7: Set-ID for CUSERIN elem (typically the “R”, or boundary, set), def is blank
field
Field 8: Format for how to write the comp numbers (1 thru 6) for each grid of the
CUSERIN elem. If 0, write them in compact form (e.g. 1356). If > 0 write them in
expanded form (1 3 56), default = 0
DARPACK Int Default = 2
how many extra modes to find above EIG_N2 on the EIGRL entry. These few
highest mode are not used due to difficulty with getting good GP force balance.
DELBAN Int Default 1. If equal to 1 delete the bandit output files on exit
EIGESTL Int Default 5000
For eigenvalue problems by the Lanczos method, if the number of L-set DOF’s
exceed EIGESTL the method for specifying the search range will be changed from
F1 to F2 to N (see EIGRL Bulk Data entry) to avoid excessive run times (since the
code to estimate the number of eigens in the F1 to F2 range can be excessive).
EIGNORM2 Char Default = N. if 'Y' then eigenvectors will be renormalized a last time by multiplying
by a set of scale factors (1 per eigenvector) supplied in a file with the same name as
the input file and extension 'EIN' (if it exists)

