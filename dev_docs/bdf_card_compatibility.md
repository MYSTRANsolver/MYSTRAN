
# Elements

| Element | Compatibility Issues |
| ------  | ---  |
| CUSERIN | Not a Nastran card |
| CROD    |   |
| CBAR    | OFFT (GGG, BOO, ...) not supported <br> unclear what is meant by "global coordinate system of grid G1" in User Manual |
| CBAR    | Doesn't support I12 |
| CONROD  |   |
| CSHEAR  |   |
| CELAS1  | Support for SPOINTs?  Support for Grounding? |
| CELAS2  | Support for SPOINTs?  Support for Grounding? |
| CELAS3  | Support for Grounding? |
| CELAS4  | Support for Grounding? |
| CTRIA3  | MCID support <br> No differential thickness  |
| CQUAD4  | MCID support <br> No differential thickness  |
| CQUAD4K | Not a Nastran card         |
| CTRIA3K | Not a Nastran card         |
| CTETRA  |   |
| CPENTA  |   |
| CHEXA   |   |
| PLOTEL  |   |
| GENEL   | Not supported |
| CBEAM   | Not supported |
| CTUBE   | Not supported |
| CPYRAM  | Not supported |
| CVISC   | Not supported |
| CDAMP   | Not supported |

| Mass Element | Compatibility Issues |
| ------       | ---  |
| CONM2        | Support for CID=1?  cylindrical? <br> The moments of inertia I11, I22 and I33 (if entered) must be > 0. |
| CMASS1       |   |
| CMASS2       |   |
| CMASS3       |   |
| CMASS4       |   |
| CONM1        | Not supported |

| Rigid Element | Compatibility Issues |
| ------        | ---  |
| RBE2          |   |
| RBE3          |   |
| MPC           |   |
| MPCADD        |   |
| RSPLINE       |   |
| RBE1       | Not supported |
| RBAR       | Not supported |
| RBAR1      | Not supported |

| Constraints | Compatibility Issues |
| ------      | ---  |
| SPC         |   |
| SPC1        |   |
| SPCADD      |   |
| DEFORM      | Not supported |


| Craig-Bampton | Compatibility Issues |
| ------        | ---  |
| ASET       |   |
| ASET1      |   |
| OMIT       |   |
| OMIT1      |   |
| SUPORT     |   |
| USET       |   |
| USET1      |   |
| BSET       | Not supported |
| BSET1      | Not supported |
| CSET       | Not supported |
| CSET1      | Not supported |
| QSET       | Not supported |
| QSET1      | Not supported |
| SUPORT1    | Not supported |
| DMIG       | Not supported |

# Propertis

| Property | Compatibility Issues |
| ------   | ---  |
| PUSERIN  | Not a Nastran card |
| PELAS    |   |
| PROD     |   |
| PBAR     | Torisional coefficient (CT) is not a Nastran field |
| PBARL    | DBOX (MSC), HAT1 (MSC), TUBE2 (MSC) not supported |
| PBUSH    | B and mass options not supported  |
| PSHEAR   |                    |
| PSHELL   | No MID4            |
| PCOMP    | No SOUT support    |
| PCOMP1   | Not a Nastran card |
| PSOLID   |   |
| PMASS    |   |
| PTUBE    | Not supported  |
| PBEAM    | Not supported |
| PBEAML   | Not supported |
| PCOMPG   | Not supported |
| PBUSH1D  | Not supported |
| PBUSH2D  | Not supported |
| PVISC    | Not supported |
| PDAMP    | Not supported |

# Materials

| Material | Compatibility Issues |
| ------   | ---  |
| MAT1     |  |
| MAT2     |  |
| MAT8     |  |
| MAT9     |  |

# Loads

| Loads    | Compatibility Issues |
| ------   | ---  |
| LOAD     |   |
| FORCE    |   |
| MOMENT   |   |
| SLOAD    |   |
| PLOAD2   |   |
| PLOAD4   | No normal support <br> No CID support <br> No line load support (MSC)  |
| GRAV     | Requires MGG |
| RFORCE   |   |
| TEMP     |   |
| TEMPP1   |   |
| TEMPRB   |   |
| FORCE1   | Not supported |
| FORCE2   | Not supported |
| MOMENT1  | Not supported |
| MOMENT2  | Not supported |
| PLOAD    | Not supported |
| PLOAD1   | Not supported |
| ACCEL    | Not supported; Requires MGG |
| ACCEL1   | Not supported; Requires MGG |

# Other

| Other   | Compatibility Issues |
| ------  | ---  |
| BAROR   | OFFT (GGG, BOO, ...) not supported  |

