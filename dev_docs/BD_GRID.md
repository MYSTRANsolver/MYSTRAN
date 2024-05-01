# GRID

Grid point definition

## Format:

| 1  | 2  | 3  | 4  | 5   | 6   | 7   | 8    | 9 | 10 |
| -- | -- | -- | -- | --  | --  | --  | --   | --   | -- |
| GRID | GID | CID1 | X1  | X2  | X3  | CID2 | PSPC | 
| GRID | 58  | 12   | 10. | 20. | 30. | 42   | 245  | 


## Data Description:


| Field | Contents | Type | Default |
| ----- | ---------| ---- | ------- |
| GID  | Grid point ID number | Integer > 0 | None | 
| CID1 | ID of the coordinate system that the Xi are defined in | Integer >= 0 | 0 | 
| Xi   | Coordinates of the grid defined in coordinate system CID1 | Real | 0. | 
| CID2 | ID of the global coordinate system for this grid point | Integer >= 0 | 0 |
| PSPC | Permanent single point constraints at this grid point | Integers 1-6 |  Blank |

# Remarks:

1. Grid IDs must be unique among all GRID entries.
2. The word “permanent” in regards to the single point constraints (SPC’s) defined on the GRID entry is
merely a designation given to SPC’s defined on GRID entries. The PSPC field does not have to be
used. Any, or all, of the zero value (i.e., not enforced displacement) single point constraints used in a
model can be specified on Bulk Data SPC or SPC1 entries or as PSPC’s on the GRID entry.
3. A blank entry for CIDi implies the basic coordinate system.
