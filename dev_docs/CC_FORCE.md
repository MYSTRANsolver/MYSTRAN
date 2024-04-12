# ELFORCE

## Description:

Requests output of nodal or engineering forces for selected elements.

## Format:

 $$ ELFO[RCE]
 \begin{bmatrix}
   **ENGR** \\
   (NODE) \\
   (BOTH) 
   \end{bmatrix} = \begin{bmatrix}
   ALL \\
   n \\
   NONE
   \end{bmatrix}$$

## Examples:

``ELFORCE = ALL`` (requests output of element engineering forces for all elements)

``ELFO(NODE) = 125`` (requests output of element nodal forces for elements included in SET 125)

## Options:

| Option | Meaning |
| ------ | ------- |
| ALL    |  Element forces for all elements in the model will be output.                       | 
| n      |  ID of a SET Case Control entry previously defined. Element forces for the elements | 
|        |  defined by SET n will be output. Integer > 0, no default value.                    | 
| NONE   |  No element forces will be output.                                                  | 

## Remarks:

1. NONE is used to override an overall output request made above the SUBCASE level
2. The forces can be output in local element, basic, or global coordinates. See Bulk Data PARAM ELFORCEN entry
