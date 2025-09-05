# LLoD_Probit
Calculation of Lower limit of detection using Probit regression

---------------------------

Under CLIA guidelines, the lower limit of detection (LLoD) for molecular assays is the lowest concentration of an analyte that a test can reliably distinguish from a negative or blank sample. 

For laboratory-developed tests (LDTs), CLIA requires clinical laboratories to establish and document this analytical sensitivity before the test is used on patient samples. 

Key CLIA guidelines for determining LLoD
To properly determine the LoD and meet CLIA standards, laboratories must establish and verify several related performance specifications. 


CLIA requires that for any LDT, the laboratory must define performance characteristics like analytical sensitivity, specificity, accuracy, precision, and the reportable range. 

The LoD is a key component of analytical sensitivity.



*Calculate the LLoD: 

The LLoD is determined as the lowest concentration that can be reliably distinguished from the LoB. 


Use appropriate reference materials. The validation studies should use reference materials or control materials with known copy numbers or quantities of the target analyte to challenge the assay's detection capabilities. 

Typically a dilution series with several concentration levels (minimum 3 ) should be tested with mupltiple replicates at each level (commonly n=20 per concentation level).

Create the table formatted as below in Excel:
-------------------------
conc	total	detected
200	20	20
100	20	18
50	20	12
25	20	4
-------------------------

# both of the two R files are necessary and should be copied
#     into the same folder or directory
#     (1) preload_for_LLoD_Probit.R  [will be used by other file 2] 
#     (2) LLoD_Probit.R              [ can not run without file 1 ]
#


## step 1 : import function code

source('preload_for_LLoD_Probit.R')


# step 2: copy from Excel and paste data here, between apostrophes 
#         ENSURE CORRECT COLUMN NAMES here, do NOT change

from_excel = "conc	total	detected
200	20	20
100	20	18
50	20	12
25	20	4

" 

## step 3 run the remaining lines to calculate


## The LLoD and confidence intervals are discplayed on screen

## Errors or NaN will appear if the detection data is not linear in relation to the concentation range.




-- end of file --

