#
###    LLoD_Probit     ###
#     Calculate 95% Lower Limit of Detection using probit regression model
#     use the data entry example below to enter your data  
#     (for ease: create the table in excel and then 
#                copy paste here to replace the example)
# 
## running from Rstudio
#
## step 1 keep cursor on line below saying: "source ('preload_....."
#         then run by CTRL+ENTER  or click on [-->Run] button 
#         on the top right of this sub-window
source('preload_for_LLoD_Probit.R')


# step 2: copy from Excel and paste data here, between apostrophes 
#         ENSURE CORRECT COLUMN NAMES here, do NOT change

from_excel = "conc	total	detected
200	20	20
100	20	18
50	20	12
25	20	4

" 

## step 3 run below lines by CTRL+ENTER  or click on [-->Run] button

mdf = as.data.frame(read.table(text = from_excel, sep='\t', header=TRUE))

m2 <- glm(detected ~ conc, family=binomial(link="probit"), data = expand_probit_data(mdf)); gep = get_estimate(m2, 0.95)

gep_clipboard = sprintf(" LLoD: %.3f (95%% confidence interval %.3f - %.3f)", gep[1,"conc_est"], gep[1,"LCL"], gep[1,"UCL"]) ; 

write(x = gep_clipboard, file = "clipboard") ; sprintf ('copied: %s [ready to paste]', gep_clipboard)

# --- end of script ---


