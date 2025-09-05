#
# preload_for_LLoD_Probit.R
#    posted into github on 2025 SEP 04 after taking form  version of  2023 05 04 
#    
###  Author : Malaya K Sahoo (Stanford University)

#
## ***********************************************************************
## CALCULATE PROBIT VALUE FOR LLOD FROM TABLED DATA --- ###
#   -----  04 MAR 2017---- rearranged code 21 MAR 2017 ---

# there is no-way I could have figured out how to predict back conc from the probit model myself, without
# http://stackoverflow.com/questions/29205349/r-language-probit-model-calculate-standard-error-for-mean-ed50


###-------- above is from that stack overflow post -------------
# figured out : Probit 0.95 estimate and confidence limits #
#   from residuals that I had, some of them match to the SPSS document sheet, but dont actualy need them

## FUNCTIONS ONLY HERE : SOURCE FROM SCRIPT 

#==== instead of using derived data try using the data for true probit 
#      where reponses entered as binary 1 / 0  or T / F (above one is "also works")
expand_probit_data <- function(mdf) {
  md_binary = mdf[0, c("conc", "detected")]
  for (x in 1:nrow(mdf)){
    n = mdf[x,"total"] ; r = mdf[x, "detected"] ; d= mdf[x, "conc"]
    md_binary <- rbind(md_binary, data.frame(conc=rep(d, n), detected = c(rep(1,r),rep(0,n-r))))
  }   # binary data can be T/F or TRUE/FALSE or 1 / 0 # but NOT character or levels e.g. "DET" / "NDET" 
  return (md_binary)
} # this will expand the short table to long binary form (somewhat reverse of : xtabs(~conc+detected, md_binary))

##-------------------------------------------------------------------------------------.
### now predict the probit values , concentrations at the levels, and confidence limits

get_estimate <- function (mod, p_level= c(0.1, 0.5, 0.9, 0.95, 0.99)) {
  ### Calculate heterogeneity correction to confidence intervals according to Finney, 1971, (p.
  ### 72, eq. 4.27; also called "h")
  het = deviance(mod)/df.residual(mod) ; if (het < 1){het = 1} ### Heterogeneity cannot be less than 1  
  # sets dispersion paramerter (het) to 1 by default ! ## change below if need to use het >1 when model returns that
  ## Extract slope and intercept 
  summary <- summary(mod, dispersion= 1, cor = F) # summary(mod, dispersion= het, cor = F)  # summary might change if het is lot > 1
  intercept <- summary$coefficients[1] ; interceptSE <- summary$coefficients[3]
  slope     <- summary$coefficients[2] ;     slopeSE <- summary$coefficients[4]
  z.value <- summary$coefficients[6]
  N <- sum(mdf$total) # or for m3 #  N <- nrow(md_binary)
  
  ## Intercept (alpha)  ## Slope (beta)
  b0 <- intercept   ;   b1 <- slope
  ## Slope variance  # Intercept variance  # Slope intercept covariance
  vcov = summary(mod)$cov.unscaled
  var.b0<-vcov[1,1] ;  var.b1<-vcov[2,2] ; cov.b0.b1<-vcov[1,2]
  
  ## Adjust alpha depending on heterogeneity (Finney, 1971, p. 76)
  alpha= 0.05  # fixed, otherwise # 1-conf.level  # e.g. if conf.level = 0.95
  if (het > 1) {talpha = -qt(alpha/2, df=df.residual(mod))} else {talpha = -qnorm(alpha/2)}
  
  ## Calculate g (Finney, 1971, p 78, eq. 4.36)  
  ## "With almost all good sets of data, g will be substantially
  ##      smaller than 1.0 and  seldom greater than 0.4."
  g <- het * ((talpha^2 * var.b1)/b1^2)
  
  ## Calculate theta.hat for all LD levels based on probits in eta ~~| 
  ## (Robertson et al., 2007, pg.27; or "m" in Finney, 1971, p. 78)  |
  eta = family(mod)$linkfun(p_level)   #  probit distribution curve  |   p_levels = c(0.5, 0.9, 0.95, 0.99)
  eta_conc = (eta - b0)/b1  #  returns the conc or dose at p level   |   b0 = intercept ; b1 = slope
  ##----- this section was critical to my calculation _______________|
  # theta.hat replaced by eta_conc here
  
  ## Calculate correction of fiducial limits according to Fieller method (Finney, 1971, 
  ## p. 78-79. eq. 4.35) 
  const1 <- (g/(1-g))*(eta_conc + cov.b0.b1/var.b1) # const1 <- (g/(1-g))*(eta_conc -   cov.b0.b1/var.b1)
  const2a <- var.b0 + 2*cov.b0.b1*eta_conc + var.b1*eta_conc^2 - g*(var.b0 - (cov.b0.b1^2/var.b1))
  const2 <- talpha/((1-g)*b1) * sqrt(het * (const2a))
  
  ## Calculate the confidence intervals LCL=lower, UCL=upper (Finney, 1971, p. 78-79. eq. 4.35) 
  LCL <- (eta_conc + const1 - const2)
  UCL <- (eta_conc + const1 + const2)
  print (paste("const1 and 2:", const1, const2, "LCL-UCL" , LCL, UCL, sep="   "))
  # correction to LCL and UCL # this will fix the variations when they become negative
  if ( const1 < 0 | const2 < 0 | const1 > const2 ) {
    const1 = abs(const1); const2= abs(const2)  # fix if they are minus
    if (const1 > const2) { x2= const1; const1= const2; const2=x2} # switch if needed
    
    LCL <- (eta_conc + const1 - const2)
    UCL <- (eta_conc + const1 + const2)
    print (paste("corrected   :", const1, const2, "LCL-UCL" , LCL, UCL, sep="   "))  
  }
  ## extra # Calculate variance for theta.hat (Robertson et al., 2007, pg. 27)
  var.eta_conc <- (1/(eta_conc^2)) * ( var.b0 + 2*cov.b0.b1*eta_conc + var.b1*eta_conc^2 )
  #
  xtxt = cbind(p_level,conc_est=eta_conc, LCL=LCL, UCL=UCL, variance=var.eta_conc, sd.eta_conc=sqrt(var.eta_conc))
  return (xtxt)
}
##=========  END OF LLoD calc FUNCTION using probit ======================



# 
## end of script
