model{
  #################################
  #########Model likelihood########
  #################################
  for(i in 1:Nobs){
    # likelihood for soil temp
    s_temp[i] ~ dnorm(mu_temp[i], tau_temp[modforestID[i]])
    rep_temp[i] ~ dnorm(mu_temp[i], tau_temp[modforestID[i]])
    mu_temp[i] <- beta_naught[forestID[i]]+beta[1,modforestID[i]]*air_temp[i]+beta[2,modforestID[i]]*SWC[i]
  }
  
  #################################
  #########priors          ########
  #################################	
  #define prior distributions for parameters
  #All parameters are given non-informative dist
  
  for(j in 1:Nmodforest){
    tau_temp[j] <- pow(sig_temp[j],-2)
    sig_temp[j] ~ dunif(0,1000)
    
  }
  
  # reg parm
 
   for(j in 1:Nforest){
     beta_naught[j] ~ dnorm(0,0.0001)
   }
  
  
    for(j in 1:Nmodforest){
     for(k in 1:Nparm){
     beta[k,j] ~ dnorm(0,0.0001)
    }
   }
}