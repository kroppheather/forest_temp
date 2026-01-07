model{
  #################################
  #########Model likelihood########
  #################################
  for(i in 1:Nobs){
    # likelihood for soil temp
    s_temp[i] ~ dnorm(mu_temp[i], tau_temp[modforestID[i]])
    rep_temp[i] ~ dnorm(mu_temp[i], tau_temp[modforestID[i]])
    #covariate center soil moisture
    mu_temp[i] <- beta_naught[forestID[i]]+beta[1,modforestID[i]]*air_temp[i]+beta[2,modforestID[i]]*(SWC[i]-0.3)
    
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
  ##############################################
  #########model fit for plots          ########
  ##############################################
  for( i in 1:plotLengthFreeze){
    for(j in 1:5){
    mu_temp_freeze_15[i,j] <- beta_naught[j]+beta[1,j]*plot_tempFreeze[i]+beta[2,j]*(0.15-0.3)
    mu_temp_freeze_30[i,j] <- beta_naught[j]+beta[1,j]*plot_tempFreeze[i]+beta[2,j]*(0.3-0.3)
    mu_temp_freeze_50[i,j] <- beta_naught[j]+beta[1,j]*plot_tempFreeze[i]+beta[2,j]*(0.5-0.3)
    }
  }
  
  
  for( i in 1:plotLengthFreeze){
    for(j in 1:5){
      mu_swc_freeze_n5[i,j] <- beta_naught[j]+beta[1,j]*-5+beta[2,j]*(swc_freeze[i]-0.3)
      mu_swc_freeze_0[i,j] <- beta_naught[j]+beta[1,j]*0+beta[2,j]*(swc_freeze[i]-0.3)
    }
  }
  for( i in 1:plotLengthWarm){
    for(j in 1:5){
    mu_temp_warm_15[i,j] <- beta_naught[j]+beta[1,j+5]*plot_tempWarm[i]+beta[2,j+5]*(0.15-0.3)
    mu_temp_warm_30[i,j] <- beta_naught[j]+beta[1,j+5]*plot_tempWarm[i]+beta[2,j+5]*(0.3-0.3)
    mu_temp_warm_50[i,j] <- beta_naught[j]+beta[1,j+5]*plot_tempWarm[i]+beta[2,j+5]*(0.5-0.3) 
      }
  }
 
  for( i in 1:plotLengthWarm){
    for(j in 1:5){
      mu_swc_warm_5[i,j] <- beta_naught[j]+beta[1,j+5]*5+beta[2,j+5]*(swc_warm[i]-0.3)
      mu_swc_warm_20[i,j] <- beta_naught[j]+beta[1,j+5]*20+beta[2,j+5]*(swc_warm[i]-0.3)
    }
  }
  
}