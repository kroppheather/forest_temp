model{
  #################################
  #########Model likelihood########
  #################################
  for(i in 1:Nobs){
    # likelihood for soil temp
    s_temp[i] ~ dnorm(mu_temp[i], tau_temp[modforestID[i]])
    rep_temp[i] ~ dnorm(mu_temp[i], tau_temp[modforestID[i]])
    #covariate center soil moisture
    mu_temp[i] <- beta_naught[forestID[i],swID[i]]+beta[modforestID[i],swID[i]]*air_temp[i]
    
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
     for(k in 1:NSW){
      beta_naught[j,k] ~ dnorm(0,0.0001)
     }
   }
  
  
    for(j in 1:Nmodforest){
     for(k in 1:NSW){
       beta[j,k] ~ dnorm(0,0.0001)
     }
    }
  ##############################################
  #########model fit for plots          ########
  ##############################################
  for( i in 1:plotLengthFreeze){
    for(j in 1:5){
        for(k in 1:2){
     mu_temp_freeze[i,j,k] <- beta_naught[j,k]+beta[j,k]*plot_tempFreeze[i]
    

        }
    }
  }
  
  

  
  for( i in 1:plotLengthWarm){
    for(j in 1:5){
      for(k in 1:2){
      mu_temp_warm[i,j,k] <- beta_naught[j,k]+beta[j+5,k]*plot_tempWarm[i]
      
      }
    }
  }
 
 
  
}