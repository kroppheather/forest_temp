model{
  #################################
  #########Model likelihood########
  #################################
  
  
  for(i in 1:Nobs){
    # likelihood for soil temp
    s_temp[i] ~ dnorm(mu_s[i], tau_s[forestID[i],swID[i],snowID[i]])
    rep_s[i] ~ dnorm(mu_s[i], tau_s[forestID[i],swID[i],snowID[i]])
    #covariate center soil moisture
    mu_s[i] <- beta_naught[forestID[i],swID[i],snowID[i]]+beta[forestID[i],swID[i],regID[i]]*air_temp[i]
    
  }
  
  #################################
  #########priors          ########
  #################################	
  #define prior distributions for parameters
  #All parameters are given non-informative dist
  
  
  # reg parm
 
   for(j in 1:Nforest){
     for(k in 1:NSW){
       for(m in 1:Nsnow){
      beta_naught[j,k,m] ~ dnorm(0,0.0001)
       tau_s[j,k,m] <- pow(sig_s[j,k,m],-2)
       sig_s[j,k,m] ~ dunif(0,1000)
     }
     }
   }
  
  
     for(j in 1:Nforest){
       for(k in 1:NSW){
         for(m in 1:3){
         beta[j,k,m] ~ dnorm(0,0.0001)
         }
     }
    }
  ##############################################
  #########model fit for plots          ########
  ##############################################
  
  for( i in 1:plotLengthSnow){
    for(j in 1:Nforest){
      for(k in 1:NSW){
        mu_temp_snow[i,j,k] <- beta_naught[j,k,2]+beta[j,k,3]*plot_tempSnow[i]
        
        
      }
    }
  }
  
  
  
  for( i in 1:plotLengthFreeze){
    for(j in 1:5){
        for(k in 1:2){
     mu_temp_freeze[i,j,k] <- beta_naught[j,k,1]+beta[j,k,1]*plot_tempFreeze[i]
    

        }
    }
  }
  
  

  
  for( i in 1:plotLengthWarm){
    for(j in 1:5){
      for(k in 1:2){
      mu_temp_warm[i,j,k] <- beta_naught[j,k,1]+beta[j,k,2]*plot_tempWarm[i]
      
      }
    }
  }
 

}