############################################################
# MODEL SIR
#
# A toy population-based, SIR-style model.
############################################################

# ---------------------------------------------------------
# Run a single simulation of the SIR model for given parameters
# ---------------------------------------------------------
model_sir = function(p) {
  
  message(" > Running population-based model")
  
  # ---- Initial set up ----
  
  # Preallocate disease state vectors
  s = i = r = rep(NA, p$time)
  
  # Set initial conditions
  s[1] = p$N - p$i0
  i[1] = p$i0
  r[1] = 0
  
  # Convert durations to rates
  gamma = 1 / p$duration_infection
  theta = 1 / p$duration_immunity
  
  # Preallocate metric vectors
  prevalence = incidence = rep(NA, p$time)
  
  # ---- Main model loop ----
  
  # Iterate through daily time steps
  for (t in 1 : p$time) {
    
    # Number of people still alive
    pop = s[t] + i[t] + r[t]
    
    # Prevalence of infection (used in FoI)
    prevalence[t] = i[t] / pop
    
    # Force of infection (FoI)
    lambda = 1 - (1-p$beta)^(p$n*prevalence[t])
    
    # Store incidence
    incidence[t] = lambda * s[t]
    
    # Don't update next states on final timestep
    if (t < p$time) {
      
      # Update number susceptible
      s[t+1] = s[t] - lambda*s[t] + theta*r[t]
      
      # Update number infected
      i[t+1] = i[t] + lambda*s[t] - gamma*i[t]
      
      # Update number recovered
      r[t+1] = r[t] + gamma*i[t] - theta*r[t]
    }
  }
  
  # Construct results datatable
  results_dt = data.table(
    time = 1 : p$time, s, i, r, 
    prevalence, incidence)
  
  # Save results to file
  saveRDS(results_dt, "results_sir.rds")
}

