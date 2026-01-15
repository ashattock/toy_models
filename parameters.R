############################################################
# PARAMETERS
#
# Helper function related to model parameters and other
# model input.
############################################################

# ---------------------------------------------------------
# Load model input parameters (used for both models)
# ---------------------------------------------------------
load_model_parameters = function() {
  
  # Initiate parameter list
  p = list()
  
  # ---- Meta parameters ----
  
  # Number of daily time steps
  p$time = 720
  
  # Number of people in population
  p$N = 10000
  
  # ---- Initial conditions ----
  
  # Number to start infected
  p$i0 = 200
  
  # ---- Transmission parameters ----
  
  # Prob of inf for given S->I contact
  p$beta = 0.02
  
  # Number of daily contacts
  p$n = 6
  
  # ---- Disease state durations ----
  
  # Number of days infected
  p$duration_infection = 60
  
  # Number of days with immunity
  p$duration_immunity = 5
  
  return(p)
}

