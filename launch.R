############################################################
# LAUNCH
#
# Run and visualise two toy models: an individual-based model,
# and a population-based model.
############################################################

# Set working directory to sourced file
if (interactive()) setwd(getSrcDirectory(function() {}))

# Load all required packages and functions
source("dependencies.R")

message("* Running toy models")

# Load model input parameters (used for both models)
p = load_model_parameters()  # See parameters.R

# Run the SIR population-based model
model_sir(p)  # See model_sir.R

# Run the individual-based model
model_ibm(p)  # See model_ibm.R

# Plot results of both models side-by-side
plot_model_results()  # See plotting.R

message("* Finished")

