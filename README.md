# Evaluation of Simulation Results

This is a replication kit belonging to the paper "Investigation and Prediction of Open Source Software Evolution Using Automated Parameter Mining for Agent-Based Simulation".

The "SimSE_Data" folder contains all projects of the case study conducted for this paper. It consists of ten Open Source Software projects. For each, empirical software evolution data from repository mining as well as simulated software evolution data is provided. The used simulation framework can be found here: https://github.com/dhonsel/SimSE.

Run the evalution:

1. Open cc_graph_analysis_ext.R from the analysis folder. 
2. Edit the path and savedir variable. The path should lead to the "SimSE_Data" folder and the savedir should lead to the "plots" folder.
3. Run the R script. All results will be shown in your R environment and all plots will be saved in the directory. 
