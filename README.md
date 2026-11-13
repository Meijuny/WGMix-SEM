# WGMix-SEM
Scripts for WGMix-SEM Simulation Study and Empirical Illustration

## Simulation Study:
This folder contain a total of 10 files related to our simulation study following the order below:
- DataGeneration.R: R codes to generate the data;
- FactorScoreDF.R: R codes to conduct MG-CFA on the generated data file for each factor and compute factor scores as well as their standard deviation;
- RunLG_OutResult.R: R codes to call LatentGOLD in R to estimate the structural model and results are saved as txt files (Note that the full simulation is conducted on Windows Cluster of our research center);
- Out_RegCoeff.R, Out_GroupClus.R, Out_GroupClusProfile.R: R codes to read the results from the txt files and extract the parameters of interests;
- evaluation.R: R codes to evalaute the recovery of the parameters of interests (with ARI.R used as a helper function);
- do_sim.R: R codes to conduct the simulation across all conditions and all replications

## Empirical Illustration:
This folder contain both R scripts and LatentGOLD syntax:
