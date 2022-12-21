# frontdoor_criterion
This repository hosts all the code related to my master seminar thesis 'Causal inference through the front-door' at the chair of Marketing of Prof. Dr. Dominik Papies at the Faculty of Economics and Social Sciences of the University of Tübingen.
Generally this repository is divided into two parts:
- **01_simulations**: hosts R scripts that challenge the FDC using simulated data
  - *DAG_Simulations.html*: provides a theoretical and empirical introduction into the Front-Door Criterion (FDC).
  - *DAG_Simulations_Confoundedness.html*: examines violations of the FDCs identifying assumptions using simulated data.
  - *Final_Thesis_Results.R*: R-Script that runs the simulations that are finally addressed in the seminar thesis itself.
- **02_taxi**: hosts R scripts for the replication of the empirical application of the FDC by Bellemare et al. (2020)
  - *01_local*: contains the initial R-scripts that were used for data import and first estimation steps + examines the results that were computed on the bwUniCluster and stored in a csv-file.
  - *02_bwUnicluster*: contains the R as well as the Rout files from the bwUniCluster. Here, the steps of data import, preparation and final estimation were done in separate steps and scripts.

<br>

References:<br>
Bellemare, M. F., Bloem, J. R., and Wexler, N. (2020). The paper of How: Estimating treatment effects using the front-door criterion. *Working paper*.
