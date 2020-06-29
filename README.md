# PhDPaper2: Separating habitat niches of selected wetland birds using country-wide ALS data

This repository consist the scripts which was used for 

Z. Koma, M. W. Grootes, C. W. Meijer, F. Nattino, A. C. Seijmonsbergen, H. Sierdsema, R. Foppen & W. D. Kissling (in prep.): Niche separation of wetland birds revealed from fine-scale LiDAR metrics

The codes are still under a cleaning process. 

# Instructions for usage

To pre-process the country-wide ALS data for the Netherlands (AHN3: https://downloads.pdok.nl/ahn3-downloadpage/ ) we used laserchicken python package (https://zenodo.org/record/3836593#.XutyQGgzaUk). The scripts regarding the lidar data pre-processing step is avaialble under https://github.com/eEcoLiDAR/Laserfarm/tree/2020_01_status_quo.

In the directory of data_analysis we have published the scripts which were used to analyse the derived lidar metrics from the country-wide ALS data. This scripts are written in R. 
  - bird_data_process directory consist the scripts which were used to prepare presence-absence dataset for further analysis from the territory bird observations
  - lidar_process directory consist the scripts which were used to additionaly extract horizontal variability related lidar metrics and the intersection with the prepared bird observation data
   - analysis directory consist the scripts which were used to define the ecological niche space of the selected species using the ecospat package and carry out the niche equivalency and similiraty tests. The scripts start with Fig_* related to the figures which were generated for the manuscript. 
