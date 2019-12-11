
# Single Cell Signaling in Breast Cancer DREAM challenge

This repository stores the scoring, validation and post-challenge analysis scripts used in the [Single Cell Signaling in Breast Cancer DREAM challenge](https://www.synapse.org/#!Synapse:syn20366914/wiki/593925 WIKI page).

The scripts for the evaluation of the teams are in the `submission_analysis` folder, organised by subchallenges. 

To re-run the submission analysis, you need to download all the submissions (~31Gb) in the final round. 
To do so,
1. find the leaderboard files in the `submission_data/final_round/SC*` folders.
2. In the leaderboard files each line identifies a final submission from a team. You can query the files from Synapse using the `objectId` field. For further details, please check the vignette of the `synapser` package: https://github.com/Sage-Bionetworks/synapser.
3. The scripts assumes you saved all the submissions to `submission_data/final_round/SC*` folders with the naming convention `<objectId>.csv`.
  
Feedbacks and questions are welcommed on the Issue page of the repo.

