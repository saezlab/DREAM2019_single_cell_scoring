
# Single Cell Signaling in Breast Cancer DREAM challenge

This repository stores the scoring and validation scripts used in the challenge. 
The scripts for the evaluation of the teams are in the `submission_analysis` folder, organised by subchallenges. 

To re-run the submission analysis, you need to download all the submissions (~31Gb) in the final round. 
To do so,
1.  find the leaderboard files in the `submission_data/final_round/SC*` folders.
2. In that file each line identifies a final submission from a team. You can query the files from Synapse using the `objectId` field.
3. The scripts assumes you save all the submissions to `submission_data/final_round/SC*` folders with the name `<objectId>.csv`.
  
 

