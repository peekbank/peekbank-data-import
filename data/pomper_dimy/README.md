# pomper_dimy readme

1. **Reference.**

Contributors: Ron Pomper

2. **Abstract.**

This study investigated whether infants associate diminuitive endings ("daxy") with unfamiliar animals (vs. vehicles). The study also includes trials of familiar words with and without diminuitive endings. There are two "pilot" studies total (never published).

3. **Original study info.**

Condition info:
- intro: these are vanilla familiar trials
- vehicle: novel word trial (ending in non-y, e.g. blicket) with an unfamiliar vehicle paired with an animal
- animal: novel word trial (ending in y, e.g. zebby)  with an unfamiliar animal paired with an vehicle
- fam-y: familiar word trial, all animals, no -y added (e.g. dog)
- fam+y: familiar word trial, all animals, -y added (e.g. kitty)

There are two datasets available (pilot1 and pilot2). There are some small differences in the trial lists, but these are basically very similar studies at two different age ranges (pilot1: ~14-16 months; pilot2: ~17-19 months).


4. **Importing decisions.**

- There are raw by-participant trial lists (zipped data folder) and raw eyetracking data (zipped eyetrackingData folder) available, but we choose to import the somewhat cleaned up gaze data from the original author. Raw data kept on OSF in case ever useful for validating the import.
- Tobii x/y coordinates start from top left, so need to compute y-coordinate as SCREEN_SIZE - tobii_y_coordinate (i.e. flip the y-axis) to get bottom-right origin.
- AOI Regions computed using aoi_info_dimy.txt and validated against raw eyetracking data (main density of eyetracking data lies within AOI regions)
- The full phrases are included from the "pomper_dimy_full_phrases.csv" file, which is compiled by hand based on listening to the original audio stimuli (on OSF).

5. **Importing ambiguity.**

