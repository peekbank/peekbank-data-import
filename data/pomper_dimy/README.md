# pomper_dimy readme

1. **Reference.**

Pomper, R., & Saffran, J. R. (unpublished). Unpublished "Dimy" study: Do infants learn to associate diminutive forms with animates?

2. **Abstract.**

This study investigated whether infants associate diminutive endings ("daxy") with unfamiliar animals (vs. vehicles). The study also includes trials of familiar words with and without diminutive endings. There are two "pilot" studies total (never published) at two different age ranges (~ 15 months and ~ 18 months).

3. **Original study info.**

Condition info:
- intro: these are vanilla familiar trials
- vehicle: novel word trial (ending in non-y, e.g. blicket) with an unfamiliar vehicle paired with an animal
- animal: novel word trial (ending in y, e.g. zebby)  with an unfamiliar animal paired with an vehicle
- fam-y: familiar word trial, all animals, no -y added (e.g. dog)
- fam+y: familiar word trial, all animals, -y added (e.g. kitty)

There are two datasets available (pilot1 and pilot2). There are some small differences in the trial lists, but these are basically very similar studies at two different age ranges (pilot1: ~14-16 months; pilot2: ~17-19 months).

There appears to be a CDI data file available (short form), in the file DimY_cdi_deID.csv, but it looks messy and has not been imported (but could be in the future). 


4. **Importing decisions.**

- There are raw by-participant trial lists (zipped data folder), raw eyetracking data (zipped eyetrackingData folder), and trial lists available, but we choose to import the somewhat cleaned up gaze data from the original author. Raw data kept on OSF in case ever useful for validating the import (MZ: I dug out a lot of additional files here from the old lab servers to help triangulate missing information if ever helpful).
- Tobii x/y coordinates start from top left, so need to compute y-coordinate as SCREEN_SIZE - tobii_y_coordinate (i.e. flip the y-axis) to get bottom-right origin.
- AOI Regions computed using aoi_info_dimy.txt and validated against raw eyetracking data (main density of eyetracking data lies within AOI regions)
- The full phrases are included from the "pomper_dimy_full_phrases.csv" file, which is compiled by hand based on listening to the original audio stimuli (on OSF).
- applied exclusions based on comments in participant demographics, only excluded those participants with an explicit "no" decision (these rows are also greyed out)
- we treat all animal/vehicle trials as non-vanilla and novel (novel words, unfamiliar objects); all fam(-y/+y) and intro trials are treated as vanilla
- we compute trial-wise point of disambiguation by accounting for when the audio onset trigger is recorded for each trial. This should lead to a more precise estimate of point of disambiguation sensitive to small variation in audio timing onset / presentation lags (MZ: this is based on some familiarity with the underlying psychopy scripts and how eyetracking data was recorded).
- data for a small set of participants is quite sparse and/or appears to be low quality (this also tends to track with the comments in the participant demographics, e.g. "bad calibration"). We retain this data, with the exception of one participant with almost no valid AOI samples (7 total), namely the participant with the subject code "217".

- there are many trial types (near identical to each other) because the audio onsets were slightly different in different trials in different administrations
5. **Importing ambiguity.**

- the point of disambiguation within audio stimuli was determined to be roughly 2930 ms based on inspecting the actual onsets within the auditory stimuli (communication with the original authors suggested 2950 ms, but this seems a little late relative to actual files). This seems to vary a tad across trials (sometimes onset seems 10ish ms earlier or later, but seems pretty close to right).
- half of the novel/unfamiliar animals are never named/ appear only in distractor position. For these items, we mark the stimulus_novelty as "novel" and make their stimulus label identical to the image name (for lack of a better obvious alternative label)
