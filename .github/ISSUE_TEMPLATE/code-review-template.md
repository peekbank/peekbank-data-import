---
name: Code Review Template
about: Describe this issue template's purpose here.
title: ''
labels: ''
assignees: ''

---

- [ ] Dataset is ready for code review 

Code Review Checklist:

- [ ] Git pull this repo to get the latest version
- [ ] Run `renv::restore()` to sync your packages with the lockfile
- [ ] Get the latest version of the dataset from osf (delete your raw_data, so that the script automatically downloads the data)
- [ ] Run the import script
- [ ] Does it run into issues due to missing libraries? If so, install them and run `renv::snapshot()` to update `renv.lock`
- [ ] Does the validator run without issues? If not, complain to Adrian or fix the listed issues if you feel like it

Common issues to check:

Trials
- [ ] Are trials unique between administrations?
- [ ] Is exclusion info handled correctly? (look closely at how exclusion information is marked)

Trial Types 
- [ ] Check if the trial type IDs are created independently of administrations/subjects
- [ ] Is vanilla_trial coded appropriately?

Stimuli
- [ ] If the images are on osf, make sure the image path contains the image path on osf
- [ ] Make sure each row represents a label-image association
    - [ ] the labels should be the words that the participants hear. For example, "apple" is okay, "red_apple_little" is wrong and was probably erroneously extracted from the file name
- [ ] Are there items in the imported dataset not mentioned in the paper?
- [ ] Are distractors represented correctly?
    - _Special explanation for distractors: If an item only ever appeared in distractor position, it still gets its own row. The label is typically the label given to the image in the experiment description (e.g., "the distractor was an image of a chair"). If there is no obvious label provided in the experiment design, leave label blank._

Subjects
- [ ] Does CDI data follow the new aux_data format?
- [ ] Age rounded correctly? (decision: we do no rounding)

General
- [ ] Double-check the citation and update it in the dataset table and make sure it’s consistent with the peekbank datasets google sheet: [peekbank datasets](https://docs.google.com/spreadsheets/d/1nGXWRu6_q7ATGrdWrFDuDmb9VoGHCnTO9NLhqhpuN28/edit?usp=sharing)
- [ ] Are there any TODOs left in the code - resolve/double check
- [ ] Review (or add) a ReadME ([example](https://github.com/langcog/peekbank-data-import/blob/master/data/baumgartner_2014/import_scripts/ReadME.md))
    - [ ] Make sure any TO-DOs or other decision points in the comments of the code are documented in the ReadMe AND removed from the code to prevent ambiguity
- [ ] General data sanity-checking (summary output helps here)
    - [ ] is there are the general numbers (e.g. # of participants, # of stimuli, average trials per administratoin) in the summary consistent with the paper? aoi_timepoints are hard to gague, but a super small number is probably bad
    - [ ] is the subject summary (age, sex distribution) approximately consistent with the paper? (note that it is not surprising if it is not identical - often we have a slightly different dataset and are not trying reproduce the exact numbers)
    - [ ] is the target side distribution weirdly skewed towards one side?
    - [ ] any weird trial durations?
    - [ ] do the cdi rawscore numbers match the instrument and measure?
    - [ ] is the exclusion % and the exclusion reasons sensible? (bearing in mind that we only have exclusion info for some datasets)
    - [ ] Inspect the timecourse and accuracy plots/output at the end of the import:
        - [ ] Compare timecourse patterns with paper (as best as possible)
        - [ ] Does the timing seem right? (accuracy spike later than PoD might be sensible, earlier is suspicious)
        - [ ] (if multiple conditions) Does the number conditions make sense in the context of the paper?
        - [ ] (if multiple conditions) Are the overall accuracies for conditions vastly different in a way not explained by the paper?
        - [ ] Any odd item-level patterns? 
        - [ ] Any odd subject-level patterns? 
    - [ ] Any large (unexpected) discrepancies between data reported in paper vs. data in the imported dataset?
- [ ] After checking everything and rerunning the script: set the write_and_validate upload flag to TRUE temporarily to automatically upload the output to [osf](https://osf.io/pr6wu/)
