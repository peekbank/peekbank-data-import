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
- [ ] Does the validator pass? It reports two tiers, both blocking:
    - [ ] **Errors** must be fixed. Complain to Adrian or fix the listed issues if you feel like it
    - [ ] **Warnings** are things that usually indicate an import error but are legitimate for some datasets (`cdi_collision` for duplicate CDI entries on the same subject/instrument/age, `stimulus_image_filetype` for stimulus images that are not jpg/jpeg/png). Either fix the underlying issue, or opt out per dataset by passing the warning id to `write_and_validate_list(..., suppress_warnings = c("cdi_collision"))`
- [ ] Does the import script pass anything to `suppress_warnings` in `write_and_validate_list`? If so, go through each one:
    - [ ] Is it actually legitimate for this dataset?
    - [ ] Is the reason written down in the dataset README?

Common issues to check:

Trials
- [ ] Is exclusion info handled correctly?
- [ ] Does the number of trials per administration match the design described in the paper? (edgecase to keep track of: If the study can present two trials of the same type back to back, the import must set the optional `trial_index` column because otherwise they are collapsed automatically, manually check here)

Trial Types 
- [ ] Is vanilla_trial coded appropriately?

Stimuli
- [ ] Make sure each row represents a label-image association
    - [ ] the labels should be the words that the participants hear. For example, "apple" is okay, "red_apple_little" is wrong and was probably erroneously extracted from the file name
- [ ] Are there items in the imported dataset not mentioned in the paper?
- [ ] Are distractors represented correctly?
    - _Special explanation for distractors: If an item only ever appeared in distractor position, it still gets its own row. The label is typically the label given to the image in the experiment description (e.g., "the distractor was an image of a chair"). If there is no obvious label provided in the experiment design, leave label blank._

Subjects
- [ ] Is there aux data beyond CDI that we could be capturing but are not? (e.g. parental report vocabulary counts, bilingual exposure percentages, standardized language scores.)
- [ ] Does the number of subjects with CDI/aux data match the paper?

Digest configuration
- [ ] Is the `coding_method` right? If the dataset has usable gaze coordinates and aoi region sets, use `eyetracking`, if we use the provided aoi tags, use `manual gaze coding` or `preprocessed eyetracking` depending on whether they used an eyetracker or did manual coding.

General
- [ ] Double-check the citation and update it in the dataset table and make sure it’s consistent with the peekbank datasets google sheet: [peekbank datasets](https://docs.google.com/spreadsheets/d/1nGXWRu6_q7ATGrdWrFDuDmb9VoGHCnTO9NLhqhpuN28/edit?usp=sharing)
- [ ] Are there any TODOs left in the code - resolve/double check
- [ ] Review readme
    - [ ] Make sure any TO-DOs or other decision points in the comments of the code are documented in the ReadMe AND removed from the code to prevent ambiguity (code comments about technical oddities in data structure etc. are fine to leave in the code)
- [ ] Is the dataset still listed in [`helper_functions/pipeline_ignore.txt`](https://github.com/langcog/peekbank-data-import/blob/master/helper_functions/pipeline_ignore.txt)? If the import is finished, remove it so the dataset actually runs in the pipeline
- [ ] General data sanity-checking (summary output helps here)
    - [ ] are the general numbers (e.g. # of participants, # of stimuli, average trials per administration) in the summary consistent with the paper? aoi_timepoints are hard to gauge, but a super small number is probably bad
    - [ ] is the subject summary (age, sex distribution) approximately consistent with the paper? (note that it is not surprising if it is not identical; often we have a slightly different dataset and are not trying reproduce the exact numbers, but large deviations should be investigated).
    - [ ] is the target side distribution weirdly skewed towards one side?
    - [ ] any weird trial durations? (there is a histogram of trial durations and one of trials per administration)
    - [ ] do the cdi rawscore numbers match the instrument and measure?
    - [ ] is the exclusion % and the exclusion reasons sensible? (bearing in mind that we only have exclusion info for some datasets)
    - [ ] does the AOI distribution look sane?
    - [ ] (if the dataset has xy data) check the XY timepoint summary: how much is NA, and what percentage of samples falls outside the monitor bounds? A large off-screen share points at a wrong coordinate origin or a wrong `monitor_size_x`/`monitor_size_y`
    - [ ] Inspect the timecourse and accuracy plots/output at the end of the import:
        - [ ] Compare timecourse patterns with paper (as best as possible)
        - [ ] Does the timing seem right? For idless imports using `digest`, keep in mind that wrongly specified `rezero`/`normalize` flags can shift the graph
        - [ ] (if multiple conditions) There is a separate per-condition timecourse plot when the dataset has 2 or more conditions. Does the number of conditions make sense in the context of the paper?
        - [ ] (if multiple conditions) Are the overall accuracies for conditions vastly different in a way not explained by the paper?
        - [ ] Any odd item-level patterns? 
        - [ ] Any odd subject-level patterns? 
        - [ ] (if the dataset has xy data) Does the XY gaze scatter plot show any odd patterns?
    - [ ] Any other, large (unexpected) discrepancies between data reported in paper vs. data in the imported dataset?
- [ ] After checking everything and rerunning the script: set the write_and_validate upload flag to TRUE temporarily to automatically upload the output to [osf](https://osf.io/pr6wu/)
