# nih_babytoolbox_2025

## 1. Reference

Novack, M. A., Dworak, E. M.,  Han, Y. C., Kaat, A. J., Ustsinovich, V., Saffran, J., Frank, M. C., Waxman, S., Gershon,  R. C. (2025). Development and validation of the NIH Baby Toolbox® language measures. Infant Behavior and Development, 80, 102121.

## 2. Abstract

Language acquisition is a fundamental aspect of infancy and early childhood development. This paper describes the development and validation of the language measures within the NIH Baby Toolbox® (Baby Toolbox), a state-of-the-art assessment system designed for children aged 16 days to 42 months. The Baby Toolbox language domain includes six measures that assess both expressive and receptive language-Looking while Listening, Picture Vocabulary, CDI-CAT Comprehension, CDI-CAT Production, Mullen Receptive, and Mullen Expressive. These measures, offered in both English and Spanish, leverage advanced technologies-such as computerized adaptive testing and gaze-based paradigms-to enhance both the accuracy and accessibility of the assessment. A large-scale norming study (N = 2490 children; 2025 English-exposed, 465 Spanish-exposed) provided strong evidence of reliability and validity, with high test-retest correlations and robust construct validity, although small sample sizes prohibited analyses for some of the Spanish measures. Findings support the Baby Toolbox as a reliable, scalable tool for assessing early language development in diverse populations.

## 3. Original study info

The data comes from the national norming study for the NIH Baby Toolbox. Looking while Listening is
one of six language measures inside an administered battery which also carries other gaze tasks, caregiver
report and a sociodemographic questionnaire. LWL is administered from 6 to 24 months of age.
The data made available to Peekbank has 1,120 sessions from 1,044 children.

Apparatus: A 4th-gen 11-inch iPad Pro on an adjustable stand, the child in a
highchair or on a caregiver's lap, and gaze data is collected using the front camera
through Apple's ARKit face tracking at 60 Hz. Areas of interest are built per child from a
four-point calibration, children who fail calibration still perform the
test, but their gazes are never classified.

Design: The design consists of 5 sections in a fixed order. Each section holds ten slots, of which eight
are test items and two are animated attention-getters:

1 item    2 item    3 item    4 item    5 FILLER
6 item    7 item    8 item    9 item   10 FILLER

In total, we have 40 items (image-pairs) of which children see 24: sections 1 to 3 for younger
children at 6-14 months and 3 to 5 for older children at 15-24 months, sharing section 3's eight items.

Each of the 40 image is used in two items and mostly appears once on each side (except for Bucket and
Foot always appearing on the left, and Shoe and Donkey always appearing on the right). Each stimulus is target exactly once.

There is a second paper associated with this data: Novack MA, Han YC, Kaat AJ, Pila S, Flynn RM, Bedjeti K, Diaz MV, Hanrahan RT, Glinberg S, Sievert PH, Frederick C, Rajiv P, Clare C, Ustsinovich V, Gershon RC. Automated iPad-based gaze detection in the NIH Baby Toolbox® norming study. Infant Behav Dev. 2025 Sep;80:102119. doi: 10.1016/j.infbeh.2025.102119. Epub 2025 Aug 1. PMID: 40752054; PMCID: PMC12372363.

The gaze-detection paper found the app's classifications agree with hand coding 87.45% of the time for looks to the left but only 70.21%
for looks to the right. The authors argue that this was caused by iPad's camera being placed on the left side of the device. Target sides are counterbalanced across the item set, so analyses averaging over all items is unaffected. Per item we have a coundoung: each word is asked for on only one side each, so target looking is confounded with side for all 40 (inflated for the twenty asked on the left and deflated for the twenty on the right.)

Also their methodology paper here relates to this dataset:  Han, Y. C., Dworak, E. M., Mansolf, M., Adam, H., Yao, L., Novack, M. A., Pila, S., Flynn, R. M., Flagg, A. M., Ustsinovich, V., Savio, K., Byrne, G. J., Gershon, R. C., & Kaat, A. J. (2025). NIH Baby Toolbox® methodology and norms development. Infant behavior & development, 80, 102117. https://doi.org/10.1016/j.infbeh.2025.102117


## 4. Importing decisions

Item side: The authors confirmed via email that the first word in the `itemID` (e.g.
`Flower_Eye_G3`) denotes the images shown on the left.

Subjects and sessions: The authors confirmed `registrationID` is unique to a
person-by-administration, not to a person, so subjects are keyed by `userPIN`.
For the children that have multiple sessions, we extract the ordering from the timestamp
in the camera image filenames.

Multisession age correction: Norming data records only one age per child, at their entry sitting,
so any later session is older by however long has passed.
For the children that have more than one LWL session, the camera timestamps give the gaps by which to shift the later ones.
9 children sat the battery twice and ran the LWL task only at the second one. Thus, we shift the age by the gap
between the LWL session (camera timestamp) and the entry questionnaire.


30 English LWL sessions carry a PIN absent from the norming data and are dropped.

Duplicated sessions: Fifteen sessions appear twice in the raw data, every row having an
identical second copy, and are removed by deduplicating the raw table.


Point of disambiguation: The app records when each prompt started and finished, but doesn't say where the target word sits inside the
recording (author confirmed via mails). We manually measured the target word onset off the audio by manually checking in Praat, saved in the
the `noun_onset_ms` column of `item_meta_manually_curated.csv` (+ check section 5 and the code for difference between recorded audio playtime and actual file audio duration).


Vanillaness: Each trial names the target twice, so non-vanilla.
This is not mentioned in the paper directly, but:
- The authors confirmed it by email
- every trial carries exactly two `audioStarted`/`audioCompleted` pairs
- the item script in the raw data names two different recordings per item
- the main paper says "scoring on each trial begins as soon as the first audio file ends"


There are two errors in the item script:

*`Filler6` is labelled section 4 although it is the last thing presented in section 3.

*The `Next Item ID` column is erronerius: Each row supposedly specifies the item that comes after it giving an ordering
20 of the 63 pointers name an identifier that is nowhere in the file/data:

- 18 have the item's two words flipped, e.g. pointing at `Nose_Book_G1` where the file has
  `Book_Nose_G1`
- 1 has the wrong section number, `Blanket_Toothbrush_G1` for `Blanket_Toothbrush_G2`
- 1 names a `Filler 10` that does not exist

Thus, we take the trial order is taken from the event log instead.

CDI: The toolbox runs an adaptive CDI (CDI-CAT, Kachergis et al. 2022).
The data carries Change Sensitive Scores, which we ship as a generic language measure as they are not comparable to the regular CDI rawscores.


## 5. Importing ambiguity

The data dictionary does not state how the values  `CaregiverEnglish` and `English` differ
for the native language field. We treat them as as both pointing to the same language.

POD inaccuracy: the app reports when playback began and ended, but that
window is always longer than the playback time of the audio file. We need to keep this difference in mind when placing the target word onset relative to the event denoting the audio started playing. There is a pattern that groups these differences into two clusters:

    1. Prompts of trials that do NOT directly follow another trial (start of experiment or attention getter) have an average of 67.2 ms discrepancy between audio duration and recorded playback duration. We don't know wether this time sits before of after the audio playback, so we assume equal padding at the beginning and end to minimise the worst case error of our assumption.

    2. An additional 119.6 ms discrepancy appears on those prompts whose trial directly followed
    another trial. The second audio onset does NOT show this pattern, so we conclude that there is
    likely some playback/buffering lag in the switching of trials, thus leading us to add that delay to the beginning of the time window (shifitng our POD back).



CDI Expected Rawscores: According to the third paper mentioned in Section 3, the CSS scores are a linear transformation from IRT theta scores.
We estimate the parameters of the transform using the data (pending possible confirmation by the authors) and use that to reconstruct
an expected rawscore wordcount if the caregivers were given the full list instead of the adaptive one (by summing every word's response probability at a childs transformed ability). We save these rawscores as generic language measures.

The per-word parameters come from the published CDI-CAT calibration on OSF
(<https://osf.io/xdp73/>, the archive for Kachergis et al. 2022), extracted and saved as
`cdi_cat_item_parameters.csv`. Comprehension comes in a 396 and a 386 item version;
we use the 396, matching the item counts the language paper reports, "679
items for production and 396 for comprehension".

A handful of CDI scores carry a standard error of 901.1376, undocumented anywhere.
Divided by the scale factor it is exactly 99 theta units, and it coincides
exactly with the two scale ends, in both directions. We read it as a sentinel for an estimate the test could not identify.
Thus, those scores do not get a expected work count.
