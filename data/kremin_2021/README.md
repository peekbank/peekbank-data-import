# kremin_2021 dataset

## Reference
Kremin, L. V., Jardak, A., Lew-Williams, C., & Byers-Heinlein, K. (2023). Bilingual children’s comprehension of code-switching at an uninformative adjective. Language Development Research 3(1), 249–276.

## Abstract
Bilingual children regularly hear sentences that contain words from both languages, also known as code-switching. Investigating how bilinguals process code-switching is important for understanding bilingual language acquisition, because young bilinguals have been shown to experience processing costs and reduced comprehension when encountering code-switched nouns. Studies have yet to investigate if processing costs are present when children encounter code-switches at other parts of speech within a sentence. The current study examined how 30 young bilinguals (age range: 37 – 48 months) processed sentences with code-switches at an uninformative determiner-adjective pair before the target noun (e.g., “Can you find le bon [the good] duck?) compared to single-language sentences (e.g., “Can you find the good duck?”). Surprisingly, bilingual children accurately identified the target object in both sentence types, contrasting with previous findings that sentences containing codeswitching lead to processing difficulties. Indeed, children showed similar (and in some cases, better) comprehension of sentences with a code-switch at an uninformative adjective phrase, relative to single-language sentences. We conclude that functional information conveyed by a code-switch may contribute to bilingual children’s sentence processing.

## Original study info
Participants were 36-month-old bilinguals (Eng-Fre from Montreal, and Eng-Spa from Princeton).
The key manipulation was code-mixing in prenominal adjectives before the target noun (e.g., "Can you see the good cow?" vs "Can you see le bon cow?"); note that the adjectives were uninformative.

Data from Montreal was collected with a Tobii T60-XL eyetracker, and data from Princeton was collected using a video camera and manual gaze coding.

Note that the data only include "single" and "mixed" conditions; there are also other "filler" trials that were not in the data (although the filler data for the Eng-Fra subset can be found in Sander-Montant et al. ([2022](osf.io/2m345/))).

## Importing decisions
Stimuli are available although these are in video files so will need to be extracted.
CDI and Language exposure data for the Montreal subset are also available in Perez et al. ([2024](https://osf.io/mxksz/)) and could be imported (although they have not yet been).
There are also vocabulary data from DVAP; these could be imported in the lang_measures field for subject_aux_data.

We included importing decisions from the [processing script](https://osf.io/ug7t3/files/github/01_load.R), including the rectification of AOIs and the removal of duplicated header rows.
Age was calculated from years, months, and days using the formula: years * 365.2425 + months * (365.2425/12) + days.

Monitor size for the Montreal data was set as 1920x1200 based on the Tobii export.

We decided that the data reflect non-vanilla trials, although the "single" condition trials (e.g., "Can you see the good cow?") could be construed as "vanilla" in the sense that the carrier phrase is unlikely to bias the results.
(This would also be true for the "filler" condition trials should we decide to include these data).
The data may be worth further analysis despite their non-vanilla status.

## Importing ambiguity
None other than those reported above.
