# kremin_2021 dataset

## Reference
Kremin, L. V., Jardak, A., Lew-Williams, C., & Byers-Heinlein, K. (2023). Bilingual children’s comprehension of code-switching at an uninformative adjective. Language Development Research 3(1), 249–276.

## Abstract
Bilingual children regularly hear sentences that contain words from both languages,
also known as code-switching. Investigating how bilinguals process code-switching 
is important for understanding bilingual language acquisition, because young 
bilinguals have been shown to experience processing costs and reduced comprehension 
when encountering code-switched nouns. Studies have yet to investigate if processing 
costs are present when children encounter code-switches at other parts of speech 
within a sentence. The current study examined how 30 young bilinguals 
(age range: 37 – 48 months) processed sentences with code-switches at an uninformative
determiner-adjective pair before the target noun 
(e.g., “Can you find le bon [the good] duck?) compared to single-language sentences
(e.g., “Can you find the good duck?”). Surprisingly, bilingual children accurately 
identified the target object in both sentence types, contrasting with previous 
findings that sentences containing codeswitching lead to processing difficulties. 
Indeed, children showed similar (and in some cases, better) comprehension of 
sentences with a code-switch at an uninformative adjective phrase, relative to
single-language sentences. We conclude that functional information conveyed by 
a code-switch may contribute to bilingual children’s sentence processing.

## Original study info
Participants were 36-month-old to 48-month-old bilinguals (Eng-Fre from Montreal, 
and Eng-Spa from Princeton).
The key manipulation was code-mixing in prenominal adjectives before the target noun
(e.g., "Can you see the good cow?" vs "Can you see le bon cow?"); note that the 
adjectives were uninformative.

Data from Montreal was collected with a Tobii T60-XL eyetracker, and data from 
Princeton was collected using a video camera and manual gaze coding.

Note that the data only include "single" and "mixed" conditions; there are also
other "filler" trials that were not in the data (although the filler data for the 
Eng-Fra subset can be found in Sander-Montant et al. ([2022](osf.io/2m345/))).

## Importing decisions

We included importing decisions from the [processing script](https://osf.io/ug7t3/files/github/01_load.R), including the rectification of AOIs and the removal of duplicated header rows.
Age was calculated from years, months, and days using the formula: years * 365.2425 + months * (365.2425/12) + days.

Monitor size for the Montreal data was set as 1920x1200 based on the Tobii export.

(vanilla-ness update Jan 2026): We have decided that uninformative adjectives are vanilla for our purposes. 
The language switch are non-vanilla because of the language switch, but the "single" language trials 
(e.g., "Can you see the good cow?") have now been deemed vanilla. 

child-level exclusions are recorded, but trial level are not

this paper does not report CDI data, although the CDI data at  https://osf.io/mxksz/ 
may be for the Fr-Eng kids is this sample (but we don't have subject id's lined up)

## Importing ambiguity
None other than those reported above.
