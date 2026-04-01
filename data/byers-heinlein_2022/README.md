# byers-heinlein_2022 README

1. **Reference**
Byers-Heinlein, K., Jardak, A., Fourakis, E., & Lew-Williams, C. (2022). Effects of language mixing on bilingual children's word learning. Bilingualism, 25(1): 55-69.

2. **Abstract**
Language mixing is common in bilingual children’s learning environments. Here, we investigated effects of language mixing on children’s learning of new words. We tested two groups of 3-year-old bilinguals: French–English (Experiment 1) and Spanish–English (Experiment 2). Children were taught two novel words, one in single-language sentences (“Look! Do you see the dog on the teelo?”) and one in mixed-language sentences with a mid-sentence language switch (“Look! Do you see the chien/perro on the walem?”). During the learning phase, children correctly identified novel targets when hearing both single-language and mixed-language sentences. However, at test, French–English bilinguals did not successfully recognize the word encountered in mixed-language sentences. Spanish–English bilinguals failed to recognize either word, which underscores the importance of examining multiple bilingual populations. This research suggests that language mixing may sometimes hinder children’s encoding of novel words that occur downstream, but leaves open several possible underlying mechanisms.

3. **Original study info**
Two experiments tested whether language mixing affects 3-year-old bilinguals' novel word learning.
Children saw pairs of familiar objects (dog, bunny, fish) standing/sitting on novel objects (teelo, walem), one pair on each side of the screen. Sentences named both the familiar and novel object (e.g., "Look! Do you see the dog on the walem?").

12 training trials per child (6 single-language, 6 mixed-language). In mixed trials the familiar word switches to the other language. 4 test trials per child asking them to find the novel object (e.g., "Can you find the walem?").

- Experiment 1 (Montreal): French-English bilinguals, Tobii T60XL at 60 Hz, 24-inch monitor
- Experiment 2 (New Jersey): Spanish-English bilinguals, video camera with manual gaze coding at 29.97 Hz
- Condition: "Mixed" (familiar word in the other language) or "Single" (all one language)
- No trials are vanilla: training trials have 4 objects on screen with both familiar and novel words named; test trials target novel objects

4. **Importing decisions**

- Montreal AOI data was already precomputed in the raw data (boolean hit columns: targetfamiliar, targetnovel, distractorfamiliar, distractornovel), so we use coding_method "preprocessed eyetracking". NJ gaze data was manually coded as T(arget)/D(istractor)/A(way), so coding_method "manual gaze coding". AOI region set coordinates are not available for either experiment.

- 21 duplicate rows in Montreal data (same timepoint appearing twice, once with trackloss) removed via distinct(), following the authors' own analysis script.

- full_phrase_language is "multiple" for mixed-language training trials (carrier in one language, familiar word in the other), and the carrier language for single-language training and all test trials. Note: test trial sentence.type ("Mixed"/"Single") in the data refers to the word in the learning condition, not the test phrase language, as all test phrases are monolingual.

- During training, each screen side shows a familiar-novel object pair. Montreal's eye-tracker provides per-object AOI hits (targetfamiliar, targetnovel, distractorfamiliar, distractornovel), but we collapse by side: looks to either object on the target side count as "target", either on the distractor side as "distractor". This matches the original authors' analysis and is consistent with NJ's manual coding, which could only resolve left/right (and obvs the Peekbank format only supports left/right).

- NJ raw data was missing target side and distractor identity; derived by joining with sample_order.csv on trial number. sample_order.csv phrases contain French words (e.g., "chien") matching the Montreal experiment, not the Spanish words actually used in NJ, so NJ phrases were constructed from the paper's template instead: English carrier -> "Look! Do you see the [word] on the [novel]?", Spanish carrier -> "¡Mira! ¿Puedes ver el [word] encima del [novel]?" The paper describes both experiments as using the same design, and the join is consistent (every NJ target.image for a given trial number appears among sample_order's objects for that same trial number, no mismatches). 

- DVAP vocabulary scores (Libertus 2015) included as language measures.

- Trial data files only contain included participants (keeper=1). Excluded participants (10 Montreal, 5 NJ) have no trial data in the raw CSVs, so all subjects in the import have excluded = FALSE in our data.

5. **Importing ambiguity**

- For mixed-language trials, target_stimulus_label_original is the word as actually spoken (i.e., in the non-carrier language). Montreal French translations confirmed from phrase data: dog->chien, bunny->lapin, fish->poisson. For NJ, the paper only explicitly mentions "perro" (dog). We used common Spanish translations: dog->perro, bunny->conejo, fish->pez. The exact Spanish words used in the experiment were not available.

- Spanish-carrier test phrase phrasing is unknown, set to NA.

- The novel words "walem" and "teelo" had French phonological forms ("walème" and "tileau") when spoken in French carrier phrases, but we use the English spellings consistently as stimulus labels since these are made-up words.
