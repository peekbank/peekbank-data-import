# steil_schild_2021

## 1. Reference

Steil JN, Friedrich CK and Schild U (2021) No Evidence of Robust Noun-Referent Associations in German-Learning 6- to 14-Month-Olds. Front. Psychol. 12:718742. doi: 10.3389/fpsyg.2021.718742

## 2. Abstract

Work with the looking-while-listening (LWL-) paradigm suggested that 6-month-old English-learning infants associated several labels for common nouns with pictures of their referents: While one distractor picture was present, infants systematically fixated the named target picture. However, recent work revealed constraints of infants' noun comprehension. The age at which these abilities can be obtained appears to relate to the infants' familiarity with the talker, the target language, and word frequency differences in target-distractor pairs. Here, we present further data to this newly established field of research. We tested 42 monolingual German-learning infants aged 6–14 months by means of the LWL-paradigm. Infants saw two pictures side-by-side on a screen, whilst an unfamiliar male talker named one of both. Overall, infants did not fixate the target picture more than the distractor picture. In line with previous results, infants' performance on the task was higher when target and distractor differed within their word frequency—as operationalized by the parental rating of word exposure. Together, our results add further evidence for constraints on early word learning. They point to cross-linguistic differences in early word learning and strengthen the view that infants might use extra-linguistic cues within the stimulus pairing, such as frequency imbalance, to disambiguate between two potential referents.

## 3. Original study info

TODO

## 4. Importing decisions

- `sex_child` in `LWL_data.csv` is coded 1/2 with no key. We use 1 = female, 2 = male as the resulting counts (29/13) match the paper's 29 female of 42.

- Vocabulary questionnaire is a custom 41-word list (28 LWL targets + 13 others) "based on" ELFRA-1 plus other sources. Stored as `lang_measures` with rawscore = `comprehension_n`.

- Some `unique_trial` row groups contain rows from multiple trial phases that share the same label (an aborted attempt followed by a restart). Each new (phase-)attempt resets the trial-internal `time` clock, so we detect segments and keep only the largest segment. Then drop phase groups with 20 or fewer rows (failed recordings or ghosts) and any trial left without an audio phase.

- Paper's trial exclusion criterion "< 12.5% on-screen looking" was NOT marked as per Peekbank MO to not reverse engineer these sorts of criteria

- We use the trial-local `time` column (fractional ms) rather than `pos_time` as the digest's `t`. At 60 Hz, integer-valued `pos_time` produces duplicate timestamps that fail our resamplers strict monotonicity check.

## 5. Importing ambiguity

- `point_of_disambiguation` set per trial to the first sample where `sound` switches from `"nosound"` to `"sound [word]"`. Visually matches the time-course graphs in the paper (along with their markings for word onset), but might need author verification here.

- Gaze coordinates seem to be screen-centered in the raw data (pos_x in [-960, 960], pos_y in [-540, 540] on a 1920x1080 display). Translated by (+960, +540) to the bottom left origin. We assume that their coordinates are positive up, meaning that in their original coordinates gazes above screen center have positive pos_y values.

- AOI regions are not given, but are likely just the screen split in two (`l = [0, 960] x [0, 1080]`, `r = [960, 1920] x [0, 1080]`) as that empirically matches the lab's precomputed `AOI` column.




