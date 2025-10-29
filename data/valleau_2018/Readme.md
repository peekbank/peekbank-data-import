# Valleau 2018  Readme

1. Reference
Valleau, M. J., Konishi, H., Golinkoff, R. M., Hirsh-Pasek, K., & Arunachalam, S. (2018). An eye-tracking study of receptive verb knowledge in toddlers. Journal of speech, language, and hearing research, 61(12), 2917-2933.

2. Abstract
Purpose
We examined receptive verb knowledge in 22- to 24-month-old toddlers with a dynamic video eye-tracking test. The primary goal of the study was to examine the utility of eye-gaze measures that are commonly used to study noun knowledge for studying verb knowledge.
Method
Forty typically developing toddlers participated. They viewed 2 videos side by side (e.g., girl clapping, same girl stretching) and were asked to find one of them (e.g., “Where is she clapping?”). Their eye-gaze, recorded by a Tobii T60XL eye-tracking system, was analyzed as a measure of their knowledge of the verb meanings. Noun trials were included as controls. We examined correlations between eye-gaze measures and score on the MacArthur–Bates Communicative Development Inventories (CDI; Fenson et al., 1994), a standard parent report measure of expressive vocabulary to see how well various eye-gaze measures predicted CDI score.
Results
A common measure of knowledge—a 15% increase in looking time to the target video from a baseline phase to the test phase—did correlate with CDI score but operationalized differently for verbs than for nouns. A 2nd common measure, latency of 1st look to the target, correlated with CDI score for nouns, as in previous work, but did not for verbs. A 3rd measure, fixation density, correlated for both nouns and verbs, although the correlation went in different directions.
Conclusions
The dynamic nature of videos depicting verb knowledge results in differences in eye-gaze as compared to static images depicting nouns. An eye-tracking assessment of verb knowledge is worthwhile to develop. However, the particular dependent measures used may be different than those used for static images and nouns.

3. Original study info
40 children age 22-24 months
* children see left side image, then right side image, then both, then hear a query phrase (with fixation star),
then both stim reappear (with additional carrier phrases + target)
* noun trials (7/kid) use still images
* verb trials (18/kid) use videos

4. Importing decisions
* decision to not include the baseline (pre-any-auditory-stimulus) window

* stimuli_lookup_table was re-created at some point (the stimuli_lookup_table.csv existed at some point but got lost (origin not known, a past peekbank importer likely created it)
this is a reproduction based on the output files that were still existing, so it is at least as credible as that old csv.))

* they excluded trials with >25% trackerloss, we don't recreate that. (We only have data from children who were included in their final sample.)

5. Importing ambiguity
no known ambiguities
