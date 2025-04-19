# hurtado_2008 README

1. **Reference**

Hurtado, N., Marchman, V. A., & Fernald, A. (2008). Does input influence uptake? Links between maternal talk, processing speed and vocabulary size in Spanish‐learning children. Developmental Science, 11(6), F31-F39. https://doi.org/10.1111/j.1467-7687.2008.00768.x

2. **Abstract**

It is well established that variation in caregivers’ speech is associated with language outcomes, yet little is known about the learning principles that mediate these effects. This longitudinal study (n = 27) explores whether Spanish-learning children's early experiences with language predict efficiency in real-time comprehension and vocabulary learning. Measures of mothers’ speech at 18 months were examined in relation to children's speech processing efficiency and reported vocabulary at 18 and 24 months. Children of mothers who provided more input at 18 months knew more words and were faster in word recognition at 24 months. Moreover, multiple regression analyses indicated that the influences of caregiver speech on speed of word recognition and vocabulary were largely overlapping. This study provides the first evidence that input shapes children's lexical processing efficiency and that vocabulary growth and increasing facility in spoken word comprehension work together to support the uptake of the information that rich input affords the young language learner.

3. **Original study info**
Longitudinal (LWL at 18 and 24 months), all "vanilla" trials of familiar object/labels in Spanish.

4. **Importing decisions**
- "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
  - “0” ~“distractor” (look to distractor)
  - “1” ~ “target” ( look to target )
  - “0.5” ~ “other” (child is fixating the center of the screen)
  - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as   - “missing” because it can mean multiple things, including that gaze was not codable)
  - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
- left/right locations were reversed to reflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)

- Full phrases were left empty as they are missing from the data and the paper does not provide strong enough information to reconstruct them, only stating:
Speech stimuli, recorded by a native Spanish-speaker, consisted of simple sentences ending
with a target noun (¿Dónde está el/la [target]? ‘Where's the [target]?’). A

5. **Importing ambiguity**
- There are more subjects in the data (n=76) than reported in the paper (n=27)
