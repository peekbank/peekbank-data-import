1. **Reference.**

Pomper, R., & Saffran, J. R. (2018). Familiar object salience affects novel word learning. Child Development, 90(2), e246-e262. doi:10.1111/cdev.13053.

2. **Abstract.**

Children use the presence of familiar objects with known names to identify the correct referents of novel words. In natural environments, objects vary widely in salience. The presence of familiar objects may sometimes hinder rather than help word learning. To test this hypothesis, 3-year-olds (N = 36) were shown novel objects paired with familiar objects that varied in their visual salience. When the novel objects were labeled, children were slower and less accurate at fixating them in the presence of highly salient familiar objects than in the presence of less salient familiar objects. They were also less successful in retaining these word-referent pairings. While familiar objects may facilitate novel word learning in ambiguous situations, the properties of familiar objects matter.

3. **Original study info.**

Stimuli:
- 4 visually distinct novel objects matched in visual salience
  - Novel words: "sprock", "jang" , "pifo", "tever"
- 12 familiar objects in two salience conditions:
  - High salience (6 objects): juice, fish, bird, bus, cat, cake
  - Low salience (6 objects): bed, door, chair, box, brush, sock

Structure:
- Looking-while-listening task with (frame-by-frame) video-coded eye-tracking
- 1 warm-up trial (fireworks images)
- Referent selection phase (24 trials total):
  - 12 trials where a novel object was labeled (3 trials per novel object)
  - 12 trials where a familiar object was labeled (novel object as distractor)
  - Each trial: 1000ms silent viewing, then carrier phrase + target label, then 1000ms silence
- Test phase (8 test trials + filler trials):
  - 8 test trials: 2 novel objects from the same salience condition (2 trials per novel object)
  - Filler trials interspersed with pairs of familiar objects

Conditions:
- high_salience_selection: Novel objects paired with high salience familiar objects during referent selection (2 novel objects)
- low_salience_selection: Novel objects paired with low salience familiar objects during referent selection (2 novel objects)
- high_salience_test: Test trials for novel objects learned in high salience condition
- low_salience_test: Test trials for novel objects learned in low salience condition


4. **Importing decisions.**

- Time column truncation: We truncated columns at F4367 (maximum coded timepoint), as trials never extend beyond this point
- AOI coding from iCoder was mapped as follows:
  - "0" → "distractor"
  - "1" → "target"
  - "0.5" → "other"
  - "." → "missing"
  - "-" → "missing"
- Flipped left and right target sides from the raw data, as iCoder has those flipped
- Filler trials (marked with condition = "Filler", showing hot air balloons and oceans) were not imported since they did not feature two items
- Full phrase construction: Carrier phrases were reconstructed from codes, with punctuation determined by carrier type:
  - "Where is the X?" (question mark for "Where" phrases)
  - "Find the X!" and "Look at the X!" (exclamation mark for others)
- The paper randomized which label went with which novel image across participants ("The assignment of novel words to novel objects was counterbalanced across children") - thus we have stimuli that have the same image but different labels.
  - This is the reason why we have that many trials types
  - It appears they did not do a full 4x4 counterbalancing here, but only creates 8 of the possible 16 pairings
  - Also, all of the novel items appear as both target and distractors, so the randomized labels don't cause problems as distractor-only items.

5. **Importing ambiguity.**

- The dataset contains 44 participants while the published paper reports N = 36 (maybe some piloting data).
- A comment stated that the screen had a 140cm diagonal - feels unlikely?
