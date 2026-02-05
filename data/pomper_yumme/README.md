# pomper_yumme README

1. **Reference**

Pomper, R., & Saffran, J. R. (2018). More than distractors: Familiar objects influence toddlers' semantic representations in novel word learning. XXI Biennial International Congress of Infant Studies. Philadelphia, PA (poster).

2. **Abstract**

To learn a new word, children must first identify its referent. When shown a novel object and one or more familiar objects, children select the novel object as the referent of the novel word (e.g., Markman & Wachtel, 1988). Word learning, however, extends beyond this success in referent selection. Learning a novel word involves not only linking that word to a referent, but also making connections to other words and referents to form a semantic network. In past research on referent selection, familiar objects were selected predominantly (or exclusively) based on children’s knowledge of their labels, leading to eclectic sets of familiar objects (e.g., a cat, juicebox, and bus in Pomper & Saffran, under review). In more naturalistic environments, familiar objects co-occur in systematic ways (e.g., food in a kitchen). In the current experiment, we examine whether this systematicity affects novel word learning. Do children associate a novel word with its semantic context? Specifically, if a novel target and label appears together with a familiar food during learning, will children be more likely to associate it with eating? In the task, children learn novel labels in a mutual exclusivity-type design where they see a novel object (that looks edible) with a familiar object. The familiar object is either edible/ a food (e.g., grapes) or not (e.g., a crayon). In the test trials, infants participate in the LWL task where novel objects are paired (one of which appeared w/ something edible and one of which did not). In the key condition, infants hear a sentence that only refers to eating. There are also control trials that provide the explicit label for one of the target objects.

3. **Original study info**

- Two iterations (v4 and v5) from a study on how children use associations between familiar object semantics (e.g. foods) to support learning novel words. The study included a LWL component. On most trials, a familiar object appears in the presence of a novel object, and either the familiar object is labeled with its name or the novel object is labeled with a novel label.
- v5 began with a 4-trial teaching phase in which each of the novel objects was labeled in isolation (only one object on screen).
- v5 has only non-vanilla trials: a familiar object is always paired with a novel object
- v4 has two novel items and a more diverse mix of trial types:
-- Fam-Name: vanilla familiar object naming trials
-- Nov/Fam-ME(-Food/Non): mutual exclusivity trials with a familiar and novel object (either in the food category or not) - these are also considered "teaching" trials, since they are introducing the novel object labels
-- Fam-Verb: naming trials with an informative verb only, no familiar object label (e.g., "Which one can you kick?")
-- Nov-Eat: trials with two novel objects and an informative verb
-- Nov-Name: two novel objects, neutral carrier phrase

4. **Importing decisions**

- v5 data is in iCoder format, v4 is in long format, presumably from a later postprocessing stage
- Time column truncation: We truncated columns at F5400 (maximum coded timepoint), as trials never extend beyond this point
- AOI coding from iCoder for v5 was mapped as follows (basically the same for v4, NA -> missing):
  - "0" → "distractor"
  - "1" → "target"
  - "0.5" → "other"
  - "." → "missing"
  - "-" → "missing"
- Flipped left and right target sides from the raw data for v5, as iCoder flips the locations for easier coding
- Removed teaching trials from v5 dataset (only one (novel) image on screen, labeled)
- for full_phrase (both v4 and v5): created stimulus mapping files between audio names and full_phrase based on listening to the original stimuli. Only added the carrier phrase + target, did not include additional attention getters in the recording.
- v5: some familiar distractors are never named; for these, we use the image label as the stimulus label
- v5: image description is a little whimsical, trying to capture spirit of original images
- v4: we use the participant file and the trial orders to join in meta information
- v4: fixed a typo in the numbering of L2 orders on OSF (tr_num 9 was mistyped as 8)
- added some more detailed, but a little whimsical descriptions of the novel objects, based on the intent of the original study and the available image files
- decided to keep participant 402/ mark as not excluded, because the participant spreadsheet notes that the LWL data was "fine"
- note that the eyetracking data we received appears to contain *only* included participants - we still retain some code for marking potential exclusions within the import codebase, in case ever useful

5. **Importing ambiguity**

- v4: For Fam-Verb/Fam-Nov trials, we keep the target label as the verb ("Which one can you eat?"), but it's a little unclear what label to assign to the distractors (for the purposes of the stimulus table). Assigning them their respective noun label in the context of the experiment in the absence of a better option.
- v4: data is a mix of eyetracking data (tobii) and manual gaze coding. Opted to classify this as "preprocessed eyetracking"