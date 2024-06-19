# pomper_yumme README

1. **Reference**

    Pomper, R., & Saffran, J. R. (unpublished).

2. **Abstract**

Do children associate a novel word with its semantic context? Specifically, if a novel target and label appears together with a familiar food during learning, will children be more likely to associate it with eating? In the task, children learn novel labels in a mutual exclusivity-type design where they see a novel object (that looks edible) with a familiar object. The familiar object is either edible/ a food (e.g., grapes) or not (e.g., a crayon). In the test trials, infants participate in the LWL task where novel objects are paired (one of which appeared w/ something edible and one of which did not). In the key condition, infants hear a sentence that only refers to eating. There are also control trials that provide the explicit label for one of the target objects.

3. **Original study info**

    Key condition (and field name in raw data): whether or not the target label referred to the shape/ object or the color of that object (no identifying column in raw data; used the order files to classify each trial).

4. **Importing decisions**
    - Each row in stimulus table represents a unique label-object pairing (see below for some resulting questions that arise with distractor_id)
    - "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
        - “0” ~“distractor” (look to distractor)
        - “1” ~ “target” ( look to target )
        - “0.5” ~ “other” (child is fixating the center of the screen)
        - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as “missing” because it can mean multiple things, including that gaze was not codable)
        - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
    - left/ right locations were reversed to refflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)

5. **Importing ambiguity**
    - distractor_ids were matched with target_ids based on condition (since a distractor never occurs with a label, but stimulus objects are object-label pairings, forcing a decision about which label to implicitly choose for a particular distractor object). E.g., if the target label was a color, the distractor_id chosen corresponded to the distractor object on the screen along with its respective color label.
