# perry_cowpig README

1. **Reference**

    Perry, L. K., & Saffran, J. R. (2017). Is a pink cow still a cow? Individual differences in toddlers' vocabulary knowledge and lexical representations. Cognitive Science, 41(4), 1090-1105. doi: 10.1111/cogs.12370


2. **Abstract**

    When a toddler knows a word, what does she actually know? Many categories have multiple relevant properties; for example, shape and color are relevant to membership in the category banana. How do toddlers prioritize these properties when recognizing familiar words, and are there systematic differences among children? In this study, toddlers viewed pairs of objects associated with prototypical colors. On some trials, objects were typically colored (e.g., Holstein cow and pink pig); on other trials, colors were switched (e.g., pink cow and Holstein-patterned pig). On each trial, toddlers were directed to find a target object. Overall, recognition was disrupted when colors were switched, as measured by eye movements. Moreover, individual differences in vocabularies predicted recognition differences: Toddlers who say fewer shape-based words were more disrupted by color switches. "Knowing" a word may not mean the same thing for all toddlers; different toddlers prioritize different facets of familiar objects in their lexical representations.

3. **Original study info**

    Key condition (and field name in raw data): whether or not the object is of the "familiar" color (recoded as 'typical_color') or an odd "test" color (recoded as 'atypical_color'; e.g. a pink cow), and whether it's the labeled object ("target") or the unlabeled object ("distractor")

4. **Importing decisions**
    - Each row in stimulus table represents a unique label-object pairing (see below for some resulting questions that arise with distractor_id)
    - "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
        - “0” ~ “distractor” (look to distractor)
        - “1” ~ “target” ( look to target )
        - “0.5” ~ “other” (child is fixating the center of the screen)
        - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as “missing” because it can mean multiple things, including that gaze was not codable)
        - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
    - left/right locations were reversed to reflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)

146, 149, and 130 are participants that are included in the eyetracking data abut have no age in their cdi scores. As we require ages for cdi scores to be valid, we use the age median age of the other cdi entries as imputed scores (20 months in this case)

Full phrases were not available in the data, the paper says the following about the structure (though this is not enough to reconstruct them on a trial level):
Toddlers saw drawings of two familiar objects on the screen (e.g., a cow and pig) and heard
infant-directed speech describing one object (e.g., “Where’s the pig?”) followed by 1s of
silence, an attention-getting phrase (e.g., “Can you see it?”), and another 1s of silence. The
target word (e.g., “pig”) occurred 2s into each trial

AOIs were coded past 3200ms from the target onset for only a single trial and so were removed.

5. **Importing ambiguity**
    - Stimulus information was used to add picture-by-picture `image_description`.
