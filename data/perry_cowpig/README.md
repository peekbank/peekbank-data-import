# perry_cowpig README

1. **Reference**

    Perry, L. K., & Saffran, J. R. (2017). Is a pink cow still a cow? Individual differences in toddlers' vocabulary knowledge and lexical representations. Cognitive Science, 41(4), 1090-1105. doi: 10.1111/cogs.12370


2. **Abstract**

    When a toddler knows a word, what does she actually know? Many categories have multiple relevant properties; for example, shape and color are relevant to membership in the category banana. How do toddlers prioritize these properties when recognizing familiar words, and are there systematic differences among children? In this study, toddlers viewed pairs of objects associated with prototypical colors. On some trials, objects were typically colored (e.g., Holstein cow and pink pig); on other trials, colors were switched (e.g., pink cow and Holstein-patterned pig). On each trial, toddlers were directed to find a target object. Overall, recognition was disrupted when colors were switched, as measured by eye movements. Moreover, individual differences in vocabularies predicted recognition differences: Toddlers who say fewer shape-based words were more disrupted by color switches. "Knowing" a word may not mean the same thing for all toddlers; different toddlers prioritize different facets of familiar objects in their lexical representations.

3. **Original study info**

    Key condition (and field name in raw data): whether or not the object is of the "familiar" color (recoded as 'typical_color') or an odd color ('atypical_color'; e.g. a pink cow), and whether it's the labeled object ("target") or the unlabeled object ("distractor")

4. **Importing decisions**
    - Each row in stimulus table represents a unique label-object pairing (see below for some resulting questions that arise with distractor_id)
    - "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
        - “0” ~ “distractor” (look to distractor)
        - “1” ~ “target” ( look to target )
        - “0.5” ~ “other” (child is fixating the center of the screen)
        - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as “missing” because it can mean multiple things, including that gaze was not codable)
        - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
    - left/right locations were reversed to reflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)

156, 149, and 136 are participants that are included in the eyetracking data abut have no age in their cdi scores. As we require ages for cdi scores to be valid, we use the age median age of the other cdi entries as imputed scores (20 months in this case)

5. **Importing ambiguity**
    - Stimulus information was used to add picture-by-picture `image_description`.
