1. Reference

Moore, C., & Bergelson, E. (2022). Examining the roles of regularity and lexical class in 18–26-month-olds’ representations of how words sound. Journal of Memory and Language, 126, 104337.
https://doi.org/10.1016/j.jml.2022.104337

2. Abstract

By around 12 months, infants have well-specified phonetic representations for the
nouns they understand, for instance looking less at a car upon hearing ‘cur’ than 
‘car’ (Swingley and Aslin, 2002). Here we test whether such high-fidelity 
representations extend to irregular nouns, and regular and irregular verbs.
A corpus analysis confirms the intuition that irregular verbs are far more common
than irregular nouns in speech to young children. Two eyetracking experiments 
then test whether toddlers are sensitive to mispronunciation in regular and 
irregular nouns (Experiment 1) and verbs (Experiment 2). For nouns, we find a 
mispronunciation effect and no regularity effect in 18-month-olds. For verbs, 
in Experiment 2a, we find only a regularity effect and no mispronunciation effect
in 18-month-olds, though toddlers’ poor comprehension overall limits interpretation. 
Finally, in Experiment 2b we find a mispronunciation effect and no regularity effect 
in 26-month-olds. The interlocking roles of lexical class and regularity for 
wordform representations and early word learning are discussed.

3. Original study info

This dataset currently contains data from Experiments 1,  2a,  and 2b.

All experiments compare 
* correct and incorrect pronunciation of 
* regular and irregular words. 

Experiment 1 includes younger (16-20 month) kids tested on nouns.
Experiment 2a includes younger (16-20 month) kids tested on verbs.
Experiment 2b includes older (24-28 month) kids tested on verbs. 

32 test trials (2 repeats of ) correct v incorrect x 8 words

4. Importing decisions

    # Stimuli

    When creating the stimuli table, we defined a target stimulus to be a combination of:

    - Label (verb). This includes the pronunciation. So, “jump” and “joomp” are considered to be different labels.
    - Carrier phrase. This information was redundant with the label information in this experiment.
    - Image (video) associated with the label. There is a “jump” video but there isn’t a separate “joomp” video.

    For example, label "joomp" spliced into "Look! She can ..." and the associated "jump" video constitute one target stimulus which we will label as "jump_can-joomp" in the "lab_stimulus_label" column.

    Distractors, however, don't have a label or a carrier phrase associated with them and so there isn’t an obvious target stimulus we could assign to represent them. We could have arbitrarily selected one of the two target stimuli that use the same video; for example, the one with the correctly pronounced verb. Instead, we added distractors as separate stimuli and gave them labels like “jump_distractor”.

    Mispronuniciations are marked as "novel" stimulus even if the image is familiar. 

    there are some video files for the verbs (expt 2) in the osf raw data but 
     A) peekbank is not build with stimulus videos in mind and
     B) they would need manual editing anyway, since they are not split into separate videos yet
     so they do not go in the file_path which is for images only. 

    # Trial types

    The target word onset time varied considerably between trials. Because of that, we ended up with as many trial types as there were trials.
    
    The noun trials (CP and MP conditions) are consistent in onset and offset times (-2500 -- 4000 ms compared to point of disambiguation). 
    The verb trials generally start around -2500 but range: -5000 to 0 with some outliers beyond that; and generally end at 4000ms, again with some outliers. 
    We don't know what happened on trials where the time course starts after pod, but these trials are rare. 
    
    # AOI region sets

    Points with x coordinate 640 were assigned to the “left” AOI, and with 640.1 and above - to the right AOI. To represent that in the table, we would need to set r_x_min to 640.1 or any other number larger than 640 and not larger than 640.1. The table columns are interger, however, so we could either go with 640 or 641. We went with 640.

    even though the location of the stimuli is given in pixels, the paper makes it clear that the screen was split in two halfs for the eyetracking and the aois were counted that way

5. Importing ambiguity

All issues are described in “Importing decisions” above.
