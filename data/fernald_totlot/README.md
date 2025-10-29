# fernald_totlot readme

1. **Reference.**
Fernald, A., Perfors, A., & Marchman, V. A. (2006). Picking up speed in understanding: Speech processing efficiency and vocabulary growth across the 2nd year.Developmental Psychology, 42(1), 98–116.

Contributors: Virginia Marchman

2. **Abstract.**

To explore how online speech processing efficiency relates to vocabulary growth in the 2nd year, the authors longitudinally observed 59 English-learning children at 15, 18, 21, and 25 months as they looked at pictures while listening to speech naming one of the pictures. The time course of eye movements in response to speech revealed significant increases in the efficiency of comprehension over this period. Further, speed and accuracy in spoken word recognition at 25 months were correlated with measures of lexical and grammatical development from 12 to 25 months. Analyses of growth curves showed that children who were faster and more accurate in online comprehension at 25 months were those who showed faster and more accelerated growth in expressive vocabulary across the 2nd year.

3. **Original study info.**

Condition info:

18 months:
* whole: vanilla (just regular trials)
* yfill: also vanilla
* gated: these are the "cut off" words, non-vanilla
* zteach: this is a teaching trial in which a novel object is mapped to a novel label (nonce or kreeb) - non-vanilla
* (a)learn: this is a test trial for the novel labels - non-vanilla

21 months:
* xfacil: these are trials in which the verb facilitates the recognition of the label ("eat the cookie") - non-vanilla
* whole: regular vanilla trials
* look: these are matched to the xfacil trials, e.g. "Look at the cookie" instead of the xfacil trial "eat the cookie". These are vanilla, with the caveat that I'm not entirely sure where F0 is set within the trial (beginning of the trial or onset of cookie?). We should look carefully at these specific trials to clear this up.
gated: as before, non-vanilla (truncated words)
* new: these are mutual exclusivity-style trials, non-vanilla
* ylearn: novel word trials, non-vanilla

25 months:
losse: this is like "gated", non-vanilla
hard: vanilla - this just means that the specific words were "harder"
nice/super/pretty/none - all vanilla; these just mean that an additional adjective was shown (except for the none trials, these are just paired with the pretty trials e.g.), but I think it is safe to consider these all vanilla for our purposes. Let's also have a close eye on the timing of the curve inflection for these, just to make sure the points of disambiguation are right

Important note: the script currently filters out the "super" trial as the looking score for that trial is constantly above 60%, which is odd. Wait for an explanation to emerge (hopefully)


4. **Importing decisions.**

- word_onset column was set as F0 (i.e. the word onset column in the canonical icoder format)
- no pre-word onset coded data seems to be available
- super trials not imported (see above)

5. **Importing ambiguity.**

- 

