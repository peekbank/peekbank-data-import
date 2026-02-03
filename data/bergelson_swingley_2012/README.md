1. **Reference.**
Bergelson, E., & Swingley, D. (2012). At 6–9 months, human infants know the meanings of many common nouns. PNAS, 109(9), 3253-3258.

2.  **Abstract.**
It is widely accepted that infants begin learning
their native language not by learning words, but by discovering
features of the speech signal: consonants, vowels, and combinations
of these sounds. Learning to understand words, as opposed to just
perceiving their sounds, is said to come later, between 9 and 15mo
of age, when infants develop a capacity for interpreting others’
goals and intentions. Here, we demonstrate that this consensus about
the developmental sequence of human language learning is flawed: in
fact, infants already know the meanings of several common words from
the age of 6 mo onward. We presented 6- to 9-mo-old infants with
sets of pictures to view while their parent named a picture in each
set. Over this entire age range, infants directed their gaze to the
named pictures, indicating their understanding of spoken words.
Because the words were not trained in the laboratory, the results
show that even young infants learn ordinary words through daily
experience with language. This surprising accomplishment indicates
that, contrary to prevailing beliefs, either infants can already
grasp the referential intentions of adults at 6 mo or infants can
learn words before this ability emerges. The precocious discovery of
word meanings suggests a perspective in which learning vocabulary
and learning the sound structure of spoken language go hand in hand
as language acquisition begins.

3.  **Original study info.**

6-9-mo-olds (n = 33, M = 7.45 mo, R = 5.99–10.00 mo; 19 female)
10-13-mo-olds (n = 30, M = 12.13 mo, SD = 1.08 mo; 18 females)
14-16-mo-olds (n = 7, M = 14.49 mo, SD = 1.04 mo; 3 females)
18-20-mo-olds (n = 13, M = 19.38 mo, SD = 0.86 mo; 6 females).

All native English hearing/speaking.

(This roughly matches our data, though our data shows 12 females for the
10-13-mo-olds while the paper claims 18, this could be a typo)

Participants saw 32 traditional LWL trials ("paired"/"sidebyside") and 16 scene
trials, in which 4 items/body parts were presented with one of them being a target.

16 target words:
- 8 food items (apple, banana, bottle, cookie, juice, milk, spoon, yogurt)
- 8 body parts (ear, eyes, foot, hair, hand, leg, mouth, nose)

The trials lasted about 7.5 seconds each. During the first 1-1.5 seconds of presentation,
parents were given a carrier phrase via headphones and repeated that carrier
phrase to the infant (1.5-2 seconds). Afterwards, the infant's gaze was captured
using an Eyelink CL eye-tracker at 500Hz. The paper analyzed data from 367-3500ms
after target onset.

4.  **Importing decisions.**

The carrier phrase and word were spoken by the parent in this study - they were
instructed in a standardized way, so we don't count this against vanilla status.

No exclusion information provided, so we treat all participants as included.
Given that the average trials per administration sits at around 24, some trials
were excluded before we got the data.

We don't import the scene trials, as they don't feature a binary forced choice
and are thus not transformable into the peekbank format.

We think there was a typo/data import error in the Picture names which we have normalized.
In particular, for all put one combination of target-stimulus + left-right position, 
there is only one distractor image and at least 22 subjects see it. 
And each target/distractor image is always paired with the same counterpart. 
However, for target "hair1" when it is on the Left, 
there is a different number banana as the distractor for each Subject (except two have the same). 
Our best guess is this is an error (possibly from a spreadsheet editor propogating labels incorrectly).
We believe that when "hair1" was the target it was always paired with "banana1".

5.  **Importing ambiguity.**

CDI: Both "cdi_reg" and "cdi_old" in the data are mapped to Words &
Gestures since all subjects were infants 6-20 months
  
Each participant has 500 timebins per trial. The KETtime column seems to number
timebins centered around the POD. We worked out these timebins to be 20ms each (50 Hz),
as multiplying the KETtime column by 20 results in a close recreation of the plots found
in the paper. 

We code timepoints where the "gaze" value in the original data is "NA"
as "other". The data does not distinguish between other and missing. In
the paired setup, the aois touch each other and stretch the entire
horizontal length of the presented scene, but there is space above and
below both of them on the screen that is non aoi. (In the scene setup,
there is plenty of "other" space between the objects, so the NA values
likely indicate that.)
