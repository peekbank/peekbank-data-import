# Newman and Morini 2017 README

1. Reference
Newman, R. S., & Morini, G. (2017). Effect of the relationship between target and masker sex on infants' recognition of speech. The Journal of the Acoustical Society of America, 141(2), EL164-EL169.

2. Abstract
When faced with multiple people speaking simultaneously, adult listeners use the sex of the talkers as a cue for separating competing streams of speech. As a result, adult listeners show better performance when a target and a background voice differ from one another in sex. Recent research suggests that infants under 1 year do not show this advantage. So when do infants begin to use talker-gender cues for stream segregation? These studies find that 16-month-olds do not show an advantage when the masker and target differ in sex. However, by 30 months, toddlers show the more adult-like pattern of performance.


3. Original study info
Expt 1 is on 18 30-month toddlers (+3 reported exclusions).

Target stream is a female voice attracting attention to the target object. Mixed with a distractor stream at 5dB signal to noise, either from a female or male voice. 

Each pair of objects occurred 5 times in the 20 trials (2 left, 2 right, 1 "look in general" = baseline). of the 16 test trials, 8 with male and 8 with female voice. 

Expt 2 is on 24 16-17-month toddlers (+8 reported exclusions). 

Same as expt 1, except that *pilot testing* indicated 5db was too hard, so it was 10db as well. 

The raw data makes it look like there's 5db and 0db for 30months and 10db and 5db for 16 months. 

4. Importing decisions:

3 participants who had less than the full set of 20 trials in the raw data were dropped of issues with trial order coding. However, some other participants seem to only have missing data for some of trials and are kept because the start times for all the trials were present?

30 frames have conflicting codes, they are resolved to the preceding coded area

full phrase is taken from raw audio, but this mismatches what is reported in the paper

The ambiguous baseline trials, where no object is named are removed because they are not of interest to peekbank (and don't fit the schema)

start frames is marked as 68 which is what is reported in the paper. (There was a note about potential ambiguity for whether this is onset of phrase or word, but the paper says target word.)


5. Importing ambiguity

unclear how to apply exclusions -- paper talks about some fussiness exclusions and one child excluded for a later autism diagnosis. Demographic data has a keep-drop column (with one YES for the autism diagnosis, and a handful of maybes and one notes about side bias) and session notes and fussiness scores, but it's unclear what happened with the maybes for fussiness. 

There are 156 kids listed in the demographic sheets, 97 kids that we have looking data for (not all of whom do we have demographic data for it seems). There's only a couple of the 97 who have anything about exclusions -- some maybe's and one who was excluded later for an autism diagnosis. So, my guess is that kids who were excluded for fussiness or language input don't have coded data at all. The one child excluded for autism diagnosis was marked as excluded, but we did not mark the kids who were labelled as "maybe" for exclude. 