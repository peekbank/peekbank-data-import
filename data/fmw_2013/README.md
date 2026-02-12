# fmw_2013 readme

1. **Reference.**
Fernald, A., Marchman, V. A., & Weisleder, A. (2013). SES differences in language processing skill and vocabulary are evident at 18 months. Developmental Science, 16(2), 234-248

Contributors: Virginia Marchman

2. **Abstract.**
This research revealed both similarities and striking differences in early language proficiency among infants from a broad range of advantaged and disadvantaged families. English-learning infants (n = 48) were followed longitudinally from 18 to 24 months, using real-time measures of spoken language processing. The first goal was to track developmental changes in processing efficiency in relation to vocabulary learning in this diverse sample. The second goal was to examine differences in these crucial aspects of early language development in relation to family socioeconomic status (SES). The most important findings were that significant disparities in vocabulary and language processing efficiency were already evident at 18 months between infants from higher- and lower-SES families, and by 24 months there was a 6-month gap between SES groups in processing skills critical to language development.

3. **Original study info.**
CDIs and LWL were collected at 18 and 24 months. For LWL, two overlapping sets of targets were used for the different age groups. The dataset indicates different carrier phrases (not reflected in the paper). Looking behaviour was recorded on video and coded offline frame-by-frame by a trained blind coder. 

There are more participants in this dataset than were reported in the paper:
* the default seems to have been two sessions around 18 months and 1 session around 24 months
* 33 subjects had 3 administrations (all 2 sessions around 18 months,  and 1 session around 24 months)
* 33 subjects had 2 administrations (19 with one at 18ish and 1 at 24 ish, 14 with 2 at roughly the same time)
14 subjects had just 1 administration (12 at 18mo, 2 at 24 mo)

We assume that the 48 kids reported in the paper are probably a subset of the 33+19 with data at both ~18mo and ~24 mo. 

4. **Importing decisions.**

- Data were re-zeroed based on "crit_on_set" (onset of critical period) variable.
- `distinct` was used to remove duplicates---suggests data error (e.g., participants with different sex labels at 18 and 24 months).
- AOIs labelled ".", "-", or NA were treated as missing (see also README for pomper_saffran_2016).
- **Note that stimuli table has image paths but images are not present on OSF.**

- Participant (20136) had an accidental double coding of a video, where session 2's data (TL2-2A) was likely the session 1 video coded again. We removed that participant from the data.


- Vanillaness (as of Jan 2026): 
Vanilla: vanilla-noun, familiar, U-prime-noun (unrelated prime), and Uninf-Adj-Noun are vanilla (Uninf is "red car" when both target and distractor are red)
Non-vanilla: Inf-Adj-Adj (blue car when the target but not the distractor is blue), R-prime-verb (eat the cookie)

Note that even though they are called "R-prime-verb" the trial has been rezeroed to when the noun starts (in the import script)
5. **Importing ambiguity.**

- What is the right point of disambiguation for trials with informative verbs or adjectives? Current decision is to always make the target noun onset the point of disambiguation.
- For CDI data with no specified age at administration, the age group (18mo / 24mo) was attached to the CDI data.

