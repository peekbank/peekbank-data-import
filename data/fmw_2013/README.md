# fmw_2013 readme

1. **Reference.**
Fernald, A., Marchman, V. A., & Weisleder, A. (2013). SES differences in language processing skill and vocabulary are evident at 18 months. Developmental Science, 16(2), 234-248

Contributors: Virginia Marchman

2. **Abstract.**
This research revealed both similarities and striking differences in early language proficiency among infants from a broad range of advantaged and disadvantaged families. English-learning infants (n = 48) were followed longitudinally from 18 to 24 months, using real-time measures of spoken language processing. The first goal was to track developmental changes in processing efficiency in relation to vocabulary learning in this diverse sample. The second goal was to examine differences in these crucial aspects of early language development in relation to family socioeconomic status (SES). The most important findings were that significant disparities in vocabulary and language processing efficiency were already evident at 18 months between infants from higher- and lower-SES families, and by 24 months there was a 6-month gap between SES groups in processing skills critical to language development.

3. **Original study info.**
CDIs and LWL were collected at 18 and 24 months. For LWL, two overlapping sets of targets were used for the different age groups. The dataset indicates different carrier phrases (not reflected in the paper). Looking behaviour was recorded on video and coded offline frame-by-frame by a trained blind coder. 

4. **Importing decisions.**

- Data were re-zeroed based on "crit_on_set" (onset of critical period) variable.
- `distinct` was used to remove duplicates---suggests data error (e.g., participants with different sex labels at 18 and 24 months).
- AOIs labelled ".", "-", or NA were treated as missing (see also README for pomper_saffran_2016).
- **Note that stimuli table has image paths but images are not present on OSF.**

- Participant (20136) had an accidental double coding of a video, where session 2's data (TL2-2A) was likely the session 1 video coded again. We removed that participant from the data.


5. **Importing ambiguity.**

- What is the right point of disambiguation for trials with informative verbs or adjectives? Current decision is to always make the target noun onset the point of disambiguation.
- For CDI data with no specified age at administration, the age group (18mo / 24mo) was attached to the CDI data.

