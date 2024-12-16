# kartushina_2019 README

1. **Reference**

Kartushina N, Mayor J. (2019) Word knowledge in six- to nine-month-old Norwegian infants? Not without additional frequency cuesR. Soc. Open Sci.6180711. http://doi.org/10.1098/rsos.180711

2. **Abstract**

The past 5 years have witnessed claims that infants as young as six months of age understand the meaning of several words. To reach this conclusion, researchers presented infants with pairs of pictures from distinct semantic domains and observed longer looks at an object upon hearing its name as compared with the name of the other object. However, these gaze patterns might indicate infants' sensibility to the word frequency and/or its contextual relatedness to the object regardless of a firm semantic understanding of this word. The current study attempted, first, to replicate, in Norwegian language, the results of recent studies showing that six- to nine-month-old English-learning infants understand the meaning of many common words. Second, it assessed the robustness of a ‘comprehension’ interpretation by dissociating semantic knowledge from confounded extra-linguistic cues via the manipulation of the contingency between words and objects. Our planned analyses revealed that Norwegian six- to nine-month-old infants did not understand the meaning of the words used in the study. Our exploratory analyses showed evidence of word comprehension at eight to nine months of age—rather than from six to seven months of age for English-learning infants—suggesting that there are cross-linguistic differences in the onset of word comprehension. In addition, our study revealed that eight- to nine-month-old infants cannot rely exclusively on single extra-linguistic cues to disambiguate between two items, thus suggesting the existence of early word-object mappings. However, these mappings are weak, as infants need additional cues (such as an imbalance in frequency of word use) to reveal word recognition. Our results suggest that the very onset of word comprehension is not based on the infants' knowledge of words per se. Rather, infants use a converging set of cues to identify referents, among which frequency is a robust (pre-semantic) cue that infants exploit to guide object disambiguation and, in turn, learn new words.

3. **Original study info**

- mean = 7.07 months, range from 6.05 to 8.77 months, girls n = 29
- monolingual Norwegian
- Thirty-two words were used in the study. Sixteen of them are the labels of the 16 objects used in the context (n = 8: cookie–belly, banana–hair, apple–foot, bread–leg) and in the frequency (n = 8: dog-glasses, cat–keys, car–couch, jacket–book) conditions. These words were used on matching trials in their respective conditions. Among the other 16 words, eight of them (spoon–bathtub, bottle-toothbrush, cup–pants, table–diaper), referring to food and bathroom/body-cleaning objects, were used on related trials in the context condition. The other eight words ( pacifier–pillow, ball–sun, phone–moon, water–carpet) are frequency related to the words used in the frequency condition; thus, they were used on related trials in the frequency condition
- Tobii TX300 eye-tracker, which has a sampling rate (binocular) of 300 Hz and a screen resolution of 1920 × 1080 pixels
- There were two experimental blocks, i.e. one for the context and one for the frequency condition, with 16 trials in each. The order of conditions was randomized across participants and there were no pauses between the conditions.


4. **Importing decisions**

Two participants saw more trials than intended:

OS_034
OS_091

Due to the position of the repeated stimuli, these could not have been caused by restarting the experiment halfway through. Furthermore the paper + the timestamps indicates that only one session took place, making it unlikely that the duplicated trials originate from multiple sessions or aggregate data for the participant.
Each participant saw each of the 32 targets once on a random side - meaning there were a total of 64 possible trial types across participants. All of the duplicate trials for these two participant have flipped sides. Our current hunch is that the 32 list of trials for these participatns accidentally was doubled to all 64 possible combinations of target & side - and the experiment was stopped manually once the experimenters figured out that too many items were shown. It is also noteworthy that both participants were included in the original study.
For the purposes of peekbank, the repeat trials do not present a problem, so we decided to include these two participants anyway.

There is also a subset of participants that saw less trials, but we also include these.


The original stimuli provided by the authors include the full image pairs presented to the participants. We have used a shell script using imagemagick to split them up into their separate parts.
If the split needs to be changed (crop, filetype, etc.), edit the script in this directory and run

```
./split_original_images.sh ./raw_data/stimulus_images/
```
to generate new splits.

5. **Importing ambiguity**



