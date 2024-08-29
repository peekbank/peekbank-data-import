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


5. **Importing ambiguity**


6. **Outstanding issues**

- We do not currenlty exclude any trials, although in the paper they report the following exclusion criteria:
    following criteria were used to exclude single trials: 
    (i) no looking at either image for at least 0.5 s in the post-naming period, and 
    (ii) no looking was recorded in the pre-naming period (as in [1]). 
    Also, we excluded trials in which the experimenter had reported that 
    (iii) the parent interfered (e.g. pointed to the screen, shifted his/her body or moved his/her chair), or
    (iv) the trial was interfered by a third person or due to a technical error. 
    Finally, individual picture-pair trials were removed from individual child data if the parents reported that they did not use one (or both) of the produced words with the child (or in his presence), since the child was born. For example, if a parent did not use the word ‘apple’, then all ‘apple–foot’ picture trials were removed from his/her child analyses.

- Stimulus path currently lists two possible images that could have been seen on a given trial (e.g., cookie_belly.png; belly_cookie.png when "cookie" is target word). Eventually, someone should go through and split into two trial types (cookie+left; cookie+right), but skipping that for now.

- Needs CDI data imported (data is in CDI tab in exp_info_path)


