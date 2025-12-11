1. **Reference.**

Weaver H, Saffran JR. Interrogating Early Word Knowledge: Factors That Influence the Alignment Between Caregiver-Report and Experimental Measures. Dev Sci. 2026 Jan;29(1):e70088. doi: 10.1111/desc.70088. PMID: 41217046.

2. **Abstract.**

Questions about early word knowledge pervade the literature on both typical and atypical language trajectories. To determine which words an infant knows, researchers have relied on two types of measures—caregiver-report and eye-gaze behavior. When these measures are compared, however, their results frequently fail to converge, making it difficult to ascertain whether a given infant knows a given set of words. What are the reasons for these misalignments in gold-standard tasks that are designed to investigate the same underlying construct, and can convergent validity be improved? The current study was designed to investigate multiple methodological features of caregiver-report and looking-while-listening (LWL) tasks hypothesized to contribute to their alignment. American English-learning infants (18–20 months; N = 52) completed an LWL task assessing their understanding of eight early-acquired words. Caregivers reported their child's knowledge of the same eight words, as well as their confidence in their responses and the amount of time they spend with their child. Overall, caregivers’ reports of word knowledge did not predict infants’ eye-gaze behavior. However, the measures were more likely to be aligned when caregivers reported higher confidence in their responses. Caregivers’ reports about both the target and the distractor word on each trial were related to infants’ eye-gaze behavior, suggesting that LWL tasks capture knowledge about the labels of both objects tested, not just the label of the target object. The results suggest several critical methodological modifications that could be implemented to improve the measurement validity of both caregiver-report and eye-gaze measures of word comprehension.

3. **Original study info.**

Participants: 52 infants, 18–20 months (M = 18.81), 28 female

Methods:
- LWL task with 8 words (apple, ball, crayon, flower, glasses, peas, sock, toothbrush), 32 trials, 4 different images per word
- Unyoked design: each target paired with different distractors across trials (no fixed target-distractor pairs)
- Tobii X60 eyetracker at 60Hz, noun onset at 2850ms after trial start
- MB-CDI short form + study-specific vocabulary checklist with per-word confidence ratings


4. **Importing decisions.**

AOI computation: The original data doesn't feature coded AOI values for "other". Thus, we ignore the AOIs provided by the authors and recomputed AOI from coordinates with an "other" category:
- If GazePointXMean or GazePointYMean is NA -> "missing"
- If coordinates fall within left or right bounding box -> "Left"/"Right"
- Otherwise -> "other"

Exclusions are marked based on the `Include?` column in subject log. Only `N` = excluded; the values `Y`, `y`,  `M` (maybe), or empty/NA are treated as included.

The data indicated that the "visit#" column in the demographics is NOT referring to multiple visits for the same study.

The paper featured data from eyetrackers and some kids with tracking issues were handcoded. The raw data we were provided includes the automatically tracked kids, but not the backup handcoded data.
It follows that our (included) sample size differs from the one reported in the paper.

We reconstructed the four conditions found in the paper. This approach resulted in an uneven number of trial types, as not every wordpair appeared in every constellation (e.g. "ball" was understood by every participant, meaning pairs including it would never appear in the condition "comprehends neither").


5. **Importing ambiguity.**

AOI interpolation: The original study applied 300ms gap-filling (18 frames at 60Hz) to fill in missing frames. Right now, we are reconstructing this step after the step in which we introduce the "other" category. This point is still up for debate.

The AOI regions are currently estimated based on the existing data. We are still waiting for the authors to get back to us to provide the actual reagionset.

