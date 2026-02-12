# sander-montant_2022 dataset

## Reference
Sander-Montant, A., Byers-Heinlein, K., & Perez, M. L. (2023). The more they hear the more they learn? Using data from bilinguals to test models of early lexical development. Cognition, 238, 105525.

## Abstract
Children have an early ability to learn and comprehend words, a skill that develops as they age. A critical question remains regarding what drives this development. Maturation-based theories emphasise cognitive maturity as a driver of comprehension, while accumulator theories emphasise children's accumulation of language experience over time. In this study we used archival looking-while-listening data from 155 children aged 14-48 months with a range of exposure to the target languages (from 10% to 100%) to evaluate the relative contributions of maturation and experience. We compared four statistical models of noun learning: maturation-only, experience-only, additive (maturation plus experience), and accumulator (maturation times experience). The best-fitting model was the additive model in which both maturation (age) and experience were independent contributors to noun comprehension: older children as well as children who had more experience with the target language were more accurate and looked faster to the target in the looking-while-listening task. A 25% change in relative language exposure was equivalent to a 4 month change in age, and age effects were stronger at younger than at older ages. Whereas accumulator models predict that the lexical development of children with less exposure to a language (as is typical in bilinguals) should fall further and further behind children with more exposure to a language (such as monolinguals), our results indicate that bilinguals are buffered against effects of reduced exposure in each language. This study shows that continuous-level measures from individual children's looking-while-listening data, gathered from children with a range of language experience, provide a powerful window into lexical development.

## Original study info
The study was an archival study combining five distinct studies (details in the [Appendix](https://osf.io/b4ja5))
These studies are listed as follows (with updated citations), along with importing approaches.

- Schott et al. ([2022](https://osf.io/preprints/osf/hgdvq), imported): On 26-month-olds. Key manipulation is correct vs incorrect noun pronunciation of cognate and non-cognate words in English and French.
- Byers-Heinlein et al. ([2017](https://doi.org/10.1073/pnas.1703220114), **NOT** imported): On 20-month-olds. Key manipulation is code-mixing within/across sentences. Excluded because of partial overlap with byers-heinlein_2017 (subject_ids were not consistent between these, so overlapping administrations could not be identified).
- unpublished (imported): On 14-month-olds. Same paradigm as BH17. 
- Kremin et al. ([2023](https://doi.org/10.34842/zyvj-cv60), **NOT** imported): On 36-month-olds. Key manipulation is code-mixing in prenominal adjective before target noun. Excluded because original paper has Eng-Fra and Eng-Spa data, and only the former were included in this paper.
- Byers-Heinlein et al. ([2022](https://osf.io/preprints/psyarxiv/298cz), **NOT** imported): On 36-month-olds. Key manipulation is code-mixing in familiar + novel noun. Excluded because original paper has Eng-Fra and Eng-Spa data, and only the former were included in this paper.

Thus, we only imported two of the component five datasets. 
BH17 was already import, and we will import Kremlin23 and BH22 separately.

The data were collected with a Tobii T60-XL eyetracker using a 60Hz sampling rate.
All studies were conducted in a mix of English and French on English-French bilinguals.

## Importing decisions
Stimuli images are imported only for the for the unpub dataset (as with BH17).
Stimuli are in principle available for S+22, but the OSF is still private.

We included importing decisions from the [processing script](https://osf.io/2ysab), including the renaming of projects and media names, as well as one participant relabelling.
There was an additional participant that had two sets of trials; we assumed these data were actually from two participants and made a guess that the first set of data was from a participant (previously thought to be) missing data.

Some trials also had a media name--trial order pairing that was not in the trial specification from the paper. 
We excluded these as potential errors.

Monitor size was set as 1920x1080 (based on the studies), although note that the Tobii export has 1920x1200.

Stimuli and trial properties in trial_info were coded off of the image/trial naming system. 

There is no full_phrase data available. However, by looking at other datasets from that lab and asking people who worked with them, we conclude that the full phrases don't hurt the vanilla status of the trials (e.g. double word onset)

Construction of trial_info file:
(for schott)
* trial info was reconstructed from the pre-prints/papers and from the available files
according to AOI_maker.xlsx the files with names like Cmisp_n2o2_cooC_e4T24.avi the "C" or "M" after "coo" refers to C=correct pronounciation and M=mispronunciation
according to 4_other_exploratory_analyses (https://osf.io/2m345/files/g6p8a) the "coo" are three letter abbreviations for the target names (mapping included)
* mispronunciations and cognate/non-cognateness as well come from Table 2 of https://osf.io/preprints/osf/hgdvq_v1
* not sure where the distractor-target pairings came from, but probably in https://osf.io/n9uv4/ (note that this is access denied, sad)

(for unpublished, looking at bh17):
* files with mixed or "sw" (for switch) are mixed
* ones with Fr or Eng (or unmarked) are single
* (other facts probably come from looking at the raw stimuli files in bh17?)
* note that trials about dog on teebo (or whatever) are listed in trial_info, but not included in the imported trials

## Importing ambiguity
None other than those reported above.
