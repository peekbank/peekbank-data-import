# weaver_zettersten_2024 README

1. **Reference.**
Weaver, H., Zettersten, M., & Saffran, J. (2024). Becoming word meaning experts: Infants’ processing of familiar words in the context of typical and atypical exemplars. Child Development, 95(5), e352-e372.

Contributors: Martin Zettersten

2. **Abstract.**

How do infants become word meaning experts? This registered report investigated the structure of infants' early lexical representations by manipulating the typicality of exemplars from familiar animal categories. 14- to 18-month-old infants (N=84; 51 female; M=15.7 months; race/ethnicity: 64% White, 8% Asian, 2% Hispanic, 1% Black, and 23% multiple categories; participating 2022–2023) were tested on their ability to recognize typical and atypical category exemplars after hearing familiar basic-level category labels. Infants robustly recognized both typical (d=0.79, 95% CI [0.54, 1.03]) and atypical (d=0.70, 95% CI [0.46, 0.94]) exemplars, with no significant difference between typicality conditions (d=0.14, 95% CI [−0.08, 0.35]). These results support a broad-to-narrow account of infants' early word meanings. Implications for the role of experience in the development of lexical knowledge are discussed.

3. **Original study info.**

- condition: Images were either of typical (e.g. robin) or atypical (e.g. heron) category exemplars.
- Both typical and atypical trials are treated as vanilla because the general design follows a canonical LWL structure (non-informative carrier phrases, familiar words, typical event timing)
- the data was collected on Lookit/ Children Helping Science, so there are many trials with timing issues. Here, we rely on the time correction steps and the trial/ participant exclusions in the original paper, since these are well-documented in the open data from the study. However, there are some residual unusual (shorter and longer) trial times for a small set of trials.

4. **Importing decisions.**
- vanilla trials: we decided to keep atypical trials as vanilla. This also fits with the fact that there were no differences for typical vs. atypical trials in the original study.
- removed a tiny set of trials (3 total) where an audio file reference was missing (either these trials ended early and one is unexplained)
- used the column "time_normalized_corrected" in the original data as the normalized time column. This column also accounted for lags in audio timing

5. **Importing ambiguity.**

none 