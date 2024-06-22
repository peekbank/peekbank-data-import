# xsectional_2007 README

1. **Reference**

Hurtado, N., Marchman, V. A., & Fernald, A. (2007). Spoken word recognition by Latino children learning Spanish as their first language. Journal of Child Language, 34(2), 227-249. https://doi.org/10.1017/S0305000906007896

2. **Abstract**

Research on the development of efficiency in spoken language under- standing has focused largely on middle-class children learning English. Here we extend this research to Spanish-learning children (n=49; M=2;0; range=1;3–3;1) living in the USA in Latino families from primarily low socioeconomic backgrounds. Children looked at pictures of familiar objects while listening to speech naming one of the objects. Analyses of eye movements revealed developmental increases in the efficiency of speech processing. Older children and children with larger vocabularies were more efficient at processing spoken language as it unfolds in real time, as previously documented with English learners. Children whose mothers had less education tended to be slower and less accurate than children of comparable age and vocabulary size whose mothers had more schooling, consistent with previous findings of slower rates of language learning in children from disadvantaged backgrounds. These results add to the cross-linguistic literature on the development of spoken word recognition and to the study of the impact of socioeconomic status (SES) factors on early language development.

3. **Original study info**
Straightforward LWL, all "vanilla" trials of familiar object/labels in Spanish.

4. **Importing decisions**
- "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
  - “0” ~“distractor” (look to distractor)
  - “1” ~ “target” ( look to target )
  - “0.5” ~ “other” (child is fixating the center of the screen)
  - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as   - “missing” because it can mean multiple things, including that gaze was not codable)
  - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
- left/ right locations were reversed to reflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)


5. **Importing ambiguity**
- paper says there were 16 test trials, but some administrations have up to 24 trials (average 20.61 trials per administration)
- no trial-level exclusion criteria included in data
- CDI was administered, but we don't have data (yet) (from the paper: "Spanish-language adaptations of the MacArthur-Bates Communicative Development Inventory (CDI) were used to gather parental report data on children’s lexical development. For children younger than 1;6, parents completed the MacArthur-Bates Inventarios del Desarrollo de Habilidades Comunicativas: Inventario I; for children 1;6 and older, parents completed Inventario II (Jackson-Maldonado et al., 2003).")
