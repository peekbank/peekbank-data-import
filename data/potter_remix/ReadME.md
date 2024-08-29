# potter_remix README

1. **Reference**

Potter, C.E., Fourakis, E., Morin-Lessard, E., Byers-Heinlein, K., & Lew-Williams, C. (2019). Bilingual toddlers’ comprehension of mixed sentences is asymmetrical across their two languages. Developmental Science, 22(4), e12794.

2. **Abstract**

In bilingual language environments, infants and toddlers listen to two separate languages during the same key years that monolingual children listen to just one and bilinguals rarely learn each of their two languages at the same rate. Learning to understand language requires them to cope with challenges not found in monolingual input, notably the use of two languages within the same utterance (e.g., Do you like the perro? or ¿Te gusta el doggy?). For bilinguals of all ages, switching between two languages can reduce the efficiency in real-time language processing. But language switching is a dynamic phenomenon in bilingual environments, presenting the young learner with many junctures where comprehension can be derailed or even supported. In this study, we tested 20 Spanish-English bilingual toddlers (18- to 30-months) who varied substantially in language dominance. Toddlers' eye movements were monitored as they looked at familiar objects and listened to single-language and mixed-language sentences in both of their languages. We found asymmetrical switch costs when toddlers were tested in their dominant versus non-dominant language, and critically, they benefited from hearing nouns produced in their dominant language, independent of switching. While bilingualism does present unique challenges, our results suggest a united picture of early monolingual and bilingual learning. Just like monolinguals, experience shapes bilingual toddlers' word knowledge, and with more robust representations, toddlers are better able to recognize words in diverse sentences.

3. **Original study info**

- Four main trial types tested: an english sentence with an english target word (called english_to_english in the script when importing data), an english sentence with a spanish target word (english_to_spanish), a spanish sentence with a spanish target word (spanish_to_spanish), and a spanish sentence with an english target word (spanish_to_english).
- the main conditions were either "same" (spanish_to_spanish and english_to_english) or "mix" (english_to_spanish and spanish_to_english)

4. **Importing decisions**
    - time columns truncated at 3900 ms (since only missing data after this point)
    - "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
        - “0” ~“distractor” (look to distractor)
        - “1” ~ “target” ( look to target )
        - “0.5” ~ “other” (child is fixating the center of the screen)
        - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as “missing” because it can mean multiple things, including that gaze was not codable)
        - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
    - flipped left-right to participant's perspective: in the data, it is labeled from the coder's perspective
    - when the order column in the initial data contained "Mix_e", it was labeled english_to_english if the condition was "same" or english_to_spanish if the condition was "mix"
    - When "Mix_s", labeled spanish_to_spanish if the condition was "same" or spanish_to_english if the condition was "mix"
    - the data was initially imported with only the english labels, so the spanish labels were hard-coded and matched to the english labels in a dictionary-like style, with reference to the published data.
    - "full_phrase_language" was set to the language of the carrier phrase (excluding the target word)
    - distractor_id was matched based on the target label's language (so if the distractor object was a balloon and the target label was in Spanish, the implicit distractor label was determined to be globo)

5. **Importing ambiguity**
    - unknown whether the other components of the order column in the initial import had significance, so they were disregarded (ex. "Mix_e1_wc" vs. "Mix_e2_wc" vs. "Mix_e1" etc.)
    - how to handle distractor_id is unclear (i.e., what label to implicitly assign to distractor)

