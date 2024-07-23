potter_canine readme

1. **Reference**
Potter, C. E., & Lew-Williams, C. (2023). Frequent vs. infrequent words shape toddlers’ real-time sentence comprehension. Journal of Child Language, 1-11. doi:10.1017/S0305000923000387

Contributors: Christine Potter, Casey Lew-Williams

2. **Paper abstract**
We examined how noun frequency and the typicality of surrounding linguistic context contribute to children’s real-time comprehension. Monolingual English-learning toddlers viewed pairs of pictures while hearing sentences with typical or atypical sentence frames (Look at the… vs. Examine the…), followed by nouns that were higher- or lower-frequency labels for a referent (horse vs. pony). Toddlers showed no significant differences in comprehension of nouns in typical and atypical sentence frames. However, they were less accurate in recognizing lower-frequency nouns, particularly among toddlers with smaller vocabularies. We conclude that toddlers can recognize nouns in diverse sentence contexts, but their representations develop gradually.

3. **Original study info**

There are data from two studies:
1. Canine1: Canine.n36.raw.txt
2. Canine2: rawLookingTimeData.n34.txt

Only the data from Canine2 is reported in the main manuscript. CanineOrderDesign.xlsx provides an overview of the orders for each study.

Key condition (and field name in raw data): 
Sentence frame that the stimuli is presented in (common or uncommon) and frequency of the object label (high-frequency or low-frequency). The field name is "Condition" in the raw data, which includes Common-High, Common-Low, Uncommon-High, and Uncommon-Low conditions. 

Image/Word pairs (high frequency/ low frequency) in Canine1:
dog - puppy, 
chicken - bird, 
cow - animal, 
baby - boy, 
milk - drink, 
shoe - boot, 
cookie - food, 
ball - toy 

Sentence Frames Manipulation:
*Common*	
Do you like…	
Look at the	
Where is…	
Can you find…	

*Uncommon*
Would you prefer…
Examine the…
Which side has
Could you spot…


4. **Importing decisions.** 
- original_stimulus_label for distractor stimuli always the condition (in trial_types), so if the target was in the high-frequency condition, than the distractor chosen from the stimulus table was the high-frequency label-object pairing.
- truncate columns at the timepoint 3600, since trials were rarely coded after this point
- left/ right locations were reversed to reflect the participant's perspective, because iCoder files encode left/ right from the perspective of the coder (who has a frontal view of the infant)
-point of disambiguation set to zero: original data was set at point of disambiguation, so it was re-centered to zero here.
- "offs" and "aways" ("." and "-" in the raw data) were coded as missing. Below is an overview of what each coded value represents in iCoder data:
        - “0” ~“distractor” (look to distractor)
        - “1” ~ “target” ( look to target )
        - “0.5” ~ “other” (child is fixating the center of the screen)
        - “.” ~ “missing” (This entry means a shift/ “off” in the language of iCoder. This was coded as “missing” because it can mean multiple things, including that gaze was not codable)
        - “-” ~ “missing” (This entry means “away” in the language of iCoder. In practice, this can also correspond to multiple looking situations; e.g., child is turned away from the screen; very long blinks. All of these situations should correspond to the situation “missing”)
- trials with low-frequency label-object pairing are treated as vanilla.

5. **Importing ambiguity.** 
- note the ambiguity with respect to the distractor_id choice.