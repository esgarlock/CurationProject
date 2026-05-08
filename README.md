# CurationProject

  Anecdotes about questionable MEDLINE indexing circulate among health/medical librarians, and the National Library of Medicine works to curate them. On the basis of external librarian feedback, ‘red flags’ no longer automatically results in the MeSH ‘Emblems and Insignia’; ‘sex assigned at birth’ no longer invokes ‘Infant’.
“As of April 2022, all journals indexed for MEDLINE are done by automated indexing, with human review and curation of results as appropriate.”  
  
We seek to illuminate the character of this curation, as details are challenging to access. A record’s indexing can change from one day to the next, with no indication of changes other than the Modification Date-[LR] field. Search retrieval may be impacted by reindexing; the status quo of obscurity leaves users in the dark.  
  
Using the NLM e-utilities API, we downloaded every record added to PubMed over five days (n = 25,439), and continue to re-download them regularly. Scripts were implemented in R to identify records that changed method-of-indexing and isolate any MeSH added or removed. We summarize and describe this data.  
  
We present the world’s first public PubMed indexing changelog. This work is fundamental to gauging the impacts of changes-to-indexing on search results; these changes constitute data points for future research. Reindexing is metadata errata; greater accessibility and transparency empower users to engage more fully with literature platforms.


## Files

CSV: [Most Recent Comparison Document](results/all_comparisions_2026-05-06.csv) (includes indexing methods and indexing revision date for gallery)  
CSV: [Titles and Abstracts for Gallery](https://github.com/esgarlock/CurationProject/blob/80ff762a73828220350c1cd8f178135fe25daf93/gallerystuff/TitlesAbstracts_Changed.csv)  
Figure: [How long does it take something to get curated](https://github.com/esgarlock/CurationProject/blob/80ff762a73828220350c1cd8f178135fe25daf93/results/plots/change_timeline.png)  
Figure: [What percentage of terms for Final Indexing are a result of curation?](results/plots/added_terms.png)  
Figure: [What percentage of terms are removed from the original indexing after curation?](results/plots/removed_terms.png)  
*I know for the figures there are some overlapping labels. Working on that*

## Check Tags 

"adolescent",
             "adult",
             "aged",
             "aged 80 and over",
             "animals",
             "bees",
             "cats",
             "cattle",
             "chlorocebus aethiops",
             "chick embryo",
             "child",
             "child preschool",
             "dogs",
             "female",
             "guinea pigs",
             "cricetinae",
             "history of medicine",
             "horses",
             "humans",
             "infant",
             "infant, newborn",
             "male",
             "middle aged",
             "pregnancy",
             "rabbits",
             "sheep",
             "swine",
             "united states",
             "history 15th century",
             "history 16th century",
             "history 17th century",
             "history 18th century",
             "history 19th century",
             "history 20th century",
             "history 21st century",
             "history ancient",
             "history medieval",
             "mice",
             "rats",
             "young adult"  
[Source](https://lhncbc.nlm.nih.gov/LHC-publications/PDF/CheckTagsMeSHTermsResearchTechnicalReport.pdf)  
## Population Groups
"African People",
             "North African People",
             "Sub-Saharan African People",
             "Asian People",
             "Asian",
             "Central Asian People",
             "East Asian People",
             "North Asian People",
             "Southeast Asian People",
             "West Asian People",
             "Black People",
             "Black or African American",
             "Caribbean People",
             "Central American People",
             "Indians, Central American",
             "Ethnic and Racial Minorities",
             "European People",
             "Eastern European People",
             "Scandinavians and Nordic People",
             "Middle Eastern and North Africans",
             "Arabs",
             "Middle Eastern People",
             "North African People",
             "North American People",
             "American Indian or Alaska Native",
             "Population Groups, US",
             "Oceanians",
             "Australasian People",
             "South American People",
             "Indians, South American",
             "White People",
             "White"  
[Source](https://meshb.nlm.nih.gov/record/ui?ui=D044382)
             
