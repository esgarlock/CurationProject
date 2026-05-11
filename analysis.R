library(tidyverse)
library(lubridate)
library(stringi)


#load in all the text files and convert to csv that we can analyze 
tbl <-
  list.files(path = "./rawtextfiles/",
             pattern = "\\.csv$",
             full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = "c"),col_names=FALSE, id="file_path"))%>%
  dplyr::select(1:2)%>%
  mutate(delim_num=str_count(X1,"\\|"))%>%
  dplyr::filter(delim_num<= 11)%>%
  dplyr::select(-delim_num)%>%
  separate(X1,into=c("PMID","Indexing","Title","Journal","ISO","ISSN","Keyword","Substances","PubDate","DateCompleted","DateRevised","all_mesh"),sep="\\|",extra="warn",fill="warn")

flowchart_total_records=nrow(tbl)
flowchart_total_pmid=length(unique(tbl$PMID))

#Get a dataset that shows the first indexing status recorded, and then the most recent indexing status recorded
indexing_change=tbl%>%
  dplyr::select(file_path,PMID,Indexing)%>%
  mutate_all(na_if," ")%>%
  distinct()%>%
  pivot_wider(id_cols=PMID,names_from = file_path,values_from = Indexing)%>%
  ungroup()%>%
  mutate(eq = apply(pick(2:16), 1, n_distinct, na.rm = T) <= 1 )%>%##CHANGE WHEN NEW DATA IS PULLED
  dplyr::filter(eq=="FALSE")%>%
  dplyr::select(-eq)%>%
  pivot_longer(cols=c(2:16))%>% ##CHANGE WHEN NEW DATA IS PULLED
  mutate(date=str_sub(name,-12,-5))%>%
  dplyr::select(PMID,date,value)%>%
  mutate(date=mdy(date))%>%
  drop_na(value)%>%
  #arrange(PMID,date)
  distinct()%>%
  group_by(PMID)%>%
  filter(date == min(date) | date == max(date))%>%
  mutate(rank = rank(date))%>%
  pivot_wider(id_cols=PMID,names_from=rank,values_from = value)%>%
  dplyr::select(PMID,"start"="1","end"="2")

change_pmid=indexing_change$PMID

flowchart_change_pmid_total=length(unique(change_pmid))


weirdcases=tbl%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  dplyr::select(PMID,Indexing,all_mesh)%>%
  mutate_all(na_if," ")%>%
  drop_na(Indexing)%>%
  distinct()%>%
  group_by(PMID)%>%
  count()%>%
  dplyr::filter(n>2)

weird_pmid=weirdcases$PMID
flowchart_weird_pmid=length(unique(weird_pmid))

#get the date of the data pull where the change occurred
change_date=tbl%>%
  dplyr::select(file_path,PMID,Indexing,DateCompleted,DateRevised)%>% 
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(DateRevised=ifelse(test=="fix",NA,DateRevised))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%#select columns
  dplyr::filter(PMID %in% change_pmid)%>%
  mutate(date_collected=str_sub(file_path,-12,-5))%>%
  dplyr::select(PMID,date_collected,DateCompleted,DateRevised,Indexing)%>%
  mutate(date_collected=mdy(date_collected))%>%
  drop_na(Indexing)%>%
  group_by(PMID,Indexing)%>%
  mutate(rank = rank(date_collected))%>%
  dplyr::filter(rank==1)%>%
  ungroup()%>%
  group_by(PMID)%>%
  dplyr::filter(date_collected==max(date_collected))


#get the data set up for the loops 
loop_setup=indexing_change%>%
  filter(!PMID %in% weird_pmid)%>%
  left_join(tbl,by="PMID")%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  dplyr::select(PMID,start,end,Indexing,all_mesh)%>%
  mutate(all_mesh=gsub(",","",all_mesh))%>%
  distinct()%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(5:6),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  distinct()%>%
  pivot_longer(cols=c(2:3))%>%
  filter(Indexing==value)%>%
  dplyr::select(PMID,name,term)%>%
  distinct()%>%
  pivot_wider(id_cols=PMID,values_from = term,names_from = name)%>%
  dplyr::select(PMID,start,end)

flowchart_pmids_checked=length(unique(loop_setup$PMID))
  
for(i in 1:nrow(loop_setup)) { #for every row in the dataframe, do this: 
  start_all=unlist(loop_setup[i,2])
  end_all=unlist(loop_setup[i,3])
  start_not_end_all=setdiff(start_all,end_all)
  end_not_start_all=setdiff(end_all,start_all)
  loop_setup[i,4]=toString(start_not_end_all)
  loop_setup[i,5]=toString(end_not_start_all)
}#CLOSE LOOP 

##Get this into a csv format 





#trying things out with just the weird PMIDS
#get the data set up for the loops 
  

weird_loop_setup=indexing_change%>%
  filter(PMID %in% weird_pmid)%>%
  left_join(tbl,by="PMID")%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  dplyr::select(PMID,DateRevised,Indexing,all_mesh)%>%
  drop_na(all_mesh)%>%
  distinct()%>%
  group_by(PMID)%>%
  mutate(indexing_version=rank(DateRevised))%>%
  dplyr::filter(indexing_version==1 | indexing_version==max(indexing_version))%>%
  mutate(indexing_version=ifelse(indexing_version==1,"start","end"))%>%
  ungroup()%>%
  mutate(all_mesh=gsub(",","",all_mesh))%>%
  distinct()%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  dplyr::select(-major_topic)%>%
  drop_na(all)%>%
  mutate(all= as.list(strsplit(all, "_")))%>%
  distinct()%>%
  pivot_wider(id_cols=PMID,values_from = all,names_from = indexing_version)%>%
  dplyr::select(PMID,start,end)
 
 
for(i in 1:nrow(weird_loop_setup)) { #for every row in the dataframe, do this: 
  w_start_all=unlist(weird_loop_setup[i,2])
  w_end_all=unlist(weird_loop_setup[i,3])
  w_start_not_end_all=setdiff(w_start_all,w_end_all)
  w_end_not_start_all=setdiff(w_end_all,w_start_all)
  weird_loop_setup[i,4]=toString(w_start_not_end_all)
  weird_loop_setup[i,5]=toString(w_end_not_start_all)
  
}#CLOSE LOOP 



all_comp_csv=rbind(loop_setup,weird_loop_setup)%>%
  left_join(indexing_change,by="PMID")%>%
  dplyr::select(PMID,"orig_status"="start.y","update_status"="end.y","orig_index"="start.x","update_index"="end.x","orig_not_update"="...4","update_not_orig"="...5")%>%
  mutate(orig_index = sapply(orig_index, toString))%>%
  mutate(update_index = sapply(update_index, toString))%>%
  mutate(orig_not_update = sapply(orig_not_update, toString))%>%
  mutate(update_not_orig = sapply(update_not_orig, toString))%>%
  left_join(change_date,by="PMID")%>%
  mutate(MultipleRevisions=ifelse(PMID %in% weird_pmid,"Multiple Revisions",NA))%>%
  dplyr::select(PMID,orig_status,update_status,MultipleRevisions,DateCompleted,DateRevised,orig_index,update_index,orig_not_update,update_not_orig)
 

current_date=Sys.Date()
all_comp_name=(paste0("results/all_comparisions_",current_date,".csv"))

#write.csv(all_comp_csv,all_comp_name,row.names = FALSE)
  


###LETS GET SOME ANALYSES GOING 
###Check tags####
check_tags=c("adolescent", "adult", "aged", "aged 80 and over", "animals", "bees", "cats", "cattle", "chlorocebus aethiops", "chick embryo", "child", "child preschool", "dogs", "female", "guinea pigs", "cricetinae", "history of medicine", "horses", "humans", "infant", "infant, newborn", "male", "middle aged", "pregnancy", "rabbits", "sheep", "swine", "united states", "history 15th century", "history 16th century", "history 17th century", "history 18th century", "history 19th century", "history 20th century", "history 21st century", "history ancient", "history medieval", "mice", "rats", "young adult")
#####

####Population Groups####
pop_groups=str_to_lower(c("African People",
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
             "White"))
#####

#what we have below shows whether or not there was an actual heading change or not 
all_changes_summary=all_comp_csv%>%
  ungroup()%>%
  dplyr::select(PMID,orig_status,update_status,"removed"="orig_not_update","added"="update_not_orig")%>%
  mutate_all(na_if,"")%>%
  mutate(headingchange=ifelse(is.na(added)& is.na(removed),"no","yes"))%>%
  dplyr::select(PMID,headingchange)%>%
  distinct()%>%
  group_by(headingchange)%>%
  count()

flowchart_heading_change_Y=as.character(all_changes_summary%>%filter(headingchange=="yes")%>%ungroup()%>%dplyr::select(n))
flowchart_heading_change_N=as.character(all_changes_summary%>%filter(headingchange=="no")%>%ungroup()%>%dplyr::select(n))

##LOOKING AT JUST HEADINGS####
#now look at the check tag changes and the as topic changes 

#NOTE WE ARE LOOKING AT HEADINGS ONLY NOT THE HEADING/SUBHEADING COMBO 
headingtype_change_summary=all_comp_csv%>%
  ungroup()%>%
  dplyr::select(PMID,orig_status,update_status,"removed"="orig_not_update","added"="update_not_orig")%>%
  mutate_all(na_if,"")%>%
  mutate(headingchange=ifelse(is.na(added)& is.na(removed),"no","yes"))%>%
  group_by(headingchange)%>%
  count()

headingtype_changes=all_comp_csv%>%
  ungroup()%>%
  dplyr::select(PMID,orig_status,update_status,"removed"="orig_not_update","added"="update_not_orig")%>%
  mutate_all(na_if,"")%>%
  mutate(headingchange=ifelse(is.na(added)& is.na(removed),"no","yes"))%>%
  dplyr::filter(headingchange=="yes")%>%
  dplyr::select(-headingchange)%>%
  pivot_longer(cols=c(4:5))%>%
  mutate_all(na_if,"")%>%
  drop_na(value)%>%
  mutate(value=as.list(strsplit(value, ",")))%>%
  unnest_longer(value)%>%
  separate(value,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6"),sep=":")%>%
  dplyr::select(PMID,name,heading)%>%
  mutate(heading=str_to_lower(str_squish(heading)))%>%
  mutate(check_tag=ifelse(heading %in% check_tags,"Yes","No"))%>%
  mutate(is_topic=ifelse(grepl("as topic",heading),"Yes","No"))

check_tag_change_summary=headingtype_changes%>%
  dplyr::select(PMID,check_tag)%>%
  distinct()%>%
  mutate(names="name")%>%
  pivot_wider(id_cols = PMID,values_from = check_tag,names_from = names)%>%
  unnest_wider(name,names_sep="_")%>%
  mutate(check_tag_change=ifelse(name_1=="Yes"| name_2=="Yes","Yes","No"))%>%
  mutate(check_tag_change=ifelse(is.na(check_tag_change),"No","Yes"))%>%
  dplyr::select(PMID,check_tag_change)%>%
  distinct()%>%
  group_by(check_tag_change)%>%
  count()

flowchart_checktag_change_Y=as.character(check_tag_change_summary%>%filter(check_tag_change=="Yes")%>%ungroup()%>%dplyr::select(-check_tag_change))
flowchart_checktag_change_N=as.character(check_tag_change_summary%>%filter(check_tag_change=="No")%>%ungroup()%>%dplyr::select(-check_tag_change))

astopic_change_summary=headingtype_changes%>%
  dplyr::select(PMID,is_topic)%>%
  distinct()%>%
  mutate(names="name")%>%
  pivot_wider(id_cols = PMID,values_from = is_topic,names_from = names)%>%
  unnest_wider(name,names_sep="_")%>%
  mutate(topic_change=ifelse(name_1=="Yes"| name_2=="Yes","Yes","No"))%>%
  mutate(topic_change=ifelse(is.na(topic_change),"No","Yes"))%>%
  dplyr::select(PMID,topic_change)%>%
  distinct()%>%
  group_by(topic_change)%>%
  count()

flowchart_astopic_change_Y=as.character(astopic_change_summary%>%filter(topic_change=="Yes")%>%ungroup()%>%dplyr::select(-topic_change))

#Now we are looking at the entire dataset 
all_original_headings=tbl%>%
  dplyr::select(file_path,PMID,Indexing,all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  dplyr::select(date,PMID,all_mesh)%>%
  drop_na(all_mesh)%>%
  group_by(PMID)%>%
  mutate(rank = rank(date))%>%
  dplyr::filter(rank==1)%>%
  dplyr::select(PMID,all_mesh)%>%
  mutate(all_mesh=gsub(",","",all_mesh))%>%
  distinct()%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(2:3),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6","suhbeading_7"),sep=":")%>%
  dplyr::select(PMID,heading)%>%
  mutate(heading=str_to_lower(str_squish(heading)))%>%
  group_by(heading)%>%
  count()


#now we want to look at how often different terms are added/removed 

added_removal_summary=headingtype_changes%>%
  dplyr::select(PMID,name,heading)%>%
  mutate(heading=str_to_lower(str_squish(heading)))%>%
  distinct()%>%
  group_by(name,heading)%>%
  count()%>%
  pivot_wider(id_cols=heading,values_from=n,names_from=name)%>%
  replace(is.na(.), 0)%>%
  mutate(sum=added+removed)%>%
  mutate(check_tag=ifelse(heading %in% check_tags,"Yes","No"))%>%
  mutate(is_topic=ifelse(grepl("as topic",heading),"Yes","No"))


#Now we are going to look and see how long it took things to get changed
correction_time=tbl%>%
  dplyr::filter(PMID %in% change_pmid)%>%
  dplyr::filter(!PMID %in% weird_pmid)%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  filter(date == max(date))%>%
  mutate(DateCompleted=as.Date(DateCompleted,format="%Y-%m-%d"))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  dplyr::select(PMID,DateCompleted,DateRevised)%>%
  mutate(interval=DateRevised-DateCompleted)%>%
  drop_na(interval)

correction_time_for_headings=headingtype_changes%>%
  full_join(correction_time,by="PMID")%>%
  dplyr::select(PMID,check_tag,interval)%>%
  drop_na(interval)%>%
  mutate(check_tag=ifelse(is.na(check_tag),"No",check_tag))%>%
  distinct()%>%
  group_by(PMID)%>%
  mutate(rank = rank(check_tag))%>%
  dplyr::filter(rank==max(rank))%>%
  ungroup()%>%
  mutate(bin = as.numeric(round(interval/7,0)))%>%
  mutate(bin=ifelse(is.na(bin),999,bin))%>%
  mutate(bin =ifelse(bin==0,1,bin))%>%
  group_by(check_tag)%>%
  count(bin)%>%
  mutate(totals=ifelse(check_tag=="Yes",as.numeric(flowchart_checktag_change_Y),1753))%>% ###THESE NEED TO BE UPDATED.
  mutate(percentage=((n/totals)*100))%>%
  dplyr::select(check_tag,bin,percentage)%>%
  ungroup()%>%
  mutate(check_tag=fct_relevel(check_tag,c("Yes","No")))%>%
  mutate(month=as.numeric(floor((bin-1)/4)))%>%
  mutate(axis_label=ifelse(month==0,paste("week", as.character(bin)), paste("month",month+1)))%>%
  dplyr::select(check_tag,axis_label,percentage)%>%
  group_by(axis_label,check_tag)%>%
  summarise(percentage_sum=sum(percentage))%>%
  ungroup()%>%
  mutate(axis_label = factor(axis_label,levels = c("week 1","week 2","week 3","week 4","month 2","month 3","month 4","month 5","month 6","month 7","month 8","month 9")))%>%
  mutate(facet=ifelse(grepl("month",axis_label),"Month 2-9","First Month"))%>%
  mutate(facet=factor(facet,levels=c("First Month","Month 2-9")))%>%
  mutate(bar_label=as.character(round(percentage_sum,2)))


#Now we are going to see if certain terms only appear in certain types of indexing 
#note that I am looking at the most RECENT indexing data with this 
Terms_per_indexing_type=tbl%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  mutate(Indexing=ifelse(Indexing==" ",NA,Indexing))%>%
  mutate(all_mesh=ifelse(all_mesh==" ",NA,all_mesh))%>%
  drop_na(all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  group_by(PMID)%>%
  filter(date == max(date))%>%
  dplyr::select(PMID,Indexing,all_mesh)%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  drop_na(Indexing)%>%
  pivot_longer(cols=c(3:4),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  drop_na(Indexing)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6","subheading_7"),sep=":")%>%
  pivot_longer(cols=c(4:10),names_to = "sh_type")%>%
  dplyr::select(-sh_type)%>%
  unite(heading_sh_combo,heading,value,sep="/",na.rm = T,remove = F)%>%
  distinct()%>%
  dplyr::select(PMID,Indexing,heading)%>%
  distinct()%>%
  group_by(Indexing,heading)%>%
  count()%>%
  pivot_wider(id_cols=heading,names_from=Indexing,values_from=n)%>%
  arrange(desc(Automated))

#Now we look at original indexing being removed
orignal_indexing_removed=tbl%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  mutate(Indexing=ifelse(Indexing==" ",NA,Indexing))%>%
  mutate(all_mesh=ifelse(all_mesh==" ",NA,all_mesh))%>%
  drop_na(all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  group_by(PMID)%>%
  filter(date == min(date))%>%
  ungroup()%>%
  dplyr::select(PMID,Indexing,all_mesh)%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(3:4),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  drop_na(Indexing)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading"),sep=":")%>%
  dplyr::select(-subheading)%>%
  distinct()%>%
  group_by(heading)%>%
  count()%>%
  mutate(heading=str_squish(str_to_lower(heading)))%>%
  left_join(added_removal_summary,by="heading")%>%
  drop_na(sum)%>%
  dplyr::select(heading,check_tag,is_topic,n,removed)%>%
  mutate(removed_percentage=round(removed/n*100,2))%>%
  dplyr::filter(removed!=1)%>%
  arrange(desc(removed_percentage))

# How much of final indexing is added? 
final_indexing_added=tbl%>%
  mutate(test=ifelse(nchar(DateRevised)==10,DateRevised,"fix"))%>%
  mutate(all_mesh=ifelse(test=="fix",DateRevised,all_mesh))%>%
  mutate(DateRevised=as.Date(DateRevised,format="%Y-%m-%d"))%>%
  mutate(Indexing=ifelse(Indexing==" ",NA,Indexing))%>%
  mutate(all_mesh=ifelse(all_mesh==" ",NA,all_mesh))%>%
  drop_na(all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  group_by(PMID)%>%
  filter(date == max(date))%>%
  ungroup()%>%
  dplyr::select(PMID,Indexing,all_mesh)%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(3:4),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  drop_na(Indexing)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading"),sep=":")%>%
  dplyr::select(-subheading)%>%
  distinct()%>%
  group_by(heading)%>%
  count()%>%
  mutate(heading=str_squish(str_to_lower(heading)))%>%
  left_join(added_removal_summary,by="heading")%>%
  drop_na(sum)%>%
  dplyr::select(heading,check_tag,is_topic,n,added)%>%
  mutate(added_percentage=round(added/n*100,2))%>%
  dplyr::filter(added!=1)%>%
  arrange(desc(added_percentage))

#####
####NOW LOOKING AT HEADING/SUBHEADING COMBOS####
sh_headingtype_changes=all_comp_csv%>%
  ungroup()%>%
  dplyr::select(PMID,orig_status,update_status,"removed"="orig_not_update","added"="update_not_orig")%>%
  mutate_all(na_if,"")%>%
  mutate(headingchange=ifelse(is.na(added)& is.na(removed),"no","yes"))%>%
  dplyr::filter(headingchange=="yes")%>%
  dplyr::select(-headingchange)%>%
  pivot_longer(cols=c(4:5))%>%
  mutate_all(na_if,"")%>%
  drop_na(value)%>%
  mutate(value=as.list(strsplit(value, ",")))%>%
  unnest_longer(value)%>%
  separate(value,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6"),sep=":")%>%
  pivot_longer(cols=c(6:11),names_to = "sh_type")%>%
  dplyr::select(-sh_type)%>%
  unite(heading_sh_combo,heading,value,sep="/",na.rm = T,remove = F)%>%
  dplyr::select(PMID,name,heading,heading_sh_combo)%>%
  mutate(heading=str_to_lower(str_squish(heading)))%>%
  mutate(heading_sh_combo=str_to_lower(str_squish(heading_sh_combo)))%>%
  mutate(check_tag=ifelse(heading %in% check_tags,"Yes","No"))%>%
  mutate(is_topic=ifelse(grepl("as topic",heading),"Yes","No"))





#Now we are looking at the entire dataset 
sh_all_original_headings=tbl%>%
  dplyr::select(file_path,PMID,Indexing,all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  dplyr::select(date,PMID,all_mesh)%>%
  drop_na(all_mesh)%>%
  group_by(PMID)%>%
  mutate(rank = rank(date))%>%
  dplyr::filter(rank==1)%>%
  dplyr::select(PMID,all_mesh)%>%
  mutate(all_mesh=gsub(",","",all_mesh))%>%
  distinct()%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(2:3),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6"),sep=":")%>%
  pivot_longer(cols=c(3:8),names_to = "sh_type")%>%
  dplyr::select(-sh_type)%>%
  unite(heading_sh_combo,heading,value,sep="/",na.rm = T,remove = F)%>%
  dplyr::select(PMID,heading_sh_combo)%>%
  mutate(heading=str_to_lower(str_squish(heading_sh_combo)))%>%
  group_by(heading_sh_combo)%>%
  count()


#now we want to look at how often different terms are added/removed 

sh_added_removal_summary=sh_headingtype_changes%>%
  dplyr::select(PMID,name,heading_sh_combo)%>%
  mutate(heading_sh_combo=str_to_lower(str_squish(heading_sh_combo)))%>%
  distinct()%>%
  group_by(name,heading_sh_combo)%>%
  count()%>%
  pivot_wider(id_cols=heading_sh_combo,values_from=n,names_from=name)%>%
  replace(is.na(.), 0)%>%
  mutate(sum=added+removed)%>%
mutate(check_tag=ifelse(heading_sh_combo %in% check_tags,"Yes","No"))%>%
  mutate(is_topic=ifelse(grepl("as topic",heading_sh_combo),"Yes","No"))

#####

#stuff specific to the static site

titles_abstract=read.csv("gallerystuff/TitleAbstract_All.csv",header = FALSE)%>%
  separate(V1,into=c("PMID","Title","Abstract","Extra"),sep="\\|",extra="warn",fill="warn")%>%
  dplyr::filter(PMID %in% change_pmid)
  dplyr::filter(PMID %in% weird_pmid)

write.csv(titles_abstract,"gallerystuff/TitlesAbstracts_Changed.csv",row.names = FALSE)

rosie_pmid_check=c("40888291",
                   "40888362",
                   "40888685",
                   "40888925",
                   "40889068",
                   "40889534",
                   "40889545",
                   "40890575",
                   "40890737",
                   "40890775",
                   "40890879",
                   "40891166",
                   "40891793",
                   "40892102",
                   "40892261",
                   "40892412",
                   "40892465",
                   "40892511",
                   "40892532",
                   "40892617",
                   "40896880",
                   "40897122",
                   "40897481",
                   "40897491",
                   "40897497",
                   "40897499",
                   "40897513",
                   "40897749",
                   "40897855",
                   "40897873",
                   "40898319",
                   "40898442",
                   "40898467",
                   "40898533",
                   "40898572",
                   "40898742",
                   "40898975",
                   "40899152",
                   "40899193",
                   "40899645",
                   "40900110",
                   "40900360",
                   "40900504",
                   "40900607",
                   "40900629",
                   "40900633",
                   "40901800",
                   "40901901",
                   "40902200",
                   "40902553",
                   "40902883",
                   "40903099",
                   "40903476",
                   "40903716",
                   "40905232",
                   "40905237",
                   "40905264",
                   "40905717",
                   "40905870",
                   "40906494",
                   "40906535",
                   "40906720",
                   "40906953",
                   "40907527",
                   "40907711",
                   "40907712",
                   "40908353",
                   "40909225",
                   "40910331",
                   "40911013",
                   "40911086",
                   "40911638",
                   "40912004",
                   "40912312",
                   "40912314",
                   "40912703")

rosie_pmid_check %in% weird_pmid
#PLOTS AND FIGURES 

check_tag_plot=ggplot(correction_time_for_headings, aes(x=(axis_label),y=percentage_sum,fill=check_tag))+
  geom_bar(position=position_dodge2(preserve = "single",padding=0.1),stat = "identity")+
  geom_text(aes(label=bar_label),position=position_dodge(width=0.9),vjust=-0.25,size=4)+
  xlab("Time Since Record Creation")+
  ylab("Percentage of Papers")+
  ggtitle("When was the Indexing Updated on the Changed Records? \n(n=1969)")+
  scale_y_continuous(expand=c(0,0),limits = c(0,100))+
  #scale_x_continuous(breaks=seq(1,25,1))+
  scale_fill_manual(values=c("#59C6C8","#282F50"))+
  labs(fill="Did the Record have a \n Check Tag Changed?")+
  guides(fill = guide_legend(title.position = "top"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),legend.position = "bottom",legend.title.align=0.5,plot.title = element_text(hjust = 0.5))+
  facet_grid(cols=vars(facet),scales="free_x",space="free_x")
  
check_tag_plot
ggsave("results/plots/change_timeline.png",plot=check_tag_plot,height = 18.2, width =30, units = "cm")

indexing_added_plot=final_indexing_added%>%
  mutate(population=ifelse(heading %in% pop_groups,"Yes","No"))%>%
  dplyr::select(heading,population,check_tag,is_topic,added_percentage)%>%
  pivot_longer(cols=c(2:4),names_to = "type",values_to = "test")%>%
  mutate(fill_label=ifelse(type=="check_tag" & test=="Yes","Check Tag",ifelse(type=="is_topic" & test =="Yes","As Topic",ifelse(type=="population" & test=="Yes","Population Groups","Regular Heading"))))%>%
  mutate(alpha_fill=ifelse(fill_label=="Regular Heading","alpha","no_alpha"))%>%
  dplyr::filter(added_percentage!=0)%>%
  mutate(text_label=ifelse(type=="is_topic" & test=="Yes",heading,ifelse(type=="population" & test=="Yes",heading,"")))%>%
  ggplot(aes(x=reorder(heading,-added_percentage),y=added_percentage,fill=fill_label,alpha=alpha_fill))+
  geom_bar(position="dodge2",stat = "identity")+
  geom_text(aes(label=text_label),angle=45,hjust=0,size=3)+
  xlab("Heading")+
  ylab("How often does the term appear \nafter curation? (%)")+
  scale_y_continuous(expand=c(0,0),limits = c(0,100))+
  scale_fill_manual(values=c("#F707A4","#282F50","#59C6C8","gray"),labels=c("As Topic","Check Tag","Population Groups","Regular Heading"),name="MeSH Type")+
  scale_alpha_manual(values=c(0.25,1),guide="none")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

indexing_added_plot


ggsave("results/plots/added_terms.png",plot=indexing_added_plot,height = 18.2, width =30, units = "cm")


indexing_removed_plot=orignal_indexing_removed%>%
  mutate(population=ifelse(heading %in% pop_groups,"Yes","No"))%>%
  dplyr::select(heading,check_tag,is_topic,population,removed_percentage)%>%
  pivot_longer(cols=c(2:4),names_to = "type",values_to = "test")%>%
  mutate(fill_label=ifelse(type=="check_tag" & test=="Yes","Check Tag",ifelse(type=="is_topic" & test =="Yes","As Topic",ifelse(type=="population" & test=="Yes","Population Groups","Regular Heading"))))%>%
  mutate(alpha_fill=ifelse(fill_label=="Regular Heading","alpha","no_alpha"))%>%
  dplyr::filter(removed_percentage!=0)%>%
  mutate(text_label=ifelse(type=="is_topic" & test=="Yes",heading,ifelse(type=="population" & test=="Yes",heading,"")))%>%
  ggplot(aes(x=reorder(heading,-removed_percentage),y=removed_percentage,fill=fill_label,alpha=alpha_fill))+
  geom_bar(position="dodge2",stat = "identity")+
  geom_text(aes(label=text_label),angle=45,hjust=0,size=3)+
  xlab("Heading")+
  ylab("How often does the term dissapear \nafter curation? (%)")+
  scale_y_continuous(expand=c(0,0),limits = c(0,100))+
  scale_fill_manual(values=c("#F707A4","#282F50","#59C6C8","gray"),labels=c("As Topic","Check Tag","Population Groups","Regular Heading"),name="MeSH Type")+
  scale_alpha_manual(values=c(0.25,1),guide="none")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

indexing_removed_plot
ggsave("results/plots/removed_terms.png",plot=indexing_removed_plot,height = 18.2, width =30, units = "cm")


#we need 3 numbers: number of times robot assigned term, number of times it had to be removed, number of times it had to be added

original_automated_pmid_df=tbl%>%
  dplyr::filter(Indexing=="Automated")%>%
  dplyr::select(file_path,PMID,Indexing,all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  dplyr::select(date,PMID,all_mesh)%>%
  drop_na(all_mesh)%>%
  group_by(PMID)%>%
  mutate(rank = rank(date))%>%
  dplyr::filter(rank==1)%>%
  dplyr::select(PMID)%>%
  distinct()

original_automated_pmids=original_automated_pmid_df$PMID

original_automated_headings=tbl%>%
  dplyr::filter(Indexing=="Automated")%>%
  dplyr::select(file_path,PMID,Indexing,all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  dplyr::select(date,PMID,all_mesh)%>%
  drop_na(all_mesh)%>%
  group_by(PMID)%>%
  mutate(rank = rank(date))%>%
  dplyr::filter(rank==1)%>%
  dplyr::select(PMID,all_mesh)%>%
  mutate(all_mesh=gsub(",","",all_mesh))%>%
  distinct()%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(2:3),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6","suhbeading_7"),sep=":")%>%
  dplyr::select(PMID,heading)%>%
  mutate(heading=str_to_lower(str_squish(heading)))%>%
  group_by(heading)%>%
  count()



final_automated_headings=tbl%>%
  dplyr::filter(PMID %in% original_automated_pmids)%>%
  dplyr::select(file_path,PMID,Indexing,all_mesh)%>%
  mutate(date=str_sub(file_path,-12,-5))%>%
  mutate(date=mdy(date))%>%
  dplyr::select(date,PMID,all_mesh)%>%
  drop_na(all_mesh)%>%
  group_by(PMID)%>%
  mutate(rank = rank(date))%>%
  dplyr::filter(rank==max(rank))%>%
  dplyr::select(PMID,all_mesh)%>%
  mutate(all_mesh=gsub(",","",all_mesh))%>%
  distinct()%>%
  mutate(all_mesh=stri_replace_last_fixed(all_mesh,"_",";"))%>%
  separate(all_mesh,into=c("all","major_topic"),sep=";")%>%
  pivot_longer(cols=c(2:3),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  mutate(term= as.list(strsplit(term, "_")))%>%
  dplyr::filter(type=="all")%>%
  dplyr::select(-type)%>%
  unnest_longer(term)%>%
  separate(term,into=c("heading","subheading_1","subheading_2","subheading_3","subheading_4","subheading_5","subheading_6","suhbeading_7"),sep=":")%>%
  dplyr::select(PMID,heading)%>%
  mutate(heading=str_to_lower(str_squish(heading)))%>%
  group_by(heading)%>%
  count()

waterfall_charts_1=original_automated_headings%>%
  #dplyr::filter(grepl("as topic",heading))%>%
  dplyr::filter(heading %in% pop_groups)%>%
  left_join(added_removal_summary,by="heading")%>%
  dplyr::select(heading,n,added,removed,sum)%>%
  mutate(added=ifelse(is.na(added),0,added))%>%
  mutate(removed=ifelse(is.na(removed),0,removed))%>%
  mutate(sum=ifelse(is.na(sum),0,sum))%>%
  dplyr::filter(!sum==0)%>%
  dplyr::select(-sum)%>%
  arrange(desc(n))%>%
  rownames_to_column(var="group_id")%>%
  pivot_longer(cols=c(4:5))%>%
  mutate(min=ifelse(name=="removed",n-value,n))%>%
  mutate(max=ifelse(name=="removed",n,n+value))%>%
  dplyr::select(-value)%>%
  mutate(group_id=as.numeric(group_id))%>%
  rownames_to_column(var="x_pos")%>%
  mutate(x_pos=as.numeric(x_pos))%>%
  mutate(heading=fct_reorder(heading,n))%>%
  ggplot(aes(x=reorder(heading,-n)))+
  geom_rect(aes(
                xmin=group_id-0.25,
                xmax=group_id+0.25,
                ymin=max,
                ymax=min,
                fill=name))+
  geom_rect(aes(
                xmin=group_id-0.3,
                xmax=group_id+0.3,
                ymin=n,
                ymax=n), colour="black")+
    scale_x_discrete(drop=FALSE)+
  theme_bw()+
  theme(panel.grid.major.x =element_blank(),
        legend.position = "bottom",
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45,vjust=0.5))
  
waterfall_charts_1


waterfall_charts_2=original_automated_headings%>%
  #dplyr::filter(grepl("as topic",heading))%>%
  dplyr::filter(heading %in% pop_groups)%>%
  left_join(final_automated_headings,by="heading")%>%
  left_join(added_removal_summary,by="heading")%>%
  dplyr::select(heading,"original_n"=n.x,"final_n"=n.y,added,removed,sum)%>%
  mutate(added=ifelse(is.na(added),0,added))%>%
  mutate(removed=ifelse(is.na(removed),0,removed))%>%
  mutate(sum=ifelse(is.na(sum),0,sum))%>%
  dplyr::filter(!sum==0)%>%
  dplyr::select(-sum)%>%
  arrange(desc(final_n))%>%
  rownames_to_column(var="group_id")%>%
  pivot_longer(cols=c(5:6))%>%
  mutate(min=ifelse(name=="removed",original_n-value,final_n-value))%>%
  mutate(max=ifelse(name=="removed",original_n,final_n))%>%
  dplyr::select(-value)%>%
  mutate(group_id=as.numeric(group_id))%>%
  rownames_to_column(var="x_pos")%>%
  mutate(x_pos=as.numeric(x_pos))%>%
  mutate(heading=fct_reorder(heading,final_n))%>%
  mutate(name=fct_relevel(name,c("removed","added")))%>%
  ggplot(aes(x=reorder(heading,-final_n)))+
  geom_rect(aes(
    xmin=group_id-0.25,
    xmax=group_id+0.25,
    ymin=max,
    ymax=min,
    fill=name),position = "dodge")+
  geom_rect(aes(
    xmin=group_id-0.4,
    xmax=group_id,
    ymin=original_n,
    ymax=original_n), colour="black")+
geom_rect(aes(
  xmin=group_id,
  xmax=group_id+0.4,
  ymin=final_n,
  ymax=final_n), colour="black")
    

waterfall_charts_2
