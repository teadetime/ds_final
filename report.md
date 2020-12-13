report
================
Nathan Faber and Thomas Jagielski
2020-11-27

  - [How Couples Meet and If They Stay
    Together](#how-couples-meet-and-if-they-stay-together)
      - [Question](#question)
      - [Dataset and Background](#dataset-and-background)
      - [EDA](#eda)
      - [Correlation Matrix for Predictive
        Factors](#correlation-matrix-for-predictive-factors)
      - [Modelling](#modelling)
      - [Remaining Questions](#remaining-questions)

## How Couples Meet and If They Stay Together

### Question

Are there predicting factors for if a couple are more likely to stay
together or split up? If so, can we model the likelihood of a couple
staying together or splitting up based on inputs for the predicting
factors?

### Dataset and Background

This project utilizes the HCMST (How Couples Meet and Stay Together)
\[<https://data.stanford.edu/hcmst2017>\] This dataset was collected in
2017 and is made up of \~4,000 survey results from individuals 18+ that
had been in a relationship previously. This survey is a remake of an
earlier version of this survey with similar data. The data contains a
wide range of variables–all pertaining to the individual and their
partner. Some examples of the data available include: Race, Religion,
Level of Education, Sexual Orientation, How their relationship ended
etc.

The survey does have several notable quirks. The survey separates out an
LGB group that was specifically sampled for. The survey was orchestrated
through a rewards platform (KnowledgeBase) in which participants were
compensated. This could introduce some form of sample bias. There were
specific measures taken to attempt to get a representative sample, for
example, random dialing of telephone numbers, as well as using address
based sampling.

Given the above, this data source suits our purposes and will aid us in
understanding which factors cause couples to stay together.

### EDA

### Correlation Matrix for Predictive Factors

``` r
df_raw <- read_dta(file = "./data/HCMST.dta")

df_column_rename <-
  df_raw %>%
    as.tibble() %>%
    rename(
      married = S1,
      current_partner_status = S2,
      ever_had_partner = S3,
      partner_sex = Q4,
      partner_same_sex = Q5,
      partner_latino_hispanic = Q6A,
      partner_race = Q6B,
      partner_age = Q9,
      partner_education_level = Q10,
      partner_mother_education_level = Q11,
      partner_polictial_belief = Q12,
      your_mother_education_level = Q14,
      #country_you_grew_up_in = Q15A1,
      #state_you_grew_up_in = Q15A2,
      #city_you_grew_up_in = Q15A3,
      #your_country_met_partner = Q15A4,
      #your_state_met_partner = Q15A5,
      #your_city_met_partner = Q15A6,
      met_partner_location = Q15A7,
      number_relatives_you_visit = Q16,
      times_married_inclusive = Q17A, # if S1=1
      times_married = Q17B, # if S1=2
      gender_sexually_attracted_to_1 = Q17C, # ppgender = 2
      gender_sexually_attracted_to_2 = Q17D, #ppgender = 1
      currently_living_with_partner = Q19,
      ever_lived_together_w_partner = Q20,
      date_met_year = Q21A_Year,
      date_met_month = Q21A_Month,
      date_relationship_began_year = Q21B_Year,
      date_relationship_began_month = Q21B_Month,
      date_first_lived_together_year = Q21C_Year,
      date_first_lived_together_month = Q21C_Month,
      date_married_year = Q21D_Year,
      date_married_month = Q21D_Month,
      sexual_orientation = w6_identity,
      outness = w6_outness,
      age_came_out = w6_outness_timing,
      income_disparity = Q23,
      #story_how_met = Q24,
      same_high_school = Q25,
      same_university = Q26,
      grow_up_same_town = Q27,
      parents_knew_each_other_before_met = Q28,
      mutual_friends_prior_1 = w6_friend_connect_1,
      mutual_friends_prior_2 = w6_friend_connect_2,
      mutual_friends_prior_3 = w6_friend_connect_3,
      mutual_friends_prior_4 = w6_friend_connect_4,
      met_online = Q32,
      relationship_quality = Q34,
      #realtionship_explaination = Q35,
      sex_frequency = w6_sex_frequency,
      other_date = w6_otherdate,
      number_others_dated = w6_how_many, # how many other people met with besides partner
      #how_met = w6_how_meet,
      dating_app_met_other = w6_otherdate_app,
      how_many_met_dating_app = w6_how_many_app,
      #most_commonly_used_dating_app = Q40,
      ever_married_to_partner = Past_Partner_Q1,
      relationship_end_nonmar = w6_relationship_end_nonmar,
      who_wanted_breakup_more_nonmar = w6_breakup_nonmar,
      marriage_end_mar = w6_relationship_end_mar,
      who_broke_up = w6_who_breakup,
      #reason_break_up = w6_why_broke_up,
      partner_same_sex_2 = Q5_2,
      partner_hispanic_latino_2 = Q6A_2,
      year_partner_born_2 = Q9B_2,
      partner_education_level_2 = Q10_2,
      partner_mother_education_level_2 = Q11_2,
      partner_polictial_belief_2 = Q12_2,
      your_mother_education_level_2 = Q14_2,
      #country_you_grew_up_in_2 = Q15A1_2,
      #state_you_grew_up_in_2 = Q15A2_2,
      #city_you_grew_up_in_2 = Q15A3_2,
      #your_country_met_partner_2 = Q15A4_2,
      #your_state_met_partner_2 = Q15A5_2,
      #your_city_met_partner_2 = Q15A6_2,
      met_partner_location_2 = Q15A7_2_1,
      number_relatives_you_visit_2 = Q16_2,
      times_married_2 = Q17B_2,
      gender_sexually_attracted_to_1_2 = Q17C_2, # ppgender = 2
      gender_sexually_attracted_to_2_2 = Q17D_2, #ppgender = 1
      ever_lived_together_w_partner_2 = Q20_2,
      date_met_year_2 = Q21A_2_Year,
      date_met_month_2 = Q21A_2_Month,
      date_relationship_began_year_2 = Q21B_2_Year,
      date_relationship_began_month_2 = Q21B_2_Month,
      date_first_lived_together_year_2 = Q21C_2_Year,
      date_first_lived_together_month_2 = Q21C_2_Month,
      date_married_year_2 = Q21D_2_Year,
      date_married_month_2 = Q21D_2_Month,
      date_broken_up_year_2 = Q21E_2_Year,
      date_broken_up_month_2 = Q21E_2_Month,
      date_partner_pass_year = Q21F_2_Year,
      date_partner_pass_month = Q21F_2_Month,
      sexual_orientation_2 = w6_identity_2,
      outness_2 = w6_outness_2,
      age_came_out_2 = w6_outness_timing_2,
      income_disparity_2 = Q23_2,
      #story_how_met_2 = Q24_2,
      same_high_school_2 = Q25_2,
      same_university_2 = Q26_2,
      grow_up_same_town_2 = Q27_2,
      parents_knew_each_other_before_met_2 = Q28_2,
      mutual_friends_prior_1_2 = w6_friend_connect_2_1,
      mutual_friends_prior_2_2 = w6_friend_connect_2_2,
      mutual_friends_prior_3_2 = w6_friend_connect_2_3,
      mutual_friends_prior_4_2 = w6_friend_connect_2_4,
      met_online_2 = Q32_2,
      other_date_2 = w6_otherdate_2,      
      number_others_dated_2 = w6_how_many_2, # how many other people met with besides partner
      dating_app_met_other_2 = w6_otherdate_app_2,
      how_many_met_dating_app_2 = w6_how_many_app_2      
      )
```

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
df_data <-
  df_column_rename %>%
    mutate(across(everything(), as_factor))
```

``` r
df_filtered <- df_data 
df_filtered$married <- ifelse(is.na(df_filtered$married) | df_filtered$married == "Refused" ,
                              NA,
                              as.logical(!is.na(str_extract(df_filtered$married, "^Yes"))))


df_filtered$ever_had_partner <- ifelse(is.na(df_filtered$ever_had_partner) | df_filtered$ever_had_partner == "Refused" ,
                              NA,
                              as.logical(!is.na(str_extract(df_filtered$ever_had_partner, "^Yes"))))

df_filtered$partner_sex <- ifelse(is.na(df_filtered$partner_sex) | df_filtered$partner_sex == "Refused",
                                              NA,
                                              as.logical(!is.na(str_extract(df_filtered$partner_sex, "Male$"))))

df_filtered$same_high_school <- ifelse(is.na(df_filtered$same_high_school) | df_filtered$same_high_school== "Refused",
                                              NA,
                                              as.logical(!is.na(str_extract(df_filtered$same_high_school, "^Same"))))


df_filtered$same_university <- ifelse(is.na(df_filtered$same_university) | df_filtered$same_university== "Refused",
                                              NA,
                                              as.logical(!is.na(str_extract(df_filtered$same_university, "^Atten"))))

df_filtered$same_high_school_2 <- ifelse(is.na(df_filtered$same_high_school_2) | df_filtered$same_high_school_2== "Refused",
                                              NA,
                                              as.logical(!is.na(str_extract(df_filtered$same_high_school_2, "^Same"))))


df_filtered$same_university_2 <- ifelse(is.na(df_filtered$same_university_2) | df_filtered$same_university_2== "Refused",
                                              NA,
                                              as.logical(!is.na(str_extract(df_filtered$same_university_2, "^Atten"))))


df_filtered$parents_knew_each_other_before_met<- ifelse(is.na(df_filtered$parents_knew_each_other_before_met) | df_filtered$parents_knew_each_other_before_met == "Refused",
                                             NA,
                                             as.logical(!is.na(str_extract(df_filtered$parents_knew_each_other_before_met, "^Yes"))))

# df_filtered$grow_up_same_town <- ifelse(is.na(df_filtered$grow_up_same_town) | df_filtered$grow_up_same_town == "Refused",
#                                              NA,
#                                              as.logical(!is.na(str_extract(df_filtered$grow_up_same_town, "^Yes"))))

df_filtered$other_date <- ifelse(is.na(df_filtered$other_date) | df_filtered$other_date == "Refused",
                                             NA,
                                             as.logical(!is.na(str_extract(df_filtered$other_date, "^Yes"))))
df_filtered$other_date_2 <- ifelse(is.na(df_filtered$other_date_2) | df_filtered$other_date_2 == "Refused",
                                             NA,
                                             as.logical(!is.na(str_extract(df_filtered$other_date_2, "^Yes"))))
```

``` r
df_filtered <- df_filtered %>%
  filter(married | ever_had_partner | !is.na(str_extract(current_partner_status, "^Yes"))   ) %>% 
  mutate(ever_married = coalesce(times_married_inclusive, times_married, times_married_2)) %>%
  mutate(has_married = if_else(ever_married== "Never married" , FALSE, TRUE))  %>%
  # Now look if marriage ended
  mutate(end = coalesce(relationship_end_nonmar,marriage_end_mar)) %>%
  mutate(ended = if_else(married == TRUE | !is.na(str_extract(current_partner_status, "^Yes")) | !is.na(str_extract(end, "deceased$")), FALSE, TRUE )) 

df_filtered %>% 
  select(married,has_married, current_partner_status, ended, relationship_quality)
```

    ## # A tibble: 3,320 x 5
    ##    married has_married current_partner_status            ended relationship_qua~
    ##    <lgl>   <lgl>       <fct>                             <lgl> <fct>            
    ##  1 FALSE   TRUE        No, I am single, with no boyfrie~ TRUE  <NA>             
    ##  2 TRUE    TRUE        <NA>                              FALSE Excellent        
    ##  3 TRUE    TRUE        <NA>                              FALSE Good             
    ##  4 FALSE   FALSE       No, I am single, with no boyfrie~ TRUE  <NA>             
    ##  5 TRUE    TRUE        <NA>                              FALSE Excellent        
    ##  6 TRUE    TRUE        <NA>                              FALSE Good             
    ##  7 TRUE    TRUE        <NA>                              FALSE Excellent        
    ##  8 TRUE    TRUE        <NA>                              FALSE Excellent        
    ##  9 FALSE   FALSE       No, I am single, with no boyfrie~ FALSE <NA>             
    ## 10 TRUE    TRUE        <NA>                              FALSE Fair             
    ## # ... with 3,310 more rows

``` r
# Lets merge the columns for people who got divorced and those who didn't
df_of_interest <- df_filtered %>% 
  mutate(gender_sexually_attracted_to = coalesce(gender_sexually_attracted_to_1, gender_sexually_attracted_to_2, gender_sexually_attracted_to_1_2, gender_sexually_attracted_to_2_2)) %>% 
  mutate(mutual_friends_prior_1 = coalesce(mutual_friends_prior_1, mutual_friends_prior_1_2)) %>%
  mutate(mutual_friends_prior_2 = coalesce(mutual_friends_prior_2, mutual_friends_prior_2_2)) %>%
  mutate(mutual_friends_prior_3 = coalesce(mutual_friends_prior_3, mutual_friends_prior_3_2)) %>%
  mutate(mutual_friends_prior_4 = coalesce(mutual_friends_prior_4, mutual_friends_prior_4_2)) %>%
  
  mutate(partner_education_level = coalesce(partner_education_level, partner_education_level_2)) %>%
  # mutate(times_married = coalesce(times_married_inclusive, times_married_2)) %>%
  mutate(ever_lived_together_w_partner = coalesce(ever_lived_together_w_partner, ever_lived_together_w_partner_2, currently_living_with_partner)) %>% 
  mutate(same_high_school = coalesce(same_high_school, same_high_school_2)) %>% 
  mutate(same_university = coalesce(same_university, same_university_2)) %>% 
  mutate(met_online = coalesce(met_online, met_online_2)) %>% 
  mutate(sexual_orientation = coalesce(sexual_orientation, sexual_orientation_2)) %>% 
  mutate(partner_polictial_belief = coalesce(partner_polictial_belief, partner_polictial_belief_2)) %>% 
  mutate(sexual_orientation = coalesce(sexual_orientation, sexual_orientation_2)) %>% 
  
  mutate(grow_up_same_town = coalesce(grow_up_same_town, grow_up_same_town_2)) %>%
  mutate(other_date = coalesce(other_date, other_date_2)) %>% 

  select(married, has_married, ended,
       ever_had_partner,
       relationship_end_nonmar,
       marriage_end_mar,
       ever_lived_together_w_partner,
       age_when_met,
       partnership_status,
       other_date,
       grow_up_same_town,
       partner_polictial_belief,
       sexual_orientation,
       met_online,
       same_high_school,
       same_university,
       who_broke_up,
       times_married,
       partner_education_level, 
       xlgb ,
       w6_same_sex_couple,
       gender_sexually_attracted_to,
       mutual_friends_prior_1,
       mutual_friends_prior_2,
       mutual_friends_prior_3,
       mutual_friends_prior_4,
       relationship_quality,
       partyid7
  )
  # 
  # unite("gender_sexually_attracted_to", gender_sexually_attracted_to_1, gender_sexually_attracted_to_2, gender_sexually_attracted_to_1_2, gender_sexually_attracted_to_2_2, na.rm = TRUE, remove = F) %>%
  # 
  # unite("mutual_friends_prior_1", mutual_friends_prior_1, mutual_friends_prior_1_2, na.rm = TRUE, remove = F) %>%
  # 
  # unite("mutual_friends_prior_2", mutual_friends_prior_2, mutual_friends_prior_2_2, na.rm = TRUE, remove = F) %>%
  # unite("mutual_friends_prior_3", mutual_friends_prior_3, mutual_friends_prior_3_2, na.rm = TRUE, remove = F) %>%
  # unite("mutual_friends_prior_4", mutual_friends_prior_4, mutual_friends_prior_4_2, na.rm = TRUE, remove = F) %>%
  # unite("partner_education_level", partner_education_level, partner_education_level_2, na.rm = TRUE, remove = F) %>%
  # unite("times_married", times_married_inclusive, times_married_2, na.rm = TRUE, remove = F) %>%
  # unite("ever_lived_together_w_partner", ever_lived_together_w_partner, ever_lived_together_w_partner_2, currently_living_with_partner, na.rm = TRUE, remove = F) %>%
  # unite("same_high_school", same_high_school, same_high_school_2, na.rm = TRUE, remove = F) %>%
  # unite("same_university", same_university, same_university_2, na.rm = TRUE, remove = F) %>%
  # unite("met_online", met_online, met_online_2, na.rm = TRUE, remove = F) %>%
  # unite("sexual_orientation", sexual_orientation, sexual_orientation_2, na.rm = TRUE, remove = F) %>%
  # unite("partner_polictial_belief", partner_polictial_belief, partner_polictial_belief_2, na.rm = TRUE, remove = F) %>%
  # unite("grow_up_same_town", grow_up_same_town, grow_up_same_town_2, na.rm = TRUE, remove = F) %>%
  # unite("other_date", other_date, other_date_2, na.rm = TRUE, remove = F) %>%


df_of_interest
```

    ## # A tibble: 3,320 x 28
    ##    married has_married ended ever_had_partner relationship_en~ marriage_end_mar
    ##    <lgl>   <lgl>       <lgl> <lgl>            <fct>            <fct>           
    ##  1 FALSE   TRUE        TRUE  TRUE             We broke up      <NA>            
    ##  2 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ##  3 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ##  4 FALSE   FALSE       TRUE  TRUE             We broke up      <NA>            
    ##  5 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ##  6 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ##  7 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ##  8 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ##  9 FALSE   FALSE       FALSE TRUE             [Partner Name] ~ <NA>            
    ## 10 TRUE    TRUE        FALSE NA               <NA>             <NA>            
    ## # ... with 3,310 more rows, and 22 more variables:
    ## #   ever_lived_together_w_partner <fct>, age_when_met <fct>,
    ## #   partnership_status <fct>, other_date <lgl>, grow_up_same_town <fct>,
    ## #   partner_polictial_belief <fct>, sexual_orientation <fct>, met_online <fct>,
    ## #   same_high_school <lgl>, same_university <lgl>, who_broke_up <fct>,
    ## #   times_married <fct>, partner_education_level <fct>, xlgb <fct>,
    ## #   w6_same_sex_couple <fct>, gender_sexually_attracted_to <fct>,
    ## #   mutual_friends_prior_1 <fct>, mutual_friends_prior_2 <fct>,
    ## #   mutual_friends_prior_3 <fct>, mutual_friends_prior_4 <fct>,
    ## #   relationship_quality <fct>, partyid7 <fct>

``` r
data <-
  df_of_interest %>%
    mutate(across(everything(), as_factor)) %>%
    # Default is 0 is false and 1 is true
    mutate(married = as.numeric(married) - 1,
           relationship_quality = as.numeric(relationship_quality),
           age_when_met = as.numeric(age_when_met),
           marriage_end_mar = as.numeric(marriage_end_mar),
           met_online = as.numeric(met_online),
           met_online = convert_met_online(met_online), # 0 = no; 1 = yes
           partner_polictial_belief = as.numeric(partner_polictial_belief),
           partyid7 = as.numeric(partyid7),
           political_diff = abs(partner_polictial_belief - partyid7),
           grow_up_same_town = as.numeric(grow_up_same_town), 
           grow_up_same_town = convert_same_town(grow_up_same_town), # 0 = no; 1 = yes 
           ever_had_partner = as.numeric(ever_had_partner),
           other_date = as.numeric(other_date) - 1, # 0 = no; 1 = yes
           same_high_school = as.numeric(same_high_school) - 1,
           same_university = as.numeric(same_university) - 1,
           sexual_orientation = as.numeric(sexual_orientation),
           ever_lived_together_w_partner = as.numeric(ever_lived_together_w_partner),
           ever_lived_together_w_partner = convert_ever_lived_with_partner(ever_lived_together_w_partner),
           xlgb = as.numeric(xlgb),
           ended = as.numeric(ended) - 1,
           partnership_status = as.numeric(partnership_status), # 1 = married; 2 = partnered_not_married; 3 = unpartnered, has had past partner
           who_broke_up = as.numeric(who_broke_up) - 1,  #1 = I wanted it more; 2 = partner wanted separation more; 3 = equal
           times_married = as.numeric(times_married) - 2,
           partner_education_level = as.numeric(partner_education_level) - 1, # 0 = LGB sample; 1 = gen pop
           w6_same_sex_couple = as.numeric(w6_same_sex_couple) - 1) # 0 = not same-sex couple; 1 = same-sex couple

complete_list <- 
  c("married", 
    "relationship_quality",
    "age_when_met",
    "marriage_end_mar",
    "met_online",
    "political_diff",
    "grow_up_same_town",
    "ever_had_partner",
    "other_date",
    "same_high_school",
    "same_university",
    "sexual_orientation",
    "ever_lived_together_w_partner",
    "xlgb",
    "ended",
    "partnership_status",
    "who_broke_up",
    "times_married",
    "w6_same_sex_couple")

complete_label <- 
  c("married" = "Married", 
    "relationship_quality" = "Relationship Quality",
    "age_when_met" = "Age When Met",
    "marriage_end_mar" = "Marriage Ended",
    "met_online" = "Met Online",
    "political_diff" = "Difference in Political Orientation",
    "grow_up_same_town" = "Grew up in the Same Town",
    "ever_had_partner" = "Ever had a Partner",
    "other_date" = "Dated Someone Else in Past Year",
    "same_high_school" = "Went to Same High School",
    "same_university" = "Went to Same University",
    "sexual_orientation" = "Sexual Orientation",
    "ever_lived_together_w_partner" = "Ever Lived With Partner",
    "xlgb" = "LGBTQ",
    "ended" = "Relationship/Marriage Ended",
    "partnership_status" = "Current Partnership Status",
    "who_broke_up" = "Who Broke Up",
    "times_married" = "Number of Times Married",
    "w6_same_sex_couple" = "Same Sex Couple")

data[ ,complete_list] %>%
  cor(use="pairwise.complete.obs", method = c("pearson", "kendall", "spearman")) %>%
  round(5) %>%
  ggcorrplot(show.diag = T, type = "full", lab = TRUE, lab_size = 2) +
  ggtitle("Complete Correlation Matrix for Broken Up Couples") + 
  scale_x_discrete(labels=complete_label) +
  scale_y_discrete(labels=complete_label)
```

    ## Warning in cor(., use = "pairwise.complete.obs", method = c("pearson",
    ## "kendall", : the standard deviation is zero

![](report_files/figure-gfm/correlation_all_factors-1.png)<!-- -->

``` r
selected_list <- 
  c("married",
    "ended",
    "marriage_end_mar",
    "met_online",
    "ever_lived_together_w_partner",
    "w6_same_sex_couple",
    "age_when_met",
    "political_diff")

data[ ,selected_list] %>%
  cor(use="pairwise.complete.obs", method = c("pearson", "kendall", "spearman")) %>%
  round(5) %>%
  ggcorrplot(show.diag = T, type = "full", lab = TRUE, lab_size = 2) +
  ggtitle("Correlation Matrix for Broken Up Couples Selected Factors") + 
  scale_x_discrete(labels=complete_label) +
  scale_y_discrete(labels=complete_label)
```

    ## Warning in cor(., use = "pairwise.complete.obs", method = c("pearson",
    ## "kendall", : the standard deviation is zero

![](report_files/figure-gfm/correlation_selected_factors-1.png)<!-- -->

### Modelling

``` r
model_ended <-
  glm(
    formula = ended ~ met_online + ever_lived_together_w_partner + w6_same_sex_couple + age_when_met + political_diff + other_date,
    data = data,
    family = "binomial"
  ) 


model_ended %>%
  tidy(
    conf.int = TRUE,
    conf.level = 0.90
  ) #%>% mutate(
```

    ## # A tibble: 7 x 7
    ##   term                  estimate std.error statistic  p.value conf.low conf.high
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)           -0.809     0.196      -4.13  3.62e- 5 -1.13      -0.487 
    ## 2 met_online            -0.371     0.172      -2.16  3.09e- 2 -0.658     -0.0918
    ## 3 ever_lived_together_~ -1.26      0.0641    -19.6   9.29e-86 -1.36      -1.15  
    ## 4 w6_same_sex_couple     0.738     0.175       4.21  2.53e- 5  0.447      1.02  
    ## 5 age_when_met           0.00466   0.00476     0.979 3.27e- 1 -0.00320    0.0125
    ## 6 political_diff         0.202     0.0407      4.96  7.07e- 7  0.135      0.268 
    ## 7 other_date             0.335     0.168       1.99  4.63e- 2  0.0564     0.610

``` r
    #mse = mse_val,
    #rsquare = rsquare_val
  #)
```

``` r
data_test <- tibble(met_online = 0, 
                    ever_lived_together_w_partner = 1,  
                    w6_same_sex_couple = 0,  
                    age_when_met  = 26,
                    political_diff = 2,  
                    other_date = 0)

df_prediction <-
  data_test %>%
  add_predictions(model_ended, var = "log_odds_ratio") %>%
  arrange(log_odds_ratio) %>%
  rowid_to_column(var = "order") %>%
  mutate(pr_ended = inv.logit(log_odds_ratio))

df_prediction
```

    ## # A tibble: 1 x 9
    ##   order met_online ever_lived_toge~ w6_same_sex_cou~ age_when_met political_diff
    ##   <int>      <dbl>            <dbl>            <dbl>        <dbl>          <dbl>
    ## 1     1          0                1                0           26              2
    ## # ... with 3 more variables: other_date <dbl>, log_odds_ratio <dbl>,
    ## #   pr_ended <dbl>

### Remaining Questions
