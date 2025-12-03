# Shiny_app_project
Creating shiny app to explore recruitment trends in Project CannTalk

The study, Project CannTalk is an observational study. It is meant to explores trends in cannabis use
and the kinds of information about cannabis people who use cannabis receive/are exposed to. The study
tracks participants over 9 months, and collects EMA(ecological momentary assessments) data for 21 days
at 3 different timepoints; the study also collects more traditional surveys at baseline, 4 months, and 
8 months.

This exploration and app are meant to be entirely about recruitment; to explore relationships between
different demographics and enrollment status. There is a certain burden to entry in the study, the number
of steps a participant needs to go through in order to join the study. They first fill out an online 
survey(10 min). If they are found eligible, they then will do a phone screening(15 min). If eligible after the phone screening,
they then do a Zoom visit(45 min) where they undergo consent and additional screening. If eligible after the zoom, they
then come to our study center for an in-person visit(1.5 hr).

Our eligibility criteria is fairly low (of the 570 who took the initial online screener, 461 were eligible).
But even of those who were intially eligible, only about 30% of those were eventually enrolled. What I want to
determine is: where are people "washing out" and is there any correlation between the stage at which they
wash out and other categorical variables? The primary one we will will be age (18-74).

A few notes on the data:
Age classes are: Youth (18-24), Adult (25-44), Middle-aged(45-59), and Older-Adult(60-74)

The last four columns are ordered as the order of the screening process:
*screen_result (online screener)
*phone_result (phone screening)
*zoom_result (zoom visit)
*enrolled (whether they completed in-person and joined the study)

NOTE: *CT_recruit_data.csv is the original cleaned data set. A few additional steps were taken to create CT_washout, our final dataset.
*scrap_records.R was a place to trial code before commiting it to the app. *CT_washout is our final dataset.
