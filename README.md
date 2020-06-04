# Prediction-Kickstarter-Campaign-Success

This project examines a Kickstarter crowdfunding campaigns dataset focusing on campaign success prediction. According to Kickstarter’s website “Kickstarter helps artists, musicians, filmmakers, designers, and other creators find the resources and support they need to make their ideas a reality. To date, tens of thousands of creative projects — big and small — have come to life with the support of the Kickstarter community.” The company provides opportunities for individuals to fund their creative projects using entirely crowd funding resources, meaning that the public is what sends these projects into production. Every project is brought to reality while friends, fans or complete strangers offer their funds in return for rewards or the finished product itself.

The data was scraped from webrobots.io (https://webrobots.io/kickstarter-datasets/), combined, pre-processed, and created into a simple and easy to understand dataset that contains project data from 2014 to February 2019. The dataset is available on Kaggle.com: https://www.kaggle.com/yashkantharia/kickstarter-campaigns. 

The dataset contains 20 variables and over 192K observations and it is fairly large which is helpful for building accurate prediction models. Majority of the data describes various campaign characteristics such as campaign length, location, start and end dates, amount of money needed and actually funded. Variable named “status” identifies each campaign success or failure. The goal is to build an accurate classification model using various machine learning algorithms, predicting binary outcome of each campaign: succeeded or failed.

Based on preliminary examination of the dataset, following key questions should be addressed during the analysis.  These focal questions aim to diagnose potential factors that affect Kickstarter campaign effectiveness:

●	What demographics (if any) are most likely to succeed?

●	Which categories of product or service are more successful in funding their projects? 

●	Which is the most important factor effecting each campaign?

While there are many potential variables that could be the cause of success or failure, our goal is to build a prediction model that would identify those major campaign success factors.

Methods of Analysis included creating a decison tree, descriptive statistics, svm, naive bayes, and random forest



[Click Here To See The Final Report and Results](https://github.com/ParInsights/Prediction-Kickstarter-Campaign-Success/blob/master/Final_Project_Report.pdf)
