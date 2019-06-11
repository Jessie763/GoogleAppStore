# GoogleAppStore
In this project we use Python and SAS to construct regression models to evaluate the installs and ratings of Google Play Sotre apps.

We have three research questions which are how to get a high rating on Google play store, how to boost installation amount and does price really matters.

We would break the first question down into three small questions, so that it would be much easier to us to find out the answer. These three small questions are finding the most relevant features, selecting a best model to predict the application rating and trying to look for effective strategies to improve the ratings.

For the second research question, how to boost installs, we explore the relationship between installation amount and the types (free or paid), and we also include the possible relative components, like size, rating, and categories into consideration.

For the third research question, does price matters, we also find two small question. These questions are: the difference of application average price in various categories, whether the size and content rating of applications are the important factors of pricing strategy, and how often should paid applications update.

We have three dependent variables, which are ratings, installs, and price. Linear regression model is used in this report and various variables and interaction terms are also implemented within our models. Then we try to compare different value in Mallows’Cp (forward, backward and stepwise algorithm), cross validation (forward, backward and stepwise algorithm), Lasso and Elastic Net, to see which model could provide best performances.

The results and analyses of three models are reviewed in addition to the improvement of the research for further study and conclusion will also be provided.

Data Set: https://www.kaggle.com/lava18/google-play-store-apps

Contributors:

Qingyuan Zhu, Minke Li, Spencer Lu, Xinyi Zhang, Jinglin Zhao

(MS Business Analytics Program, the University of Texas at Dallas)

March 2019 - May 2019
