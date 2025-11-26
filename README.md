# ssbm-web-scraper
This code scrapes supermajor.gg and liquipedia for performance metrics of ranked players in the SSBMRank from 2016 to 2024.

This uses RSelenium, which requires Java to be installed. [I recommend this video to help you set up RSelenium.](https://www.youtube.com/watch?v=GnpJujF9dBw)

---
The dataset (all_years) created from the web scraper is visualized in a Shiny app that you can visit here: https://andy-time.shinyapps.io/SSBMRank_2016_2024/.

It is also used in a random forest model that aims to accurately reproduce SSBM Rank 2024 using 2016 - 2023 as the training data. A writeup can be viewed here: https://www.kaggle.com/code/andytime/predicting-ssbm-ranks-with-random-forest
