# Quantifying Government Behavior Through Text Analysis (WIP)
## SodaLabs

This project aims to combine advanced computational linguistic techniques with the digital trail of governments’ interactions to extract their underlying content and sentiments. Specifically, unstructured collections of text extracted from speeches/statements made by country leaders (currently targetting Australia) will be analysed using NLP tools to develop quantified indicators of content and emotions of government behaviour. These indicators will then be combined with exogenous shocks in multiple settings to causally explore the reciprocal relationship between the public and governments.

Currently, the project has two major modules, 
- The first involves scraping the data from various sources and processing it to make it available for modelling. (`01-Scrape.R`)
- Model building and optimisation employing hyperparameter tuning, cross validation, performance indicators like topic coherence and perpexility, and finally visual inference are all part of the second module. (`02-TopicModeling.R`)

The project will in future be expanded and will involve putting topic models on a timeline and also calculate sentiment scores to provide a temporal summary of government behavior.
