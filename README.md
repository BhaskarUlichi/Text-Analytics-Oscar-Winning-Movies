# Text Analytics Project for Oscar Winning Movies

Welcome to the  repository! This project delves into the heart of Academy Award-winning films for Best Picture (1970 to 2015), employing advanced text mining and topic modeling techniques to unravel 
underlying themes and patterns within these cinematic masterpieces.

This project has been done using R programming language for scraping and cleaning HTML content from a list of movie URLs, automating the data collection process.

Extensive text pre-processing is conducted, including removing stopwords, punctuations, and script-specific keywords, to prepare the dataset for analysis.

The optimal number of topics for the dataset is determined using coherence score, enhancing the accuracy of topic modeling results.

A Latent Dirichlet Allocation (LDA) topic modeling has been implemented to identify key topics within the film scripts, providing a structured understanding of thematic elements.

Results have been visualized through word clouds, bar plots, and pie charts, offering a comprehensive overview of prevalent topics and their distribution across the dataset.

Actionable insights are generated into the dominant themes, assisting filmmakers and screenwriters in content creation.

A Hierarchical clustering is also implemented to reveal script clusters based on topic similarities, contributing to a nuanced understanding of script relationships.

## Fair Use Disclaimer

This project strictly adheres to fair use principles. Movie scripts were sourced from publicly accessible websites for academic and research purposes, contributing to scholarly discussions on text mining, 
natural language processing, and thematic exploration in award-winning cinema. Intellectual property rights are respected, and any unintended infringement is regretted.

## Movie Insights Generator

The project initiated with web scraping using R, involves a CSV file, "Movies_List.csv," that contains movie names and URLs for scraping. 
You can create your own "movies.csv" for desired movie lists and can use this model as a "Movie Insights Generator" tool. 
Whether you're interested in films by your favorite director, screenwriter, actor, or the highest-grossing movies over the years, this project has got you covered.

## How to Use

1. Create a "movies.csv" file with desired movie names and URLs in HTML format.
2. Replace the 'csv_file' path in the web scraping code with your file path.
3. Set 'folder_path' and 'input_dir' variables in the R code for downloaded content.
4. Download and reference stoplists mentioned in the report in the R code.
5. Adjust 'num_topics' in the R code for your desired number of topics.
6. Run the R code for data scraping, loading, text preprocessing, and topic modeling.
7. Create an LDA model, view keywords in each topic, and utilize various visualizations such as grouped bar plots, pie charts, word clouds, dendrograms, etc., to gain deep insights into the movies.

Feel free to explore and analyze your favorite movies with ease using this Movie Insights Generator. For more details on the topic modeling, refer to the report uploaded in the repository.

Happy analyzing!
