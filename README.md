# NCAA Baseball Analytics Project

Welcome to my NCAA Baseball Analytics project repository! This project aims to enhance the capabilities of the popular baseballr library by implementing the necessary functions to create a Fangraphs-style Guts! dataframe for the NCAA. It includes a play-by-play parser that generates a run expectancy matrix and retrieves the run values for each batting outcome. These run values are then used to create weights for statistics like wOBA (weighted on-base average) and wRC+ (weighted runs created plus).

## Project Overview

### Goals
- Expand the functionality of the baseballr library to provide comprehensive analytics for NCAA baseball
- Develop a play-by-play parser to extract relevant data for creating advanced statistics
- Implement run expectancy calculations and generate a run expectancy matrix for each game situation
- Calculate run values for each batting outcome to create weights for statistics like wOBA and wRC+
- Contribute to the open-source community by updating the park factor function in baseball for accurate league-adjusted statistics in the NCAA

### Key Features
- Fangraphs-style Guts! dataframe: generates a comprehensive Guts! dataframe with detailed information for all NCAA baseball division
- Play-by-play parser: extracts play-by-play data from NCAA baseball games to feed into the analysis pipeline
- Run expectancy matrix: creates a matrix that calculates the expected number of runs based on the number of outs and the position of runners on base
- Run value calculation: assigns run values to different batting outcomes to determine their impact on scoring runs
- Advanced statistics: utilized weights derived from run values and other insights to calculate advanced statistics for both hitters and pitchers such as wOBA, wRC+, and FIP

## Contributions to baseballr Library
As part of this project, I have also made contributions to the baseballr library on GitHub. Specifically, I have updated the park factor function to enable the calculation of accurate league-adjusted statistics in the NCAA. This contribution aims to provide more reliable insights into the performance of baseball players and teams in the unique NCAA hitting and pitching environments.

## Skills Showcased
- Data Wrangling: parsing play-by-play data, cleaning and organizing datasets for analysis
- Statistical Analysis: calculating run expectancy, assigning run values, and deriving weights for advanced statistics
- Programming Languages: R, used extensively for data analysis and statistical calculations
- Collaboration and Contribution: contributing to the baseballr library to improve its functionality and expand its scope
