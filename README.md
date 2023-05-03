# acton_elections
This repository contains my work for processing the data and visualizing the results of elections in Acton, MA.


<h2>R Processing Files</h2>
Everything in this main directory was used in some capacity to process the code and prepare the data for Tableau.

<body>
  <ul>
    <li>`annual_election_2023.R`: This parsed the pdf results from the 2023 Annual Town Election into a tibble.</li>
    <li>`cleaned_data_mapping.R`: This file was a cleaned version of `scrap.R` in which I brainstormed the type of plots I could make in Tableau before moving my data into Tableau (this helped guide how I cleaned my data before uploading them to Tableau following the `cleaning_data_for_t.R` process).</li>
    <li>`cleaning_data_for_t.R`: Once each of the election results were cleaned, this R file compiled the data and calculated metrics such as voter turnout, the percentage of votes cast that a candidate won, and the percentage of registered voters that a candidate won.</li>
    <li>`clipping_acton.R`: I used the pairwise clip to trim the 2019 ACS Census data down to the size of Acton.</li>
    <li>`midterms_2022.R`: This parsed the pdf results from the 2022 Midterm Election as well as local candidates into a tibble.</li>
    <li>`presidential_election_2008.R`: This parsed the pdf results from the 2008 Presidential Election into a tibble.</li>
    <li>`presidential_election_2012.R`: This parsed the pdf results from the 2012 Presidential Election into a tibble.</li>
    <li>`presidential_election_2016.R`: This parsed the pdf results from the 2016 Presidential Election into a tibble.</li>
    <li>`presidential_election_2020.R`: This parsed the pdf results from the 2020 Presidential Election into a tibble.</li>
    <li>`recent_presidential_elections.R`: Once each of the Presidential Elections were parsed, this file aggregated those elections and performed an additional cleaning step to add Party affiliation and parsing of the candidates' last names.</li>
    <li>`scrap.R`: This file served as motivation for my cleaning steps. It did not directly contribute to my Tableau dashboard, but it helped keeping track of the various tasks that I needed to perform and list the ways in which I could meet my goals.</li>
    <li>`special_election_2021.R`: This parsed the pdf results from the 2021 Special Town Election to fill a vacancy on the Select Board into a tibble.</li>
  </ul>
</body>

<h2> Data </h2>
My geographic data as well as the pdf downloads for each election in Acton are without folders.

<h3> R Processed </h3>
These csv files are the result of processing the pdfs into csvs via my R scripts.

<h3> Data For Tableau </h3>
These data are the further processed data from the `R Processed` folder that were acceptable for Tableau.
