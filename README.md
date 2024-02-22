# Higher Education Student Data

## Data

The data is taken from the [HESA website](https://www.hesa.ac.uk/data-and-analysis/students).

Data for where students come from is from the [where from page](https://www.hesa.ac.uk/data-and-analysis/students/where-from) on the HESA website, under the *UK domiciled HE students by HE provider and domicile* section. Data was manually extracted for years 2014 through to 2021 for England only. 

## Where do students come from?

### Who is going to university in their home town or city?

Extracted data was cleaned using R, mostly using the `tidyverse` set of packages. Only 6 universities were selected: Oxford and Cambridge, as well as four other English universities in the top twenty rankings from 2023. 

![](https://github.com/andrewmoles2/HE-students/blob/main/outputs/home_uni.png)

The visual is also made with Tableau and is viewable on [Tableau Public](https://public.tableau.com/views/HigherEducationStudentData/sametowncityHEStudents?:language=en-GB&:display_count=n&:origin=viz_share_link). A Tableau file is also available in this repository. 

### LSE Students - how many come from London compared to the total numbers per year?

![](https://github.com/andrewmoles2/HE-students/blob/main/outputs/lse_london.png)
