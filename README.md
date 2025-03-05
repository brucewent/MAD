## MAD

The [Mercer Area District](https://www.wccscouting.org/mercer-area) of [Scouting America](https://www.scouting.org/) (MAD) serves Mercer County New Jersey along with several adjacent towns. This project is a volunteer effort to analyze scout leader training records using R.

Note that the primary input data contains Protected Personal Information (PPI), so it is not tracked here. You may be authorized to download your own district or council-wide data from the My Scouting portal's [Training Manager](https://my.scouting.org/).

The source file [trained.R](https://github.com/brucewent/MAD/blob/main/trained.R) does the following:

-   Load the two source data files (Trained Leaders Status, YPT detailed CSVs)
-   Clean up by removing inactive or duplicate rows
-   Summarize unit leader training counts
-   Write a spreadsheet with three sheets:
    -   Units - the unit summary
    -   Trained - registered positions and position-specific training status
    -   Leader - distinct adult leaders and youth protection training status
-   For each unit, write a spreadsheet containing unit-filtered training records (excluding PPI) along with the unit Key 3 email addresses

### License

See the [LICENSE](https://github.com/brucewent/MAD/blob/main/LICENSE) file for license rights and limitations (MIT).
