
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ValuGaps_Data

<!-- badges: start -->

<!-- badges: end -->

This git repo allows you to access the data used in the ValuGaps project
and understand the process of creating the dataset. If you only want to
download the raw dataset, you can find it on the iData repository \[LINK
WILL FOLLOW\]

If you want to work with the repo, you first have to clone itYou can
then run the script `clean_data.R` to create the dataset used in the
ValuGaps project. It will automatically download the data. This is the
recommended dataset to use for the project, as it is cleaned and ready
to use.

## Description of the output

If you run the script clean_data.R, four objects will be created. For
choice analysis, use `database`. For everything else use
`complete_data`. The other two objects are `raw_data` and `all_data`.
`raw data` is a list that contains the dataset from each survey round.
While it is already cleaned, it contains all observations including
those that were not complete. `all_data` is the same as `raw_data` but
merged into one dataframe. Both `raw_data` and `all_data` are useful if
you want to inspect dropouts or want to include observations that are
not complete but may contain the relevant information for your analysis.

`@tbl-dataset-summary` is a summary of the datasets created by
`clean_data.R`:

<div id="tbl-dataset-summary">

| **Object Name** | **Description** | **Format** | **Usage** |
|----|----|----|----|
| `database` | Prepared dataset specifically for choice analysis. | Dataframe | Choice analysis |
| `complete_data` | Fully cleaned dataset with only complete observations. | Dataframe | Most analyses (regression, classification, clustering, etc.) |
| `raw_data` | A list containing the dataset from each survey round. Includes all observations, even incomplete ones. | List of dataframes | Inspecting dropouts, including incomplete observations for analysis |
| `all_data` | Merged version of `raw_data` into a single dataframe. | Dataframe | Same as `raw_data`, but structured as one dataset |

</div>
