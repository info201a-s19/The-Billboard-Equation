# Midpoint Deliverable Rubric

This rubric is intended to help guide expectations and increase transparency. However, it is not necessarily fully exhaustive. Make sure to read the detailed explanation on Canvas, and follow the best practices described in the course book and in class.

## Introductory paragraph (**3 points**)
Includes an introductory paragraph describing:
- The purpose/importance of the project (**2 points**)
- The source of the data of the project (**1 point**)

## Summary Information (**15 points**)
Computes (and includes) 5 pieces of _relevant_ information using a function saved in a `script/` file. For each piece of information, make sure to:
- Compute the information accurately in the script (**1 point**)
- Include the piece of information using in-line R code in the paragraph (**1 point**)
- Make clear the relevance of including the information (if not apparent) (**1 point**)

## Summary Table (**10 points**)
- Introduces the table, explaining why the particular grouping calculation was performed (**2 points**)
- Creates a summarized data frame to include as the table using `group_by()` (**2 points**)
- Intentionally sorts the table in a relevant way (**1 point**)
- Only displays relevant columns in the table (**1 point**)
- Displays well formatted column names (**1 point**)
- Successfully renders the table in the report using an appropriate package (e.g., don't just print out the data frame) (**1 point**)
- Interprets information the table, honing in on important information (**2 points**)

## Charts (**30 points**, 10 points each)
For each chart, you will be evaluated based on the following (remember, each chart must be a **different chart type**):
- A description of what the chart attempts to answer/understand (**1 point**)
- Selected the appropriate chart type / graphical encoding based on the question of interest and the data type(s) of the features (**3 point**)
- Appropriate labels/legends, depending on your chart type (**4 points**). Including, but not limited to:
    - X axis label (if there's an x axis)
    - Y axis label (if there's an y axis)
    - Title (if there's an ability to add a title -- e.g., not a map)
    - Legend for any encodings that are not immediately apparent
- Interprets information from the chart, honing in on important information (**2 points**)

## Code and Report structure (**7 points**)
Code is broken up appropriately into the described files. 
- Each file is in the appropriate folder: `scripts/`, except `index.Rmd`, which is in the root (**1 point**)
- Each file is loaded into the `index.Rmd` file (**1 point**)
- Only the `index.Rmd` file loads the data (**1 point**)
- Project uses appropriate packages and logic throughout to produce the report (**1 points**)
- All unused code is removed. (**1 point**)
- Comments are used throughout to express purpose of each section (**1 point**)

Report is professionally formatted, including (but not limtted to):
- Using headers to delineate between sections (**1 point**)
- Removing any warnings/messages from your script in the report (**1 point**)


## Code clarity (**5 points**)
To earn full points, you must not have any `lintr()` errors. 
- -1 point for 1 - 2 `lintr` errors
- -2 points for 3 - 4 `lintr` errors
- -3 points for 5 - 6 `lintr` errors
- -4 points for 7 - 8 `lintr` errors
- -5 points for 9+ `lintr` errors