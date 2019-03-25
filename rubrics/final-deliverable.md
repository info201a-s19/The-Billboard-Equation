# Final Deliverable Rubric

This rubric is intended to help guide expectations and increase transparency. However, it is not necessarily fully exhaustive. Make sure to read the detailed explanation on Canvas, and follow the best practices described in the course book and in class.

## Project Structure (**13 points**)
- Create a multi-page (e.g., multi-tab) layout (**4 points**)
- Navigation tabs have meaningful labels (**2 points**)
- At least 5 custom CSS styles are applied throughout the project (**5 points**, 1 point each)
- Host the project on ShinyApps.io (**2 points**)

## Introductory page (**5 points**)
Includes an introductory paragraph describing:
- The purpose/importance of the project (**2 points**)
- The source of the data of the project (**1 point**)
- Page includes additional (relevant) visual flare beyond the summary paragraph (**2 points**)

## Three interactive pages (66 points // 22 points each)
On each page, the visualization is worth 14 points, and the controls are worth 8:

**Visualization**

- A description of what the chart attempts to answer/understand (**2 point**)
- Selected the appropriate chart type / graphical encoding based on the question of interest and the data type(s) of the features (**4 point**)
- Appropriate labels/legends, depending on your chart type (**4 points**). Including, but not limited to:
    - X axis label (if there's an x axis)
    - Y axis label (if there's an y axis)
    - Title (if there's an ability to add a title -- e.g., not a map)
    - Legend for any encodings that are not immediately apparent
- Provides well-formatted and detailed information when hovering each mark on the chart (**4 points**)

**Controls**
- 1 point for creating a widget with a label
- 1 point for setting an appropriate default value
- 4 points for the widget working (e.g., changing the desired output)
- 2 points for appropriate layout of controls and visualization (e.g., a `sidebarLayout()`)

## Conclusion Page (**21 points**, 7 points each)
For each major takeaway, you should include the following:
- A description of the notable data-insight or pattern discovered in your project (**2 points**)
- A specific piece of data, table, or chart that demonstrates the pattern/insight (**3 points**)
- The broader implications of the insight (**2 points**)

## Code structure (**10 points**)
- Server and UI code is separated into different files (**2 points**)
- Project uses appropriate packages and logic throughout to produce the report (**4 points**)
- All unused code is removed (**1 point**)
- Comments are used throughout to express purpose of each section (**1 point**)
- Clear files names and variable/function names are used throughout the project (**2 points**)

## Code clarity (**5 points**)
To earn full points, you must not have any `lintr()` errors. 
- -1 point for 1 - 2 `lintr` errors
- -2 points for 3 - 4 `lintr` errors
- -3 points for 5 - 6 `lintr` errors
- -4 points for 7 - 8 `lintr` errors
- -5 points for 9+ `lintr` errors