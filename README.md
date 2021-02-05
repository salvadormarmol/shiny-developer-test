# shiny-developer-test

# Goals

- User can select a vessel type (Ship_type) from the dropdown field
- User can select a vessel from a dropdown field (available vessels should correspond to the selected type). Dropdown fields was created as a Shiny module
- For the vessel selected, I found the observation when it sailed the longest distance between two consecutive observations. If there was a situation when a vessel moved exactly the same amount of meters, I selected the most recent.  
- I display that on the map - and show two points, the beginning and the end of the movement. The map was created using the leaflet library. Changing type and vessel name re-rends the map and the note.
- I provided a short note saying how much the ship sailed - distance should be provided in meters.
- I use the best practices I know, to ensure project quality. The application is reasonably efficient and tested.
- I added an additional visualizations to see a box plot of the sailed distances
