# geocode_coyote
Process and functions used to geolocate coyote observations by the California Department of Public Health (CDPH)

SHAREABLE VERSION (i.e. without personal API key)

## Notes:
1. Process
    1. Relied on a paid version of Google Geocoding API. Limits: 2,500 free every 24 hours. 50 cents per 1,000 beyond that. 100,000 entries maximum per 24 hours.
    3. Cleaned-up the county_ID for the towns for more accurate google API look-up, however, bearing+direction likely occurs in the county the colleciton event was specified before modification. These were flagged for manual geocoding
    4. Geocoding API cannot handle cells with empty descriptions (i.e. locations with previously manually geocoded coordinates). A subset of the full dataset was created and ran through the geocoding API.
    5. Manually geocoded any location outside of California with bearing and direction that lead to a location within California.
    6. Process Notes:
        1. Does not include the key to run the getGeoDetails_paid() function to prevent overcharging.
        2. Free version of Google Geocoding API: getGeoDetails_free()
        3. Google API is not effective at finding road intersections. Geocoded these locations manually
        4. For ~ 4,000 locations the function takes ~ 30 minutes to run
    
2. Data structure
    1. Using State, County, and City/Neighborhood, finding lat/long of missing CDPH records
    2. Using bearing and distance from a city/neighborhood, finding lat/long of missing CDPH records
    3. Data notes
        1. Able to handle location descriptions with a maximum of two commas of infromation
        2. Town/city/neighborhood (e.g "Bakersfield") must be separated from bearing and direction (e.g."2N") by a comma (e.g "Bakersfield, 2N")
        3. Locations with "City, Bearing+Direction, place" are difficult to interpret. Primarily looked up the place manually and double checked if close to destination from city following bearing+direction
        4. Phrase after first comma cannot start with a number unless it is a bearing/direction (e.g "5-Dog" will not work)
        5. Extra details in the location description after a comma must not start with a numerical or code detects as a distance+bearing
