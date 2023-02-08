import pandas as pd
import geopandas as gpd
import transit_service_analyst as tsa
from pathlib import Path


def frequent_stops(tsa_instance, list_of_hours, min_tph):
    freq = tsa_instance.get_tph_at_stops()
    # create binary column for each hour
    cols = []
    for hour in list_of_hours:
        # print (len(freq))
        freq = freq[freq[hour] >= min_tph]
    freq["frequent_sum"] = freq[list_of_hours].sum(axis=1)
    return freq["stop_id"].values.tolist()


def get_level_stops(tsa_instance, level, service_list, buffer_size):
    x = 0
    for item in service_list:
        stops = frequent_stops(item[0], item[1], item[2])
        if x == 0:
            all_stops = stops
        else:
            all_stops = [x for x in all_stops if x in stops]
        x += 1
    all_stops = tsa_instance.stops[tsa_instance.stops["stop_id"].isin(all_stops)]
    all_stops = gpd.GeoDataFrame(
        all_stops, geometry=gpd.points_from_xy(all_stops.stop_lon, all_stops.stop_lat)
    )
    all_stops = all_stops.set_crs(4326)
    all_stops = all_stops.to_crs(2285)
    buffer = all_stops.copy()
    buffer.geometry = buffer.geometry.buffer(buffer_size)
    return level, all_stops, buffer


gtfs_path = Path("W:/gis/projects/OSM/Transit/Transit_2022/gtfs/combined_spring_2022")

sunday_tsa = tsa.load_gtfs(gtfs_path / "sunday", "20220403")
saturday_tsa = tsa.load_gtfs(gtfs_path / "saturday", "20220402")
weekday_tsa = tsa.load_gtfs(gtfs_path / "weekday", "20220330")


# level 1

weekday_hours_all_day = [
    "hour_9",
    "hour_10",
    "hour_11",
    "hour_12",
    "hour_13",
    "hour_14",
    "hour_15",
    "hour_16",
]
weekday_hours_peak = [
    "hour_6",
    "hour_7",
    "hour_8",
    "hour_17",
    "hour_18",
    "hour_19",
    "hour_20",
    "hour_21",
]
weekend_hours = [
    "hour_9",
    "hour_10",
    "hour_11",
    "hour_12",
    "hour_13",
    "hour_14",
    "hour_15",
    "hour_16",
]

service_dict_list = [
    {
        "name": "level_1",
        "service_list": [
            (sunday_tsa, weekend_hours, 4),
            (saturday_tsa, weekend_hours, 4),
            (weekday_tsa, weekday_hours_all_day, 5),
            (weekday_tsa, weekday_hours_peak, 4),
        ],
        "buffer_size": 2640,
    },
    {
        "name": "level_2",
        "service_list": [
            (sunday_tsa, weekend_hours, 2),
            (saturday_tsa, weekend_hours, 2),
            (weekday_tsa, weekday_hours_all_day, 4),
            (weekday_tsa, weekday_hours_peak, 2),
        ],
        "buffer_size": 2640,
    },
    {
        "name": "level_3",
        "service_list": [
            (sunday_tsa, weekend_hours, 1),
            (saturday_tsa, weekend_hours, 1),
            (weekday_tsa, weekday_hours_all_day, 2),
            (weekday_tsa, weekday_hours_peak, 1),
        ],
        "buffer_size": 1320,
    },
]

x = 0
results_list = []
for dict in service_dict_list:
    results_list.append(
        get_level_stops(
            weekday_tsa, dict["name"], dict["service_list"], dict["buffer_size"]
        )
    )


for item in results_list:
    item[1].to_file(f"T:/2023February/Stefan/parking_bill/{item[0]}_stops.shp")
    item[2].to_file(f"T:/2023February/Stefan/parking_bill/{item[0]}_stops_buffer.shp")


print("done")
