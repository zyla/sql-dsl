{-# LANGUAGE DataKinds, TypeOperators, QuasiQuotes #-}
import SQL
import TH

newtype StationId = StationId { unStationId :: Int } deriving (Eq, Show)

type AppSchema = '[ BikeTable, StationTable, RentalTable ]

type BikeTable = Table "bike"
  '[ Binding "id" Int
   , Binding "number" String -- human-readable identifier, like 60312
   ]

type StationTable = Table "station"
  '[ Binding "id" StationId
   , Binding "name" String
   ]

type RentalTable = Table "rental"
  '[ Binding "id" Int
   , Binding "begin_station_id" StationId
   , Binding "end_station_id" StationId  
   ]

type RentalViewColumns =
  '[ Binding "rental_id" Int
   , Binding "begin_station_name" String
   , Binding "end_station_name" String
   ]

userRentalsQuery :: Select AppSchema RentalViewColumns
userRentalsQuery = [sql|
    SELECT rental.id AS rental_id,
           begin_station.name AS begin_station_name,
           end_station.name AS end_station_name
    FROM rental AS rental, station AS begin_station, station AS end_station
    WHERE begin_station.id = begin_station_id
      AND end_station.id = end_station_id
  |]

