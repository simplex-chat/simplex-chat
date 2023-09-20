{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

getZones :: DB.Connection -> IO [ZoneId]
getZones db = DB.query_ db [sql| SELECT zone_id FROM zones; |]

getZoneInfo :: DB.Connection -> ZoneId -> IO ZoneInfo
getZoneInfo db zoneId = DB.query db [sql| SELECT zone_id, display_name, kind, properties FROM zones WHERE zone_id = ?; |] (Only zoneId)
