(ns infinite-raid.core
  (:import [squidpony.squidgrid.mapping DungeonGenerator DungeonUtility]
           [squidpony.squidgrid.mapping.styled TilesetType])
  (:gen-class))

(defn -main
  [& args]
  (println (.toString
             (doto (DungeonGenerator. 80 40) (.addWater 15) (.generate TilesetType/DEFAULT_DUNGEON)))))

