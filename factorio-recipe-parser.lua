json = require "json"

data = {}
data["extend"] = function (data, t)
   for n, recipe in ipairs(t) do
      print(json.encode(recipe))
   end
end

files = {
   "ammo",
   "capsule",
   "demo-furnace-recipe",
   "demo-recipe",
   "demo-turret",
   "equipment",
   "fluid-recipe",
   "furnace-recipe",
   "inserter",
   "module",
   "recipe",
   "turret",
}

for i, f in ipairs(files) do
   dofile("/home/robert/.steam/steam/steamapps/common/Factorio/data/base/prototypes/recipe/" .. f .. ".lua")
end
