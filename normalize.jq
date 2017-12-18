# normalize the recipes list. Each result should be in the form of the
# "results" array, and normal/expensive recipes are reduced to the
# normal version.
# jq -sf normalize.jq < recipes.json > recipes-normalized.json

map(
if has("normal") then
  del(.["normal", "expensive"]) + .normal
else . end
|
if has("results") then .
else
  if has("result_count") | not then
    . + { result_count: 1 }
  else . end
  | 
  . + { results: [{ amount: .result_count, name: .name }] }
end
|
setpath(["ingredients"];
        .ingredients |
         map(if type | . == "object" then .
	     else { name: .[0], amount: .[1] } end
	     )
       )
|
if has("energy_required") then .
else .energy_required = 0.5
end
)