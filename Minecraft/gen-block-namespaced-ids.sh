# get the source from this wiki page and put it in blocks.txt
# https://minecraft.gamepedia.com/Java_Edition_data_values/Blocks
cat blocks.txt  | grep '{{code' | awk '{ print $2 $3}' | sed 's/{{code|//' | sed 's/}}//' | sed 's/style="background-color:#D3D3D3;"|//' > block_namespaced_ids.txt

