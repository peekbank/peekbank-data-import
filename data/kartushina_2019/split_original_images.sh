#!/bin/bash
# needs imagemagick installed

if [ -z "$1" ]; then
    echo "Usage: $0 /path/to/directory"
    exit 1
fi

directory="$1"
cd "$directory" || exit 1

for file in *_*.png; do
    [ -e "$file" ] || continue
    
    part1=$(echo "$file" | cut -d'_' -f1)
    part2=$(echo "$file" | cut -d'_' -f2)
    
   
    magick "$file" -crop 2x1@ +repage +adjoin tmp_%d.png
    
    magick tmp_0.png -crop 67%x50%+33%+250% "${part1}.png"
    
    magick tmp_1.png -crop 67%x50%+0+250% "${part2}"
    
    rm tmp_*.png
done
