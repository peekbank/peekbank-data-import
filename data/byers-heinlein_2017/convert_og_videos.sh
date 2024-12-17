# script to convert the original avi videos of the study to something that can
# be played back using vlc and the likes. This is the first step to then extract
# screenshots.
# ./convert_og_videos.sh ./path/to/videos/from/montat/study

for i in "$1"/*.AVI; do
  ffmpeg -i "$i" -c:v libx264 -crf 15 -c:a aac -b:a 128k "${i%.*}.mp4"
done