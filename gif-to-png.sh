# https://unix.stackexchange.com/questions/19654/how-do-i-change-the-extension-of-multiple-files
# Rename all *.txt to *.text
for f in *.gif; do 
    mv -- "$f" "${f%.txt}.png"
done