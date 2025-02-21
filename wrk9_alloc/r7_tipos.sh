

cat plan.txt | awk -F' ' '{ print $1 }' | sort -u
