

path=""
i="0"
while true;
do
    json=$(curl -s "https://api.baidu.com/run?search=shortId:^${sample}$%20status:^completed$&token=abcdefghigklmnopqrstuvwxyz0&limit=999")
    path=$(echo $json | awk -F"\"path\":" '{print $2}' | awk -F"," '{print $1}' | sed 's/"//g')

    sample_id=$(echo $json | awk -F"\"sample_id\":" '{print $2}'| awk -F"," '{print $1}' | sed 's/"//g')
    pathdir="/k11e/pvdisk/bigbase/kbdata/sampledata$path"

    i=`echo "$i + 1"|bc`
    echo "$sample search $i times"

    if [ $path ]; then
            break
    fi

    sleep 200
done
