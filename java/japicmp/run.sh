#!/bin/bash
#

set -o errexit

declare -r japicmp=japicmp-0.2.1.jar
declare -r new=commons-lang3-3.3.2.jar
declare -r old=commons-lang3-3.2.jar

if [[ ! -f $japicmp ]]; then
    wget https://github.com/siom79/japicmp/releases/download/japicmp-base-0.2.1/japicmp-0.2.1.jar
    wget http://search.maven.org/remotecontent?filepath=org/apache/commons/commons-lang3/3.3.2/commons-lang3-3.3.2.jar -O $new
    wget http://search.maven.org/remotecontent?filepath=org/apache/commons/commons-lang3/3.2/commons-lang3-3.2.jar -O $old
fi

java -jar $japicmp -n $new -o $old -m -b

exit 0
