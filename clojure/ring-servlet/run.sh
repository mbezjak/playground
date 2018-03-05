#!/bin/bash
#

set -o errexit

declare -r tomversion=8.5.28
declare -r tomdir=target/apache-tomcat-$tomversion

if [[ -d $tomdir ]]; then
    $tomdir/bin/shutdown.sh
    sleep 2
fi
rm -rf target

lein uberjar
wget http://ftp.carnet.hr/misc/apache/tomcat/tomcat-8/v$tomversion/bin/apache-tomcat-$tomversion.tar.gz
tar -xf apache-tomcat-$tomversion.tar.gz
rm apache-tomcat-$tomversion.tar.gz
mv apache-tomcat-$tomversion target
mkdir -p $tomdir/webapps/test/WEB-INF/lib
cp target/uberjar/*-standalone.jar $tomdir/webapps/test/WEB-INF/lib
cp web.xml $tomdir/webapps/test/WEB-INF/

$tomdir/bin/startup.sh
sleep 2
echo -e "\n\nVisit http://localhost:8080/test/\n\n"
tail -f $tomdir/logs/localhost.*.log

exit 0
