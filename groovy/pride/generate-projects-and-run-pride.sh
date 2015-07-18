#!/bin/bash
#

set -o errexit

declare -r projectsdir=/tmp/pride-example-projects
declare -r libdir=$projectsdir/lib
declare -r appdir=$projectsdir/app
declare -r pridedir=$projectsdir/pride
declare -r maindir=src/main/groovy/com/example
declare -r testdir=src/test/groovy/com/example

function text-gitignore {
    cat <<'EOF'
.gradle/
build/
EOF
}
function text-library-class {
    cat<<'EOF'
package com.example

class Library {
    static int getNumber() {
        42
    }
}
EOF
}
function text-spec-class {
    cat <<'EOF'
package com.example

import spock.lang.*

class LibrarySpec extends Specification {

    def "it should return number 42"() {
        expect:
        Library.number == 42
    }

}
EOF
}
function text-lib-build-gradle {
    cat <<'EOF'
buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath "com.prezi.pride:gradle-pride-plugin:0.10"
    }
}

apply plugin: 'groovy'
apply plugin: 'maven'
apply plugin: 'pride'

version = '0.1'
group = 'com.example'
sourceCompatibility = 1.7
targetCompatibility = 1.7


repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    compile 'org.codehaus.groovy:groovy-all:2.4.4'
}
EOF
}
function text-lib-settings-gradle {
    echo "rootProject.name = 'lib'"
}
function text-app-build-gradle {
    cat <<'EOF'
buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath "com.prezi.pride:gradle-pride-plugin:0.10"
    }
}

apply plugin: 'groovy'
apply plugin: 'pride'

version = '0.1'
group = 'com.example'
sourceCompatibility = 1.7
targetCompatibility = 1.7


repositories {
    mavenLocal()
    mavenCentral()
}

dynamicDependencies {
    compile 'com.example:lib:0.1'
}

dependencies {
    compile 'org.codehaus.groovy:groovy-all:2.4.4'
    testCompile 'org.spockframework:spock-core:1.0-groovy-2.4'
}
EOF
}
function text-app-settings-gradle {
    echo "rootProject.name = 'app'"
}




rm -rf $projectsdir
mkdir $projectsdir


mkdir $libdir
cd $libdir
mkdir -p $maindir
text-gitignore           > .gitignore
text-lib-build-gradle    > build.gradle
text-lib-settings-gradle > settings.gradle
text-library-class       > $maindir/Library.groovy
gradle --quiet install
git init --quiet
git add .
git commit --quiet -m "Initial version of lib"


mkdir $appdir
cd $appdir
mkdir -p $testdir
text-gitignore           > .gitignore
text-app-build-gradle    > build.gradle
text-app-settings-gradle > settings.gradle
text-spec-class          > $testdir/LibrarySpec.groovy
git init --quiet
git add .
git commit --quiet -m "Initial version of app"


mkdir -p $pridedir
cd $pridedir
pride init &> /dev/null
pride add --repo-base-url $projectsdir app lib &> /dev/null
./gradlew check


sed -i 's|42|11|' $pridedir/lib/$maindir/Library.groovy
./gradlew check

exit 0
