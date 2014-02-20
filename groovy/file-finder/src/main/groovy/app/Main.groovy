package app

class Main {

    static void main(String[] args) {
        def finder = new FileNameFinder()
        println basedir
        def names  = finder.getFileNames(basedir, "**/*.groovy", "**/target/**,**/xforeign/**")
        names.each { println it }
    }

    private static getBasedir() {
        def home = System.getProperty('user.home')
        new File(home, "workspace").absolutePath
    }

}
