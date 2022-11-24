@groovy.transform.ToString
class Image {
    int timestamp
    int[][] intensities

    Image(int timestamp, int[][] intensities) {
        this.timestamp = timestamp
        this.intensities = intensities
    }

    boolean hasAsteroid() {
        def empty = intensities.every { row ->
            row.every { it == 0 }
        }
        !empty
    }

    int rowCount() {
        intensities.size()
    }

    int colCount() {
        intensities[0].size()
    }

    String picture() {
        def img = intensities.collect { row ->
            row.collect { v ->
                if (v > 0) 'x' else ' '
            }.join(' ')
        }.join('\n')
        "ts = $timestamp; dim=${rowCount()}x${colCount()}\n$img"
    }

    boolean rowHasIntensity(int[] row) {
        row.any { it > 0 }
    }

    int minX() {
        for(int i=0; i<rowCount(); i++) {
            if (rowHasIntensity(intensities[i])) {
                return i
            }
        }
        -1
    }

    int maxX() {
        for(int i=rowCount()-1; i>=0; i--) {
            if (rowHasIntensity(intensities[i])) {
                return i
            }
        }
        -1
    }

    int minY() {
        for(int c=0; c<colCount(); c++) {
            for(int r=0; r<rowCount(); r++) {
                if (intensities[r][c] > 0) {
                    return c
                }
            }
        }
        -1
    }

    int maxY() {
        for(int c=colCount()-1; c>=0; c--) {
            for(int r=0; r<rowCount(); r++) {
                if (intensities[r][c] > 0) {
                    return c
                }
            }
        }
        -1
    }

    Image zoomIn() {
        def minx = minX()
        def maxx = maxX()
        def miny = minY()
        def maxy = maxY()
        def rowCount = maxx - minx + 1
        def colCount = maxy - miny + 1
        def newIntensities = new int[rowCount][colCount]
        for(int x=0; x<rowCount; x++) {
            for(int y=0; y<colCount; y++) {
                newIntensities[x][y] = intensities[x+minx][y+miny]
            }
        }
        new Image(timestamp, newIntensities)
    }

    List<List<Integer>> coordinatesWithValues() {
        def coords = []
        for(int r=0; r<rowCount(); r++) {
            for(int c=0; c<colCount(); c++) {
                if (intensities[r][c] > 0) {
                    coords << [r, c]
                }
            }
        }
        coords
    }

    List<Integer> shape() {
        [rowCount(), colCount()] + coordinatesWithValues()
    }
}
@groovy.transform.ToString
class Input {
    int startTime
    int endTime
    List<Image> images

    Input(int startTime, int endTime, List<Image> images) {
        this.startTime = startTime
        this.endTime = endTime
        this.images = images
    }
}

class Parser {
    List<String> remainingLines
    Parser(File f) {
        remainingLines = f.readLines()
    }

    Input parse() {
        def (start, end, count) = header()
        def images = (0..<count).toList().collect {
            image()
        }
        new Input(start, end, images)
    }

    private List<Integer> header() {
        def (startS, endS, countS) = consumeLine()
        [startS as int, endS as int, countS as int]
    }

    private Image image() {
        def (tsS, rowS, colS) = consumeLine()
        def ts = tsS as int
        def rows = rowS as int
        def cols = colS as int
        def intensities = new int[rows][cols]
        (0..<rows).toList().each { r ->
            consumeLine().collect { it as int }.eachWithIndex { v, c ->
                intensities[r][c] = v
            }
        }
        new Image(ts, intensities)
    }

    private List<String> consumeLine() {
        def l = remainingLines.first()
        remainingLines = remainingLines.tail()
        l.tokenize()
    }
}

def dumpImages = { images ->
    images.each { image ->
        println('-' * 60)
        println(image.picture())
        println('-' * 60)
    }
}

def detectSameAsteroids = { l ->
    def possibilities = [[list:[l[0]], period:0]]
    for(int ix = 1; ix< l.size(); ix++) {
        possibilities.addAll possibilities.findAll { it.period == 0 }.collect {
            [list: it.list + [l[ix]], period: l[ix] - it.list.last() ]
        }
        possibilities.addAll possibilities.findAll { it.period > 0 && it.period == (l[ix]-it.list.last()) }.collect {
            [list: it.list + [l[ix]], period: it.period ]
        }
        possibilities.removeAll possibilities.findAll { it.list.size() > 1 && it.period < (l[ix] - it.list.last())  }
        possibilities << [list: [l[ix]], period: 0]
    }

    def res = []

    while(possibilities.size() > 0) {
        def currentMax = possibilities.max { it.list.size() }
        if (currentMax.list.size() < 4) break;
        res << currentMax
        possibilities.removeAll possibilities.findAll { it.list.any { it in currentMax.list} }
    }

    res
}

def detectSameAsteroidsOnlyList = { timestamps ->
    detectSameAsteroids(timestamps)*.list
}

def shapeGroups = { images ->
    images*.zoomIn().groupBy { it.shape() }.values()
}

def input = new Parser(new File('input.txt')).parse()
def asteroidImages = input.images.findAll { it.hasAsteroid() }
shapeGroups(asteroidImages).each { groups ->
    println groups*.timestamp
}
println('-' * 60)
def sg = shapeGroups(asteroidImages).collectMany { detectSameAsteroidsOnlyList(it*.timestamp) }
def outLines = sg.sort { it.first() }.collect { xs ->
    "${xs.first()} ${xs.last()} ${xs.size()}"
}

outLines.each { println it }

new File('output.txt').text = outLines.join('\n')
