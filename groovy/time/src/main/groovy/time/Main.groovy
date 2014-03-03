package time

import groovy.time.TimeCategory

class Main {

    static void main(String[] args) {
        println "1 year ago: $oneYearAgo"
        println "2 months from now: $twoMonthsFromNow"
        println "end of the year: $endOfTheYear"

        def event = Date.parse('yyyy-MM-dd HH:mm:ss', '1985-02-03 04:05:06')
        println "event + 4 weeks: ${plus4Weeks(event)}"
    }

    static Date getOneYearAgo() {
        use (TimeCategory) { 1.year.ago }
    }

    static Date getTwoMonthsFromNow() {
        use (TimeCategory) { 2.months.from.now }
    }

    static Date getEndOfTheYear() {
        def zeroBased = 1
        new Date().copyWith(
            month      : 12 - zeroBased,
            dayOfMonth : 31
        ).clearTime()
    }

    static Date plus4Weeks(Date d) {
        use (TimeCategory) { d + 4.weeks }
    }

}
