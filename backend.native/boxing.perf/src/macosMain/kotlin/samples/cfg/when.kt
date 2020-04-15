package samples.cfg

fun simpleIf() {
    val x = 1
    if (x < 28) {
        println("y")
    }
    val z = 2
}

fun simpleIfElse() {
    val x = 1
    if (x < 28) {
        println("y")
    } else {
        println("x")
    }
    val z = 2
}

fun simpleWhen() {
    val x = 1
    when {
        x < 28 -> println("y")
        x < 50 -> println("x")
//        else -> println("t")
    }
    val z = 2
}

fun simpleWhenElse() {
    val x = 1
    when {
        x < 28 -> println("y")
        x < 50 -> println("x")
//        else -> println("t")
    }
    val z = 2
}

fun cfgWhen() {
    simpleIf()
    simpleIfElse()
    simpleWhen()
    simpleWhenElse()
}