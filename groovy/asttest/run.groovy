import groovy.transform.ASTTest

@ASTTest(phase=SEMANTIC_ANALYSIS, value = {
    println node.rightExpression
})
def f = {
    println 1
}
