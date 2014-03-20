package app

import org.codehaus.groovy.ast.*
import org.codehaus.groovy.ast.expr.*
import org.codehaus.groovy.ast.stmt.*
import org.codehaus.groovy.ast.builder.AstBuilder
import org.codehaus.groovy.control.CompilePhase
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.transform.ASTTransformation
import org.codehaus.groovy.transform.GroovyASTTransformation

@GroovyASTTransformation(phase = CompilePhase.SEMANTIC_ANALYSIS)
class DslTransformation implements ASTTransformation {

    @Override
    void visit(ASTNode[] nodes, SourceUnit sourceUnit) {
        def uses = findDslUses(nodes)

        uses.each { variableToPropertyDeclaration(it) }
    }

    List<ClosureExpression> findDslUses(ASTNode[] modules) {
        modules.collectMany { module ->
            module.classes.collect { clazz ->
                def field = clazz.getDeclaredField('dsl')
                isDslUse(field) ? field.initialExpression : null
            }.grep()
        }
    }

    boolean isDslUse(FieldNode node) {
        node && node.isStatic() && node.initialExpression instanceof ClosureExpression
    }

    void variableToPropertyDeclaration(ClosureExpression expr) {
        def block = expr.code

        def newStatements = block.statements.collect { stmt ->
            if (stmt instanceof ExpressionStatement) {
                asPropertyMethodCall(stmt.expression)
            } else {
                stmt
            }
        }

        block.statements = newStatements
    }

    Statement asPropertyMethodCall(Expression expr) {
        if (expr instanceof VariableExpression) {
            propertyMethodCall(expr.name)
        } else if (expr instanceof PropertyExpression) {
            def property = nestedPropertiesAsString(expr)
            propertyMethodCall(property)
        } else {
            new ExpressionStatement(expr)
        }
    }

    ExpressionStatement propertyMethodCall(String property) {
        def nodes = new AstBuilder().buildFromSpec {
            methodCall {
                variable "this"
                constant "property"
                argumentList {
                    constant property
                }
            }
        }

        new ExpressionStatement(nodes.first())
    }

    String nestedPropertiesAsString(Expression expr) {
        if (expr instanceof ConstantExpression) {
            expr.value as String
        } else if (expr instanceof VariableExpression) {
            expr.name
        } else {
            nestedPropertiesAsString(expr.objectExpression) + '.' + nestedPropertiesAsString(expr.property)
        }
    }

}
