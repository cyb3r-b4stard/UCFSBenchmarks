package org.iguana.datadependent.traversal;

import org.iguana.datadependent.ast.Expression.Add;
import org.iguana.datadependent.ast.Expression.And;
import org.iguana.datadependent.ast.Expression.AndIndent;
import org.iguana.datadependent.ast.Expression.Assignment;
import org.iguana.datadependent.ast.Expression.Boolean;
import org.iguana.datadependent.ast.Expression.Call;
import org.iguana.datadependent.ast.Expression.Divide;
import org.iguana.datadependent.ast.Expression.EndOfFile;
import org.iguana.datadependent.ast.Expression.Equal;
import org.iguana.datadependent.ast.Expression.Greater;
import org.iguana.datadependent.ast.Expression.GreaterThanEqual;
import org.iguana.datadependent.ast.Expression.Integer;
import org.iguana.datadependent.ast.Expression.LShiftANDEqZero;
import org.iguana.datadependent.ast.Expression.LeftExtent;
import org.iguana.datadependent.ast.Expression.Less;
import org.iguana.datadependent.ast.Expression.LessThanEqual;
import org.iguana.datadependent.ast.Expression.Multiply;
import org.iguana.datadependent.ast.Expression.Name;
import org.iguana.datadependent.ast.Expression.Not;
import org.iguana.datadependent.ast.Expression.NotEqual;
import org.iguana.datadependent.ast.Expression.Or;
import org.iguana.datadependent.ast.Expression.OrIndent;
import org.iguana.datadependent.ast.Expression.Real;
import org.iguana.datadependent.ast.Expression.RightExtent;
import org.iguana.datadependent.ast.Expression.String;
import org.iguana.datadependent.ast.Expression.Subtract;
import org.iguana.datadependent.ast.Expression.Tuple;
import org.iguana.datadependent.ast.Expression.Val;
import org.iguana.datadependent.ast.Expression.Yield;
import org.iguana.datadependent.ast.Statement;
import org.iguana.datadependent.ast.Statement.Expression;
import org.iguana.datadependent.ast.VariableDeclaration;
import org.iguana.grammar.condition.Condition;
import org.iguana.grammar.condition.DataDependentCondition;
import org.iguana.grammar.condition.PositionalCondition;
import org.iguana.grammar.condition.RegularExpressionCondition;
import org.iguana.grammar.exception.UnexpectedSymbolException;
import org.iguana.grammar.symbol.Align;
import org.iguana.grammar.symbol.Alt;
import org.iguana.grammar.symbol.Block;
import org.iguana.grammar.symbol.Code;
import org.iguana.grammar.symbol.Conditional;
import org.iguana.grammar.symbol.Error;
import org.iguana.grammar.symbol.Group;
import org.iguana.grammar.symbol.IfThen;
import org.iguana.grammar.symbol.IfThenElse;
import org.iguana.grammar.symbol.Ignore;
import org.iguana.grammar.symbol.Nonterminal;
import org.iguana.grammar.symbol.Offside;
import org.iguana.grammar.symbol.Opt;
import org.iguana.grammar.symbol.Plus;
import org.iguana.grammar.symbol.Return;
import org.iguana.grammar.symbol.Star;
import org.iguana.grammar.symbol.Start;
import org.iguana.grammar.symbol.Symbol;
import org.iguana.grammar.symbol.Terminal;
import org.iguana.grammar.symbol.While;
import org.iguana.traversal.IConditionVisitor;
import org.iguana.traversal.ISymbolVisitor;

import java.util.HashSet;
import java.util.Set;

public class ValUses implements IAbstractASTVisitor<Void>, ISymbolVisitor<Void>, IConditionVisitor<Void> {

    public final Set<java.lang.String> labels = new HashSet<>();

    @Override
    public Void visit(DataDependentCondition condition) {
        condition.getExpression().accept(this);
        return null;
    }

    @Override
    public Void visit(PositionalCondition condition) {
        return null;
    }

    @Override
    public Void visit(RegularExpressionCondition condition) {
        return null;
    }

    @Override
    public Void visit(Align symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Block symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Code symbol) {
        visitSymbol(symbol.getSymbol());

        for (Statement stat : symbol.getStatements())
            stat.accept(this);

        return null;
    }

    @Override
    public Void visit(Error error) {
        return null;
    }

    @Override
    public Void visit(Conditional symbol) {
        visitSymbol(symbol.getSymbol());
        symbol.getExpression().accept(this);
        return null;
    }

    @Override
    public Void visit(IfThen symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(IfThenElse symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Ignore symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Nonterminal symbol) {
        for (org.iguana.datadependent.ast.Expression arg : symbol.getArguments())
            arg.accept(this);
        return null;
    }

    @Override
    public Void visit(Offside symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Terminal symbol) {
        return null;
    }

    @Override
    public Void visit(While symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Return symbol) {
        symbol.getExpression().accept(this);
        return null;
    }

    @Override
    public Void visit(Alt symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Opt symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Plus symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Group symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Star symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    @Override
    public Void visit(Start symbol) {
        throw new UnexpectedSymbolException(symbol, "val-uses traversal");
    }

    private void visitSymbol(Symbol symbol) {
        for (Condition cond : symbol.getPreConditions())
            cond.accept(this);

        symbol.accept(this);

        for (Condition cond : symbol.getPostConditions())
            cond.accept(this);
    }

    @Override
    public Void visit(Boolean expression) {
        return null;
    }

    @Override
    public Void visit(Integer expression) {
        return null;
    }

    @Override
    public Void visit(Real expression) {
        return null;
    }

    @Override
    public Void visit(String expression) {
        return null;
    }

    @Override
    public Void visit(Not not) {
        not.getExp().accept(this);
        return null;
    }

    @Override
    public Void visit(Tuple expression) {
        for (org.iguana.datadependent.ast.Expression element : expression.getElements())
            element.accept(this);

        return null;
    }

    @Override
    public Void visit(Name expression) {
        return null;
    }

    @Override
    public Void visit(Call expression) {
        for (org.iguana.datadependent.ast.Expression arg : expression.getArguments())
            arg.accept(this);

        return null;
    }

    @Override
    public Void visit(Assignment expression) {
        expression.getExpression().accept(this);
        return null;
    }

    @Override
    public Void visit(LShiftANDEqZero expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(OrIndent expression) {
        expression.getFirst().accept(this);
        expression.getIndent().accept(this);
        expression.getIndex().accept(this);
        expression.getLExt().accept(this);
        return null;
    }

    @Override
    public Void visit(AndIndent expression) {
        expression.getFirst().accept(this);
        expression.getIndex().accept(this);
        expression.getLExt().accept(this);
        return null;
    }

    @Override
    public Void visit(Or expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(And expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Less expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(LessThanEqual expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Greater expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(GreaterThanEqual expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Equal expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(NotEqual expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(LeftExtent expression) {
        return null;
    }

    @Override
    public Void visit(RightExtent expression) {
        return null;
    }

    @Override
    public Void visit(EndOfFile expression) {
        return null;
    }

    @Override
    public Void visit(org.iguana.datadependent.ast.Expression.IfThenElse expression) {
        expression.getCondition().accept(this);
        expression.getThenPart().accept(this);
        expression.getElsePart().accept(this);
        return null;
    }

    @Override
    public Void visit(Yield expression) {
        return null;
    }

    @Override
    public Void visit(Val expression) {
        labels.add(expression.getLabel());
        return null;
    }

    @Override
    public Void visit(VariableDeclaration declaration) {
        if (declaration.getExpression() != null)
            declaration.getExpression().accept(this);
        return null;
    }

    @Override
    public Void visit(org.iguana.datadependent.ast.Statement.VariableDeclaration declaration) {
        declaration.getDeclaration().accept(this);
        return null;
    }

    @Override
    public Void visit(Add expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Subtract expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Multiply expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Divide expression) {
        expression.getLhs().accept(this);
        expression.getRhs().accept(this);
        return null;
    }

    @Override
    public Void visit(Expression statement) {
        statement.getExpression().accept(this);
        return null;
    }

}
