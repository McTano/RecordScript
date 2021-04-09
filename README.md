# Recordscript
a fun language

## Boolean expression language Grammar

```txt
Bool := true | false
Expression := Bool | if <Expression> then <Expression> else <Expression>
```

# SExpression Grammar

Parentheses () are literal, triangle brackets <> are not.

```
number
var
value := number | var
op := + -
bracketedExpr := (expression)
letExpr := let <var e> ..1 e
binopExpr := <bracketedExpr | simpleValue> op expression | (expression)
expression := value | letExpr | binopExpr
```