# Recordscript
a fun language

## Boolean expression language Grammar

```txt
Bool := true | false
Expression := Bool | if <Expression> then <Expression> else <Expression>
```

# RecordScript Grammar

Parentheses () are literal, triangle brackets <> are not.

```
number := Int
var : starts with a letter, contains only letters and digits
bool := true | false
value := number | var | bool
op := + -
bracketedExpr := (expression)
letExpr := let <var e> ..1 e
binopExpr := <bracketedExpr | simpleValue> op expression | (expression)
expression := value | letExpr | binopExpr

```