# Recordscript
a fun language

## Boolean expression language Grammar

```txt
Bool := true | false
Value := Bool
Expression := <Value> | if <Expression> then <Expression> else <Expression>
```

# SExpression Grammar
```
number
var
value := number | var
op := + -
expression := value | (let <var e> ..1 e) | (op expression expression)
```