# Formal Grammar

lower_case_character ::= a...z

upper_case_character ::= A...Z

character ::= lower_case_character | upper_case_character

identifier ::= ( character | _) { character | 0...9 | _ | '}

escape_literal ::= \(\ | n | t | b | r ) | \(0...9)(0...9)(0...9)

character_literal ::= '?regular? character' | 'escape_literal' 

string_literal ::= "{character}" | "{newline character}"

integer_literal ::= [-] (0...9) { 0...9 }

float_literal ::= [-] (0...9) {0...9 | .}

boolean_literal ::= true | false

polymorphic_literal ::= '{lower_case_character}

infix ::= (core_operator_char | operator_char)

prefix ::= ! | ~  

operator_char ::= ! | ~ | % | < | : | . | core_operator_char

core_operator_char ::= & | + | - | * | / | = | > | ++ | ^ | | 

keywords ::= reserved_keywords | algebra_keywords | sequence_keywords

reserved_keywords ::= ```
let fun return if else type match with of use std
```

algebra_keywords ::= ```
union record ok err some none list tuple option result hashmap int float string char bool unit 
```

sequence_keywords ::= ``` 
!= | ' | ( | ) | -> | <- | .. | :: | == | , | _ | { | } | [ | ] | ; | ;;
```

## Note
Everything is an expression.

Also, a semicolon defines a seequence so `(3*3); + 3;;` will add 9 and 3 = 12`, but `(3*3) + 3` is invalid



