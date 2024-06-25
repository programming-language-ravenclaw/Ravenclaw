# Ravenclaw

**TRAINER:** ZAMORANO GONGORA ALICIA

**AREA:** LENGUAJES DE PROGRAMACIÓN

**TEAM:** RAVENCLAW

**MEMBERS:** GUTIERREZ NAVA SALET YASMIN - GARCIA VILLALOBOS GABRIELA - GALINDO CORPA EMANUEL - ESPINOZA VERA LUIS - MENDOZA MALLCU RONALDO

## Introducción
Este documento proporciona una descripción detallada sobre el funcionamiento de un parser diseñado según las reglas gramaticales especificadas en la notación [BNF](#bnf) (Backus-Naur Form) proporcionada. Este parser está implementado para analizar y validar codigo definido entre el grupo lo cual es de un porpocito general, teninedo nuestro propio archivo `.rvc` pudiendo escribir el codigo de acuerdo a las reglas de la gramática.

## Funcionamiento del Parser

El parser sigue una estructura jerárquica basada en la gramática definida, evaluando cada componente del programa de acuerdo a las reglas especificadas. A continuación se describen las principales funciones y estructuras del parser, así como las reglas gramaticales que se aplican en cada caso.

## BNF

### program:

<a href="https://postimg.cc/9D92vngL">
  <img src="https://i.postimg.cc/0yWNJgY1/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>

<p>&nbsp;</p>

```
program  ::= global_statements
```
no references

### global_statements:

<a href="https://postimg.cc/zbntzhHh">
  <img src="https://i.postimg.cc/cHzVcMsF/image.png" alt="image" width="270"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>

<p>&nbsp;</p>

```
global_statements::= global_statement*
```

referenced by:

* <a href="#program" style="color: white; text-decoration: underline;">program</a>


### global_statement:

<a href="https://postimg.cc/4mK8yjcZ">
  <img src="https://i.postimg.cc/rFj7nkFd/image.png" alt="image" width="270"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>

<p>&nbsp;</p>

```
global_statement ::= method_declaration
                    | statement
```

referenced by:

* <a href="#global_statements" style="color: white; text-decoration: underline;">global_statements</a>

### statement:

<a href="https://postimg.cc/FkYxfJ7G">
  <img src="https://i.postimg.cc/d3BNx2vM/image.png" alt="image" width="270"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>

<p>&nbsp;</p>

```
statement::= conditional_statement
           | loop_statement
           | method_call
           | expression
           | data_type_declaration
           | comment
           | literal
           | printer
```

referenced by:

* <a href="#conditional_statement" style="color: white; text-decoration: underline;">conditional_statement</a>
* <a href="#global_statement" style="color: white; text-decoration: underline;">global_statement</a>
* <a href="#loop_statement" style="color: white; text-decoration: underline;">loop_statement</a>
* <a href="#method_declaration" style="color: white; text-decoration: underline;">method_declaration</a>

### conditional_statement:

<a href="https://postimg.cc/gwMRv297">
  <img src="https://i.postimg.cc/FRvZXYTh/image.png" alt="image" width="870"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>

<p>&nbsp;</p>

```
conditional_statement::= 'if' '('boolean_expression')' '{'statement'}' 
         ( 'diffif' '('boolean_expression')' '{'statement'}')* 
         ( 'else' '{'statement'}')?
```

referenced by:

* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>

### loop_statement:

<a href="https://postimg.cc/yD8FQbmt">
  <img src="https://i.postimg.cc/2S426pVk/image.png" alt="image" width="870"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>

<p>&nbsp;</p>

```
loop_statement ::= ('while' '(' boolean_expression')' '{'statement*'}')
                | ('for' '('identifier 'in' list_expression')' '{'statement*'}')
```

referenced by:

* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>


### method_declaration:

<a href="https://postimg.cc/wRkmFX79">
  <img src="https://i.postimg.cc/QC2J8SDK/image.png" alt="image" width="980"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
method_declaration
         ::= 'method' name_metod '(' (parameter_list)? ')' '{' (statement)* (return_statement)? '}'
```

referenced by:

* <a href="#global_statement" style="color: white; text-decoration: underline;">global_statement</a>

### parameter_list:

<a href="https://postimg.cc/Wdz5F3Hd">
  <img src="https://i.postimg.cc/t4twcndN/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
parameter_list
         ::= data_type identifier ( ',' data_type identifier )*
```

referenced by:

* <a href="#method_declaration" style="color: white; text-decoration: underline;">method_declaration</a>

### method_call:

<a href="https://postimg.cc/Q9QmGWHy">
  <img src="https://i.postimg.cc/TwS4bVGf/image.png" alt="image" width="450"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
method_call::= name_metod '(' (argument_list)? ')'
```

referenced by:

* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>

### argument_list:

<a href="https://postimg.cc/grp4y47F">
  <img src="https://i.postimg.cc/v8gjsNLB/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
argument_list::= expression ( ',' expression )*
```

referenced by:

* <a href="#method_call" style="color: white; text-decoration: underline;">method_call</a>

### expression:

<a href="https://postimg.cc/PCCMf7CZ">
  <img src="https://i.postimg.cc/pLZcCbx0/image.png" alt="image" width="290"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
expression::= arithmetic_expression
           | boolean_expression
           | literal
           | list_expression
```

referenced by:

* <a href="#argument_list" style="color: white; text-decoration: underline;">argument_list</a>
* <a href="#printer" style="color: white; text-decoration: underline;">printer</a>
* <a href="#return_statement" style="color: white; text-decoration: underline;">return_statement</a>
* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>

### arithmetic_expression:

<a href="https://postimg.cc/3ypCmDCx">
  <img src="https://i.postimg.cc/5tK7cBXL/image.png" alt="image" width="290"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
arithmetic_expression::= int_arithmetic
                        | float_arithmetic
                        | string_arithmetic
                        | mixed_arithmetic
```

referenced by:

* <a href="#comparison_expression" style="color: white; text-decoration: underline;">comparison_expression</a>
* <a href="#expression" style="color: white; text-decoration: underline;">expression</a>

### int_arithmetic:

<a href="https://postimg.cc/K4mr3jKm">
  <img src="https://i.postimg.cc/gcZM56mL/image.png" alt="image" width="390"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
int_arithmetic::= digit ( operator digit )+
```

referenced by:

* <a href="#arithmetic_expression" style="color: white; text-decoration: underline;">arithmetic_expression</a>
* <a href="#data_type_declaration_int" style="color: white; text-decoration: underline;">data_type_declaration_int</a>

### float_arithmetic:

<a href="https://postimg.cc/mcx7Lgff">
  <img src="https://i.postimg.cc/wBM2k1wv/image.png" alt="image" width="450"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
float_arithmetic::= float_literal ( operator float_literal )+
```

referenced by:

* <a href="#arithmetic_expression" style="color: white; text-decoration: underline;">arithmetic_expression</a>
* <a href="#data_type_declaration_float" style="color: white; text-decoration: underline;">data_type_declaration_float</a>

### string_arithmetic:


<a href="https://postimg.cc/JybZGCMy">
  <img src="https://i.postimg.cc/CK2Nrw24/image.png" alt="image" width="450"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
string_arithmetic::= string_literal ( '+' string_literal )+
```

referenced by:

* <a href="#arithmetic_expression" style="color: white; text-decoration: underline;">arithmetic_expression</a>
* <a href="#data_type_declaration_str" style="color: white; text-decoration: underline;">data_type_declaration_str</a>

### mixed_arithmetic:

<a href="https://postimg.cc/Th2gYsSw">
  <img src="https://i.postimg.cc/gc8D4FHv/image.png" alt="image" width="650"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
mixed_arithmetic ::= digit operator float_literal ( operator ( digit | float_literal))*
                    | float_literal operator digit ( operator ( digit | float_literal))*
```

referenced by:

* <a href="#arithmetic_expression" style="color: white; text-decoration: underline;">arithmetic_expression</a>

### boolean_expression:

<a href="https://postimg.cc/KKygXpBm">
  <img src="https://i.postimg.cc/tgsdxLgn/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
boolean_expression
         ::= comparison_expression ( boolean_operator comparison_expression )*
```

referenced by:

* <a href="#conditional_statement" style="color: white; text-decoration: underline;">conditional_statement</a>
* <a href="#data_type_declaration_bool" style="color: white; text-decoration: underline;">data_type_declaration_bool</a>
* <a href="#expression" style="color: white; text-decoration: underline;">expression</a>
* <a href="#loop_statement" style="color: white; text-decoration: underline;">loop_statement</a>

### comparison_expression:

<a href="https://postimg.cc/xJfJ8k67">
  <img src="https://i.postimg.cc/vB9WPffQ/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
comparison_expression
         ::= literal_expression ( relational_operator literal_expression )*
           | arithmetic_expression ( relational_operator arithmetic_expression )*
           | boolean_literal
```

referenced by:

* <a href="#boolean_expression" style="color: white; text-decoration: underline;">boolean_expression</a>

### literal_expression:

<a href="https://postimg.cc/5jkY92L7">
  <img src="https://i.postimg.cc/sD3pcBXr/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
literal_expression::= integer_expression
                    | float_expression
                    | string_expression
                    | mixed_expression
                    | bool_expression
```

referenced by:

* <a href="#comparison_expression" style="color: white; text-decoration: underline;">comparison_expression</a>

### integer_expression:

<a href="https://postimg.cc/3y7Wf4QR">
  <img src="https://i.postimg.cc/pd58Tz0D/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
integer_expression
         ::= integer_literal ( relational_operator integer_literal )+
```

referenced by:

* <a href="#literal_expression" style="color: white; text-decoration: underline;">literal_expression</a>

### float_expression:

<a href="https://postimg.cc/cgLCZpHG">
  <img src="https://i.postimg.cc/mDQH3RWk/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
float_expression
         ::= float_literal ( relational_operator float_literal )+
```

referenced by:

* <a href="#literal_expression" style="color: white; text-decoration: underline;">literal_expression</a>

### string_expression:

<a href="https://postimg.cc/Lh75K0FG">
  <img src="https://i.postimg.cc/7YP2zpsP/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
string_expression
         ::= string_literal ( relational_operator string_literal )+
```

referenced by:

* <a href="#literal_expression" style="color: white; text-decoration: underline;">literal_expression</a>

### mixed_expression:

<a href="https://postimg.cc/tsd46qnH">
  <img src="https://i.postimg.cc/7ZdGpbxb/image.png" alt="image" width="850"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
mixed_expression
         ::= ( integer_literal relational_operator float_literal | float_literal relational_operator integer_literal ) ( relational_operator ( integer_literal | float_literal ) )*
```

referenced by:

* <a href="#literal_expression" style="color: white; text-decoration: underline;">literal_expression</a>

### bool_expression:

<a href="https://postimg.cc/kBF0FBsw">
  <img src="https://i.postimg.cc/yYw7KRZq/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
bool_expression
         ::= boolean_literal ( relational_operator boolean_literal )+
```

referenced by:

* <a href="#literal_expression" style="color: white; text-decoration: underline;">literal_expression</a>

### return_statement:

<a href="https://postimg.cc/dLBfZgQx">
  <img src="https://i.postimg.cc/NjqvSw3B/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
return_statement::= 'return' ( expression | literal )
```

referenced by:

* <a href="#method_declaration" style="color: white; text-decoration: underline;">method_declaration</a>

### data_type_declaration:

<a href="https://postimg.cc/Bj7wMDK5">
  <img src="https://i.postimg.cc/cJdy1B1z/image.png" alt="image" width="450"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type_declaration::= data_type_declaration_int
                        | data_type_declaration_float
                        | data_type_declaration_bool
                        | data_type_declaration_str
                        | data_type_declaration_list
```

referenced by:

* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>

### data_type_declaration_int:

<a href="https://postimg.cc/mccpqDYv">
  <img src="https://i.postimg.cc/9f13DwrQ/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type_declaration_int
         ::= 'int' identifier ( '=' ( int_arithmetic | integer_literal ) )?
```

referenced by:

* <a href="#data_type_declaration" style="color: white; text-decoration: underline;">data_type_declaration</a>

### data_type_declaration_float:

<a href="https://postimg.cc/phzGW61k">
  <img src="https://i.postimg.cc/sX0tcFPF/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type_declaration_float
         ::= 'float' identifier ( '=' ( float_arithmetic | float_literal ) )?
```

referenced by:

* <a href="#data_type_declaration" style="color: white; text-decoration: underline;">data_type_declaration</a>

### data_type_declaration_bool:


<a href="https://postimg.cc/vDdMrFMY">
  <img src="https://i.postimg.cc/ryDmLMPr/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type_declaration_bool
         ::= 'bool' identifier ( '=' boolean_expression )?
```

referenced by:

* <a href="#data_type_declaration" style="color: white; text-decoration: underline;">data_type_declaration</a>

### data_type_declaration_str:


<a href="https://postimg.cc/rDb2z7QL">
  <img src="https://i.postimg.cc/PqfXVjDN/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type_declaration_str
         ::= 'str' identifier ( '=' ( string_arithmetic | string_literal ) )?
```

referenced by:

* <a href="#data_type_declaration" style="color: white; text-decoration: underline;">data_type_declaration</a>

### data_type_declaration_list:

<a href="https://postimg.cc/yJZ4Rnb3">
  <img src="https://i.postimg.cc/0Nt8LBGn/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type_declaration_list
         ::= 'list' identifier ( '=' list_expression )?
```

referenced by:

* <a href="#data_type_declaration" style="color: white; text-decoration: underline;">data_type_declaration</a>

### data_type:

<a href="https://postimg.cc/DmFkWrdm">
  <img src="https://i.postimg.cc/jqnsFvD6/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
data_type::= 'int'
           | 'float'
           | 'bool'
           | 'str'
           | 'list'
```

referenced by:

* <a href="#parameter_list" style="color: white; text-decoration: underline;">parameter_list</a>

### list_expression:

<a href="https://postimg.cc/mPKKTCgL">
  <img src="https://i.postimg.cc/N00t7xhH/image.png" alt="image" width="450"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
list_expression
         ::= '[' ( literal ( ',' literal )* )? ']'
```

referenced by:

* <a href="#data_type_declaration_list" style="color: white; text-decoration: underline;">data_type_declaration_list</a>
* <a href="#expression" style="color: white; text-decoration: underline;">expression</a>
* <a href="#loop_statement" style="color: white; text-decoration: underline;">loop_statement</a>

### operator:

<a href="https://postimg.cc/w7vrJ0tx">
  <img src="https://i.postimg.cc/ncYfyWpm/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
operator ::= '+'
           | '-'
           | '*'
           | '/'
```

referenced by:

* <a href="#float_arithmetic" style="color: white; text-decoration: underline;">float_arithmetic</a>
* <a href="#int_arithmetic" style="color: white; text-decoration: underline;">int_arithmetic</a>
* <a href="#mixed_arithmetic" style="color: white; text-decoration: underline;">mixed_arithmetic</a>

### relational_operator:

<a href="https://postimg.cc/k6sjRbqt">
  <img src="https://i.postimg.cc/rsX7b1Hg/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
relational_operator
         ::= '<'
           | '>'
           | '<='
           | '>='
           | '=='
           | '/='
```

referenced by:

* <a href="#bool_expression" style="color: white; text-decoration: underline;">bool_expression</a>
* <a href="#comparison_expression" style="color: white; text-decoration: underline;">comparison_expression</a>
* <a href="#float_expression" style="color: white; text-decoration: underline;">float_expression</a>
* <a href="#integer_expression" style="color: white; text-decoration: underline;">integer_expression</a>
* <a href="#mixed_expression" style="color: white; text-decoration: underline;">mixed_expression</a>
* <a href="#string_expression" style="color: white; text-decoration: underline;">string_expression</a>

### boolean_operator:

<a href="https://postimg.cc/f3h5HKb9">
  <img src="https://i.postimg.cc/7L5cZt8m/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
boolean_operator
         ::= '&&'
           | '||'
```

referenced by:

* <a href="#boolean_expression" style="color: white; text-decoration: underline;">boolean_expression</a>

### literal:

<a href="https://postimg.cc/8JXRVBTw">
  <img src="https://i.postimg.cc/C5SmkmXy/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
literal  ::= integer_literal
           | float_literal
           | boolean_literal
           | string_literal
```

referenced by:

* <a href="#expression" style="color: white; text-decoration: underline;">expression</a>
* <a href="#list_expression" style="color: white; text-decoration: underline;">list_expression</a>
* <a href="#return_statement" style="color: white; text-decoration: underline;">return_statement</a>
* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>

### integer_literal:

<a href="https://postimg.cc/XGPCPjgz">
  <img src="https://i.postimg.cc/MH6mvXfq/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
integer_literal
         ::= [0-9]+
```

referenced by:

* <a href="#data_type_declaration_int" style="color: white; text-decoration: underline;">data_type_declaration_int</a>
* <a href="#integer_expression" style="color: white; text-decoration: underline;">integer_expression</a>
* <a href="#literal" style="color: white; text-decoration: underline;">literal</a>
* <a href="#mixed_expression" style="color: white; text-decoration: underline;">mixed_expression</a>

### float_literal:

<a href="https://postimg.cc/gxK6WqDR">
  <img src="https://i.postimg.cc/YC5NQdLX/image.png" alt="image" width="450"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
float_literal
         ::= [0-9]+ '.' [0-9]+
```

referenced by:

* <a href="#data_type_declaration_float" style="color: white; text-decoration: underline;">data_type_declaration_float</a>
* <a href="#float_arithmetic" style="color: white; text-decoration: underline;">float_arithmetic</a>
* <a href="#float_expression" style="color: white; text-decoration: underline;">float_expression</a>
* <a href="#literal" style="color: white; text-decoration: underline;">literal</a>
* <a href="#mixed_arithmetic" style="color: white; text-decoration: underline;">mixed_arithmetic</a>
* <a href="#mixed_expression" style="color: white; text-decoration: underline;">mixed_expression</a>

### boolean_literal:

<a href="https://postimg.cc/PPsLvFVY">
  <img src="https://i.postimg.cc/vHD5jJWq/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
boolean_literal::= 'true'
                | 'false'
```

referenced by:

* <a href="#bool_expression" style="color: white; text-decoration: underline;">bool_expression</a>
* <a href="#comparison_expression" style="color: white; text-decoration: underline;">comparison_expression</a>
* <a href="#literal" style="color: white; text-decoration: underline;">literal</a>

### string_literal:

<a href="https://postimg.cc/JsRGPHLh">
  <img src="https://i.postimg.cc/dVyrQGNG/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
string_literal::= '"' (identifier)* '"'
```

referenced by:

* <a href="#data_type_declaration_str" style="color: white; text-decoration: underline;">data_type_declaration_str</a>
* <a href="#literal" style="color: white; text-decoration: underline;">literal</a>
* <a href="#string_arithmetic" style="color: white; text-decoration: underline;">string_arithmetic</a>
* <a href="#string_expression" style="color: white; text-decoration: underline;">string_expression</a>

### name_metod:

<a href="https://postimg.cc/ctQLc0kY">
  <img src="https://i.postimg.cc/WzyqT4M9/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
name_metod
         ::= identifier
```

referenced by:

* <a href="#method_call" style="color: white; text-decoration: underline;">method_call</a>
* <a href="#method_declaration" style="color: white; text-decoration: underline;">method_declaration</a>

### identifier:

<a href="https://postimg.cc/5jvyJS90">
  <img src="https://i.postimg.cc/PfVwmSBm/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
identifier
         ::= letter ( letter | digit )*
```

referenced by:

* <a href="#data_type_declaration_bool" style="color: white; text-decoration: underline;">data_type_declaration_bool</a>
* <a href="#data_type_declaration_float" style="color: white; text-decoration: underline;">data_type_declaration_float</a>
* <a href="#data_type_declaration_int" style="color: white; text-decoration: underline;">data_type_declaration_int</a>
* <a href="#data_type_declaration_list" style="color: white; text-decoration: underline;">data_type_declaration_list</a>
* <a href="#data_type_declaration_str" style="color: white; text-decoration: underline;">data_type_declaration_str</a>
* <a href="#loop_statement" style="color: white; text-decoration: underline;">loop_statement</a>
* <a href="#name_metod" style="color: white; text-decoration: underline;">name_metod</a>
* <a href="#parameter_list" style="color: white; text-decoration: underline;">methoparameter_listd_call</a>
* <a href="#string_literal" style="color: white; text-decoration: underline;">string_literal</a>

### letter:

<a href="https://postimg.cc/LJy6DyDy">
  <img src="https://i.postimg.cc/5tJYxTCM/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
letter   ::= [a-zA-Z]+
```

referenced by:

* <a href="#identifier" style="color: white; text-decoration: underline;">identifier</a>

### digit:

<a href="https://postimg.cc/67g6JyDn">
  <img src="https://i.postimg.cc/V6LJttgG/image.png" alt="image" width="250"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
digit    ::= [0-9]+
```

referenced by:

* <a href="#identifier" style="color: white; text-decoration: underline;">identifier</a>
* <a href="#int_arithmetic" style="color: white; text-decoration: underline;">int_arithmetic</a>
* <a href="#mixed_arithmetic" style="color: white; text-decoration: underline;">mixed_arithmetic</a>

### comment:

<a href="https://postimg.cc/PpDXtdjc">
  <img src="https://i.postimg.cc/d1HLj0Sq/image.png" alt="image" width="350"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
comment  ::= '#' .*
           | '##' .* '##'
```

referenced by:

* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>

### printer:

<a href="https://postimg.cc/RJQvHGqL">
  <img src="https://i.postimg.cc/BbRbRV0k/image.png" alt="image" width="550"style="filter: drop-shadow(1px 1px 20px white);"/>
</a>
<p>&nbsp;</p>

```
printer ::= 'print' '(' (expression)* ')'
```

referenced by:

* <a href="#statement" style="color: white; text-decoration: underline;">statement</a>
<p>&nbsp;</p>

**Nota:** Si quiere saber las sobre lo que hace cada parser se implemento docs en cada clase para un mejor entendimiento y saber que es lo que realiza cada parte del BNF junto con el AST.

# Herramientas Utilizadas

Durante el diseño e implementación del parser, se utilizaron las siguientes herramientas:

## Gestión de Proyectos

[GitHub](https://github.com/programming-language-ravenclaw/Ravenclaw): Utilizado para el control de versiones y la colaboración en el desarrollo del proyecto.

[ClickUp](https://app.clickup.com/9013294707/v/b/6-901303958857-2): Utilizado para la gestión de tareas y el seguimiento del progreso del proyecto.

## Desarrollo e Implementacion

[Haskell](https://www.haskell.org/): Lenguaje de programación utilizado para implementar el parser.

[GHCI](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html): Intérprete interactivo de Haskell utilizado para pruebas y depuración del código.

[Parsec](https://hackage.haskell.org/package/parsec): Biblioteca de Haskell utilizada para la construcción del parser.

[stack](https://docs.haskellstack.org/en/stable/): Herramienta de construcción y gestión de proyectos en Haskell.

[Hspec](https://hackage.haskell.org/package/hspec): Framework de pruebas para Haskell utilizado para definir y ejecutar casos de prueba, asegurando que el parser funcione correctamente de acuerdo a las especificaciones.

[Visual Studio Code](https://visualstudio.microsoft.com/es/) como entorno de desarrollo integrado (IDE).

## Diagramas y Visualización

[Railroad Diagram Generator](https://rr.red-dove.com/ui) para la generación del diagrama del BNF.
