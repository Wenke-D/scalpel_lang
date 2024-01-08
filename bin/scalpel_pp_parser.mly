%{
  open Scalpel_program
%}

%token <string> IDENTIFIER
%token <string> LITERAL_VALUE

// top-level declarations
%token TYPE_PRELUDE
%token CLASS_PRELUDE
%token MUTABLE_PRELUDE
%token FROZEN_PRELUDE
%token STATIC_PRELUDE
%token TYPING_PRELUDE

// instruction
%token TRUE_BRANCH_PRELUDE
%token FALSE_BRANCH_PRELUDE
%token LOOP_PRELUDE
%token RETURN_PRELUDE

// %token NATIVE_MARK


// operators
%token ASSIGN

// type symbol
%token <int>NON_NEGATIVE_INTEGER



// file structure control
%token COMMA
%token DOT

%token TYPE_ARGUMENT_OPENOR
%token TYPE_ARGUMENT_CLOSER

%token VALUE_ARGUMENT_OPENOR
%token VALUE_ARGUMENT_CLOSER

%token INSTRUCTIONS_OPENOR
%token INSTRUCTIONS_CLOSER


%token EOF

%start program

%type <Scalpel_program.definition> program
%type <Scalpel_type.expression> type_identifier
%type <Scalpel_modifier.typing> typing

%%

program:
| list = nonempty_list(definition) EOF
    {
        list
    }
| error { failwith "error" }
;

definition:
| t = type_definition { Type t }
| f = function_definition { Function f }
| c = class_definition { Class c }
;


type_definition:
| TYPE_PRELUDE id=IDENTIFIER ASSIGN e=type_expression 
    {
        Scalpel_type.{identifier=id; expression=e}
    }
;


type_identifier:
    id = IDENTIFIER {Scalpel_type.Identifier id}
;


type_symbol: 
    n = NON_NEGATIVE_INTEGER
    { Scalpel_type.Symbol n }
;


type_specialisation:
    id = IDENTIFIER
    args = type_expression_list
    {
        Scalpel_type.Specialization(id, args)
    }
;


type_expression:
|    s = type_symbol { s }
|   id = type_identifier    { id }
|    e = type_specialisation { e }
;


type_expression_list:
    TYPE_ARGUMENT_OPENOR
        types=separated_list(COMMA, type_expression)
    TYPE_ARGUMENT_CLOSER
    { types }
;

typing:
|       { Scalpel_modifier.Inference }
|   TYPING_PRELUDE id=IDENTIFIER
    { Scalpel_modifier.Identifier id }
;


instructions:
|   INSTRUCTIONS_OPENOR
        instructions = list(instruction)
    INSTRUCTIONS_CLOSER
    {instructions}


function_definition:
|   m=mutability
    id=IDENTIFIER
    parameters=parameter_list
    return_type = typing
    body = function_body
    {
        Scalpel_function.{
            mutability = m;
            identifier = id;
            parameters;
            body;
            return_type
        }
    }
;

function_body:
| { Scalpel_function.Native }
| instructions = instructions
    { Scalpel_function.Instructions instructions }
;


mutability:
|   MUTABLE_PRELUDE {Mutable}
|   FROZEN_PRELUDE {Frozen}
|   STATIC_PRELUDE {Static}
;

parameter_list:
| 
    VALUE_ARGUMENT_OPENOR
        parameters=separated_list(COMMA, parameter)
    VALUE_ARGUMENT_CLOSER
    {
        parameters
    }
;

parameter:
    id=IDENTIFIER
    t = typing
    {
       Scalpel_function.{identifier = id; typename = t}
    }
;

value_expression:
| id = IDENTIFIER {Variable id}
| value = LITERAL_VALUE {Literal value}
| c = construction {Scalpel_value.Construction c}
;

value_expression_chain:
| e = separated_nonempty_list(DOT, value_expression)
    {
        e
    }
;

value_expression_list:
    VALUE_ARGUMENT_OPENOR
    list = separated_list(COMMA, value_expression_chain)
    VALUE_ARGUMENT_CLOSER
    { list }

construction:
    typename=IDENTIFIER
    arguments=value_expression_list
    {
        {
            onHeap = false; 
            typename = typename; 
            arguments
        }
    }
;


variable:
|   m = mutability id = IDENTIFIER typename=typing
    {
       Scalpel_value.{
            mutability = m;
            identifier = id;
            typename
        }
    }
;


instruction:
|   s = variable
    ASSIGN
    v = value_expression_chain { Initialization(s, v) }

|   id = IDENTIFIER
    ASSIGN
    v = value_expression_chain { Assignment(id, v) }

|   TRUE_BRANCH_PRELUDE
    predicate = value_expression_chain
    true_branch = instructions
    FALSE_BRANCH_PRELUDE
    false_branch = instructions
    {
        Scalpel_instruction.Branching ({
            predicate;
            true_branch;
            false_branch
        })
    }

|   TRUE_BRANCH_PRELUDE
    predicate = value_expression_chain
    true_branch = instructions
    {
        Scalpel_instruction.Branching {
            predicate;
            true_branch;
            false_branch = []
        }
    }

|   LOOP_PRELUDE
    predicate = value_expression_chain
    body = instructions
    {
        Scalpel_instruction.Loop {
            predicate;
            body;
        }
    }

|   RETURN_PRELUDE
    expr = value_expression_chain
    {
        Scalpel_instruction.Return expr
    }
;


class_definition:
    CLASS_PRELUDE
    id=IDENTIFIER
    attributes = attributes
    methods = methods
    {
        Scalpel_class.{
            identifier = id;
            attributes;
            methods = methods
        }
    }
;

attribute:
    mut = mutability
    typename=IDENTIFIER
    id=IDENTIFIER
    {
        Scalpel_class.{
            mutability = mut;
            typename;
            identifier = id
        }
    }

attributes:
    VALUE_ARGUMENT_OPENOR
        list = separated_list(COMMA, attribute)
    VALUE_ARGUMENT_CLOSER
    {
        list
    }
;


methods:
    INSTRUCTIONS_OPENOR
        list = list(function_definition)
    INSTRUCTIONS_CLOSER
    {list}